module ElmParser exposing
  ( parse
  )

import Char
import Set exposing (Set)

import Parser as P exposing (..)
import Parser.LanguageKit as LK

import ParserUtils exposing (..)
import LangParserUtils exposing (..)
import BinaryOperatorParser exposing (..)
import Utils

import Lang exposing (..)
import Info exposing (..)
import ElmLang
import TopLevelExp exposing (TopLevelExp, fuseTopLevelExps)

import FastParser

--==============================================================================
--= Helpers
--==============================================================================

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

genericEmptyList
  :  { combiner : WS -> WS -> list
     }
  -> ParserI list
genericEmptyList { combiner } =
  paddedBefore combiner <|
    trackInfo <|
      succeed identity
        |. symbol "["
        |= spaces
        |. symbol "]"

genericNonEmptyList
  :  { item : Parser elem
     , combiner : WS -> List elem -> WS -> list
     }
  -> ParserI list
genericNonEmptyList { item, combiner }=
  lazy <| \_ ->
    let
      anotherItem =
        delayedCommit spaces <|
          succeed identity
            |. symbol ","
            |= item
    in
      paddedBefore
        ( \wsBefore (members, wsBeforeEnd) ->
            combiner wsBefore members wsBeforeEnd
        )
        ( trackInfo <|
            succeed (\e es ws -> (e :: es, ws))
              |. symbol "["
              |= item
              |= repeat zeroOrMore anotherItem
              |= spaces
              |. symbol "]"
        )

genericList
  :  { item : Parser elem
     , emptyCombiner : WS -> WS -> list
     , nonEmptyCombiner : WS -> List elem -> WS -> list
     }
  -> ParserI list
genericList { item, emptyCombiner, nonEmptyCombiner } =
  lazy <| \_ ->
    oneOf
      [ try <|
          genericEmptyList
            { combiner = emptyCombiner
            }
      , lazy <| \_ ->
          genericNonEmptyList
            { item = item
            , combiner = nonEmptyCombiner
            }
      ]

--==============================================================================
--= Identifiers
--==============================================================================

keywords : Set String
keywords =
  Set.fromList
    [ "let"
    , "in"
    , "case"
    , "of"
    , "if"
    , "then"
    , "else"
    ]

isRestChar : Char -> Bool
isRestChar char =
  Char.isLower char ||
  Char.isUpper char ||
  Char.isDigit char ||
  char == '_'

littleIdentifier : ParserI Ident
littleIdentifier =
  trackInfo <|
    LK.variable
      Char.isLower
      isRestChar
      keywords

bigIdentifier : ParserI Ident
bigIdentifier =
  trackInfo <|
    LK.variable
      Char.isUpper
      isRestChar
      keywords

symbolIdentifier : ParserI Ident
symbolIdentifier =
  trackInfo <|
    keep oneOrMore ElmLang.isSymbol

--==============================================================================
--= Numbers
--==============================================================================

num : ParserI Num
num =
  let
    sign =
      oneOf
        [ succeed (-1)
            |. symbol "-"
        , succeed 1
        ]
  in
    try <|
      inContext "number" <|
        trackInfo <|
          succeed (\s n -> s * n)
            |= sign
            |= float

--==============================================================================
-- Base Values
--==============================================================================

--------------------------------------------------------------------------------
-- Bools
--------------------------------------------------------------------------------

bool : ParserI EBaseVal
bool =
  inContext "bool" <|
    trackInfo <|
      map EBool <|
        oneOf
          [ token "True" True
          , token "False" False
          ]

--------------------------------------------------------------------------------
-- Strings
--------------------------------------------------------------------------------

singleLineString : ParserI EBaseVal
singleLineString =
  inContext "single-line string" <|
    trackInfo <|
      succeed (EString "\"")
        |. symbol "\""
        |= keep zeroOrMore (\c -> c /= '\"')
        |. symbol "\""

multiLineString : ParserI EBaseVal
multiLineString =
  inContext "multi-line string" <|
    trackInfo <|
      map (EString "\"\"\"") <|
        inside "\"\"\""

--------------------------------------------------------------------------------
-- General Base Values
--------------------------------------------------------------------------------

baseValue : ParserI EBaseVal
baseValue =
  inContext "base value" <|
    oneOf
      [ multiLineString
      , singleLineString
      , bool
      ]

--==============================================================================
-- Patterns
--==============================================================================

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

variablePattern : Parser Pat
variablePattern =
  inContext "variable pattern" <|
    mapPat_ <|
      paddedBefore
        ( \ws name ->
            PVar ws name noWidgetDecl
        )
        littleIdentifier

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

constantPattern : Parser Pat
constantPattern =
  inContext "constant pattern" <|
    mapPat_ <|
      paddedBefore PConst num

--------------------------------------------------------------------------------
-- Base Values
--------------------------------------------------------------------------------

baseValuePattern : Parser Pat
baseValuePattern =
  inContext "base value pattern" <|
    mapPat_ <|
      paddedBefore PBase baseValue

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

listPattern : Parser Pat
listPattern =
  inContext "list pattern" <|
    lazy <| \_ ->
      mapPat_ <|
        genericList
          { item =
              pattern
          , emptyCombiner =
              \wsBefore wsInside ->
                PList wsBefore [] space0 Nothing wsInside
          , nonEmptyCombiner =
              \wsBefore members wsBeforeEnd ->
                PList wsBefore members space0 Nothing wsBeforeEnd
          }

--------------------------------------------------------------------------------
-- As-Patterns (@-Patterns)
--------------------------------------------------------------------------------

asPattern : Parser Pat
asPattern =
  inContext "as pattern" <|
    lazy <| \_ ->
      mapPat_ <|
        paddedBefore
          ( \wsBefore (identifier, wsBeforeAt, pat) ->
              PAs wsBefore identifier.val wsBeforeAt pat
          )
          ( trackInfo <|
              succeed (,,)
                |. symbol "("
                |= littleIdentifier
                |= spaces
                |. keywordWithSpace "as"
                |= pattern
                |. symbol ")"
          )

--------------------------------------------------------------------------------
-- General Patterns
--------------------------------------------------------------------------------

pattern : Parser Pat
pattern =
  inContext "pattern" <|
    oneOf
      [ lazy <| \_ -> listPattern
      , lazy <| \_ -> asPattern
      , constantPattern
      , baseValuePattern
      , variablePattern
      ]

--==============================================================================
-- Operators
--==============================================================================

--------------------------------------------------------------------------------
-- Built-In Operators
--------------------------------------------------------------------------------
-- Helpful: http://faq.elm-community.org/operators.html

builtInPrecedenceList : List (Int, List String, List String)
builtInPrecedenceList =
  [ ( 9
    , [">>"]
    , ["<<"]
    )
  , ( 8
    , []
    , ["^"]
    )
  , ( 7
    , ["*", "/", "//", "%", "rem"]
    , []
    )
  , ( 6
    , ["+", "-"]
    , []
    )
  , ( 5
    , []
    , ["++", "::"]
    )
  , ( 4
    , ["==", "/=", "<", ">", "<=", ">="]
    , []
    )
  , ( 3
    , []
    , ["&&"]
    )
  , ( 2
    , []
    , ["||"]
    )
  , ( 1
    , []
    , []
    )
  , ( 0
    , ["|>"]
    , ["<|"]
    )
  ]

builtInPrecedenceTable : PrecedenceTable
builtInPrecedenceTable =
  buildPrecedenceTable builtInPrecedenceList

builtInOperators : List Ident
builtInOperators =
  List.concatMap
    (\(_, ls, rs) -> ls ++ rs)
    builtInPrecedenceList

opFromIdentifier : Ident -> Maybe Op_
opFromIdentifier identifier =
  case identifier of
    "pi" ->
      Just Pi
    "empty" ->
      Just DictEmpty
    "cos" ->
      Just Cos
    "sin" ->
      Just Sin
    "arccos" ->
      Just ArcCos
    "arcsin" ->
      Just ArcSin
    "floor" ->
      Just Floor
    "ceiling" ->
      Just Ceil
    "round" ->
      Just Round
    "toString" ->
      Just ToStr
    "sqrt" ->
      Just Sqrt
    "explode" ->
      Just Explode
    "+" ->
      Just Plus
    "-" ->
      Just Minus
    "*" ->
      Just Mult
    "/" ->
      Just Div
    "<" ->
      Just Lt
    "=" ->
      Just Eq
    "mod" ->
      Just Mod
    "pow" ->
      Just Pow
    "arctan2" ->
      Just ArcTan2
    "insert" ->
      Just DictInsert
    "get" ->
      Just DictGet
    "remove" ->
      Just DictRemove
    "debug" ->
      Just DebugLog
    _ ->
      Nothing

--------------------------------------------------------------------------------
-- Operator Parsing
--------------------------------------------------------------------------------

operator : ParserI Operator
operator =
  paddedBefore (,) symbolIdentifier

--==============================================================================
-- Expressions
--==============================================================================

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

constantExpression : Parser Exp
constantExpression =
  inContext "constant expression" <|
    mapExp_ <|
      paddedBefore
        ( \wsBefore num ->
            EConst
              wsBefore
              num
              (dummyLocWithDebugInfo unann num)
              (withDummyInfo NoWidgetDecl)
        )
        num

--------------------------------------------------------------------------------
-- Base Values
--------------------------------------------------------------------------------

baseValueExpression : Parser Exp
baseValueExpression =
  inContext "base value expression" <|
    mapExp_ <|
      paddedBefore EBase baseValue

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

variableExpression : Parser Exp
variableExpression =
  mapExp_ <|
    paddedBefore EVar littleIdentifier

--------------------------------------------------------------------------------
-- Functions (lambdas)
--------------------------------------------------------------------------------

function : Parser Exp
function =
  inContext "function" <|
    lazy <| \_ ->
      mapExp_ <|
        paddedBefore
          ( \wsBefore (parameters, body) ->
              EFun wsBefore parameters body space0
          )
          ( trackInfo <|
              succeed (,)
                |. symbol "\\"
                |= repeat oneOrMore pattern
                |. spaces
                |. symbol "->"
                |= expression
          )

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

list : Parser Exp
list =
  inContext "list" <|
    lazy <| \_ ->
      mapExp_ <|
        genericList
          { item =
              expression
          , emptyCombiner =
              \wsBefore wsInside ->
                EList wsBefore [] space0 Nothing wsInside
          , nonEmptyCombiner =
              \wsBefore members wsBeforeEnd ->
                EList wsBefore members space0 Nothing wsBeforeEnd
          }

--------------------------------------------------------------------------------
-- Conditionals
--------------------------------------------------------------------------------

conditional : Parser Exp
conditional =
  inContext "conditional" <|
    lazy <| \_ ->
      mapExp_ <|
        paddedBefore
          ( \wsBefore (condition, trueBranch, falseBranch) ->
              EIf wsBefore condition trueBranch falseBranch space0
          )
          ( trackInfo <|
              delayedCommit (keywordWithSpace "if") <|
                succeed (,,)
                  |= expression
                  |. spaces
                  |. keywordWithSpace "then"
                  |= expression
                  |. spaces
                  |. keywordWithSpace "else"
                  |= expression
          )

--------------------------------------------------------------------------------
-- Case Expressions
--------------------------------------------------------------------------------

caseExpression : Parser Exp
caseExpression =
  inContext "case expression" <|
    lazy <| \_ ->
      let
        branch =
          paddedBefore
            ( \wsBefore (p, e) ->
                Branch_ wsBefore p e space0
            )
            ( trackInfo <|
                succeed (,)
                  |= pattern
                  |. spaces
                  |. symbol "->"
                  |= expression
                  |. symbol ";"
            )
      in
        mapExp_ <|
          paddedBefore
            ( \wsBefore (examinedExpression, branches) ->
                ECase wsBefore examinedExpression branches space0
            )
            ( trackInfo <|
                delayedCommit (keywordWithSpace "case") <|
                  succeed (,)
                    |= expression
                    |. spaces
                    |. keywordWithSpace "of"
                    |= repeat oneOrMore branch
            )

--------------------------------------------------------------------------------
-- Let Bindings
--------------------------------------------------------------------------------

letBinding : Parser Exp
letBinding =
  inContext "let binding" <|
    lazy <| \_ ->
      mapExp_ <|
        paddedBefore
          ( \wsBefore (name, binding, body) ->
              ELet wsBefore Let False name binding body space0
          )
          ( trackInfo <|
              delayedCommit (keywordWithSpace "let") <|
                succeed (,,)
                  |= pattern
                  |. spaces
                  |. symbol "="
                  |= expression
                  |. spaces
                  |. keywordWithSpace "in"
                  |= expression
          )

--------------------------------------------------------------------------------
-- Comments
--------------------------------------------------------------------------------

lineComment : Parser Exp
lineComment =
  inContext "line comment" <|
    mapExp_ <|
      paddedBefore
        ( \wsBefore (text, expAfter) ->
            EComment wsBefore text expAfter
        )
        ( trackInfo <|
            succeed (,)
              |. symbol "--"
              |= keep zeroOrMore (\c -> c /= '\n')
              |. symbol "\n"
              |= expression
        )

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

option : Parser Exp
option =
  inContext "option" <|
    mapExp_ <|
      lazy <| \_ ->
        delayedCommitMap
          ( \wsStart (open, opt, wsMid, val, rest) ->
              withInfo
                (EOption wsStart opt wsMid val rest)
                open.start
                val.end
          )
          ( spaces )
          ( succeed (,,,,)
              |= trackInfo (symbol "#")
              |. spaces
              |= trackInfo
                   ( keep zeroOrMore <| \c ->
                       c /= '\n' && c /= ' ' && c /= ':'
                   )
              |. symbol ":"
              |= spaces
              |= trackInfo
                   ( keep zeroOrMore <| \c ->
                       c /= '\n'
                   )
              |. symbol "\n"
              |= expression
          )

--------------------------------------------------------------------------------
-- Parentheses
--------------------------------------------------------------------------------

parens : Parser Exp
parens =
  inContext "parentheses" <|
    mapExp_ <|
      lazy <| \_ ->
        paddedBefore
          ( \wsBefore (innerExpression, wsBeforeEnd) ->
              EParens wsBefore innerExpression wsBeforeEnd
          )
          ( trackInfo <|
              succeed (,)
                |. symbol "("
                |= expression
                |= spaces
                |. symbol ")"
          )

--------------------------------------------------------------------------------
-- General Expressions
--------------------------------------------------------------------------------

-- Not a function application nor a binary operator
simpleExpression : Parser Exp
simpleExpression =
  lazy <| \_ ->
    oneOf
      [ constantExpression
      , baseValueExpression
      , lazy <| \_ -> function
      , lazy <| \_ -> list
      , lazy <| \_ -> conditional
      , lazy <| \_ -> caseExpression
      , lazy <| \_ -> letBinding
      , lazy <| \_ -> lineComment
      , lazy <| \_ -> option
      , lazy <| \_ -> parens
      -- , lazy <| \_ -> typeCaseExpression
      -- , lazy <| \_ -> typeAlias
      -- , lazy <| \_ -> typeDeclaration
      -- , lazy <| \_ -> typeAnnotation
      , variableExpression
      ]

-- Either a simple expression or a function application
simpleExpressionWithPossibleArguments : Parser Exp
simpleExpressionWithPossibleArguments =
  let
    combine : Exp -> List Exp -> Exp
    combine first rest =
      -- If there are no arguments, then we do not have a function application,
      -- so just return the first expression. Otherwise, build a function
      -- application.
      case Utils.maybeLast rest of
        -- rest is empty
        Nothing ->
          first

        -- rest is non-empty
        Just last ->
          let
            e_ =
              exp_ <|
                let
                  default =
                    EApp space0 first rest space0
                in
                  case first.val.e__ of
                    EVar wsBefore identifier ->
                      case opFromIdentifier identifier of
                        Just op_ ->
                          EOp
                            wsBefore
                            (withInfo op_ first.start first.end)
                            rest
                            space0

                        Nothing ->
                          default
                    _ ->
                      default
          in
            withInfo e_ first.start last.end
  in
    lazy <| \_ ->
      succeed combine
        |= simpleExpression
        |= repeat zeroOrMore simpleExpression

expression : Parser Exp
expression =
  inContext "expression" <|
    lazy <| \_ ->
      binaryOperator
        { precedenceTable =
            builtInPrecedenceTable
        , minimumPrecedence =
            0
        , expression =
            simpleExpressionWithPossibleArguments
        , operator =
            operator
        , representation =
            .val >> Tuple.second
        , combine =
            \left operator right ->
              let
                (wsBefore, identifier) =
                  operator.val
              in
                case opFromIdentifier identifier of
                  Just op_ ->
                    let
                      op =
                        withInfo op_ operator.start operator.end
                    in
                      withInfo
                        ( exp_ <|
                            EOp wsBefore op [ left, right ] space0
                        )
                        left.start
                        right.end

                  -- Should result in evaluator error
                  Nothing ->
                    let
                      opExp =
                        withInfo
                          ( exp_ <|
                              EVar wsBefore identifier
                          )
                          operator.start
                          operator.end
                    in
                      withInfo
                        ( exp_ <|
                            EApp space0 opExp [ left, right ] space0
                        )
                        left.start
                        right.end
        }

--==============================================================================
-- Top-Level Expressions
--=============================================================================

--------------------------------------------------------------------------------
-- Top-Level Defs
--------------------------------------------------------------------------------

topLevelDef : Parser TopLevelExp
topLevelDef =
  inContext "top-level def binding" <|
    delayedCommitMap
      ( \(wsBefore, name) (binding, wsBeforeSemicolon, semicolon) ->
          withInfo
            ( \rest ->
                exp_ <|
                  ELet
                    wsBefore
                    Def
                    False
                    name
                    binding
                    rest
                    wsBeforeSemicolon
            )
            name.start
            semicolon.end
      )
      ( succeed (,)
          |= spaces
          |= pattern
          |. spaces
          |. symbol "="
      )
      ( succeed (,,)
          |= expression
          |= spaces
          |= trackInfo (symbol ";")
      )

--------------------------------------------------------------------------------
-- General Top-Level Expressions
--------------------------------------------------------------------------------

topLevelExpression : Parser TopLevelExp
topLevelExpression =
  inContext "top-level expression" <|
    oneOf
      [ topLevelDef
      ]

allTopLevelExpressions : Parser (List TopLevelExp)
allTopLevelExpressions =
  repeat zeroOrMore topLevelExpression

--==============================================================================
-- Programs
--=============================================================================

program : Parser Exp
program =
  succeed fuseTopLevelExps
    |= allTopLevelExpressions
    |= expression
    |. spaces
    |. end

--==============================================================================
-- Exports
--=============================================================================

--------------------------------------------------------------------------------
-- Parser Runners
--------------------------------------------------------------------------------

parse : String -> Result P.Error Exp
parse =
  run (map FastParser.freshen program)

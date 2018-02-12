module ElmParser exposing
  ( parse,
    builtInPrecedenceTable,
    builtInPatternPrecedenceTable
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
import LongStringParser
import Regex

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

genericNonEmptyListWithTail
  :  { item : Parser elem
     , tailItem: Parser elem
     , combinerTail : WS -> List elem -> WS -> elem -> WS -> list
     }
  -> ParserI list
genericNonEmptyListWithTail { item, tailItem, combinerTail }=
  lazy <| \_ ->
    let
      anotherItem =
        delayedCommit spaces <|
          succeed identity
            |. symbol ","
            |= item
    in
      paddedBefore
        ( \wsBefore (members, wsMiddle, thetail, wsBeforeEnd) ->
            combinerTail wsBefore members wsMiddle thetail wsBeforeEnd
        )
        ( trackInfo <|
            succeed (\e es wsm t wse -> (e :: es, wsm, t, wse))
              |. symbol "["
              |= item
              |= repeat zeroOrMore anotherItem
              |= spaces
              |. symbol "|"
              |= tailItem
              |= spaces
              |. symbol "]"
        )

genericList
  :  { item : Parser elem
     , combiner : WS -> List elem -> WS -> list
     , tailItem : Parser elem
     , combinerTail : WS -> List elem -> WS -> elem -> WS -> list
     }
  -> ParserI list
genericList { item, tailItem, combiner, combinerTail } =
  lazy <| \_ ->
    oneOf
      [ try <|
          genericEmptyList
            { combiner =
                \wsBefore wsAfter -> combiner wsBefore [] wsAfter
            }
      , try <|
          genericNonEmptyList
            { item =
                item
            , combiner =
                combiner
            }
      , lazy <| \_ ->
          genericNonEmptyListWithTail
            { item =
                item
            , combinerTail =
                combinerTail
            , tailItem =
                tailItem }
      ]

--------------------------------------------------------------------------------
-- Block Helper (for types) TODO
--------------------------------------------------------------------------------

block
  : (WS -> a -> WS -> b) -> String -> String -> Parser a -> ParserI b
block combiner openSymbol closeSymbol p =
  delayedCommitMap
    ( \(wsBefore, open) (result, wsEnd, close) ->
        withInfo
          (combiner wsBefore result wsEnd)
          open.start
          close.end
    )
    ( succeed (,)
        |= spaces
        |= trackInfo (symbol openSymbol)
    )
    ( succeed (,,)
        |= p
        |= spaces
        |= trackInfo (symbol closeSymbol)
    )

parenBlock : (WS -> a -> WS -> b) -> Parser a -> ParserI b
parenBlock combiner = block combiner "(" ")"

bracketBlock : (WS -> a -> WS -> b) -> Parser a -> ParserI b
bracketBlock combiner = block combiner "[" "]"

blockIgnoreWS : String -> String -> Parser a -> ParserI a
blockIgnoreWS = block (\wsBefore x wsEnd -> x)

parenBlockIgnoreWS : Parser a -> ParserI a
parenBlockIgnoreWS = blockIgnoreWS "(" ")"

--==============================================================================
--= Identifiers
--==============================================================================

keywords : Set String
keywords =
  Set.fromList
    [ "let"
    , "letrec"
    , "in"
    , "case"
    , "of"
    , "if"
    , "then"
    , "else"
    , "type"
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
    oneOf
      [ source (symbol "<|")
      , source (symbol "|>")
      , source (symbol "::")
      , keep oneOrMore (\x -> ElmLang.isSymbol x && not (x == '|'))
      ]

patternSymbolIdentifier : ParserI Ident
patternSymbolIdentifier =
  trackInfo <|
    oneOf
      [ source (symbol "::")
      , source (keyword "as")
      ]

--==============================================================================
--= Numbers
--==============================================================================

--------------------------------------------------------------------------------
-- Raw Numbers
--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------
-- Widget Declarations
--------------------------------------------------------------------------------

isInt : Num -> Bool
isInt n = n == toFloat (floor n)

widgetDecl : Caption -> Parser WidgetDecl
widgetDecl cap =
  let
    combiner a tok b =
      if List.all isInt [a.val, b.val] then
        IntSlider (mapInfo floor a) tok (mapInfo floor b) cap False
      else
        NumSlider a tok b cap False
  in
    inContext "widget declaration" <|
      trackInfo <|
        oneOf
          [ succeed combiner
              |. symbol "{"
              |= num
              |= trackInfo (token "-" "-")
              |= num
              |. symbol "}"
          , succeed NoWidgetDecl
          ]

--------------------------------------------------------------------------------
-- Frozen Annotations
--------------------------------------------------------------------------------

frozenAnnotation : ParserI Frozen
frozenAnnotation =
  inContext "frozen annotation" <|
    trackInfo <|
      oneOf <|
        List.map (\a -> token a a) [frozen, thawed, assignOnlyOnce, unann]

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

-- Allows both 'these' and "these" for strings for compatibility.
-- Escape character is one '\'
singleLineString : ParserI EBaseVal
singleLineString =
  let
    stringHelper quoteChar =
      let
        quoteString = String.fromChar quoteChar
      in
      let
        quoteEscapeRegex = Regex.regex <| "\n|\\\\|\\" ++ quoteString ++ "|" ++ quoteString
      in
        succeed (EString quoteString)
          |. symbol quoteString
          |= map String.concat (
              repeat zeroOrMore <|
                oneOf [
                  map (\_ -> quoteString) <| symbol <| "\\" ++ quoteString,
                  map (\_ -> "\\") <| symbol <| "\\\\",
                  succeed (\a b -> a ++ b)
                  |= keep (Exactly 1) (\c -> c /= quoteChar && c /= '\\' && c /= '\n')
                  |= ParserUtils.keepUntilRegex quoteEscapeRegex
                ])
          |. symbol quoteString
  in
    inContext "single-line string" <|
      trackInfo <|
        oneOf <| List.map stringHelper ['\'', '"']

multiLineString : ParserI EBaseVal
multiLineString =
  inContext "multi-line string" <|
    trackInfo <|
      map (EString "\"\"\"") <|
        inside "\"\"\""


multiLineInterpolatedString : Parser Exp
multiLineInterpolatedString =
  inContext "multi-line string" <|
    mapExp_ <|
    trackInfo <|
      succeed (\wsBefore e -> EParens wsBefore e LongStringSyntax space0 )
      |= spaces
      |. symbol "\"\"\""
      |= LongStringParser.contentParser
      |. symbol "\"\"\""

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
-- Names Helper
--------------------------------------------------------------------------------

namePattern : ParserI Ident -> Parser Pat
namePattern ident =
  mapPat_ <|
    paddedBefore (\ws name -> PVar ws name noWidgetDecl) ident

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

variablePattern : Parser Pat
variablePattern =
  inContext "variable pattern" <|
    namePattern littleIdentifier

--------------------------------------------------------------------------------
-- Wildcards
--------------------------------------------------------------------------------

wildcardPattern : Parser Pat
wildcardPattern =
  inContext "wildcard pattern" <|
    mapPat_ <|
      paddedBefore
        (\ws () -> PWildcard ws)
        (trackInfo (symbol "_"))

--------------------------------------------------------------------------------
-- Types  (SPECIAL-USE ONLY; not included in `pattern`)
--------------------------------------------------------------------------------

typePattern : Parser Pat
typePattern =
  inContext "type pattern" <|
    namePattern bigIdentifier

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
          , tailItem =
              pattern
          , combiner =
              \wsBefore members wsBeforeEnd ->
                PList wsBefore members space0 Nothing wsBeforeEnd
          , combinerTail =
              \wsBefore members wsMiddle tail wsBeforeEnd ->
                PList wsBefore members wsMiddle (Just tail) wsBeforeEnd
          }

--------------------------------------------------------------------------------
-- As-Patterns (@-Patterns)
--------------------------------------------------------------------------------

parensPattern : Parser Pat
parensPattern =
  inContext "parentheses" <|
    mapPat_ <|
      lazy <| \_ ->
        paddedBefore
          ( \wsBefore (innerPattern, wsBeforeEnd) ->
              PParens wsBefore innerPattern wsBeforeEnd
          )
          ( trackInfo <|
              succeed (,)
                |. symbol "("
                |= pattern
                |= spaces
                |. symbol ")"
          )

simplePattern : Parser Pat
simplePattern =
  inContext "simple pattern" <|
    oneOf
      [ lazy <| \_ -> listPattern
      , lazy <| \_ -> parensPattern
      , constantPattern
      , baseValuePattern
      , variablePattern
      , wildcardPattern
      ]

--------------------------------------------------------------------------------
-- General Patterns
--------------------------------------------------------------------------------

pattern : Parser Pat
pattern =
  inContext "pattern" <|
    lazy <| \_ ->
      binaryOperator
        { precedenceTable =
            builtInPatternPrecedenceTable
        , minimumPrecedence =
            0
        , expression =
            simplePattern
        , operator =
            patternOperator
        , representation =
            .val >> Tuple.second
        , combine =
            \left operator right ->
              let
                (wsBefore, identifier) =
                  operator.val
              in
                case identifier of
                  "::" ->
                    withInfo
                      (
                        pat_ <| PList space0 [left] wsBefore (Just right) space0
                      ) left.start right.end
                  "as" ->
                    case right.val.p__ of
                      PVar wsName name _ ->
                        withInfo
                          (
                            pat_ <| PAs wsName name wsBefore left
                          ) left.start right.end
                      _ ->
                        case left.val.p__ of
                          PVar wsName name _ ->
                            withInfo
                              (
                                pat_ <| PAs wsName name wsBefore right
                              ) left.start right.end
                          _ -> Debug.crash "Parser does not support conjunction of patterns" -- TODO: Let PAs support arbitrary conjonction of patterns.
                  m -> Debug.crash <| "Internal error: Got pattern operator other than :: or as " ++ m
        }



--==============================================================================
--= Types
--==============================================================================

--------------------------------------------------------------------------------
-- Base Types
--------------------------------------------------------------------------------

baseType : String -> (WS -> Type_) -> String -> Parser Type
baseType context combiner token =
  inContext context <|
    delayedCommitMap
      ( \ws _ ->
          withInfo (combiner ws) ws.start ws.end
      )
      ( spaces )
      ( keyword token )

nullType : Parser Type
nullType =
  baseType "null type" TNull "Null"

numType : Parser Type
numType =
  baseType "num type" TNum "Num"

boolType : Parser Type
boolType =
  baseType "bool type" TBool "Bool"

stringType : Parser Type
stringType =
  baseType "string type" TString "String"

--------------------------------------------------------------------------------
-- Named Types
--------------------------------------------------------------------------------

namedType : Parser Type
namedType =
  inContext "named type" <|
    paddedBefore TNamed bigIdentifier

--------------------------------------------------------------------------------
-- Variable Types
--------------------------------------------------------------------------------

variableType : Parser Type
variableType =
  inContext "variable type" <|
    paddedBefore TVar littleIdentifier

--------------------------------------------------------------------------------
-- Function Type
--------------------------------------------------------------------------------

functionType : Parser Type
functionType =
  lazy <| \_ ->
    inContext "function type" <|
      parenBlock TArrow <|
        succeed identity
          |. keywordWithSpace "->"
          |= repeat oneOrMore typ

--------------------------------------------------------------------------------
-- List Type
--------------------------------------------------------------------------------

listType : Parser Type
listType =
  inContext "list type" <|
    lazy <| \_ ->
      parenBlock TList <|
        succeed identity
          |. keywordWithSpace "List"
          |= typ

--------------------------------------------------------------------------------
-- Dict Type
--------------------------------------------------------------------------------

dictType : Parser Type
dictType =
  inContext "dictionary type" <|
    lazy <| \_ ->
      parenBlock
        ( \wsBefore (tKey, tVal) wsEnd ->
            TDict wsBefore tKey tVal wsEnd
        )
        ( succeed (,)
            |. keywordWithSpace "TDict"
            |= typ
            |= typ
        )

--------------------------------------------------------------------------------
-- Tuple Type
--------------------------------------------------------------------------------

tupleType : Parser Type
tupleType =
  inContext "tuple type" <|
    lazy <| \_ ->
      genericList
        { item =
            typ
        , tailItem =
            typ
        , combiner =
            ( \wsBefore heads wsEnd ->
                TTuple wsBefore heads space0 Nothing wsEnd
            )
        , combinerTail =
            ( \wsBefore heads wsMiddle tail wsEnd ->
                TTuple wsBefore heads wsMiddle (Just tail) wsEnd
            )
        }

--------------------------------------------------------------------------------
-- Forall Type
--------------------------------------------------------------------------------

forallType : Parser Type
forallType =
  let
    wsIdentifierPair =
      delayedCommitMap
        ( \ws name ->
            (ws, name.val)
        )
        spaces
        littleIdentifier
    quantifiers =
      oneOf
        [ inContext "forall type (one)" <|
            map One wsIdentifierPair
        , inContext "forall type (many) "<|
            untrackInfo <|
              parenBlock Many <|
                repeat zeroOrMore wsIdentifierPair
        ]
  in
    inContext "forall type" <|
      lazy <| \_ ->
        parenBlock
          ( \wsBefore (qs, t) wsEnd ->
              TForall wsBefore qs t wsEnd
          )
          ( succeed (,)
              |. keywordWithSpace "forall"
              |= quantifiers
              |= typ
          )

--------------------------------------------------------------------------------
-- Union Type
--------------------------------------------------------------------------------

unionType : Parser Type
unionType =
  inContext "union type" <|
    lazy <| \_ ->
      parenBlock TUnion <|
        succeed identity
          |. keywordWithSpace "union"
          |= repeat oneOrMore typ

--------------------------------------------------------------------------------
-- Wildcard Type
--------------------------------------------------------------------------------

wildcardType : Parser Type
wildcardType =
  inContext "wildcard type" <|
    spaceSaverKeyword "_" TWildcard

--------------------------------------------------------------------------------
-- General Types
--------------------------------------------------------------------------------

typ : Parser Type
typ =
  inContext "type" <|
    lazy <| \_ ->
      oneOf
        [ nullType
        , numType
        , boolType
        , stringType
        , wildcardType
        , lazy <| \_ -> functionType
        , lazy <| \_ -> listType
        , lazy <| \_ -> dictType
        , lazy <| \_ -> tupleType
        , lazy <| \_ -> forallType
        , lazy <| \_ -> unionType
        , namedType
        , variableType
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

builtInPatternPrecedenceList : List (Int, List String, List String)
builtInPatternPrecedenceList =
 [ ( 2
   , []
   , ["::"]
   )
 , ( 1
   , ["as"]
   , []
   )
 ]

builtInPrecedenceTable : PrecedenceTable
builtInPrecedenceTable =
  buildPrecedenceTable builtInPrecedenceList

builtInPatternPrecedenceTable : PrecedenceTable
builtInPatternPrecedenceTable =
  buildPrecedenceTable builtInPatternPrecedenceList



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
    "==" ->
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
    "noWidgets" ->
      Just NoWidgets
    _ ->
      Nothing

--------------------------------------------------------------------------------
-- Operator Parsing
--------------------------------------------------------------------------------

operator : ParserI Operator
operator =
  paddedBefore (,) symbolIdentifier

patternOperator : ParserI Operator
patternOperator =
  paddedBefore (,) patternSymbolIdentifier

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
      delayedCommitMap
        ( \ws (n, fa, w) ->
            withInfo
              (EConst ws n.val (dummyLocWithDebugInfo fa.val n.val) w)
              n.start
              w.end
        )
        spaces
        ( succeed (,,)
            |= num
            |= frozenAnnotation
            |= widgetDecl Nothing
        )

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
          , tailItem =
              expression
          , combiner =
              \wsBefore members wsBeforeEnd ->
                EList wsBefore members space0 Nothing wsBeforeEnd
          , combinerTail =
              \wsBefore members wsMiddle tail wsBeforeEnd ->
                EList wsBefore members wsMiddle (Just tail) wsBeforeEnd
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
          ( \wsBefore (condition, wsThen, trueBranch, wsElse, falseBranch) ->
              EIf wsBefore condition wsThen trueBranch wsElse falseBranch space0
          )
          ( trackInfo <|
              delayedCommit (keywordWithSpace "if") <|
                succeed (,,,,)
                  |= expression
                  |= spaces
                  |. keywordWithSpace "then"
                  |= expression
                  |= spaces
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
letBinding =     lazy <| \_ ->
  genericLetBinding "let" False

letrecBinding : Parser Exp
letrecBinding =      lazy <| \_ ->
  genericLetBinding "letrec" True

genericLetBinding : String -> Bool -> Parser Exp
genericLetBinding letkeyword isRec =
  inContext (letkeyword ++ " binding") <|
    lazy <| \_ ->
      mapExp_ <|
        paddedBefore
          ( \wsBefore (name, parameters, wsBeforeEq, binding_, wsBeforeIn, body) ->
              let
                binding =
                  if List.isEmpty parameters then
                    binding_
                  else
                    withInfo
                      (exp_ <| EFun space0 parameters binding_ space0)
                      binding_.start
                      binding_.end
              in
                ELet wsBefore Let isRec name wsBeforeEq binding wsBeforeIn body space0
          )
          ( trackInfo <|
              delayedCommit (keywordWithSpace letkeyword) <|
                succeed (,,,,,)
                  |= pattern
                  |= repeat zeroOrMore pattern
                  |= spaces
                  |. symbol "="
                  |= expression
                  |= spaces
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
          ( \wsBefore (open, opt, wsMid, val, rest) ->
              withInfo
                (EOption wsBefore opt wsMid val rest)
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
              EParens wsBefore innerExpression Parens wsBeforeEnd
          )
          ( trackInfo <|
              succeed (,)
                |. symbol "("
                |= expression
                |= spaces
                |. symbol ")"
          )

--------------------------------------------------------------------------------
-- Holes
--------------------------------------------------------------------------------

hole : Parser Exp
hole =
  inContext "hole" <|
    mapExp_ <|
      paddedBefore EHole (trackInfo <| token "??" Nothing)

--------------------------------------------------------------------------------
-- Colon Types TODO
--------------------------------------------------------------------------------

colonType : Parser Exp
colonType =
  inContext "colon type" <|
    mapExp_ <|
      lazy <| \_ ->
        parenBlock
          ( \wsBefore (e, wsColon, t) wsEnd ->
              EColonType wsBefore e wsColon t wsEnd
          )
          ( delayedCommitMap
              ( \(e, wsColon) t ->
                  (e, wsColon, t)
              )
              ( succeed (,)
                  |= expression
                  |= spaces
                  |. symbol ":"
              )
              typ
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
      , lazy <| \_ -> letrecBinding
      , lazy <| \_ -> letBinding
      , lazy <| \_ -> lineComment
      , lazy <| \_ -> option
      , lazy <| \_ -> parens
      , lazy <| \_ -> hole
      -- , lazy <| \_ -> typeCaseExpression
      -- , lazy <| \_ -> typeAlias
      -- , lazy <| \_ -> typeDeclaration
      , variableExpression
      ]

spaceColonType: Parser (WS, Type)
spaceColonType =
  lazy <| \_ ->
     try ( succeed (,)
          |= spaces
          |. symbol ":"
          |= typ
     )

-- Either a simple expression or a function application
simpleUntypedExpressionWithPossibleArguments : Parser Exp
simpleUntypedExpressionWithPossibleArguments =
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
                    EApp space0 first rest SpaceApp space0
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

simpleExpressionWithPossibleArguments : Parser Exp
simpleExpressionWithPossibleArguments =
  lazy <| \_ ->
    ( succeed (\untypedExp mbType ->
        case mbType of
          Nothing -> untypedExp
          Just (wsColon, typ) ->
            withInfo
              ( exp_ <|
                  EColonType space0 untypedExp wsColon typ space0
              )
              untypedExp.start typ.end
      )
      |= simpleUntypedExpressionWithPossibleArguments
      |= ParserUtils.optional spaceColonType
    )

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
                    if identifier == "::" then
                      withInfo (exp_ <|
                        EList space0 [left] wsBefore (Just right) space0
                      ) left.start right.end
                    else if identifier == "<|" then
                      withInfo (exp_ <|
                        EApp space0 left [right] (LeftApp wsBefore) space0
                      ) left.start right.end
                    else if identifier == "|>" then
                      withInfo (exp_ <|
                        EApp space0 right [left] (RightApp wsBefore) space0
                      ) left.start right.end
                    else
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
                              EApp space0 opExp [ left, right ] SpaceApp space0
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
      ( \(wsBefore, name, parameters, wsBeforeEq)
         (binding_, wsBeforeSemicolon, semicolon) ->
          let
            binding =
              if List.isEmpty parameters then
                binding_
              else
                withInfo
                  (exp_ <| EFun space0 parameters binding_ space0)
                  binding_.start
                  binding_.end
          in
            withInfo
              ( \rest ->
                  exp_ <|
                    ELet
                      wsBefore
                      Def
                      False
                      name
                      wsBeforeEq
                      binding
                      wsBeforeSemicolon
                      rest
                      space0
              )
              name.start
              semicolon.end
      )
      ( succeed (,,,)
          |= spaces
          |= pattern
          |= repeat zeroOrMore pattern
          |= spaces
          |. symbol "="
      )
      ( succeed (,,)
          |= expression
          |= spaces
          |= trackInfo (symbol ";")
      )

--------------------------------------------------------------------------------
-- Top-Level Type Declarations
--------------------------------------------------------------------------------

topLevelTypeDeclaration : Parser TopLevelExp
topLevelTypeDeclaration =
  inContext "top-level type declaration" <|
    lazy <| \_ ->
      delayedCommitMap
        ( \(name, wsBeforeColon) t ->
            withInfo
              ( \rest ->
                  exp_ <|
                    ETyp
                      space0
                      name
                      t
                      rest
                      wsBeforeColon
              )
              name.start
              t.end
        )
        ( succeed (,)
            |= pattern
            |= spaces
            |. symbol ":"
        )
        ( typ
        )

--------------------------------------------------------------------------------
-- Top Level Type Aliases
--------------------------------------------------------------------------------

topLevelTypeAlias : Parser TopLevelExp
topLevelTypeAlias =
  inContext "top-level type alias" <|
    delayedCommitMap
      ( \(wsBefore, typeAliasKeyword, pat) t ->
          withInfo
            ( \rest ->
                exp_ <|
                  ETypeAlias wsBefore pat t rest space0
            )
            typeAliasKeyword.start
            t.end
      )
      ( succeed (,,)
          |= spaces
          |= trackInfo (keywordWithSpace "type alias")
          |= typePattern
          |. spaces
          |. symbol "="
      )
      ( typ
      )

--------------------------------------------------------------------------------
-- Top-Level Comments
--------------------------------------------------------------------------------

topLevelComment : Parser TopLevelExp
topLevelComment =
  inContext "top-level comment" <|
    delayedCommitMap
      ( \wsBefore (dashes, text) ->
          withInfo
            ( \rest ->
                exp_ <|
                  EComment wsBefore text.val rest
            )
            dashes.start
            text.end
      )
      spaces
      ( succeed (,)
          |= trackInfo (symbol "--")
          |= trackInfo (keep zeroOrMore (\c -> c /= '\n'))
          |. oneOf
               [ symbol "\n"
               , end
               ]
      )

--------------------------------------------------------------------------------
-- Top-Level Options
--------------------------------------------------------------------------------

topLevelOption : Parser TopLevelExp
topLevelOption =
  inContext "top-level option" <|
    paddedBefore
      ( \wsBefore (opt, wsMid, val) ->
          ( \rest ->
              exp_ <| EOption wsBefore opt wsMid val rest
          )
      )
      ( trackInfo <| succeed (,,)
          |. symbol "#"
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
      )

--------------------------------------------------------------------------------
-- General Top-Level Expressions
--------------------------------------------------------------------------------

topLevelExpression : Parser TopLevelExp
topLevelExpression =
  inContext "top-level expression" <|
    oneOf
      [ topLevelDef
      , topLevelTypeDeclaration
      , topLevelTypeAlias
      , topLevelComment
      , topLevelOption
      ]

allTopLevelExpressions : Parser (List TopLevelExp)
allTopLevelExpressions =
  repeat zeroOrMore topLevelExpression

--==============================================================================
-- Programs
--=============================================================================

implicitMain : Parser Exp
implicitMain =
  let
    builder p =
      let
        withCorrectInfo x =
          WithInfo x p p
        name =
          withCorrectInfo << pat_ <|
            PVar space1 "_IMPLICIT_MAIN" (withDummyInfo NoWidgetDecl)
        binding =
          withCorrectInfo << exp_ <|
            EBase space1 (EString defaultQuoteChar "...")
        body =
          withCorrectInfo << exp_ <|
            EVar space1 "main"
      in
        withCorrectInfo << exp_ <|
          ELet newline2 Let False name space1 binding space1 body space0
  in
    succeed builder
      |= getPos

mainExpression : Parser Exp
mainExpression =
  oneOf
    [ expression
    , implicitMain
    ]

program : Parser Exp
program =
  succeed fuseTopLevelExps
    |= allTopLevelExpressions
    |= mainExpression
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

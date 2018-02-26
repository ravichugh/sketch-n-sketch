module ElmParser exposing
  ( parse,
    parsePatternUnfresh,
    builtInPrecedenceTable,
    builtInPatternPrecedenceTable,
    isRestChar,
    isTopLevelDefImplicitlyRec
  )

import Char
import Set exposing (Set)
import Pos
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
import Regex

--==============================================================================
--= Helpers
--==============================================================================

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

genericEmptyList
  :  { combiner : WS -> WS -> list
     , beforeSpacePolicy: SpacePolicy
     }
  -> ParserI list
genericEmptyList { combiner, beforeSpacePolicy } =
  paddedBefore combiner beforeSpacePolicy <|
    trackInfo <|
      succeed identity
        |. symbol "["
        |= spaces
        |. symbol "]"

genericNonEmptyList
  :  { item : Parser elem
     , combiner : WS -> List (WS, elem) -> WS -> list
     , beforeSpacePolicy: SpacePolicy
     }
  -> ParserI list
genericNonEmptyList { item, combiner, beforeSpacePolicy }=
  lazy <| \_ ->
    let
      anotherWsAndItem : Parser (WS, elem)
      anotherWsAndItem =
        delayedCommitMap (,)
          spaces
          (succeed identity
            |. symbol ","
            |= item
          )
    in
      paddedBefore
        ( \wsBefore (members, wsBeforeEnd) ->
            combiner wsBefore members wsBeforeEnd
        )
        beforeSpacePolicy
        ( trackInfo <|
            succeed (\e es ws -> ((space0, e) :: es, ws))
              |. symbol "["
              |= item
              |= repeat zeroOrMore anotherWsAndItem
              |= spaces
              |. symbol "]"
        )

genericNonEmptyListWithTail
  :  { item : Parser elem
     , tailItem: Parser elem
     , combinerTail : WS -> List (WS, elem) -> WS -> elem -> WS -> list
     , beforeSpacePolicy: SpacePolicy
     }
  -> ParserI list
genericNonEmptyListWithTail { item, tailItem, combinerTail, beforeSpacePolicy }=
  lazy <| \_ ->
    let
      anotherWsAndItem : Parser (WS, elem)
      anotherWsAndItem =
        delayedCommitMap (,)
          spaces
          (succeed identity
            |. symbol ","
            |= item
          )
    in
      paddedBefore
        ( \wsBefore (members, wsMiddle, thetail, wsBeforeEnd) ->
            combinerTail wsBefore members wsMiddle thetail wsBeforeEnd
        )
        beforeSpacePolicy
        ( trackInfo <|
            succeed (\e es wsm t wse -> ((space0, e) :: es, wsm, t, wse))
              |. symbol "["
              |= item
              |= repeat zeroOrMore anotherWsAndItem
              |= spaces
              |. symbol "|"
              |= tailItem
              |= spaces
              |. symbol "]"
        )

genericList
  :  { item : Parser elem
     , combiner : WS -> List (WS, elem) -> WS -> list
     , tailItem : Parser elem
     , combinerTail : WS -> List (WS, elem) -> WS -> elem -> WS -> list
     , beforeSpacePolicy: SpacePolicy
     }
  -> ParserI list
genericList { item, tailItem, combiner, combinerTail, beforeSpacePolicy } =
  lazy <| \_ ->
    oneOf
      [ try <|
          genericEmptyList
            { combiner =
                \wsBefore wsAfter -> combiner wsBefore [] wsAfter,
              beforeSpacePolicy = beforeSpacePolicy
            }
      , try <|
          genericNonEmptyList
            { item =
                item
            , combiner =
                combiner
            , beforeSpacePolicy =
                beforeSpacePolicy
            }
      , lazy <| \_ ->
          genericNonEmptyListWithTail
            { item =
                item
            , combinerTail =
                combinerTail
            , tailItem =
                tailItem
            , beforeSpacePolicy =
                beforeSpacePolicy }
      ]

--------------------------------------------------------------------------------
-- Block Helper (for types) TODO
--------------------------------------------------------------------------------

block
  : (WS -> a -> WS -> b) -> SpacePolicy -> String -> String -> Parser a -> ParserI b
block combiner sp openSymbol closeSymbol p =
  delayedCommitMap
    ( \(wsBefore, open) (result, wsEnd, close) ->
        withInfo
          (combiner wsBefore result wsEnd)
          open.start
          close.end
    )
    ( succeed (,)
        |= sp
        |= trackInfo (symbol openSymbol)
    )
    ( succeed (,,)
        |= p
        |= spaces
        |= trackInfo (symbol closeSymbol)
    )

parenBlock : (WS -> a -> WS -> b) -> SpacePolicy -> Parser a -> ParserI b
parenBlock combiner sp = block combiner sp "(" ")"

bracketBlock : (WS -> a -> WS -> b) -> SpacePolicy -> Parser a -> ParserI b
bracketBlock combiner sp = block combiner sp "[" "]"

blockIgnoreWS : SpacePolicy -> String -> String -> Parser a -> ParserI a
blockIgnoreWS sp = block (\wsBefore x wsEnd -> x) sp

parenBlockIgnoreWS : SpacePolicy -> Parser a -> ParserI a
parenBlockIgnoreWS sp = blockIgnoreWS sp "(" ")"

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

multiLineInterpolatedString : SpacePolicy -> Parser Exp
multiLineInterpolatedString sp =
  inContext "multi-line interpolated string" <|
    mapExp_ <|
    trackInfo <|
      delayedCommitMap (\wsBefore e -> EParens wsBefore e LongStringSyntax space0 )
      (succeed identity
        |= sp
        |. symbol "\"\"\""
       )
      (succeed identity
        |= multilineContentParser
        |. symbol "\"\"\""
      )

multilineParseUntilRegex = Regex.regex <| "@|\"\"\""

multilineContentParser : Parser Exp
multilineContentParser =
  inContext "multi-line string content" <|
  ((mapExp_ <| trackInfo <|
    succeed (\str -> EBase space0 <| EString "\"" str)
  |= ParserUtils.keepUntilRegex multilineParseUntilRegex
  )
  |> andThen (\exp -> multilineContentParserHelp [exp]))

multilineConcatExp: List Exp -> Pos.Pos -> Exp
multilineConcatExp exps startPosition =
  case exps of
    [] ->  Debug.crash "Internal error: No expression in longstring literal"
    [head] -> head
    head::tail ->
      let tailPart = multilineConcatExp tail head.end in
      case head.val.e__ of
        ELet sp0 letType isRec pattern wsBeforeEq binding wsBeforeIn _ sp1 ->
          replaceE__ head <| ELet sp0 letType isRec pattern wsBeforeEq binding wsBeforeIn tailPart sp1
        _ ->
          withInfo
            (exp_ <| EOp space0 (withInfo Plus head.end tailPart.start) [head, tailPart] space0)
            head.start tailPart.end

multilineContentParserHelp: List Exp -> Parser Exp
multilineContentParserHelp prevExps =
  inContext "multi-line string end or escape" <|
  oneOf
    [ try <| ( -- Either this is the end of the string content
        succeed (multilineConcatExp (List.reverse prevExps))
        |. lookAhead (symbol "\"\"\"")
        |= getPos
      )
    , (succeed (\x y -> (x, y))
      |= (inContext "multi-line string @expression" <| oneOf [  --Or this is an @ followed by an @ resutling in a single @ char,
             mapExp_ <| trackInfo <|
               map (\_ -> EBase space0 (EString "\"" "@")) <| symbol "@@",
             succeed identity -- Or this is an @ followed by an escaped Elm expression or a let/def definition
          |. symbol "@"
          |= (lazy <| \_ -> multilineEscapedElmExpression)
        ])
      |= (mapExp_ <| trackInfo <|
             succeed (\str -> EBase space0 <| EString "\"" str)
           |= ParserUtils.keepUntilRegex multilineParseUntilRegex
         ))
     |> andThen (\(potentialExp, stringExp) ->
        case prevExps of
          lastPrev :: lastTail ->
            case (lastPrev.val.e__, potentialExp.val.e__, stringExp.val.e__) of
              (EBase sp0 (EString qc prevChars), EBase sp1 (EString eqc expChars), EBase sp2 (EString sqc stringChars)) ->
                multilineContentParserHelp <| (withInfo (
                  exp_ <| EBase sp0 <| EString qc (prevChars ++ expChars ++ stringChars))
                  lastPrev.start stringExp.end) :: prevExps
              _ ->
                multilineContentParserHelp (stringExp :: potentialExp:: prevExps)
          [] -> Debug.crash "Internal error: There should be always at least one expression in a longstring literal."
      )
  ]

multilineEscapedElmExpression: Parser Exp
multilineEscapedElmExpression =
  inContext "expression in multi-line string" <|
  oneOf [
    try <| trackInfo <|
      succeed (\v -> exp_ <| EOp space0 (withInfo OptNumToString v.start v.start) [v] space0)
      |= variableExpression spacesWithoutNewline,
    try <| lazy <| \_ -> multilineGenericLetBinding,
    lazy <| \_ ->
      ( mapExp_ <| trackInfo <|
          (succeed (\exp -> (EOp space0 (withInfo OptNumToString exp.start exp.start) [withInfo (exp_ <| EParens space0 exp ElmSyntax space0) exp.start exp.end] space0))
          |= parens spacesWithoutNewline)
      )
  ]

multilineGenericLetBinding : Parser Exp
multilineGenericLetBinding =
  inContext ("let binding within a long string") <|
    lazy <| \_ ->
      succeed (\letdefWithInfo isRec pattern parameters wsBeforeEq binding_  ->
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
             (
                 exp_ <|
                   ELet
                     space0
                     Let
                     (case isRec of
                       Just _ -> True
                       _ -> False
                     )
                     pattern
                     wsBeforeEq
                     binding
                     space0
                     (withDummyExpInfo <| EHole space0 HoleEmpty)
                     space0
             )
             letdefWithInfo.start
             binding.end
       )
      |= (trackInfo <| source <| keyword "let")
      |= optional (keyword "rec")
      |= pattern spacesWithoutNewline
      |= repeat zeroOrMore (pattern spacesWithoutNewline)
      |= spacesWithoutNewline
      |. symbol "="
      |= expression spacesWithoutNewline
      |. ignore zeroOrMore (\c -> c == ' ' || c == '\t')-- This will remove trailing whitespace.
      |. symbol "\n"

--------------------------------------------------------------------------------
-- General Base Values
--------------------------------------------------------------------------------

baseValue : ParserI EBaseVal
baseValue =
  inContext "base value" <|
    oneOf
      [ singleLineString
      , bool
      ]

--==============================================================================
-- Patterns
--==============================================================================

--------------------------------------------------------------------------------
-- Names Helper
--------------------------------------------------------------------------------

namePattern : SpacePolicy -> ParserI Ident -> Parser Pat
namePattern sp ident =
  mapPat_ <|
    paddedBefore (\ws name -> PVar ws name noWidgetDecl) sp ident

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

variablePattern : SpacePolicy -> Parser Pat
variablePattern sp =
  inContext "variable pattern" <|
    namePattern sp littleIdentifier

--------------------------------------------------------------------------------
-- Wildcards
--------------------------------------------------------------------------------

wildcardPattern : SpacePolicy -> Parser Pat
wildcardPattern sp =
  inContext "wildcard pattern" <|
    mapPat_ <|
      paddedBefore
        (\ws () -> PWildcard ws)
        sp
        (trackInfo (symbol "_"))

--------------------------------------------------------------------------------
-- Types  (SPECIAL-USE ONLY; not included in `pattern`)
--------------------------------------------------------------------------------

typePattern : SpacePolicy -> Parser Pat
typePattern sp =
  inContext "type pattern" <|
    namePattern sp bigIdentifier

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

constantPattern : SpacePolicy -> Parser Pat
constantPattern sp =
  inContext "constant pattern" <|
    mapPat_ <|
      paddedBefore PConst sp num

--------------------------------------------------------------------------------
-- Base Values
--------------------------------------------------------------------------------

baseValuePattern : SpacePolicy -> Parser Pat
baseValuePattern sp =
  inContext "base value pattern" <|
    mapPat_ <|
      paddedBefore PBase sp baseValue

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

listPattern : SpacePolicy -> Parser Pat
listPattern sp =
  inContext "list pattern" <|
    lazy <| \_ ->
      mapPat_ <|
        genericList
          { item =
              pattern spaces
          , tailItem =
              pattern spaces
          , combiner =
              \wsBefore members wsBeforeEnd ->
                -- PList wsBefore members space0 Nothing wsBeforeEnd
                PList wsBefore (List.map Tuple.second members) space0 Nothing wsBeforeEnd
          , combinerTail =
              \wsBefore members wsMiddle tail wsBeforeEnd ->
                -- PList wsBefore members wsMiddle (Just tail) wsBeforeEnd
                PList wsBefore (List.map Tuple.second members) wsMiddle (Just tail) wsBeforeEnd
          , beforeSpacePolicy =
              sp
          }

--------------------------------------------------------------------------------
-- As-Patterns (@-Patterns)
--------------------------------------------------------------------------------

parensPattern : SpacePolicy -> Parser Pat
parensPattern sp =
  inContext "parentheses" <|
    mapPat_ <|
      lazy <| \_ ->
        paddedBefore
          ( \wsBefore (innerPattern, wsBeforeEnd) ->
              PParens wsBefore innerPattern wsBeforeEnd
          )
          sp
          ( trackInfo <|
              succeed (,)
                |. symbol "("
                |= pattern spaces
                |= spaces
                |. symbol ")"
          )

simplePattern : SpacePolicy -> Parser Pat
simplePattern sp =
  inContext "simple pattern" <|
    oneOf
      [ lazy <| \_ -> listPattern sp
      , lazy <| \_ -> parensPattern sp
      , constantPattern sp
      , baseValuePattern sp
      , variablePattern sp
      , wildcardPattern sp
      ]

--------------------------------------------------------------------------------
-- General Patterns
--------------------------------------------------------------------------------

pattern : SpacePolicy -> Parser Pat
pattern sp =
  inContext "pattern" <|
    lazy <| \_ ->
      binaryOperator
        { precedenceTable =
            builtInPatternPrecedenceTable
        , minimumPrecedence =
            0
        , expression =
            simplePattern sp
        , operator =
            patternOperator sp
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

baseType : String -> (WS -> Type_) -> SpacePolicy -> String -> Parser Type
baseType context combiner sp token =
  inContext context <|
    delayedCommitMap
      ( \ws _ ->
          withInfo (combiner ws) ws.start ws.end
      )
      ( sp )
      ( keyword token )

nullType : SpacePolicy -> Parser Type
nullType sp =
  baseType "null type" TNull sp "Null"

numType : SpacePolicy -> Parser Type
numType sp =
  baseType "num type" TNum sp "Num"

boolType : SpacePolicy -> Parser Type
boolType sp =
  baseType "bool type" TBool sp "Bool"

stringType : SpacePolicy -> Parser Type
stringType sp =
  baseType "string type" TString sp "String"

--------------------------------------------------------------------------------
-- Named Types
--------------------------------------------------------------------------------

namedType : SpacePolicy -> Parser Type
namedType sp =
  inContext "named type" <|
    paddedBefore TNamed sp bigIdentifier

--------------------------------------------------------------------------------
-- Variable Types
--------------------------------------------------------------------------------

variableType : SpacePolicy -> Parser Type
variableType sp =
  inContext "variable type" <|
    paddedBefore TVar sp littleIdentifier

--------------------------------------------------------------------------------
-- Function Type
--------------------------------------------------------------------------------

functionType : SpacePolicy -> Parser Type
functionType sp =
  lazy <| \_ ->
    inContext "function type" <|
      parenBlock TArrow sp <|
        succeed identity
          |. keywordWithSpace "->"
          |= repeat oneOrMore (typ sp)

--------------------------------------------------------------------------------
-- List Type
--------------------------------------------------------------------------------

listType : SpacePolicy -> Parser Type
listType sp =
  inContext "list type" <|
    lazy <| \_ ->
      parenBlock TList sp <|
        succeed identity
          |. keywordWithSpace "List"
          |= typ sp

--------------------------------------------------------------------------------
-- Dict Type
--------------------------------------------------------------------------------

dictType : SpacePolicy -> Parser Type
dictType sp =
  inContext "dictionary type" <|
    lazy <| \_ ->
      parenBlock
        ( \wsBefore (tKey, tVal) wsEnd ->
            TDict wsBefore tKey tVal wsEnd
        )
        sp
        ( succeed (,)
            |. keywordWithSpace "TDict"
            |= typ sp
            |= typ sp
        )

--------------------------------------------------------------------------------
-- Tuple Type
--------------------------------------------------------------------------------

tupleType : SpacePolicy -> Parser Type
tupleType sp =
  inContext "tuple type" <|
    lazy <| \_ ->
      genericList
        { item =
            typ spaces
        , tailItem =
            typ spaces
        , combiner =
            ( \wsBefore heads wsEnd ->
                -- TTuple wsBefore heads space0 Nothing wsEnd
                TTuple wsBefore (List.map Tuple.second heads) space0 Nothing wsEnd
            )
        , combinerTail =
            ( \wsBefore heads wsMiddle tail wsEnd ->
                -- TTuple wsBefore heads wsMiddle (Just tail) wsEnd
                TTuple wsBefore (List.map Tuple.second heads) wsMiddle (Just tail) wsEnd
            )
        , beforeSpacePolicy =
            sp
        }

--------------------------------------------------------------------------------
-- Forall Type
--------------------------------------------------------------------------------

forallType : SpacePolicy -> Parser Type
forallType sp =
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
              parenBlock Many sp <|
                repeat zeroOrMore wsIdentifierPair
        ]
  in
    inContext "forall type" <|
      lazy <| \_ ->
        parenBlock
          ( \wsBefore (qs, t) wsEnd ->
              TForall wsBefore qs t wsEnd
          )
          sp
          ( succeed (,)
              |. keywordWithSpace "forall"
              |= quantifiers
              |= typ spaces
          )

--------------------------------------------------------------------------------
-- Union Type
--------------------------------------------------------------------------------

unionType : SpacePolicy -> Parser Type
unionType sp=
  inContext "union type" <|
    lazy <| \_ ->
      parenBlock TUnion sp <|
        succeed identity
          |. keywordWithSpace "union"
          |= repeat oneOrMore (typ spaces)

--------------------------------------------------------------------------------
-- Wildcard Type
--------------------------------------------------------------------------------

wildcardType : SpacePolicy -> Parser Type
wildcardType sp =
  inContext "wildcard type" <|
    spaceSaverKeyword sp "_" TWildcard

--------------------------------------------------------------------------------
-- General Types
--------------------------------------------------------------------------------

typ : SpacePolicy -> Parser Type
typ sp =
  inContext "type" <|
    lazy <| \_ ->
      oneOf
        [ nullType sp
        , numType sp
        , boolType sp
        , stringType sp
        , wildcardType sp
        , lazy <| \_ -> functionType sp
        , lazy <| \_ -> listType sp
        , lazy <| \_ -> dictType sp
        , lazy <| \_ -> tupleType sp
        , lazy <| \_ -> forallType sp
        , lazy <| \_ -> unionType sp
        , namedType sp
        , variableType sp
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

operator : SpacePolicy -> ParserI Operator
operator sp =
  paddedBefore (,) sp symbolIdentifier

patternOperator : SpacePolicy -> ParserI Operator
patternOperator sp =
  paddedBefore (,) sp patternSymbolIdentifier

--==============================================================================
-- Expressions
--==============================================================================

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

constantExpression : SpacePolicy -> Parser Exp
constantExpression sp =
  inContext "constant expression" <|
    mapExp_ <|
      delayedCommitMap
        ( \ws (n, fa, w) ->
            withInfo
              (EConst ws n.val (dummyLocWithDebugInfo fa.val n.val) w)
              n.start
              w.end
        )
        sp
        ( succeed (,,)
            |= num
            |= frozenAnnotation
            |= widgetDecl Nothing
        )

--------------------------------------------------------------------------------
-- Base Values
--------------------------------------------------------------------------------

baseValueExpression : SpacePolicy -> Parser Exp
baseValueExpression sp =
  inContext "base value expression" <|
    mapExp_ <|
      paddedBefore EBase sp baseValue

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

variableExpression : SpacePolicy -> Parser Exp
variableExpression sp =
  mapExp_ <|
    paddedBefore EVar sp littleIdentifier

--------------------------------------------------------------------------------
-- Functions (lambdas)
--------------------------------------------------------------------------------

function : SpacePolicy -> Parser Exp
function sp =
  inContext "function" <|
    lazy <| \_ ->
      mapExp_ <|
        paddedBefore
          ( \wsBefore (parameters, body) ->
              EFun wsBefore parameters body space0
          )
          sp
          ( trackInfo <|
              succeed (,)
                |. symbol "\\"
                |= repeat oneOrMore (pattern spaces)
                |. spaces
                |. symbol "->"
                |= expression sp
          )

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

list : SpacePolicy -> Parser Exp
list sp =
  inContext "list" <|
    lazy <| \_ ->
      mapExp_ <|
        genericList
          { item =
              expression spaces
          , tailItem =
              expression spaces
          , combiner =
              \wsBefore members wsBeforeEnd ->
                EList wsBefore members space0 Nothing wsBeforeEnd
          , combinerTail =
              \wsBefore members wsMiddle tail wsBeforeEnd ->
                EList wsBefore members wsMiddle (Just tail) wsBeforeEnd
          , beforeSpacePolicy =
             sp
          }

--------------------------------------------------------------------------------
-- Conditionals
--------------------------------------------------------------------------------

conditional : SpacePolicy -> Parser Exp
conditional sp =
  inContext "conditional" <|
    lazy <| \_ ->
      mapExp_ <|
        paddedBefore
          ( \wsBefore (condition, wsThen, trueBranch, wsElse, falseBranch) ->
              EIf wsBefore condition wsThen trueBranch wsElse falseBranch space0
          )
          sp
          ( trackInfo <|
              delayedCommit (keywordWithSpace "if") <|
                succeed (,,,,)
                  |= expression spaces
                  |= spaces
                  |. keywordWithSpace "then"
                  |= expression spaces
                  |= spaces
                  |. keywordWithSpace "else"
                  |= expression sp
          )

--------------------------------------------------------------------------------
-- Case Expressions
--------------------------------------------------------------------------------

caseExpression : SpacePolicy -> Parser Exp
caseExpression sp =
  inContext "case expression" <|
    lazy <| \_ ->
      let
        branch =
          paddedBefore
            ( \wsBefore (p, wsBeforeArrow, e) ->
                Branch_ wsBefore p e wsBeforeArrow
            )
            sp
            ( trackInfo <|
                succeed (,,)
                  |= pattern sp
                  |= sp
                  |. symbol "->"
                  |= expression sp
                  |. symbol ";"
            )
      in
        mapExp_ <|
          paddedBefore
            ( \wsBefore (examinedExpression, wsBeforeOf, branches) ->
                ECase wsBefore examinedExpression branches wsBeforeOf
            )
            sp
            ( trackInfo <|
                delayedCommit (keywordWithSpace "case") <|
                  succeed (,,)
                    |= expression spaces
                    |= spaces
                    |. keyword "of"
                    |= repeat oneOrMore branch
            )

--------------------------------------------------------------------------------
-- Let Bindings
--------------------------------------------------------------------------------

letBinding : SpacePolicy -> Parser Exp
letBinding sp =     lazy <| \_ ->
  genericLetBinding sp "let" False

letrecBinding : SpacePolicy -> Parser Exp
letrecBinding sp =      lazy <| \_ ->
  genericLetBinding sp "letrec" True

genericLetBinding : SpacePolicy -> String -> Bool -> Parser Exp
genericLetBinding sp letkeyword isRec =
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
          sp
          ( trackInfo <|
              delayedCommit (keywordWithSpace letkeyword) <|
                succeed (,,,,,)
                  |= pattern spaces
                  |= repeat zeroOrMore (pattern spaces)
                  |= spaces
                  |. symbol "="
                  |= expression spaces
                  |= spaces
                  |. keywordWithSpace "in"
                  |= expression sp
          )

--------------------------------------------------------------------------------
-- Comments
--------------------------------------------------------------------------------

lineComment : SpacePolicy -> Parser Exp
lineComment sp =
  inContext "line comment" <|
    mapExp_ <|
      paddedBefore
        ( \wsBefore (text, expAfter) ->
            EComment wsBefore text expAfter
        )
        sp
        ( trackInfo <|
            succeed (,)
              |. symbol "--"
              |= keep zeroOrMore (\c -> c /= '\n')
              |. symbol "\n"
              |= expression sp
        )

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

option : SpacePolicy -> Parser Exp
option sp =
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
          ( sp )
          ( succeed (,,,,)
              |= trackInfo (symbol "#")
              |. spaces
              |= trackInfo
                   ( keep zeroOrMore <| \c ->
                       c /= '\n' && c /= ' ' && c /= ':'
                   )
              |. symbol ":"
              |= sp
              |= trackInfo
                   ( keep zeroOrMore <| \c ->
                       c /= '\n'
                   )
              |. symbol "\n"
              |= expression sp
          )

--------------------------------------------------------------------------------
-- Parentheses
--------------------------------------------------------------------------------

parens : SpacePolicy -> Parser Exp
parens sp =
  inContext "parentheses" <|
    mapExp_ <|
      lazy <| \_ ->
        paddedBefore
          ( \wsBefore (innerExpression, wsBeforeEnd) ->
              EParens wsBefore innerExpression Parens wsBeforeEnd
          )
          sp
          ( trackInfo <|
              succeed (,)
                |. symbol "("
                |= expression spaces
                |= spaces
                |. symbol ")"
          )

--------------------------------------------------------------------------------
-- Holes
--------------------------------------------------------------------------------

hole : SpacePolicy -> Parser Exp
hole sp =
  inContext "hole" <|
    mapExp_ <|
      paddedBefore EHole sp (trackInfo <| token "??" HoleEmpty)

--------------------------------------------------------------------------------
-- Colon Types TODO
--------------------------------------------------------------------------------

colonType : SpacePolicy -> Parser Exp
colonType sp =
  inContext "colon type" <|
    mapExp_ <|
      lazy <| \_ ->
        parenBlock
          ( \wsBefore (e, wsColon, t) wsEnd ->
              EColonType wsBefore e wsColon t wsEnd
          )
          sp
          ( delayedCommitMap
              ( \(e, wsColon) t ->
                  (e, wsColon, t)
              )
              ( succeed (,)
                  |= expression sp
                  |= sp
                  |. symbol ":"
              )
              ( typ sp )
          )

--------------------------------------------------------------------------------
-- General Expressions
--------------------------------------------------------------------------------

-- Not a function application nor a binary operator
simpleExpression : SpacePolicy -> Parser Exp
simpleExpression sp =
  oneOf
    [ constantExpression sp
    , lazy <| \_ -> multiLineInterpolatedString sp
    , baseValueExpression sp
    , lazy <| \_ -> function sp
    , lazy <| \_ -> list sp
    , lazy <| \_ -> conditional sp
    , lazy <| \_ -> caseExpression sp
    , lazy <| \_ -> letrecBinding sp
    , lazy <| \_ -> letBinding sp
    , lazy <| \_ -> lineComment sp
    , lazy <| \_ -> option sp
    , lazy <| \_ -> parens sp
    , lazy <| \_ -> hole sp
    -- , lazy <| \_ -> typeCaseExpression
    -- , lazy <| \_ -> typeAlias
    -- , lazy <| \_ -> typeDeclaration
    , variableExpression sp
    ]

spaceColonType: SpacePolicy -> Parser (WS, Type)
spaceColonType sp =
  lazy <| \_ ->
     try ( succeed (,)
          |= sp
          |. symbol ":"
          |= typ sp
     )

-- Either a simple expression or a function application
simpleUntypedExpressionWithPossibleArguments : SpacePolicy -> Parser Exp
simpleUntypedExpressionWithPossibleArguments sp =
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
                    EApp (ws (precedingWhitespace first)) (removePrecedingWhitespace first) rest SpaceApp space0
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
        |= simpleExpression sp
        |= repeat zeroOrMore (simpleExpression sp)

simpleExpressionWithPossibleArguments : SpacePolicy -> Parser Exp
simpleExpressionWithPossibleArguments sp =
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
      |= simpleUntypedExpressionWithPossibleArguments sp
      |= ParserUtils.optional (spaceColonType sp)
    )

-- This is useful to get rid of semicolon in the top-level language.
-- Expressions at top-level cannot consume a newline that is followed by an identifier, or two newlines except if they are parsed inside parentheses.
type alias SpacePolicy = Parser WS


topLevelSpaceRegex = Regex.regex "((?!\n\n|\n\\w)\\s)*"

topLevelSpacePolicy: SpacePolicy
topLevelSpacePolicy =
  trackInfo <| ParserUtils.keepRegex topLevelSpaceRegex

spaceWithoutNLRegex = Regex.regex "((?!\n)\\s)*"

spacesWithoutNewline: SpacePolicy
spacesWithoutNewline =
  trackInfo <| ParserUtils.keepRegex spaceWithoutNLRegex

expression : SpacePolicy -> Parser Exp
expression sp =
  inContext "expression" <|
    lazy <| \_ ->
      binaryOperator
        { precedenceTable =
            builtInPrecedenceTable
        , minimumPrecedence =
            0
        , expression =
            simpleExpressionWithPossibleArguments sp
        , operator =
            operator sp
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
                        EList space0 [(space0, left)] wsBefore (Just right) space0
                      ) left.start right.end
                    else if identifier == "<|" then
                      copyPrecedingWhitespace left <|
                      withInfo (exp_ <|
                        EApp space0 (removePrecedingWhitespace left) [right] (LeftApp wsBefore) space0
                      ) left.start right.end
                    else if identifier == "|>" then
                      copyPrecedingWhitespace left <|
                      withInfo (exp_ <|
                        EApp space0 right [(removePrecedingWhitespace left)] (RightApp wsBefore) space0
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
                        copyPrecedingWhitespace opExp <|
                        withInfo
                          ( exp_ <|
                              EApp space0 (removePrecedingWhitespace opExp) [ left, right ] SpaceApp space0
                          )
                          left.start
                          right.end
        }

--==============================================================================
-- Top-Level Expressions
--=============================================================================

-- The semicolon is now optional. Maybe remove that in the future?
optionalTopLevelSemicolon = optional (paddedBefore (\_ _ _ -> ()) spaces (trackInfo <| symbol ";"))

--------------------------------------------------------------------------------
-- Top-Level Defs
--------------------------------------------------------------------------------

isTopLevelDefImplicitlyRec : Pat -> Exp -> Bool
isTopLevelDefImplicitlyRec pat binding =
  isPVar (patEffectivePat pat) && isFunc (expEffectiveExp binding)
  -- Uncomment when mutually recursive functions implmented in eval
  -- || case ((patEffectivePat pat).val.p__, (expEffectiveExp binding).val.e__) of
  --       (PList _ pHeads _ Nothing _, EList _ eHeads _ Nothing _) ->
  --         List.all (patEffectivePat >> isPVar) pHeads &&
  --         List.all (expEffectiveExp >> isFunc) eHeads &&
  --         List.length pHeads == List.length eHeads
  --       _ ->
  --         False

topLevelDef : Parser TopLevelExp
topLevelDef =
  inContext "top-level def binding" <|
    delayedCommitMap
      ( \(wsBefore, pat, parameters, wsBeforeEq)
         binding_ ->
          let
            binding =
              if List.isEmpty parameters then
                binding_
              else
                withInfo
                  (exp_ <| EFun space0 parameters binding_ space0)
                  binding_.start
                  binding_.end

            isRec =
              isTopLevelDefImplicitlyRec pat binding
          in
            withInfo
              ( \rest ->
                  exp_ <|
                    ELet
                      wsBefore
                      Def
                      isRec
                      pat
                      wsBeforeEq
                      binding
                      space0
                      rest
                      space0
              )
              pat.start
              binding.end
      )
      ( succeed (,,,)
          |= spaces
          |= pattern (topLevelSpacePolicy)
          |= repeat zeroOrMore (pattern topLevelSpacePolicy)
          |= topLevelSpacePolicy
          |. symbol "="
      )
      ( succeed identity
          |= expression topLevelSpacePolicy
          |. optionalTopLevelSemicolon
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
            |= pattern topLevelSpacePolicy
            |= topLevelSpacePolicy
            |. symbol ":"
        )
        ( succeed identity
          |= typ topLevelSpacePolicy
          |. optionalTopLevelSemicolon
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
          |= typePattern topLevelSpacePolicy
          |. topLevelSpacePolicy
          |. symbol "="
      )
      ( succeed identity
        |= typ topLevelSpacePolicy
        |. optionalTopLevelSemicolon
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
      spaces
      ( trackInfo <| succeed (,,)
          |. symbol "#"
          |. spacesWithoutNewline
          |= trackInfo
               ( keep zeroOrMore <| \c ->
                   c /= '\n' && c /= ' ' && c /= ':'
               )
          |. symbol ":"
          |= spacesWithoutNewline
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
    [ expression spaces
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


parsePatternUnfresh : String -> Result P.Error Pat
parsePatternUnfresh =
  run (pattern spaces)
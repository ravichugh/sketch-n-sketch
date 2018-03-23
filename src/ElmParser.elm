module ElmParser exposing
  ( parse,
    builtInPrecedenceTable,
    builtInPatternPrecedenceTable,
    isRestChar,
    isTopLevelDefImplicitlyRec

  -- All of these exports are copied from FastParser,
  -- so that ElmParser can be a drop-in replacement
  -- and FasterParser can be deprecated soon.
  , parseT
  , prelude, isPreludeLoc, isPreludeLocId, isPreludeEId, isActualEId, isProgramEId
  , substOf, substStrOf, substPlusOf
  , sanitizeVariableName
  , clearAllIds
  , freshen
  , maxId
  )

import Char
import Set exposing (Set)
import Dict
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

-- import FastParser
import PreludeGenerated as Prelude
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
  paddedBefore combiner beforeSpacePolicy.first <|
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
        beforeSpacePolicy.first
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
        beforeSpacePolicy.first
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
              beforeSpacePolicy =
                beforeSpacePolicy
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
      , genericNonEmptyListWithTail
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
-- Records
--------------------------------------------------------------------------------

genericEmptyRecord
  :  { combiner : WS -> WS -> record
     , beforeSpacePolicy: SpacePolicy
     }
  -> ParserI record
genericEmptyRecord { combiner, beforeSpacePolicy } =
  inContext "generic empty record" <|
  paddedBefore combiner beforeSpacePolicy.first <|
    trackInfo <|
      succeed identity
        |. symbol "{"
        |= spaces
        |. symbol "}"


genericNonEmptyRecord
  :  { keyValue : Parser (String, WS, elemValue)
     , combiner : WS -> List (WS, WS, String, WS, elemValue) -> WS -> record
     , beforeSpacePolicy: SpacePolicy
     }
  -> ParserI record
genericNonEmptyRecord { keyValue, combiner, beforeSpacePolicy }=
  lazy <| \_ ->
    let
      keyValueSeqHelper : Set String -> List (WS, WS, String, WS, elemValue)-> Parser (List (WS, WS, String, WS, elemValue))
      keyValueSeqHelper keys revKeyValues =
        oneOf [
          delayedCommitMap (\ws (a, (b, c, d)) -> (ws, a, b, c, d))
            spaces
            (succeed (,)
              |. negativeLookAhead (symbol "}")
              |. optional (symbol ",")
              |= spaces
              |= keyValue
            )
           |> andThen (\((ws, a, k, c, v) as r) ->
             if Set.member k keys then fail <| "Records cannot have duplicate keys, but the key " ++ k ++ " appears at least twice"
             else keyValueSeqHelper (Set.insert k keys) (r :: revKeyValues))
          , succeed (List.reverse revKeyValues)
        ]
    in
      inContext "generic non-empty record" <|
      paddedBefore
        ( \wsBefore (members, wsBeforeEnd) ->
            combiner wsBefore members wsBeforeEnd
        )
        beforeSpacePolicy.first
        ( trackInfo <|
            succeed (\keyValues wsEnd -> (keyValues, wsEnd))
              |. symbol "{"
              |= ((succeed (,)
                  |= spaces
                  |= keyValue)
                  |> andThen (\((wsBefore, (k, w, v)) as kv) ->
                    keyValueSeqHelper (Set.singleton k) [(space0, wsBefore, k, w, v)]
                    )
                 )
              |= spaces
              |. symbol "}")


genericNonEmptyRecordWithInit
  :  { keyValue : Parser (String, WS, elemValue)
     , initItem: Parser elemInit
     , combinerInit : WS-> elemInit -> WS -> List (WS, WS, String, WS, elemValue) -> WS -> record
     , beforeSpacePolicy: SpacePolicy
     }
  -> ParserI record
genericNonEmptyRecordWithInit { keyValue, initItem, combinerInit, beforeSpacePolicy }=
  lazy <| \_ ->
    let
      keyValueSeqHelper : Set String -> List (WS, WS, String, WS, elemValue)-> Parser (List (WS, WS, String, WS, elemValue))
      keyValueSeqHelper keys revKeyValues =
        oneOf [
          delayedCommitMap (\ws (a, (b, c, d)) -> (ws, a, b, c, d))
            spaces
            (succeed (,)
              |. negativeLookAhead (symbol "}")
              |. optional (symbol ",")
              |= spaces
              |= keyValue
            )
           |> andThen (\((ws, a, k, c, v) as r) ->
             if Set.member k keys then fail <| "Records cannot have duplicate keys, but the key " ++ k ++ " appears at least twice"
             else keyValueSeqHelper (Set.insert k keys) (r :: revKeyValues))
          , succeed (List.reverse revKeyValues)
        ]
    in
      inContext "generic non-empty record with init" <|
        trackInfo <|
          delayedCommitMap (\(wsBefore, init, wsBeforeBar) (members, wsEnd) ->
            combinerInit wsBefore init wsBeforeBar members wsEnd)
            (succeed (\wsBefore init wsBeforeBar -> (wsBefore, init, wsBeforeBar))
              |= beforeSpacePolicy.first
              |. symbol "{"
              |= initItem
              |= spaces)
            (succeed (\keyValues wsEnd ->
                 (keyValues, wsEnd))
              |. symbol "|"
              |= ((succeed (,)
                  |= spaces
                  |= keyValue)
                  |> andThen (\((wsBefore, (k, w, v)) as kv) ->
                    keyValueSeqHelper (Set.singleton k) [(space0, wsBefore, k, w, v)]
                    )
                 )
              |= spaces
              |. symbol "}")



genericRecord
  :  { key : Parser String
     , equalSign: Parser ()
     , optNoEqualSign: Maybe (String -> elemValue)
     , value : SpacePolicy -> Parser elemValue
     , fundef : Maybe { -- Optional arguments to records, e.g. { myfun arg1 arg2 = value }
          arguments : Parser (List arguments),
          buildValue : List arguments -> elemValue -> elemValue
       }
     , combiner : WS -> List (WS, WS, String, WS, elemValue) -> WS -> record
     , beforeSpacePolicy: SpacePolicy
     , optionalInitParser: Maybe
       { initItem : Parser elemInit
       , combinerInit : WS -> elemInit -> WS -> List (WS, WS, String, WS, elemValue) -> WS -> record
       }
     }
  -> ParserI record
genericRecord { key, equalSign, optNoEqualSign, value, fundef, combiner, beforeSpacePolicy, optionalInitParser } =
  let keyValue = (inContext "record key or }" <| trackInfo <| key) |>
      andThen (\keyWithInfos ->
        succeed (\argList (wsBeforeEq, v) ->
           case argList of
             Nothing -> (keyWithInfos.val, wsBeforeEq, v)
             Just (builder, arguments) -> (keyWithInfos.val, wsBeforeEq, builder arguments v))
        |= Maybe.withDefault (succeed Nothing) (Maybe.map (\f -> map (\r -> Just (f.buildValue, r)) f.arguments) fundef)
        |= oneOf [
            succeed (,)
            |= delayedCommitMap (\a b -> a) spaces equalSign
            |= (let firstappargpolicy = sameLineOrIndentedByAtLeast (keyWithInfos.start.col - 1 {- The column index-} + 1 {- The indentation increment-}) in -- No newlines, or be at least indented after the keyword.
              inContext "record value" <| value firstappargpolicy
             )
          , case optNoEqualSign of
              Nothing -> fail "Expected ="
              Just f ->
                delayedCommitMap (\a b -> (a, b))
                  spaces
                  ( succeed identity
                   |. oneOf [ lookAhead (symbol ","), lookAhead (symbol "\r"), lookAhead (symbol "\n"), lookAhead (symbol "}")]
                   |= succeed (f keyWithInfos.val))
          ]
      )
  in
  lazy <| \_ ->
    inContext "generic record" <|
    oneOf
      ([ try <|
          genericEmptyRecord
            { combiner =
                \wsBefore wsAfter -> combiner wsBefore [] wsAfter,
              beforeSpacePolicy =
                beforeSpacePolicy
            }
      ] ++ (
        case optionalInitParser of
          Nothing -> []
          Just {initItem, combinerInit} ->
            [genericNonEmptyRecordWithInit
              { keyValue =
                  keyValue
              , combinerInit =
                  combinerInit
              , initItem =
                  initItem
              , beforeSpacePolicy =
                  beforeSpacePolicy }]
      ) ++ [
          genericNonEmptyRecord
            { keyValue =
                keyValue
             , combiner =
                combiner
            , beforeSpacePolicy =
                beforeSpacePolicy
            }
      ])

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------
-- If performance becomes an issue with the ambiguity between parentheses and
-- tuples, it may be necessary to combine them into a single combinator, but
-- for now we can just `try tuple` followed by `parens`.

genericTuple
  :  { beforeSpacePolicy : SpacePolicy
     , term : SpacePolicy -> Parser t
     , tagger : EBaseVal -> t
     , record : WS -> List (WS, WS, Ident, WS, t) -> WS -> r
     }
  -> ParserI r
genericTuple { beforeSpacePolicy, term, tagger, record } =
  let
    combiner wsBefore (fst, rest, wsBeforeEnd) =
      let
        name =
          "Tuple" ++ toString (1 + List.length rest)
        ctor =
          ( space0, space0, Lang.recordConstructorName, space0
          , tagger <|
              EString defaultQuoteChar name
          )

        entry : Int -> (WS, t) -> (WS, WS, Ident, WS, t)
        entry index (wsBeforeComma, binding) =
          (wsBeforeComma, space0, "_" ++ toString index, space0, binding)

        firstEntry =
          entry 1 (space0, fst)

        restEntries =
          Utils.indexedMapFrom 2 entry rest
      in
        record wsBefore (ctor :: firstEntry :: restEntries) wsBeforeEnd
  in
    lazy <| \_ ->
      paddedBefore
        combiner
        beforeSpacePolicy.first
        ( trackInfo <|
            succeed (,,)
              |. symbol "("
              |= term allSpacesPolicy
              |= repeat oneOrMore
                   ( try <| succeed (,)
                       |= spaces
                       |. symbol ","
                       |= term allSpacesPolicy
                   )
              |= spaces
              |. symbol ")"
        )

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
        |= sp.first
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
  char == '_' ||
  char == '$'

identifier : Parser Ident
identifier =
  LK.variable
    isRestChar
    isRestChar
    keywords

littleIdentifier : ParserI Ident
littleIdentifier =
  trackInfo <| identifier

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
      , source (symbol "||")
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
eString : ParserI EBaseVal
eString =
  inContext "single-line string" <|
    trackInfo <|
      (singleLineString |> map (\(quoteChar, content) -> EString quoteChar content))

multiLineInterpolatedString : SpacePolicy -> Parser Exp
multiLineInterpolatedString sp =
  inContext "multi-line interpolated string" <|
    mapExp_ <|
    trackInfo <|
      delayedCommitMap (\wsBefore e -> EParens wsBefore e LongStringSyntax space0 )
      (succeed identity
        |= sp.first
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
      succeed (\v -> exp_ <| EOp space0 (withInfo ToStrExceptStr v.start v.start) [v] space0)
      |= variableExpression spacesWithoutNewline,
    try <| lazy <| \_ -> multilineGenericLetBinding,
    lazy <| \_ ->
      ( mapExp_ <| trackInfo <|
          (succeed (\exp -> (EOp space0 (withInfo ToStrExceptStr exp.start exp.start) [withInfo (exp_ <| EParens space0 exp ElmSyntax space0) exp.start exp.end] space0))
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
                     (withDummyExpInfo <| EHole space0 Nothing)
                     space0
             )
             letdefWithInfo.start
             binding.end
       )
      |= (trackInfo <| source <| keyword "let")
      |= optional (keyword "rec")
      |= pattern spacesWithoutNewline
      |= repeat zeroOrMore (pattern spacesWithoutNewline)
      |= spacesWithoutNewline.first
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
      [ eString
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
    paddedBefore (\ws name -> PVar ws name noWidgetDecl) sp.first ident

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
        sp.first
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
      paddedBefore PConst sp.first num

--------------------------------------------------------------------------------
-- Base Values
--------------------------------------------------------------------------------

baseValuePattern : SpacePolicy -> Parser Pat
baseValuePattern sp =
  inContext "base value pattern" <|
    mapPat_ <|
      paddedBefore PBase sp.first baseValue

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
              pattern allSpacesPolicy
          , tailItem =
              pattern allSpacesPolicy
          , combiner =
              \wsBefore members wsBeforeEnd ->
                -- PList wsBefore members space0 Nothing wsBeforeEnd
                PList wsBefore (Utils.listValues members) space0 Nothing wsBeforeEnd
          , combinerTail =
              \wsBefore members wsMiddle tail wsBeforeEnd ->
                -- PList wsBefore members wsMiddle (Just tail) wsBeforeEnd
                PList wsBefore (Utils.listValues members) wsMiddle (Just tail) wsBeforeEnd
          , beforeSpacePolicy =
              sp
          }

--------------------------------------------------------------------------------
-- Records
--------------------------------------------------------------------------------

recordPattern : SpacePolicy -> Parser Pat
recordPattern sp =
  inContext "record pattern" <|
    lazy <| \_ ->
      mapPat_ <|
        genericRecord
         { key = identifier
         , equalSign = symbol "="
         , optNoEqualSign = Just (\name -> withDummyPatInfo <| PVar space0 name noWidgetDecl)
         , value = pattern -- All spaces because = requires a value afterwards.
         , fundef = Nothing
         , combiner =
            \wsBefore keyValues wsAfter ->
              PRecord wsBefore keyValues wsAfter
         , beforeSpacePolicy = sp
         , optionalInitParser = Nothing}

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

tuplePattern : SpacePolicy -> Parser Pat
tuplePattern sp =
  inContext "tuple pattern" <|
    lazy <| \_ ->
      mapPat_ <|
        genericTuple
          { beforeSpacePolicy =
              sp
          , term =
              pattern
          , tagger =
              withDummyPatInfo << PBase space0
          , record =
              PRecord
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
          sp.first
          ( trackInfo <|
              succeed (,)
                |. symbol "("
                |= pattern allSpacesPolicy
                |= spaces
                |. symbol ")"
          )

simplePattern : SpacePolicy -> Parser Pat
simplePattern sp =
  inContext "simple pattern" <|
    oneOf
      [ lazy <| \_ -> listPattern sp
      , lazy <| \_ -> recordPattern sp
      , lazy <| \_ -> try <| tuplePattern sp
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
        { spacePolicy =
           sp
        , greedySpaceParser =
            spaces
        , precedenceTable =
            builtInPatternPrecedenceTable
        , minimumPrecedence =
            0
        , expression =
            simplePattern
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
      ( sp.first )
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
    paddedBefore TNamed sp.first bigIdentifier

--------------------------------------------------------------------------------
-- Variable Types
--------------------------------------------------------------------------------

variableType : SpacePolicy -> Parser Type
variableType sp =
  inContext "variable type" <|
    paddedBefore TVar sp.first littleIdentifier

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
          |= repeat oneOrMore (typ allSpacesPolicy)

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
          |= typ allSpacesPolicy

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
            |= typ allSpacesPolicy
            |= typ allSpacesPolicy
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
            typ allSpacesPolicy
        , tailItem =
            typ allSpacesPolicy
        , combiner =
            ( \wsBefore heads wsEnd ->
                -- TTuple wsBefore heads space0 Nothing wsEnd
                TTuple wsBefore (Utils.listValues heads) space0 Nothing wsEnd
            )
        , combinerTail =
            ( \wsBefore heads wsMiddle tail wsEnd ->
                -- TTuple wsBefore heads wsMiddle (Just tail) wsEnd
                TTuple wsBefore (Utils.listValues heads) wsMiddle (Just tail) wsEnd
            )
        , beforeSpacePolicy =
            sp
        }

recordType : SpacePolicy -> Parser Type
recordType sp =
  inContext "record type" <|
    lazy <| \_ ->
        genericRecord
         { key = identifier
         , equalSign = symbol ":"
         , optNoEqualSign = Nothing
         , value = typ
         , fundef = Nothing
         , combiner =
            \wsBefore keyValues wsAfter ->
              TRecord wsBefore Nothing keyValues wsAfter
         , beforeSpacePolicy = sp
         , optionalInitParser = Just { initItem = identifier
           ,  combinerInit = \wsBefore init wsBar elems wsEnd -> TRecord wsBefore (Just (init, wsBar)) elems wsEnd
           }
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
              |= typ allSpacesPolicy
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
          |= repeat oneOrMore (typ allSpacesPolicy)

--------------------------------------------------------------------------------
-- Wildcard Type
--------------------------------------------------------------------------------

wildcardType : SpacePolicy -> Parser Type
wildcardType sp =
  inContext "wildcard type" <|
    spaceSaverKeyword sp.first "_" TWildcard

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
        , lazy <| \_ -> recordType sp
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
    "dict" ->
      Just DictFromList
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
    "replaceAllIn" ->
      Just RegexReplaceAllIn
    "replaceFirstIn" ->
      Just RegexReplaceFirstIn
    "extractFirstIn" ->
      Just RegexExtractFirstIn
    _ ->
      Nothing

--------------------------------------------------------------------------------
-- Operator Parsing
--------------------------------------------------------------------------------

operator : SpacePolicy -> ParserI Operator
operator sp =
  paddedBefore (,) sp.first symbolIdentifier

patternOperator : SpacePolicy -> ParserI Operator
patternOperator sp =
  paddedBefore (,) sp.first patternSymbolIdentifier

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
        sp.first
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
      paddedBefore EBase sp.first baseValue

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

variableExpression : SpacePolicy -> Parser Exp
variableExpression sp =
  mapExp_ <| paddedBefore EVar sp.first littleIdentifier

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
          sp.first
          ( trackInfo <|
              succeed (,)
                |. symbol "\\"
                |= repeat oneOrMore (pattern allSpacesPolicy)
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
              expression allSpacesPolicy
          , tailItem =
              expression allSpacesPolicy
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
-- Records
--------------------------------------------------------------------------------

record : SpacePolicy -> Parser Exp
record sp =
  inContext "record expression" <|
    lazy <| \_ ->
      mapExp_ <|
        genericRecord
         { key =  identifier
         , equalSign = symbol "="
         , optNoEqualSign = Nothing
         , value = expression
         , fundef = Just { -- Optional arguments to records, e.g. { myfun arg1 arg2 = value }
           arguments = repeat zeroOrMore (pattern allSpacesPolicy),
           buildValue = \parameters binding_ ->
             if List.isEmpty parameters then
               binding_
             else
               withInfo
                 (exp_ <| EFun space0 parameters binding_ space0)
                 binding_.start
                 binding_.end
         }
         , combiner =
            \wsBefore keyValues wsAfter ->
              ERecord wsBefore Nothing keyValues wsAfter
         , beforeSpacePolicy = sp
         , optionalInitParser = Just { initItem = expression allSpacesPolicy
           , combinerInit = \wsBefore init wsBar elems wsEnd -> ERecord wsBefore (Just (init, wsBar)) elems wsEnd
           }
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
          sp.first
          ( trackInfo <|
              delayedCommit (keywordWithSpace "if") <|
                succeed (,,,,)
                  |= expression allSpacesPolicy
                  |= spaces
                  |. keywordWithSpace "then"
                  |= expression allSpacesPolicy
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
        branch: Parser WS -> Parser Branch
        branch branchsp =
          delayedCommitMap
            (\wsBefore (p, wsBeforeArrow, e) ->
                withInfo (Branch_ wsBefore p e wsBeforeArrow) p.start e.end
            )
            ( inContext "Indentation for branch" <| branchsp) -- Tries to consume spaces and correct indentation.
            ( inContext "Branch" <|
                succeed identity
                  |= (pattern { spacesWithoutNewline | first = nospace }
                     |> andThen (\p ->
                        let minimumIndentation = sameLineOrIndentedByAtLeast ((p.start.col - 1) {- The column index-} + 1 {- The indentation increment-}) in
                        succeed (\wsBeforeArrow e -> (p, wsBeforeArrow, e))
                        |= spaces
                        |. symbol "->"
                        |= expression minimumIndentation
                        |. optional (delayedCommit minimumIndentation.first (symbol ";"))
                     ))
                  )
      in
      let branchHelper: List Branch -> Parser Branch -> Parser (List Branch)
          branchHelper prevBranches branchParser =
            oneOf [
              branchParser |> andThen (\b -> branchHelper (b::prevBranches) branchParser)
              , succeed (List.reverse prevBranches)
            ]
      in
        mapExp_ <|
          paddedBefore
            ( \wsBefore (examinedExpression, wsBeforeOf, branches) ->
                case examinedExpression.val.e__ of
                  EVar _ " $implicitcase" -> -- Needs to be wrapped in a lambda
                    EFun space1 [withDummyPatInfo <| PVar space0 " $implicitcase" noWidgetDecl] (
                      withInfo (exp_ <| ECase wsBefore examinedExpression branches wsBeforeOf)
                        wsBefore.start wsBeforeOf.end
                      ) space0
                  _ -> ECase wsBefore examinedExpression branches wsBeforeOf
            )
            sp.first
            ( trackInfo <|
                delayedCommit (keywordWithSpace "case") <|
                  succeed (,,)
                    |= oneOf [
                         expression allSpacesPolicy,
                         succeed (withDummyExpInfo <| EVar space1 " $implicitcase") -- Unparsable var
                       ]
                    |= spaces
                    |. keyword "of"
                    |= (
                        branch allSpacesPolicy.first |>
                        andThen (\b ->
                          case b.val of
                            Branch_ wsBefore p e wsBeforeArrow ->
                              let branchIndentation = sameLineOrIndentedByExactly (p.start.col - 1 {- The column index-}) in
                              branchHelper [b] (branch branchIndentation.first)
                        )
                      )
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
          sp.first
          ( trackInfo <|
              delayedCommit (keywordWithSpace letkeyword) <|
                succeed (,,,,,)
                  |= pattern allSpacesPolicy
                  |= repeat zeroOrMore (pattern allSpacesPolicy)
                  |= spaces
                  |. symbol "="
                  |= expression allSpacesPolicy
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
        sp.first
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
          ( sp.first )
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
          sp.first
          ( trackInfo <|
              succeed (,)
                |. symbol "("
                |= expression allSpacesPolicy
                |= spaces
                |. symbol ")"
          )

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

tuple : SpacePolicy -> Parser Exp
tuple sp =
  inContext "tuple" <|
    lazy <| \_ ->
      mapExp_ <|
        genericTuple
          { beforeSpacePolicy =
              sp
          , term =
              expression
          , tagger =
              withDummyExpInfo << EBase space0
          , record =
              \ws1 entries ws2 ->
                ERecord ws1 Nothing entries ws2
          }

--------------------------------------------------------------------------------
-- Holes
--------------------------------------------------------------------------------

hole : SpacePolicy -> Parser Exp
hole sp =
  inContext "hole" <|
    mapExp_ <|
      paddedBefore EHole sp.first (trackInfo <| token "??" Nothing)

--------------------------------------------------------------------------------
-- General Expressions
--------------------------------------------------------------------------------

selection : SpacePolicy -> Parser (Exp -> Exp)
selection sp =
  delayedCommitMap (\wsBeforeDot (wsAfterDot,idWithInfo) exp ->
      let wsBefore = precedingWhitespaceWithInfoExp exp in
      let expWithoutWhitespace = mapPrecedingWhitespace (\_ -> "") exp in
      withInfo (exp_ <| ESelect wsBefore expWithoutWhitespace wsBeforeDot wsAfterDot idWithInfo.val)
         exp.start idWithInfo.end
    )
    sp.first
    (succeed (,)
     |. symbol "."
     |= sp.apparg
     |= (trackInfo <| identifier)
    )

-- Add all following .identifier1.identifiers2 as wrappers to the original expression
addSelections : SpacePolicy -> Parser Exp -> Parser Exp
addSelections sp parser =
  succeed (\simpExp selections ->
      List.foldl (\sel updatedExp -> sel updatedExp) simpExp selections
      )
    |= parser
    |= repeat zeroOrMore (selection sp)

-- Not a function application nor a binary operator
simpleExpression : SpacePolicy -> Parser Exp
simpleExpression sp =
  oneOf
    [ constantExpression sp
    , lazy <| \_ -> multiLineInterpolatedString sp
    , baseValueExpression sp
    , lazy <| \_ -> function sp
    , lazy <| \_ -> (addSelections sp <| list sp)
    , lazy <| \_ -> (addSelections sp <| record sp)
    , lazy <| \_ -> conditional sp
    , lazy <| \_ -> caseExpression sp
    , lazy <| \_ -> letrecBinding sp
    , lazy <| \_ -> letBinding sp
    , lazy <| \_ -> lineComment sp
    , lazy <| \_ -> option sp
    , lazy <| \_ -> (addSelections sp <| try <| tuple sp)
    , lazy <| \_ -> (addSelections sp <| parens sp)
    , lazy <| \_ -> (addSelections sp <| hole sp)
    -- , lazy <| \_ -> typeCaseExpression
    -- , lazy <| \_ -> typeAlias
    -- , lazy <| \_ -> typeDeclaration
    , (addSelections sp <| variableExpression sp)
  ]

spaceColonType: SpacePolicy -> Parser (WS, Type)
spaceColonType sp =
  lazy <| \_ ->
     try ( succeed (,)
          |= sp.first
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
        |= simpleExpression sp
        |= repeat zeroOrMore (simpleExpression {sp | first = sp.apparg})

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
      |= ParserUtils.optional (spaceColonType { sp | first = sp.apparg })
    )

-- No indentation for top-level expressions, at least one newline or the beginning of the string.
topLevelBetweenDefSpacePolicty: SpacePolicy
topLevelBetweenDefSpacePolicty =
  { first = LangParserUtils.spacesWithoutIndentation,
    apparg = LangParserUtils.spacesWithoutIndentation
  }

topLevelInsideDefSpacePolicy: SpacePolicy
topLevelInsideDefSpacePolicy =
  {first = LangParserUtils.spacesNotBetweenDefs,
   apparg = LangParserUtils.spacesNotBetweenDefs }

spacesWithoutNewline: SpacePolicy
spacesWithoutNewline =
  { first = LangParserUtils.spacesWithoutNewline,
    apparg = LangParserUtils.spacesWithoutNewline }

allSpacesPolicy: SpacePolicy
allSpacesPolicy = { first = spaces, apparg = spaces }

sameLineOrIndentedByAtLeast: Int -> SpacePolicy
sameLineOrIndentedByAtLeast nSpaces =
  let indentedParser = LangParserUtils.spacesDefault <| ParserUtils.keepRegex <| Regex.regex <| "( |\t)*((r?\n *)*(\r?\n" ++ String.repeat nSpaces " " ++ " *))?" in
  SpacePolicy indentedParser indentedParser


sameLineOrIndentedByExactly: Int -> SpacePolicy
sameLineOrIndentedByExactly nSpaces =
  let indentedParser = LangParserUtils.spacesDefault <| ParserUtils.keepRegex <|  Regex.regex <| "( |\t)*((\r?\n *)*(\r?\n" ++ String.repeat nSpaces " " ++ "))?" in
  SpacePolicy indentedParser (sameLineOrIndentedByAtLeast nSpaces).apparg


expression : SpacePolicy -> Parser Exp
expression sp =
  inContext "expression" <|
    lazy <| \_ ->
      binaryOperator
        { spacePolicy =
            sp
        , greedySpaceParser =
            spaces
        , precedenceTable =
            builtInPrecedenceTable
        , minimumPrecedence =
            0
        , expression =
            simpleExpressionWithPossibleArguments
        , operator =
            operator {sp | first = sp.apparg}
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
                              EApp space0 opExp [ left, right ] InfixApp space0
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
          |= topLevelBetweenDefSpacePolicty.first
          |= pattern (topLevelInsideDefSpacePolicy)
          |= repeat zeroOrMore (pattern topLevelInsideDefSpacePolicy)
          |= topLevelInsideDefSpacePolicy.first
          |. symbol "="
      )
      ( succeed identity
          |= expression topLevelInsideDefSpacePolicy
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
            |= pattern topLevelInsideDefSpacePolicy
            |= topLevelInsideDefSpacePolicy.first
            |. symbol ":"
        )
        ( succeed identity
          |= typ topLevelInsideDefSpacePolicy
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
          |= typePattern topLevelInsideDefSpacePolicy
          |. topLevelInsideDefSpacePolicy.first
          |. symbol "="
      )
      ( succeed identity
        |= typ topLevelInsideDefSpacePolicy
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
          |. spacesWithoutNewline.first
          |= trackInfo
               ( keep zeroOrMore <| \c ->
                   c /= '\n' && c /= ' ' && c /= ':'
               )
          |. symbol ":"
          |= spacesWithoutNewline.first
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
    [ expression allSpacesPolicy
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
  run (map freshen program)

parseT : String -> Result P.Error Type
parseT =
  run (typ topLevelInsideDefSpacePolicy)



--------------------------------------------------------------------------------

-- The rest of this file is basically copied from FastParser,
-- so that ElmParser can be a drop-in replacement
-- and FasterParser can be deprecated soon.
-- At that point, it will probably be nicer to move a bunch of
-- these definitions elsewhere, maybe into Prelude directly.

validIdentifierRestChar : Char -> Bool
validIdentifierRestChar c =
  Char.isLower c || Char.isUpper c || Char.isDigit c || c == '_' || c == '\''

validVariableIdentifierFirstChar : Char -> Bool
validVariableIdentifierFirstChar c =
  Char.isLower c || c == '_'

--------------------------------------------------------------------------------
-- Code from old parser
--------------------------------------------------------------------------------

-- Cousin of variableIdentifierString.
-- Removes invalid characters.
sanitizeVariableName : String -> String
sanitizeVariableName unsafeName =
  unsafeName
  |> String.toList
  |> Utils.dropWhile (not << (Char.toLower >> validVariableIdentifierFirstChar))
  |> Utils.mapHead Char.toLower
  |> Utils.changeTail (List.filter validIdentifierRestChar)
  |> String.fromList

(prelude, initK) =
  Prelude.preludeLeo
    |> run program  -- not parse, since don't want to call freshen
    |> (\i ->
        case i of
          Ok k -> k
          Err msg -> Debug.crash <|  "In prelude.leo" ++ ParserUtils.showError msg
        )
    |> freshenClean 1 -- need this?

preludeIds = allIds prelude

isPreludeLoc : Loc -> Bool
isPreludeLoc (k,_,_) = isPreludeLocId k

isPreludeLocId : LocId -> Bool
isPreludeLocId k = k < initK

isPreludeEId : EId -> Bool
isPreludeEId k = k < initK

isActualEId : EId -> Bool
isActualEId eid = eid >= 0

isProgramEId : EId -> Bool
isProgramEId eid = isActualEId eid && not (isPreludeEId eid)

clearAllIds : Exp -> Exp
clearAllIds root =
  mapExp clearNodeIds root

-- assign EId's and locId's
-- existing unique EId's/locId's are preserved
-- duplicated and dummy EId's/locId's are reassigned

freshen : Exp -> Exp
freshen e =
  -- let _ = Debug.log ("To Freshen:\n" ++ LangUnparser.unparseWithIds e) () in
  let (duplicateIds, allIds) = duplicateAndAllIds e in
  -- let _ = Debug.log "Duplicate Ids" duplicateIds in
  -- let _ = Debug.log "All Ids" allIds in
  let idsToPreserve = Set.diff allIds duplicateIds in
  -- let _ = Debug.log "Ids to preserve" idsToPreserve in
  let startK      = (List.maximum (initK :: Set.toList allIds) |> Utils.fromJust) + 1 in
  let (result, _) = freshenPreserving idsToPreserve startK e in
  -- let _ = Debug.log ("Freshened result:\n" ++ LangUnparser.unparseWithIds result) () in
  result

-- Overwrite any existing EId's/locId's
freshenClean : Int -> Exp -> (Exp, Int)
freshenClean initK e = freshenPreserving Set.empty initK e

-- Reassign any id not in idsToPreserve
freshenPreserving : Set.Set Int -> Int -> Exp -> (Exp, Int)
freshenPreserving idsToPreserve initK e =
  let getId k =
    if Set.member k idsToPreserve
    then getId (k+1)
    else k
  in
  let assignIds exp k =
    let e__ = exp.val.e__ in
    let (newE__, newK) =
      case e__ of
        EConst ws n (locId, frozen, ident) wd ->
          if Set.member locId idsToPreserve then
            (e__, k)
          else
            let locId = getId k in
            (EConst ws n (locId, frozen, ident) wd, locId + 1)

        ELet ws1 kind b p ws2 e1 ws3 e2 ws4 ->
          let (newP, newK) = freshenPatPreserving idsToPreserve k p in
          let newE1 = recordIdentifiers (newP, e1) in
          (ELet ws1 kind b newP ws2 newE1 ws3 e2 ws4, newK)

        EFun ws1 pats body ws2 ->
          let (newPats, newK) = freshenPatsPreserving idsToPreserve k pats in
          (EFun ws1 newPats body ws2, newK)

        ECase ws1 scrutinee branches ws2 ->
          let (newBranches, newK) =
            branches
            |> List.foldl
                (\branch (newBranches, k) ->
                  let (Branch_ bws1 pat ei bws2) = branch.val in
                  let (newPi, newK) = freshenPatPreserving idsToPreserve k pat in
                  (newBranches ++ [{ branch | val = Branch_ bws1 newPi ei bws2 }], newK)
                )
                ([], k)
          in
          (ECase ws1 scrutinee newBranches ws2, newK)

        ETyp ws1 pat tipe e ws2 ->
          let (newPat, newK) = freshenPatPreserving idsToPreserve k pat in
          (ETyp ws1 newPat tipe e ws2, newK)

        ETypeAlias ws1 pat tipe e ws2 ->
          let (newPat, newK) = freshenPatPreserving idsToPreserve k pat in
          (ETypeAlias ws1 newPat tipe e ws2, newK)

        _ ->
          (e__, k)
    in
    if Set.member exp.val.eid idsToPreserve then
      (replaceE__ exp newE__, newK)
    else
      let eid = getId newK in
      (WithInfo (Exp_ newE__ eid) exp.start exp.end, eid + 1)
  in
  mapFoldExp assignIds initK e


-- Reassign any id not in idsToPreserve
freshenPatsPreserving : Set.Set Int -> Int -> List Pat -> (List Pat, Int)
freshenPatsPreserving idsToPreserve initK pats =
  pats
  |> List.foldl
      (\pat (finalPats, k) ->
        let (newPat, newK) = freshenPatPreserving idsToPreserve k pat in
        (finalPats ++ [newPat], newK)
      )
      ([], initK)


-- Reassign any id not in idsToPreserve
freshenPatPreserving : Set.Set Int -> Int -> Pat -> (Pat, Int)
freshenPatPreserving idsToPreserve initK p =
  let getId k =
    if Set.member k idsToPreserve
    then getId (k+1)
    else k
  in
  let assignIds pat k =
    if Set.member pat.val.pid idsToPreserve then
      (pat, k)
    else
      let pid = getId k in
      (setPId pid pat, pid + 1)
  in
  mapFoldPatTopDown assignIds initK p

maxId : Exp -> Int
maxId exp =
  let ids = allIds exp in
  List.maximum (initK :: Set.toList ids) |> Utils.fromJust

-- Excludes EIds, PIds, and locIds less than initK (i.e. no prelude locs or dummy EIds)
allIds : Exp -> Set.Set Int
allIds exp = duplicateAndAllIds exp |> Tuple.second

-- Raw list of all ids
allIdsRaw : Exp -> List Int
allIdsRaw exp =
  let pidsInPat pat   = flattenPatTree pat |> List.map (.val >> .pid) in
  let pidsInPats pats = pats |> List.concatMap pidsInPat in
  let flattened = flattenExpTree exp in
  let eids = flattened |> List.map (.val >> .eid) in
  let otherIds =
    flattened
    |> List.concatMap
        (\exp ->
          case exp.val.e__ of
            EConst ws n (locId, frozen, ident) wd -> [locId]
            ELet ws1 kind b p _ e1 _ e2 ws2       -> pidsInPat p
            EFun ws1 pats body ws2                -> pidsInPats pats
            ECase ws1 scrutinee branches ws2      -> pidsInPats (branchPats branches)
            ETyp ws1 pat tipe e ws2               -> pidsInPat pat
            ETypeAlias ws1 pat tipe e ws2         -> pidsInPat pat
            _                                     -> []
        )
  in
  eids ++ otherIds

-- Excludes EIds, PIds, and locIds less than initK (i.e. no prelude locs or dummy EIds)
duplicateAndAllIds : Exp -> (Set.Set Int, Set.Set Int)
duplicateAndAllIds exp =
  allIdsRaw exp
  |> List.foldl
      (\id (duplicateIds, seenIds) ->
        if id >= initK then
          if Set.member id seenIds
          then (Set.insert id duplicateIds, seenIds)
          else (duplicateIds, Set.insert id seenIds)
        else
          (duplicateIds, seenIds)
      )
      (Set.empty, Set.empty)

preludeSubst = substPlusOf_ Dict.empty prelude

substPlusOf : Exp -> SubstPlus
substPlusOf e =
  substPlusOf_ preludeSubst e

substOf : Exp -> Subst
substOf = Dict.map (always .val) << substPlusOf

substStrOf : Exp -> SubstStr
substStrOf = Dict.map (always toString) << substOf


-- Record the primary identifier in the EConsts_ Locs, where appropriate.
recordIdentifiers : (Pat, Exp) -> Exp
recordIdentifiers (p,e) =
 let ret e__ = WithInfo (Exp_ e__ e.val.eid) e.start e.end in
 case (p.val.p__, e.val.e__) of

  -- (PVar _ x _, EConst ws n (k, b, "") wd) -> ret <| EConst ws n (k, b, x) wd
  (PVar _ x _, EConst ws n (k, b, _) wd) -> ret <| EConst ws n (k, b, x) wd

  (PList _ ps _ mp _, EList ws1 es ws2 me ws3) ->
    case Utils.maybeZip ps (Utils.listValues es) of
      Nothing  -> ret <| EList ws1 es ws2 me ws3
      Just pes -> let es_ = List.map recordIdentifiers pes in
                  let me_ =
                    case (mp, me) of
                      (Just p1, Just e1) -> Just (recordIdentifiers (p1,e1))
                      _                  -> me in
                  ret <| EList ws1 (Utils.listValuesMake es es_) ws2 me_ ws3

  (PAs _ _ _ p_, _) -> recordIdentifiers (p_,e)

  (_, EColonType ws1 e1 ws2 t ws3) ->
    ret <| EColonType ws1 (recordIdentifiers (p,e1)) ws2 t ws3

  (_, e__) -> ret e__

-- this will be done while parsing eventually...

substPlusOf_ : SubstPlus -> Exp -> SubstPlus
substPlusOf_ substPlus exp =
  let accumulator e s =
    case e.val.e__ of
      EConst _ n (locId,_,_) _ ->
        case Dict.get locId s of
          Nothing ->
            Dict.insert locId { e | val = n } s
          Just existing ->
            if n == existing.val then
              s
            else
              Debug.crash <| "substPlusOf_ Duplicate locId " ++ toString locId ++ " with differing value " ++ toString n ++ "\n" ++ "BLAH" -- LangUnparser.unparseWithIds exp
      _ -> s
  in
  foldExp accumulator substPlus exp

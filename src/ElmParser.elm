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
  , preludeNotParsed
  , prelude, isPreludeLoc, isPreludeLocId, isPreludeEId, isActualEId, isProgramEId
  , substOf, substStrOf, substPlusOf
  , sanitizeVariableName
  , clearAllIds
  , freshen
  , maxId
  , implicitVarName
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
import HTMLParser
import ImpureGoodies

--==============================================================================
--= Helpers
--==============================================================================

implicitVarName = " $implicitcase"

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

genericEmptyList
  :  { combiner : WS -> WS -> list
     }
  -> ParserI (WS -> list)
genericEmptyList { combiner } =
  transferInfo (flip combiner) <|
    trackInfo <|
      succeed identity
        |. symbol "["
        |= spaces
        |. symbol "]"

genericNonEmptyList
  :  { item : Parser elem
     , combiner : WS -> List (WS, elem) -> WS -> list
     }
  -> ParserI (WS -> list)
genericNonEmptyList { item, combiner }=
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
      transferInfo
        ( \(members, wsBeforeEnd) spaceBefore ->
            combiner spaceBefore members wsBeforeEnd
        )
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
     }
  -> ParserI (WS -> list)
genericNonEmptyListWithTail { item, tailItem, combinerTail }=
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
      transferInfo
        ( \(members, wsMiddle, thetail, wsBeforeEnd) spaceBefore ->
            combinerTail spaceBefore members wsMiddle thetail wsBeforeEnd
        )
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
     }
  -> ParserI (WS -> list)
genericList { item, tailItem, combiner, combinerTail } =
  lazy <| \_ ->
    oneOf
      [ try <|
          genericEmptyList
            { combiner =
                \wsBefore wsAfter -> combiner wsBefore [] wsAfter,
            }
      , try <|
          genericNonEmptyList
            { item =
                item
            , combiner =
                combiner
            }
      , genericNonEmptyListWithTail
          { item =
              item
          , combinerTail =
              combinerTail
          , tailItem =
              tailItem }
      ]

--------------------------------------------------------------------------------
-- Records
--------------------------------------------------------------------------------

genericEmptyRecord
  :  { combiner : WS -> WS -> record
     }
  -> ParserI (WS -> record)
genericEmptyRecord { combiner} =
  inContext "generic empty record" <|
  transferInfo (flip combiner) <|
    trackInfo <|
      succeed identity
        |. symbol "{"
        |= spaces
        |. symbol "}"


genericNonEmptyRecord
  :  { keyValue : Parser (String, WS, elemValue)
     , combiner : WS -> List (WS, WS, String, WS, elemValue) -> WS -> record
     }
  -> ParserI (WS -> record)
genericNonEmptyRecord { keyValue, combiner }=
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
      transferInfo
        ( \(members, wsBeforeEnd) spaceBefore ->
            combiner spaceBefore members wsBeforeEnd
        )
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
     }
  -> ParserI (WS -> record)
genericNonEmptyRecordWithInit { keyValue, initItem, combinerInit }=
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
          delayedCommitMap (\(init, wsBeforeBar) (members, wsEnd) wsBefore ->
            combinerInit wsBefore init wsBeforeBar members wsEnd)
            (succeed (\init wsBeforeBar -> (init, wsBeforeBar))
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
     , optionalInitParser: Maybe
       { initItem : Parser elemInit
       , combinerInit : WS -> elemInit -> WS -> List (WS, WS, String, WS, elemValue) -> WS -> record
       }
     }
  -> ParserI (WS -> record)
genericRecord { key, equalSign, optNoEqualSign, value, fundef, combiner, optionalInitParser } =
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
            |= (let firstappargpolicy =
                  sameLineOrIndentedByAtLeast "for a value in this record" (keyWithInfos.start.col - 1 {- The column index-} + 1 {- The indentation increment-}) in -- No newlines, or be at least indented after the keyword.
              inContext "record value" <| value { first = spaces, apparg = firstappargpolicy }
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
                  initItem }]
      ) ++ [
          genericNonEmptyRecord
            { keyValue =
                keyValue
             , combiner =
                combiner
            }
      ])

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

genericTuple
  :  { term : SpacePolicy -> Parser t
     , tagger : String -> t
     , one : WS -> t -> WS -> r
     , record : WS -> List (WS, WS, Ident, WS, t) -> WS -> r
     , implicitFun: Maybe (Ident -> (pvar, t), WS -> List pvar -> r -> r)
     }
  -> ParserI (WS -> r)
genericTuple { term, tagger, one, record, implicitFun } =
  let
    combiner:(t, List (WS, t), WS)       -> WS -> r
    combiner (fst, rest, wsBeforeEnd) wsBefore =
      if List.isEmpty rest then
        one wsBefore fst wsBeforeEnd
      else
      let
        name =
          Lang.ctorTupleName (1 + List.length rest)

        ctorEntry =
          Lang.ctor tagger Lang.TupleCtor name

        firstEntry =
          Lang.numericalEntry 1 (space0, fst)

        restEntries =
          Utils.indexedMapFrom 2 Lang.numericalEntry rest
      in
        record wsBefore (ctorEntry :: firstEntry :: restEntries) wsBeforeEnd

    combinerZero: WS -> WS -> r
    combinerZero wsBeforeEnd wsBefore =
      let
        name =
          Lang.ctorTupleName 0
        ctorEntry =
          Lang.ctor tagger Lang.TupleCtor name
      in
      record wsBefore [ctorEntry] wsBeforeEnd

    implicitTupling = flip Maybe.map implicitFun <| \(identToPvarVar, funBuilder) numberOfCommas wsBefore ->
      let (firstPvar, firstVar) = identToPvarVar <| implicitVarName in
      let (restPvar, restVar) = List.range 1 numberOfCommas |> List.map (\i ->
           implicitVarName ++ toString i
        ) |> List.map identToPvarVar |> List.unzip
      in
      funBuilder wsBefore (firstPvar::restPvar) (combiner (firstVar, (List.map ((,) space0) restVar), space0) space0)

  in
    lazy <| \_ ->
      delayedCommitMap (\pos posToParens -> posToParens pos)
        getPos
        (succeed identity
      |. symbol "(" -- Here we loose the start information (and recover it using setStartInfo) but we gain in efficiency..
      |= (oneOf <| [setStartInfo (oneOf [
          transferInfo combiner -- One or more elements
            ( trackInfo <|
                succeed (,,)

                  |= term allSpacesPolicy
                  |= repeat zeroOrMore
                       ( delayedCommitMap (\ws term -> (ws, term))
                          spaces
                          (succeed identity
                           |. symbol ","
                           |= term allSpacesPolicy)
                       )
                  |= spaces
                  |. symbol ")"
            ),
          transferInfo combinerZero -- Unit ()
          (trackInfo <| (
            succeed identity
            |= spaces
            |. symbol ")"))
        ])] ++ (case implicitTupling of
          Nothing -> []
          Just commasToImplicitFun ->
            [setStartInfo (trackInfo <| map (String.length >> commasToImplicitFun)
                 (keep oneOrMore (\c -> c == ',')
              |. symbol ")"))])))

--------------------------------------------------------------------------------
-- Block Helper (for types) TODO
--------------------------------------------------------------------------------

block
  : (WS -> a -> WS -> b) -> String -> String -> Parser a -> ParserI (WS -> b)
block combiner openSymbol closeSymbol p =
  delayedCommitMap
    ( \open (result, wsEnd, close) ->
        withInfo
          (\wsBefore -> combiner wsBefore result wsEnd)
          open.start
          close.end
    )
    (trackInfo (symbol openSymbol))
    ( succeed (,,)
        |= p
        |= spaces
        |= trackInfo (symbol closeSymbol)
    )

parenBlock : (WS -> a -> WS -> b) -> Parser a -> ParserI (WS -> b)
parenBlock combiner = block combiner "(" ")"

bracketBlock : (WS -> a -> WS -> b) -> Parser a -> ParserI (WS -> b)
bracketBlock combiner = block combiner "[" "]"

blockIgnoreWS : String -> String -> Parser a -> ParserI (WS -> a)
blockIgnoreWS = block (\wsBefore x wsEnd -> x)

parenBlockIgnoreWS : Parser a -> ParserI (WS -> a)
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
    , "True"
    , "False"
    ]



isFirstChar : Char -> Bool
isFirstChar char =
  Char.isLower char ||
  Char.isUpper char ||
  char == '_' ||
  char == '$'

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
    isFirstChar
    isRestChar
    keywords

anyIdentifier : ParserI Ident
anyIdentifier =
  trackInfo identifier

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
      , source (symbol "||")
      , keep oneOrMore (\x -> ElmLang.isSymbol x && not (x == '|'))
      ]

htmlAttributeSymbolIdentifier : ParserI Ident
htmlAttributeSymbolIdentifier =
  trackInfo <|
    oneOf
      [ source (symbol "<|")
      , source (symbol "::")
      , source (symbol "||")
      , keep oneOrMore (\x -> ElmLang.isSymbol x && not (x == '|' || x == '>' || x == '/'))
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

multiLineInterpolatedString : Parser (WS -> Exp)
multiLineInterpolatedString =
  inContext "multi-line interpolated string" <| lazy <| \_ ->
    mapWSExp_ <| trackInfo <|
      (succeed (\e wsBefore -> EParens wsBefore e LongStringSyntax space0 )
        |. symbol "\"\"\""
        |= multilineContentParser
        |. symbol "\"\"\""
      )

multilineParseUntilRegex = Regex.regex <| "@|\"\"\"(?!\")"

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
            (exp_ <| EOp space0 space1 (withInfo Plus head.end tailPart.start) [head, tailPart] space0)
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
    , (succeed (\potentialExp strAfterExp -> (potentialExp, strAfterExp))
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
                  lastPrev.start stringExp.end) :: lastTail
              _ ->
                multilineContentParserHelp (stringExp :: potentialExp:: prevExps)
          [] -> Debug.crash "Internal error: There should be always at least one expression in a longstring literal."
      )
  ]

multilineEscapedElmExpression: Parser Exp
multilineEscapedElmExpression =
  inContext "expression in multi-line string" <|
  oneOf [
    lazy <| \_ -> trackInfo <|
      succeed (\wsv -> let v = wsv space0 in exp_ <| EOp space0 space1 (withInfo ToStrExceptStr v.start v.start) [v] space0)
      |= (variableExpression |> addSelections False nospace),
    lazy <| \_ -> multilineGenericLetBinding,
    lazy <| \_ -> mapExp_ <| trackInfo <|
        (succeed (\wsToExp ->
          let exp = wsToExp space0 in
          (EOp space0 space1 (withInfo ToStrExceptStr exp.start exp.start) [
            withInfo (exp_ <| EParens space0 exp ElmSyntax space0) exp.start exp.end] space0))
        |= tuple)
  ]

multilineGenericLetBinding : Parser Exp
multilineGenericLetBinding =
  inContext ("let binding within a long string") <|
    lazy <| \_ ->
      succeed (\letdefWithInfo pattern parameters wsBeforeEq binding_  ->
         let isRec = letdefWithInfo.val == "letrec" in
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
                     isRec
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
      |= (trackInfo <| source <| oneOf [keywordWithSpace "let", keywordWithSpace "letrec"])
      |= pattern spacesWithoutNewline
      |= repeat zeroOrMore (pattern spacesWithoutNewline)
      |= spacesWithoutNewline.first
      |. symbol "="
      |= expression spacesWithoutNewline
      |. ignore zeroOrMore (\c -> c == ' ' || c == '\t')-- This will remove trailing whitespace.
      |. symbol "\n"


--------------------------------------------------------------------------------
-- HTML Literals
--------------------------------------------------------------------------------
htmlEscape = ImpureGoodies.htmlescape

forbiddenTagsInHtmlInner = Regex.regex "</[^>\n]*>"

htmlText: WithInfo a -> String -> Exp
htmlText  source        htmltext =
  let origin = replaceInfo source << exp_ in
  origin <| EList space0 [
    (space0, withInfo (exp_ <| EBase space0 (EString "\"" "TEXT")) source.start source.start),
    (space0, origin  <| EBase space0 (EString "\"" (ImpureGoodies.htmlunescape htmltext)))] space0 Nothing space0

htmlnode: WithInfo a -> HTMLParser.HTMLTag -> Exp ->     WS ->                    Exp ->  Bool ->     Bool ->     WS ->                  Exp
htmlnode source         tagName               attributes spaceBeforeEndOpeningTag children autoclosing voidClosing spaceAfterTagClosing =
  let origin = replaceInfo source << exp_ in
  let spaceBeforeTail = withDummyInfo <| if autoclosing then " " else if voidClosing then "  " else"" in -- Hack: If there is a space and no children, mark the element autoclose.
  let tag = case tagName of
    HTMLParser.HTMLTagString s -> withInfo (exp_ <| EBase space0 <| EString "\"" s.val) s.start s.end
    HTMLParser.HTMLTagExp e -> e
  in
  origin <| EList space0 [
    (space0, tag),
    (space0, attributes),
    (spaceBeforeEndOpeningTag, children)] spaceBeforeTail Nothing spaceAfterTagClosing

appendToLeft: (WS, Exp) -> Exp -> Result String Exp
appendToLeft thisAttribute x = case x.val.e__ of
  EList sp0 attrs sp1 t sp2 ->
    Ok <| withInfo (exp_ <| EList sp0 (thisAttribute::attrs) sp1 t sp2) (Tuple.second thisAttribute |> .start) x.end
  EApp sp1 fun [left, right] appType sp2 ->
    appendToLeft thisAttribute left |> Result.map (\newLeft ->
      withInfo (exp_ <| EApp sp1 fun [newLeft, right] appType sp2) (newLeft.start) x.end
    )
  _ -> Err <| "Expected EList, EApp, but got something else for attributes (line " ++ toString x.start.line ++ ")"

attrsToExp: Pos.Pos -> List HTMLParser.HTMLAttribute -> Parser Exp
attrsToExp lastPos attrs =
  case attrs of
    [] -> succeed <| withInfo (exp_ <| EList space0 [] space0 Nothing space0) lastPos lastPos
    head::tail ->
      let origin = replaceInfo head << exp_ in
      case head.val of
        HTMLParser.HTMLAttribute sp nameInfo value ->
          let nameExp = replaceInfo nameInfo <| exp_ <| EBase space0 (EString "\"" nameInfo.val) in
          let attrValueSpace =
            case value.val of
               HTMLParser.HTMLAttributeNoValue ->
                 -- Hack: space1 is here to tell that it's a NoValue.
                 Ok (space0, withDummyExpInfo <| EBase space1 <| EString "\"" "")
               HTMLParser.HTMLAttributeExp s e ->
                 -- Normally, all the space is inside s
                 let final_e = case nameInfo.val of
                      "style" -> replaceInfo e <| exp_ <| EApp space0 (withInfo (exp_ <| EVar space1 "__mbstylesplit__") e.start e.start) [e] SpaceApp space0
                      _ -> e
                 in
                 Ok (s, final_e)
               _ -> Err <| "[Internal error] Tried to convert " ++ toString head ++ " to an Exp"
          in
          case attrValueSpace of
            Err msg -> fail msg
            Ok (attrSpace, attrValue) ->
              let thisAttribute = replaceInfo head <| exp_ <| EList sp [
                 (space0, nameExp),
                 (attrSpace, attrValue)
                 ] space0 Nothing space0
              in
              attrsToExp head.end tail |> andThen (\tailAttrExp ->
                case appendToLeft (space0, thisAttribute) tailAttrExp of
                  Err msg -> fail msg
                  Ok newExp -> succeed newExp
              )
        HTMLParser.HTMLAttributeListExp sp e ->
           attrsToExp head.end tail |> map (\tailAttrExp ->
             let appendFun = replaceInfo head <| exp_ <| EVar space0 "++" in
             withInfo (exp_ <| EApp sp appendFun [
               withInfo (exp_ <| EApp space0 appendFun [
                 withInfo (exp_ <| EList space0 [] space0 Nothing space0) lastPos lastPos,
                 e] InfixApp space0 ) head.start e.end,
               tailAttrExp] InfixApp space0) head.start tailAttrExp.end
           )

childrenToExp: Pos.Pos -> List HTMLParser.HTMLNode -> Parser Exp
childrenToExp lastPos children =
  case children of
    [] -> succeed <| withInfo (exp_ <| EList space0 [] space0 Nothing space0) lastPos lastPos
    head::tail ->
      htmlToExp head |> andThen (\headExp ->
        childrenToExp head.end tail |> andThen (\tailExp ->
          case headExp.val.e__ of
            EApp _ _ _ _ _ -> -- It was a HTMLListNodeExp
               let appendFun = withInfo (exp_ <| EVar space0 "++") headExp.end tailExp.start in
               succeed <| withInfo (exp_ <| EApp space0 appendFun [
                 headExp, tailExp] InfixApp space0) headExp.start tailExp.end
            _ ->
              case appendToLeft (space0, headExp) tailExp of
                Err msg -> fail msg
                Ok newExp -> succeed newExp
        )
      )

htmlToExp: HTMLParser.HTMLNode -> Parser Exp
htmlToExp node =
  case node.val of
    HTMLParser.HTMLInner content ->
      case Regex.find (Regex.All) forbiddenTagsInHtmlInner content of
        [] -> succeed <| htmlText node content
        l ->
          succeed <| htmlText node <| Regex.replace (Regex.All) forbiddenTagsInHtmlInner (\_ -> "") content
          --fail <| "Line " ++ toString node.start.line ++ ", an inner html should not contain closing tags : " ++ (l |> List.map (\m -> m.match) |> String.join ",")
    HTMLParser.HTMLElement tagName attrs sp0 endOpeningStyle children closingStyle ->
      case (closingStyle, endOpeningStyle) of
        --(HTMLParser.ForgotClosing, _)   -> fail <| "Line " ++ toString node.start.line ++ " there is a missing closing tag </" ++ HTMLParser.unparseTagName tagName ++ ">"
        --(_, HTMLParser.SlashEndOpening) -> fail <| "Line " ++ toString node.start.line ++ " don't use / because <" ++ HTMLParser.unparseTagName tagName ++ "> is not an auto-closing tag and requires in any case a </" ++ HTMLParser.unparseTagName tagName ++ ">"
        _ ->
          let endPos = case tagName of
            HTMLParser.HTMLTagString v -> v.end
            HTMLParser.HTMLTagExp e -> e.end
          in
          attrsToExp endPos attrs |>
          andThen (\finalattrs ->
            childrenToExp { line = sp0.end.line, col = sp0.end.col + (if endOpeningStyle == HTMLParser.RegularEndOpening then 1 else 2) } children |>
              andThen (\finalchildren ->
                 succeed <| htmlnode node tagName finalattrs sp0 finalchildren
                   (closingStyle == HTMLParser.AutoClosing)
                   (closingStyle == HTMLParser.VoidClosing) <|
                   case closingStyle of
                     HTMLParser.RegularClosing sp -> sp
                     _ -> space0
              )
            )
    HTMLParser.HTMLComment commentStyle ->
      fail <| "Line " ++ toString node.start.line ++ ": comments are not supported by Elm at this moment. Got " ++ HTMLParser.unparseNode node
    HTMLParser.HTMLListNodeExp e ->
      let appendFun = withInfo (exp_ <| EVar space0 "++") node.start node.start in
      succeed <| withInfo (exp_ <| EApp space0 appendFun [
        withInfo (exp_ <| EList space0 [] space0 Nothing space0) node.start node.start,
        withInfo (exp_ <| EApp space1 (withInfo (exp_ <| EVar space1  "__mbwraphtmlnode__") e.start e.start) [e] SpaceApp space0) e.start e.end
        ] InfixApp space0 ) node.start e.end

wrapWithSyntax: ParensStyle -> Parser (WS -> Exp) -> Parser Exp
wrapWithSyntax parensStyle parser =
  succeed (\wsToE ->
    let e = wsToE space0 in
    withInfo (exp_ <| EParens space0 e parensStyle space0) e.start e.end)
  |= parser

htmlliteral: Parser (WS -> Exp)
htmlliteral =
  inContext "html literal" <|
     lazy <| \_ ->
     succeed (\newExp space -> withInfo (exp_ <| EParens space newExp HtmlSyntax space0) newExp.start newExp.end)
    |= ((succeed identity
      |. (lookAhead <| (delayedCommit
           (symbol "<")
           (oneOf [identifier, source <| symbol "@"])))
      |= HTMLParser.parseOneNode (HTMLParser.Interpolation
        { attributevalue = inContext "HTML attribute value" << wrapWithSyntax ElmSyntax << (\apparg -> map always <| expressionWithoutGreater { first = nospace, apparg = apparg})
        , attributelist = inContext "HTML special attribute list" << wrapWithSyntax ElmSyntax << simpleExpression
        , childlist = inContext "HTML special child list" << wrapWithSyntax ElmSyntax << (\spaceapparg ->
              oneOf [
                variableExpression |> addSelections False nospace,
                tuple
              ]
            )
        , tagName = inContext "HTML special tag name" << wrapWithSyntax ElmSyntax << simpleExpression
        })) |> andThen htmlToExp)

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

namePattern : ParserI Ident -> Parser (WS -> Pat)
namePattern ident =
  mapWSPat_ <|
    transferInfo (\name wsBefore -> PVar wsBefore name noWidgetDecl) ident

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

variablePattern : Parser (WS -> Pat)
variablePattern =
  inContext "variable pattern" <|
    namePattern anyIdentifier

--------------------------------------------------------------------------------
-- Wildcards
--------------------------------------------------------------------------------

wildcardPattern : Parser (WS -> Pat)
wildcardPattern =
  inContext "wildcard pattern" <|
    mapWSPat_ <|
      transferInfo
        (\() -> PWildcard)
        (keywordWithSpace "_")

--------------------------------------------------------------------------------
-- Types  (SPECIAL-USE ONLY; not included in `pattern`)
--------------------------------------------------------------------------------

typePattern : Parser (WS -> Pat)
typePattern =
  inContext "type pattern" <|
    namePattern bigIdentifier

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

constantPattern : Parser (WS -> Pat)
constantPattern =
  inContext "constant pattern" <|
    mapWSPat_ <|
      transferInfo (flip PConst) num

--------------------------------------------------------------------------------
-- Base Values
--------------------------------------------------------------------------------

baseValuePattern : Parser (WS -> Pat)
baseValuePattern  =
  inContext "base value pattern" <|
    mapWSPat_ <|
      transferInfo (flip PBase) baseValue

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

listPattern : Parser (WS -> Pat)
listPattern =
  inContext "list pattern" <|
    lazy <| \_ ->
      mapWSPat_ <|
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
          }

--------------------------------------------------------------------------------
-- Records
--------------------------------------------------------------------------------

recordPattern : Parser (WS -> Pat)
recordPattern =
  inContext "record pattern" <|
    lazy <| \_ ->
      mapWSPat_ <|
        genericRecord
         { key = identifier
         , equalSign = symbol "="
         , optNoEqualSign = Just (\name -> withDummyPatInfo <| PVar space0 name noWidgetDecl)
         , value = pattern -- All spaces because = requires a value afterwards.
         , fundef = Nothing
         , combiner =
            \wsBefore keyValues wsAfter ->
              PRecord wsBefore keyValues wsAfter
         , optionalInitParser = Nothing}

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

tuplePattern : Parser (WS -> Pat)
tuplePattern =
  inContext "tuple pattern" <|
    lazy <| \_ ->
      mapWSPat_ <|
        genericTuple
          { term =
              pattern
          , tagger =
              withDummyPatInfo << PBase space0 << EString defaultQuoteChar
          , one = PParens
          , record =
              PRecord
          , implicitFun = Nothing
          }

--------------------------------------------------------------------------------
-- Data Constructor Pattern
--------------------------------------------------------------------------------

dataConstructorPattern : Parser (WS -> Pat)
dataConstructorPattern =
  inContext "data constructor pattern" <|
    lazy <| \_ ->
      let
        combiner (start, ctorName) (args, end) wsBefore =
          let
            ctorEntry =
              Lang.ctor
                (withDummyPatInfo << PBase space0 << EString defaultQuoteChar)
                Lang.DataTypeCtor
                ctorName

            insideArgsEntries =
              args
                |> List.map (\p -> (space0, p))
                |> Utils.indexedMapFrom 1 Lang.numericalEntry

            argsEntry =
              ( space0
              , space0
              , Lang.ctorArgs
              , space0
              , withDummyPatInfo <|
                  PRecord space0 insideArgsEntries space0
              )
          in
            withInfo
              ( pat_ <|
                  PRecord
                    wsBefore
                    [ctorEntry, argsEntry]
                    space0
              )
              start
              end
      in
        delayedCommitMap
          combiner
          ( succeed (,)
              |= getPos
              |= flip andThen
                   ( untrackInfo bigIdentifier
                   )
                   ( \id ->
                       map (always id) <|
                         guard "not a data constructor" <|
                           isDataConstructor id
                   )
          )
          ( succeed (,)
              |= repeat zeroOrMore (pattern allSpacesPolicy)
              |= getPos
          )

--------------------------------------------------------------------------------
-- As-Patterns (@-Patterns)
--------------------------------------------------------------------------------

parensPattern : Parser (WS -> Pat)
parensPattern =
  inContext "parentheses" <|
    mapWSPat_ <|
      lazy <| \_ ->
        transferInfo
          ( \(innerPattern, wsBeforeEnd) wsBefore ->
              PParens wsBefore innerPattern wsBeforeEnd
          )
          ( trackInfo <|
              succeed (,)
                |. symbol "("
                |= pattern allSpacesPolicy
                |= spaces
                |. symbol ")"
          )

simplePattern : Parser (WS -> Pat)
simplePattern =
  inContext "simple pattern" <|
    oneOf <| Debug.log "simplePattern=" <|
      [ lazy <| \_ -> listPattern
      , lazy <| \_ -> recordPattern
      , lazy <| \_ -> tuplePattern
      , constantPattern
      , baseValuePattern
      , lazy <| \_ -> dataConstructorPattern
      , variablePattern
      , wildcardPattern
      ]

--------------------------------------------------------------------------------
-- General Patterns
--------------------------------------------------------------------------------

pattern : SpacePolicy -> Parser Pat
pattern sp =
  inContext "pattern" <|
    lazy <| \_ ->
      delayedCommitMap (\wsFront binaryWSPat -> binaryWSPat wsFront)
        sp.first <| binaryOperator
        { greedySpaceParser =
            spaces
        , precedenceTable =
            builtInPatternPrecedenceTable
        , minimumPrecedence =
            0
        , expression =
            simplePattern
        , withZeroSpace =  \wsPat ->
            let finalPat = wsPat space0 in
            mapPrecedingWhitespacePatWS (\ws -> withInfo ws.val finalPat.start finalPat.start) finalPat
        , operator =
            patternOperator sp.apparg
        , representation =
            .val >> Tuple.second
        , combine =
            \wsBeforeEverything left operator right ->
              let
                (wsBefore, identifier) =
                  operator.val
              in
                case identifier of
                  "::" ->
                    withInfo
                      (
                        pat_ <| PList wsBeforeEverything [left] wsBefore (Just right) space0
                      ) left.start right.end
                  "as" ->
                    case right.val.p__ of
                      PVar wsName name _ ->
                        withInfo
                          (
                            pat_ <| PAs wsBeforeEverything wsName name wsBefore left
                          ) left.start right.end
                      _ ->
                        case left.val.p__ of
                          PVar wsName name _ ->
                            withInfo
                              (
                                pat_ <| PAs wsBeforeEverything wsName name wsBefore right
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

baseType : String -> (WS -> Type_) -> String -> Parser (WS -> Type)
baseType context combiner token =
  inContext context <| unwrapInfo <|
    succeed ( \{start, end} -> withInfo combiner start end )
    |= trackInfo (keyword token)

nullType : Parser (WS -> Type)
nullType =
  baseType "null type" TNull "Null"

numType : Parser (WS -> Type)
numType =
  baseType "num type" TNum "Num"

boolType : Parser (WS -> Type)
boolType =
  baseType "bool type" TBool "Bool"

stringType : Parser (WS -> Type)
stringType =
  baseType "string type" TString "String"

--------------------------------------------------------------------------------
-- Variable Types
--------------------------------------------------------------------------------

variableType : Parser (WS -> Type)
variableType =
  inContext "variable type" <| unwrapInfo <|
    transferInfo (flip TVar) littleIdentifier

--------------------------------------------------------------------------------
-- Function Type
--------------------------------------------------------------------------------

functionType : Parser (WS -> Type)
functionType =
  lazy <| \_ ->
    inContext "function type" <| unwrapInfo <|
      parenBlock TArrow <|
        succeed identity
          |. keywordWithSpace "->"
          |= repeat oneOrMore (typ allSpacesPolicy)

--------------------------------------------------------------------------------
-- List Type
--------------------------------------------------------------------------------

listType : Parser (WS -> Type)
listType =
  inContext "list type" <|
    lazy <| \_ ->
      unwrapInfo <|
      parenBlock TList <|
        succeed identity
          |. keywordWithSpace "List"
          |= typ allSpacesPolicy

--------------------------------------------------------------------------------
-- Dict Type
--------------------------------------------------------------------------------

dictType : Parser (WS -> Type)
dictType =
  inContext "dictionary type" <|
    lazy <| \_ -> unwrapInfo <|
      parenBlock
        ( \wsBefore (tKey, tVal) wsEnd ->
            TDict wsBefore tKey tVal wsEnd
        )
        ( succeed (,)
            |. keywordWithSpace "TDict"
            |= typ allSpacesPolicy
            |= typ allSpacesPolicy
        )

--------------------------------------------------------------------------------
-- Tuple Type
--------------------------------------------------------------------------------

tupleType : Parser (WS -> Type)
tupleType =
  inContext "tuple type" <|
    lazy <| \_ ->
      unwrapInfo <|
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
        }

recordType : Parser (WS -> Type)
recordType =
  inContext "record type" <|
    lazy <| \_ ->
        unwrapInfo <|
        genericRecord
         { key = identifier
         , equalSign = symbol ":"
         , optNoEqualSign = Nothing
         , value = typ
         , fundef = Nothing
         , combiner =
            \wsBefore keyValues wsAfter ->
              TRecord wsBefore Nothing keyValues wsAfter
         , optionalInitParser = Just { initItem = identifier
           ,  combinerInit = \wsBefore init wsBar elems wsEnd -> TRecord wsBefore (Just (init, wsBar)) elems wsEnd
           }
         }

--------------------------------------------------------------------------------
-- Forall Type
--------------------------------------------------------------------------------

forallType : Parser (WS -> Type)
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
            map (\idp _ -> One idp) wsIdentifierPair
        , inContext "forall type (many) "<|
            untrackInfo <|
              parenBlock Many <|
                repeat zeroOrMore wsIdentifierPair
        ]
  in
    inContext "forall type" <|
      lazy <| \_ ->
        unwrapInfo <|
        parenBlock
          ( \wsBefore (wsBeforeGiveQs, t) wsEnd ->
              let qs = wsBeforeGiveQs wsBefore in
              TForall wsBefore qs t wsEnd
          )
          ( succeed (,)
              |. keywordWithSpace "forall"
              |= quantifiers
              |= typ allSpacesPolicy
          )

--------------------------------------------------------------------------------
-- Union Type
--------------------------------------------------------------------------------

unionType : Parser (WS -> Type)
unionType=
  inContext "union type" <|
    lazy <| \_ ->
      unwrapInfo <|
      parenBlock TUnion <|
        succeed identity
          |. keywordWithSpace "union"
          |= repeat oneOrMore (typ allSpacesPolicy)

--------------------------------------------------------------------------------
-- App Types
--------------------------------------------------------------------------------

appType : Parser (WS -> Type)
appType =
  lazy <| \_ ->
    inContext "app type" <| unwrapInfo <|
      trackInfo <|
        succeed (\ident ts wsBefore ->
            TApp wsBefore ident ts
        )
        |= untrackInfo bigIdentifier
        |= repeat zeroOrMore (typ spacesWithoutNewline)

--------------------------------------------------------------------------------
-- Wildcard Type
--------------------------------------------------------------------------------

wildcardType : Parser (WS -> Type)
wildcardType =
  inContext "wildcard type" <|
    succeed (\{val, start, end} wsBefore -> withInfo (TWildcard wsBefore) start end) |= trackInfo (keyword "_")

--------------------------------------------------------------------------------
-- General Types
--------------------------------------------------------------------------------

typ : SpacePolicy -> Parser Type
typ sp =
  inContext "type" <|
    lazy <| \_ ->
      delayedCommitMap (\ws wsType -> wsType ws)
        sp.first
        (oneOf
          [ nullType
          , numType
          , boolType
          , stringType
          , wildcardType
          , lazy <| \_ -> functionType
          , lazy <| \_ -> listType
          , lazy <| \_ -> dictType
          , lazy <| \_ -> tupleType
          , lazy <| \_ -> recordType
          , lazy <| \_ -> forallType
          , lazy <| \_ -> unionType
          , lazy <| \_ -> appType
          , variableType
          ])

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
    "__DictEmpty__" ->
      Just DictEmpty
    "__CurrentEnv__" ->
      Just CurrentEnv
    "__DictFromList__" ->
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
    "__DictInsert__" ->
      Just DictInsert
    "__DictGet__" ->
      Just DictGet
    "__DictRemove__" ->
      Just DictRemove
    "debug" ->
      Just DebugLog
    "noWidgets" ->
      Just NoWidgets
    "extractFirstIn" ->
      Just RegexExtractFirstIn
    _ ->
      Nothing

--------------------------------------------------------------------------------
-- Operator Parsing
--------------------------------------------------------------------------------

operator : Parser WS -> ParserI Operator
operator appargSpace =
  paddedBefore (,) appargSpace symbolIdentifier

htmlAttributeOperator : Parser WS -> ParserI Operator
htmlAttributeOperator appargSpace =
  paddedBefore (,) appargSpace htmlAttributeSymbolIdentifier

patternOperator : Parser WS -> ParserI Operator
patternOperator appargSpace =
  paddedBefore (,) appargSpace patternSymbolIdentifier

--==============================================================================
-- Modules
--==============================================================================

moduleNames : Set String
moduleNames =
  Set.fromList
    -- preludeLeo.elm
    [ "List"
    , "Tuple"
    , "Editor"
    , "Update"
    , "SoftFreeze"
    , "Html"
    , "TableWithButtons"
    , "Results"
    , "LensLess"
    , "ListLenses"
    , "Regex"
    , "String"
    , "Dict"
    , "Debug"
    , "Maybe"
    , "Set"
    -- built-in examples
    , "UI" -- MVC
    ]


--==============================================================================
-- Data Constructors
--==============================================================================

isDataConstructor : String -> Bool
isDataConstructor name =
  let
    startsLower =
      name
        |> String.uncons
        |> Maybe.map (Tuple.first >> Char.isUpper)
        |> Maybe.withDefault False

    notModule =
      not <|
        Set.member name moduleNames
  in
    startsLower && notModule

--==============================================================================
-- Expressions
--==============================================================================

--------------------------------------------------------------------------------
-- Constants
--------------------------------------------------------------------------------

constantExpression : Parser (WS -> Exp)
constantExpression =
  inContext "constant expression" <|
    mapWSExp_ <| map (\(n, fa, w) ->
      withInfo
        (\ws -> EConst ws n.val (dummyLocWithDebugInfo fa.val n.val) w)
        n.start
        w.end
      )
      (succeed (,,)
       |= num
       |= frozenAnnotation
       |= widgetDecl Nothing
      )

--------------------------------------------------------------------------------
-- Base Values
--------------------------------------------------------------------------------

baseValueExpression : Parser (WS -> Exp)
baseValueExpression =
  inContext "base value expression" <|
    mapWSExp_ <| transferInfo (flip EBase) baseValue

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

variableExpression : Parser (WS -> Exp)
variableExpression =
  mapWSExp_ <| transferInfo (flip EVar) anyIdentifier

--------------------------------------------------------------------------------
-- Functions (lambdas)
--------------------------------------------------------------------------------

function : Parser WS -> Parser (WS -> Exp)
function appargSpace =
  inContext "function" <|
    lazy <| \_ ->
      mapWSExp_ <|
          transferInfo ( \(parameters, body) wsBefore ->
              EFun wsBefore parameters body space0
          )
          ( trackInfo <|
              succeed (,)
                |. symbol "\\"
                |= repeat oneOrMore (pattern allSpacesPolicy)
                |. spaces
                |. symbol "->"
                |= expression { first = spaces, apparg = appargSpace }
          )

--------------------------------------------------------------------------------
-- Lists
--------------------------------------------------------------------------------

list : Parser (WS -> Exp)
list =
  inContext "list" <|
    lazy <| \_ ->
      mapWSExp_ <|
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
          }

--------------------------------------------------------------------------------
-- Records
--------------------------------------------------------------------------------

record : Parser (WS -> Exp)
record  =
  inContext "record expression" <|
    lazy <| \_ ->
      mapWSExp_ <|
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
         , optionalInitParser = Just { initItem = expression allSpacesPolicy
           , combinerInit = \wsBefore init wsBar elems wsEnd -> ERecord wsBefore (Just (init, wsBar)) elems wsEnd
           }
         }

--------------------------------------------------------------------------------
-- Conditionals
--------------------------------------------------------------------------------

conditional : Parser WS -> Parser (WS -> Exp)
conditional  appargSpace =
  inContext "conditional" <|
    lazy <| \_ ->
      mapWSExp_ <|
        transferInfo
          ( \(condition, wsThen, trueBranch, wsElse, falseBranch) wsBefore ->
              EIf wsBefore condition wsThen trueBranch wsElse falseBranch space0
          )
          ( trackInfo <|
              delayedCommit (keywordWithSpace "if") <|
                succeed (,,,,)
                  |= expression allSpacesPolicy
                  |= spaces
                  |. keywordWithSpace "then"
                  |= expression allSpacesPolicy
                  |= spaces
                  |. keywordWithSpace "else"
                  |= expression { first = spaces, apparg = appargSpace }
          )

--------------------------------------------------------------------------------
-- Case Expressions
--------------------------------------------------------------------------------

caseExpression : Parser (WS -> Exp)
caseExpression =
  inContext "case expression" <|
    lazy <| \_ ->
      let
        branch: Parser WS -> Parser Branch
        branch branchsp =
          delayedCommitMap
            (\wsBefore (p, wsBeforeArrow, e) ->
                withInfo (Branch_ wsBefore p e wsBeforeArrow) p.start e.end
            )
            ( inContext "Indentation for branch" <|
              succeed identity
              |= branchsp
              |. lookAhead (ignore (Exactly 1) (\c -> not (isSpace c)))) -- Tries to consume spaces and correct indentation.
            ( inContext "Branch" <|
                succeed identity
                  |= (pattern { spacesWithoutNewline | first = nospace }
                     |> andThen (\p ->
                        let minimumIndentation = sameLineOrIndentedByAtLeast "for an expression after a branch" ((p.start.col - 1) {- The column index-} + 1 {- The indentation increment-}) in
                        succeed (\wsBeforeArrow e -> (p, wsBeforeArrow, e))
                        |= spaces
                        |. symbol "->"
                        |= expression {first = spaces, apparg = minimumIndentation }
                        |. optional (delayedCommit minimumIndentation (symbol ";"))
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
        mapWSExp_ <|
          transferInfo
            ( \(examinedExpression, wsBeforeOf, branches) wsBefore ->
                if eVarUnapply examinedExpression == Just implicitVarName then
                    EFun space1 [withDummyPatInfo <| PVar space0 implicitVarName noWidgetDecl] (
                      withInfo (exp_ <| ECase wsBefore examinedExpression branches wsBeforeOf)
                        wsBefore.start wsBeforeOf.end
                      ) space0
                 else ECase wsBefore examinedExpression branches wsBeforeOf
            )
            ( trackInfo <|
                delayedCommit (keywordWithSpace "case") <|
                  succeed (,,)
                    |= oneOf [
                         expression allSpacesPolicy,
                         succeed (withDummyExpInfo <| EVar space1 implicitVarName) -- Unparsable var
                       ]
                    |= spaces
                    |. keyword "of"
                    |= (
                        branch allSpacesPolicy.first |>
                        andThen (\b ->
                          case b.val of
                            Branch_ wsBefore p e wsBeforeArrow ->
                              let branchIndentation = sameLineOrIndentedByExactly "for a branch after the first one" (p.start.col - 1 {- The column index-}) in
                              branchHelper [b] (branch branchIndentation)
                        )
                      )
            )

--------------------------------------------------------------------------------
-- Let Bindings
--------------------------------------------------------------------------------

letBinding : Parser WS -> Parser (WS -> Exp)
letBinding appargSpace =     lazy <| \_ ->
  genericLetBinding appargSpace "let" False

letrecBinding : Parser WS -> Parser (WS -> Exp)
letrecBinding appargSpace =      lazy <| \_ ->
  genericLetBinding appargSpace "letrec" True

genericLetBinding : Parser WS -> String -> Bool -> Parser (WS -> Exp)
genericLetBinding appargSpace letkeyword isRec =
  inContext (letkeyword ++ " binding") <|
    lazy <| \_ ->
      mapWSExp_ <|
        transferInfo
          ( \(name, parameters, wsBeforeEq, binding_, wsBeforeIn, body) wsBefore ->
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
                  |= pattern allSpacesPolicy
                  |= repeat zeroOrMore (pattern allSpacesPolicy)
                  |= spaces
                  |. symbol "="
                  |= expression allSpacesPolicy
                  |= spaces
                  |. keywordWithSpace "in"
                  |= expression { first = spaces, apparg = appargSpace }
          )

--------------------------------------------------------------------------------
-- Options
--------------------------------------------------------------------------------

option : Parser WS -> Parser (WS -> Exp)
option appargSpace =
  inContext "option" <|
    mapWSExp_ <|
      lazy <| \_ ->
        succeed
          ( \open opt wsMid val rest ->
              withInfo
                (\wsBefore -> EOption wsBefore opt wsMid val rest)
                open.start
                val.end
          )
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
        |= expression { first = spaces, apparg = appargSpace }

--------------------------------------------------------------------------------
-- Tuples, Parentheses
--------------------------------------------------------------------------------

tuple : Parser (WS -> Exp)
tuple =
  inContext "tuple" <|
    lazy <| \_ ->
      mapWSExp_ <|
        genericTuple
          { term =
              expression
          , tagger =
              withDummyExpInfo << EBase space0 << EString defaultQuoteChar
          , record =
              \ws1 entries ws2 ->
                ERecord ws1 Nothing entries ws2
          , one =
              \wsBefore innerExpression wsBeforeEnd -> EParens wsBefore innerExpression Parens wsBeforeEnd
          , implicitFun =
              Just (\ident -> (pVar ident, eVar ident), \wsBefore pvars body -> EFun wsBefore pvars (withDummyExpInfo body) space0)
          }

--------------------------------------------------------------------------------
-- Holes
--------------------------------------------------------------------------------

hole : Parser (WS -> Exp)
hole =
  inContext "hole" <|
    mapWSExp_ <|
      transferInfo (flip EHole) (trackInfo <| token "??" Nothing)

--------------------------------------------------------------------------------
-- Type Aliases
--------------------------------------------------------------------------------

typeAlias : Parser WS -> Parser (WS -> Exp)
typeAlias appargSpace =
  inContext "type alias" <|
    lazy <| \_ ->
      mapWSExp_ <|
        transferInfo
          ( \(ws, wsToPat, t, rest) wsBefore ->
              ETypeAlias wsBefore (wsToPat ws) t rest space0
          )
          ( trackInfo <|
              succeed (,,,)
                |. keywordWithSpace "type alias"
                |= appargSpace
                |= typePattern
                |. spaces
                |. symbol "="
                |= typ { first = spaces, apparg = appargSpace }
                |= expression { first = spaces, apparg = appargSpace }
          )

--------------------------------------------------------------------------------
-- Type Definitions
--------------------------------------------------------------------------------

typeDefinition : Parser WS -> Parser (WS -> Exp)
typeDefinition appargSpace =
  inContext "type definition" <|
    lazy <| \_ ->
      let
        var =
          delayedCommitMap (,)
            spaces
            (untrackInfo littleIdentifier)
        dc =
          delayedCommitMap
            ( \wsBefore (i, ts, wsAfter) ->
                (wsBefore, i, ts, wsAfter)
            )
            spaces
            ( succeed (,,)
                |= untrackInfo bigIdentifier
                |= repeat zeroOrMore (typ {first = appargSpace, apparg = appargSpace})
                |= spaces
            )
      in
        mapWSExp_ <|
          transferInfo
            ( \(wsBeforeIdent, ident, vars, wsBeforeEq, dcs, rest) wsBefore ->
                ETypeDef wsBefore (wsBeforeIdent, ident) vars wsBeforeEq dcs rest space0
            )
            ( trackInfo <|
                succeed (,,,,,)
                  |. keywordWithSpace "type"
                  |= spaces
                  |= untrackInfo bigIdentifier
                  |= repeat zeroOrMore var
                  |= spaces
                  |. symbol "="
                  |= separateBy oneOrMore (symbol "|") dc
                  |= expression { first = spaces, apparg = appargSpace }
            )

--------------------------------------------------------------------------------
-- General Expressions
--------------------------------------------------------------------------------

selection : Bool -> Parser WS -> Parser ((WS -> Exp) -> (WS -> Exp))
selection tolerateSpaces appargSpace =
  delayedCommitMap (\wsBeforeDot (wsAfterDot,idWithInfo) ws2Exp ->
      \wsBefore ->
        let exp = ws2Exp space0 in
        withInfo (exp_ <| ESelect wsBefore exp wsBeforeDot wsAfterDot idWithInfo.val)
           exp.start idWithInfo.end
    )
    (if tolerateSpaces then
      appargSpace
    else
      succeed (\x -> x)
      |= (trackInfo <| source <| symbol "")
      |. symbol "."
    )
    ((if tolerateSpaces then
       succeed (,)
       |. symbol "."
       |= appargSpace
        else
       succeed (\x -> (ws "", x)))
     |= (trackInfo <| identifier)
    )

-- Add all following .identifier1.identifiers2 as wrappers to the original expression
addSelections : Bool -> Parser WS -> Parser (WS -> Exp) -> Parser (WS -> Exp)
addSelections tolerateSpaces appargSpace parser =
  succeed (\simpExp selections ->
      List.foldl (\sel updatedExp -> sel updatedExp) simpExp selections
      )
    |= parser
    |= repeat zeroOrMore (selection tolerateSpaces appargSpace)

implicitSelectionFun: Parser (WS -> Exp)
implicitSelectionFun =
  succeed (\{val,start,end} initSpace ->
    withInfo (exp_ <| EFun initSpace [withInfo (pat_ <| PVar space0 implicitVarName noWidgetDecl) start start]
    (List.foldl (\sel updatedExp -> sel updatedExp) (\wsBefore -> withInfo (exp_ <| EVar wsBefore implicitVarName) start start) val space0) space0) start end
  )
  |= (trackInfo <| repeat oneOrMore (selection False nospace))

-- Not a function application nor a binary operator
simpleExpression : Parser WS -> Parser (WS -> Exp)
simpleExpression appargSpace =
    oneOf
      [ addSelections True appargSpace <| variableExpression
      , constantExpression
      , lazy <| \_ -> htmlliteral
      , conditional appargSpace
      , caseExpression
      , letBinding appargSpace
      , letrecBinding appargSpace
      , typeAlias appargSpace
      , typeDefinition appargSpace
      , lazy <| \_ -> multiLineInterpolatedString
      , baseValueExpression
      , lazy <| \_ -> function appargSpace
      , lazy <| \_ -> implicitSelectionFun
      , lazy <| \_ -> list
      , lazy <| \_ -> (addSelections True appargSpace <| record)
      , lazy <| \_ -> option appargSpace
      , lazy <| \_ -> (addSelections True appargSpace <| tuple)
      , lazy <| \_ -> (addSelections True appargSpace <| hole)
    ]

spaceColonType: Parser WS -> Parser (WS, Type)
spaceColonType appargSpace =
  lazy <| \_ ->
     try ( succeed (,)
          |= appargSpace
          |. symbol ":"
          |= typ { first= spaces, apparg = appargSpace }
     )

maybeConvertToOp0 : Exp -> Exp
maybeConvertToOp0 exp =
  case exp.val.e__ of
    EVar wsBefore identifier ->
      case opFromIdentifier identifier of
        Just op_ ->
          replaceE__ exp <|
            EOp
              wsBefore
              space0
              (withInfo op_ exp.start exp.end)
              []
              space0

        Nothing ->
          exp
    _ ->
      exp

maybeConvertToOpN : Exp -> List Exp -> Exp__
maybeConvertToOpN first rest =
  let
    default =
      EApp space0 first (List.map maybeConvertToOp0 rest) SpaceApp space0
  in
    case first.val.e__ of
      EVar wsBefore identifier ->
        case opFromIdentifier identifier of
          Just op_ ->
            EOp
              wsBefore
              space0
              (withInfo op_ first.start first.end)
              (List.map maybeConvertToOp0 rest)
              space0

          Nothing ->
            default
      _ ->
        default


-- Either a simple expression or a function application or a data constructor
simpleUntypedExpressionWithPossibleArguments : Parser WS -> Parser (WS -> Exp)
simpleUntypedExpressionWithPossibleArguments appargSpace =
  let
    combine : (WS -> Exp) -> List Exp -> WS -> Exp
    combine wsToFirst rest wsBefore =
      let first = wsToFirst wsBefore in
      let
        constructedRest =
          flip List.map rest <| \e ->
            case e.val.e__ of
              EVar wsBefore identifier ->
                -- Datatypes desugar to records
                if isDataConstructor identifier then
                  let
                    ctorEntry =
                      Lang.ctor
                        (withDummyExpInfo << EBase space0 << EString defaultQuoteChar)
                        Lang.DataTypeCtor
                        identifier

                    -- This must be a data constructor with no args because of
                    -- precedence/associativity rules
                    argsEntry =
                      ( space0
                      , space0
                      , Lang.ctorArgs
                      , space0
                      , withDummyExpInfo <|
                          ERecord space0 Nothing [] space0
                      )
                  in
                    withInfo
                      ( exp_ <|
                          ERecord
                            wsBefore
                            Nothing
                            [ctorEntry, argsEntry]
                            space0
                      )
                      e.start
                      e.end
                else
                  e
              _ ->
                e

        -- Some special cases
        maybeSpecial =
          case first.val.e__ of
            EVar wsBefore identifier ->
              -- Datatypes desugar to records
              if isDataConstructor identifier then
                let
                  ctorEntry =
                    Lang.ctor
                      (withDummyExpInfo << EBase space0 << EString defaultQuoteChar)
                      Lang.DataTypeCtor
                      identifier

                  insideArgsEntries =
                    constructedRest
                      |> List.map (\e -> (space0, e))
                      |> Utils.indexedMapFrom 1 Lang.numericalEntry

                  argsEntry =
                    ( space0
                    , space0
                    , Lang.ctorArgs
                    , space0
                    , withDummyExpInfo <|
                        ERecord space0 Nothing insideArgsEntries space0
                    )
                in
                  Just << exp_ <|
                    ERecord
                      wsBefore
                      Nothing
                      [ctorEntry, argsEntry]
                      space0
              else
                Nothing
            _ ->
              Nothing

        start =
          first.start

        end =
          case Utils.maybeLast constructedRest of
            Nothing ->
              first.end

            Just last ->
              last.end
      in
        case maybeSpecial of
          Just special ->
            withInfo special start end

          Nothing ->
            -- If there are no arguments, then we do not have a function
            -- application, so just return the first expression. Otherwise,
            -- build a function application.
            case Utils.maybeLast constructedRest of
              -- rest is empty
              Nothing ->
                maybeConvertToOp0 first

              -- rest is non-empty
              Just last ->
                let
                  e_ = exp_ (maybeConvertToOpN first constructedRest)
                in
                  withInfo e_ first.start last.end
  in
    lazy <| \_ ->
      succeed combine
        |= simpleExpression appargSpace
        |= repeat zeroOrMore (delayedCommitMap (\ws wsExp -> wsExp ws)
           appargSpace
           (simpleExpression appargSpace))

simpleExpressionWithPossibleArguments : Parser WS -> Parser (WS -> Exp)
simpleExpressionWithPossibleArguments appargSpace =
  lazy <| \_ ->
    ( delayedCommitMap (\startPos (wS2untypedExp, mbType) ->
        case mbType of
          Nothing -> wS2untypedExp
          Just (wsColon, typ) ->
            \wsBefore ->
               withInfo
                 (exp_ <|
                     EColonType wsBefore (wS2untypedExp space0) wsColon typ space0
                 )
                 startPos typ.end
      )
      getPos
      (succeed (,)
      |= simpleUntypedExpressionWithPossibleArguments appargSpace
      |= ParserUtils.optional (spaceColonType appargSpace))
    )

-- No indentation for top-level expressions, at least one newline or the beginning of the string.
topLevelBetweenDefSpacePolicy: SpacePolicy
topLevelBetweenDefSpacePolicy =
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

noSpacePolicy: SpacePolicy
noSpacePolicy =
  { first = succeed space0
  , apparg = succeed space0 }

sameLineOrIndentedByAtLeast: String -> Int -> Parser WS
sameLineOrIndentedByAtLeast msg nSpaces =
  oneOf [
    try <| LangParserUtils.spacesCustom {forwhat = msg, withNewline=False, minIndentation=Nothing, maxIndentation=Nothing},
    LangParserUtils.spacesCustom {forwhat = msg, withNewline=True, minIndentation=Just nSpaces, maxIndentation=Nothing}
  ]

sameLineOrIndentedByExactly: String -> Int -> Parser WS
sameLineOrIndentedByExactly msg nSpaces =
  oneOf [
    try <| LangParserUtils.spacesCustom {forwhat = msg, withNewline=False, minIndentation=Nothing, maxIndentation=Nothing},
    LangParserUtils.spacesCustom {forwhat = msg, withNewline=True, minIndentation=Just nSpaces, maxIndentation=Just nSpaces}
  ]

expressionGeneral : Bool -> SpacePolicy -> Parser Exp
expressionGeneral isHtmlAttribute sp =
  inContext "expression" <|
    lazy <| \_ ->
      delayedCommitMap (\wsFront binaryExp -> binaryExp wsFront)
        sp.first <| binaryOperator
        { greedySpaceParser =
            spaces
        , precedenceTable =
            builtInPrecedenceTable
        , minimumPrecedence =
            0
        , expression =
            simpleExpressionWithPossibleArguments sp.apparg
        , withZeroSpace = \wsExp ->
            let finalExp = wsExp space0 in
            mapPrecedingWhitespaceWS (\ws -> withInfo ws.val finalExp.start finalExp.start) finalExp
        , operator =
            if isHtmlAttribute then htmlAttributeOperator sp.apparg else operator sp.apparg
        , representation =
            .val >> Tuple.second
        , combine =
            \wsBeforeEverything left operator right ->
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
                              EOp wsBeforeEverything wsBefore op [ left, right ] space0
                          )
                          left.start
                          right.end

                  -- Should result in evaluator error
                  Nothing ->
                    if identifier == "::" then
                      withInfo (exp_ <|
                        EList wsBeforeEverything [(space0, left)] wsBefore (Just right) space0
                      ) left.start right.end
                    else if identifier == "<|" then
                      withInfo (exp_ <|
                        EApp wsBeforeEverything left [right] (LeftApp wsBefore) space0
                      ) left.start right.end
                    else if identifier == "|>" then
                      withInfo (exp_ <|
                        EApp wsBeforeEverything right [left] (RightApp wsBefore) space0
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
                              EApp wsBeforeEverything opExp [ left, right ] InfixApp space0
                          )
                          left.start
                          right.end
        }

expression : SpacePolicy -> Parser Exp
expression sp = expressionGeneral False sp

expressionWithoutGreater : SpacePolicy -> Parser Exp
expressionWithoutGreater sp = expressionGeneral True sp


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
          |= topLevelBetweenDefSpacePolicy.first
          |= pattern {topLevelInsideDefSpacePolicy | first = nospace }
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
        )
        ( succeed identity
          |. symbol ":"
          |= typ topLevelInsideDefSpacePolicy
          |. optionalTopLevelSemicolon
        )

--------------------------------------------------------------------------------
-- Top-Level Type Aliases
--------------------------------------------------------------------------------

topLevelTypeAlias : Parser TopLevelExp
topLevelTypeAlias =
  inContext "top-level type alias" <|
    delayedCommitMap
      ( \wsBefore (startPos, ws, wsPat, t, endPos) ->
          withInfo
            ( \rest ->
                exp_ <|
                  ETypeAlias wsBefore (wsPat ws) t rest space0
            )
            startPos
            endPos
      )
      spaces
      ( succeed (,,,,)
        |= getPos
        |. keywordWithSpace "type alias"
        |= topLevelInsideDefSpacePolicy.apparg
        |= typePattern
        |. topLevelInsideDefSpacePolicy.apparg
        |. symbol "="
        |= typ topLevelInsideDefSpacePolicy
        |. optionalTopLevelSemicolon
        |= getPos
      )

--------------------------------------------------------------------------------
-- Top-Level Type Definitions
--------------------------------------------------------------------------------

topLevelTypeDefinition : Parser TopLevelExp
topLevelTypeDefinition =
  inContext "top-level type definition" <|
    lazy <| \_ ->
      let
        var =
          delayedCommitMap (,)
            topLevelInsideDefSpacePolicy.first
            (untrackInfo littleIdentifier)
        dc =
          delayedCommitMap
            ( \wsBefore (i, ts, wsAfter) ->
                (wsBefore, i, ts, wsAfter)
            )
            topLevelInsideDefSpacePolicy.first
            ( succeed (,,)
                |= untrackInfo bigIdentifier
                |= repeat zeroOrMore (typ topLevelInsideDefSpacePolicy)
                |= topLevelInsideDefSpacePolicy.first
            )
      in
        delayedCommitMap
          ( \wsBefore (startPos, wsBeforeIdent, ident, vars, wsBeforeEq, dcs, endPos) ->
              withInfo
                ( \rest ->
                    exp_ <|
                      ETypeDef wsBefore (wsBeforeIdent, ident) vars wsBeforeEq dcs rest space0
                )
                startPos
                endPos
          )
          spaces
          ( succeed (,,,,,,)
            |= getPos
            |. keywordWithSpace "type"
            |= topLevelInsideDefSpacePolicy.first
            |= untrackInfo bigIdentifier
            |= repeat zeroOrMore var
            |= topLevelInsideDefSpacePolicy.first
            |. symbol "="
            |= separateBy oneOrMore (symbol "|") dc
            |= getPos
            |. optionalTopLevelSemicolon
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
      , topLevelTypeAlias
      , topLevelTypeDefinition
      , topLevelTypeDeclaration
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
  inContext "Main expression" <|
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

(preludeNotParsed, (prelude, initK)) =
  -- call run program, not parse, since don't want to call freshen
  case run program Prelude.preludeLeo of
    Ok k -> (Nothing, freshenClean 1 k)
    Err parserError ->
      let msg = ParserUtils.showError parserError in
      case run program "0" of
        Ok k -> (Just msg, freshenClean 1 k)
        Err err ->
          Debug.crash <|
            """ElmParser: "0" failed to parse?""" ++ ParserUtils.showError err

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
  let startK      = (List.maximum (initK :: Set.toList allIds) |> Utils.fromJust_ "freshen") + 1 in
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
  List.maximum (initK :: Set.toList ids) |> Utils.fromJust_ "maxId"

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

  (PAs _ _ _ _ p_, _) -> recordIdentifiers (p_,e)

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

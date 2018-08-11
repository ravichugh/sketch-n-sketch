module ElmParser exposing
  ( parse
  , builtInPrecedenceTable
  , builtInPatternPrecedenceTable

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

-- import FastParser
import PreludeGenerated as Prelude
import Regex
import HTMLParser
import ImpureGoodies
import Array

--==============================================================================
--= Helpers
--==============================================================================

implicitVarName = " i"

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
                \wsBefore wsAfter -> combiner wsBefore [] wsAfter
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
     , combiner : WS -> List (Maybe WS, WS, String, WS, elemValue) -> WS -> record
     }
  -> ParserI (WS -> record)
genericNonEmptyRecord { keyValue, combiner }=
  lazy <| \_ ->
    let
      keyValueSeqHelper : Set String -> List (Maybe WS, WS, String, WS, elemValue)-> Parser (List (Maybe WS, WS, String, WS, elemValue))
      keyValueSeqHelper keys revKeyValues =
        oneOf [
          delayedCommitMap (\ws (opt, a, (b, c, d)) -> (opt |> Maybe.map (\_ -> ws), if opt == Nothing then ws else a, b, c, d))
            spaces
            (succeed (,,)
              |. negativeLookAhead (symbol "}")
              |= optional (symbol ",")
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
                    keyValueSeqHelper (Set.singleton k) [(Nothing, wsBefore, k, w, v)]
                    )
                 )
              |= spaces
              |. symbol "}")


genericNonEmptyRecordWithInit
  :  { keyValue : Parser (String, WS, elemValue)
     , initItem: Parser elemInit
     , combinerInit : WS-> elemInit -> WS -> List (Maybe WS, WS, String, WS, elemValue) -> WS -> record
     }
  -> ParserI (WS -> record)
genericNonEmptyRecordWithInit { keyValue, initItem, combinerInit }=
  lazy <| \_ ->
    let
      keyValueSeqHelper : Set String -> List (Maybe WS, WS, String, WS, elemValue)-> Parser (List (Maybe WS, WS, String, WS, elemValue))
      keyValueSeqHelper keys revKeyValues =
        oneOf [
          delayedCommitMap (\ws (opt, a, (b, c, d)) -> (opt |> Maybe.map (\_ -> ws), if opt == Nothing then ws else a, b, c, d))
            spaces
            (succeed (,,)
              |. negativeLookAhead (symbol "}")
              |= optional (symbol ",")
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
                    keyValueSeqHelper (Set.singleton k) [(Nothing, wsBefore, k, w, v)]
                    )
                 )
              |= spaces
              |. symbol "}")



genericRecord
  :  { key : Parser String
     , equalSign: Parser ()
     , optNoEqualSign: Maybe (String -> elemValue)
     , value : Parser WS -> MinStartCol -> Parser elemValue
     , fundef : Maybe { -- Optional arguments to records, e.g. { myfun arg1 arg2 = value }
          arguments : Parser (List arguments),
          buildValue : List arguments -> elemValue -> elemValue
       }
     , combiner : WS -> List (Maybe WS, WS, String, WS, elemValue) -> WS -> record
     , optionalInitParser: Maybe
       { initItem : Parser elemInit
       , combinerInit : WS -> elemInit -> WS -> List (Maybe WS, WS, String, WS, elemValue) -> WS -> record
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
            |= (inContext "record value" <| value spaces (keyWithInfos.start.col + 1))
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
                \wsBefore wsAfter -> combiner wsBefore [] wsAfter
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
  :  { term : Parser t
     , tagger : String -> t
     , one : WS -> t -> WS -> r
     , record : WS -> List (Maybe WS, WS, Ident, WS, t) -> WS -> r
     , implicitFun: Maybe (Ident -> (pvar, t), WS -> List pvar -> r -> r)
     }
  -> ParserI (WS -> r)
genericTuple { term, tagger, one, record, implicitFun } =
  let
    combiner:(t, List (Maybe WS, t), WS)       -> WS -> r
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
          Lang.numericalEntry 1 (Nothing, fst)

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

    implicitTupling = flip Maybe.map implicitFun <| \(identToPvarVar, funBuilder) -> \numberOfCommas wsBefore ->
      let (firstPvar, firstVar) = identToPvarVar <| implicitVarName in
      let (restPvar, restVar) = List.range 1 numberOfCommas |> List.map (\i ->
           implicitVarName ++ toString i
        ) |> List.map identToPvarVar |> List.unzip
      in
      funBuilder wsBefore (firstPvar::restPvar) (combiner (firstVar, (List.map ((,) (Just space0)) restVar), space0) space0)

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

                  |= term
                  |= repeat zeroOrMore
                       ( delayedCommitMap (\ws term -> (Just ws, term)) -- In the future, we could write tuples (1\n 2)
                          spaces
                          (succeed identity
                           |. symbol ","
                           |= term)
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

typeSymbolIdentifier: ParserI Ident
typeSymbolIdentifier =
  trackInfo <|
    oneOf
      [ source (symbol "&")
      , source (symbol "|")
      , source (symbol "->")
      , source (symbol "<|")
      , source (symbol "|>")
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
        ELet sp0 letType decls wsBeforeIn _ ->
          replaceE__ head <| ELet sp0 letType decls wsBeforeIn tailPart
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
      |= (variableExpression |> addSelections),
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
    map (\p -> p space0) <|
    genericLetBinding (
      succeed identity
      |. symbol "\n"
      |= succeed (withDummyExpInfo <| EHole space0 Nothing))

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


addParenthesizedParameters: Parser (WS -> Exp) -> Parser (WS -> Exp)
addParenthesizedParameters parser =
  succeed (\simpExp arguments wsBefore ->
    if List.length arguments == 0 then simpExp wsBefore
    else
     let simpExpZeroSpace = simpExp space0 in
     let argumentsZeroSpace = List.map (\arg -> arg space0) arguments in
      withInfo (exp_ <| EApp wsBefore simpExpZeroSpace argumentsZeroSpace SpaceApp space0) simpExpZeroSpace.start (Utils.last "addParenthesizedParameters" argumentsZeroSpace |> .end)
  )
  |= parser
  |= repeat zeroOrMore (oneOf [tuple, list, record])

-- In html text inner, makes @toCss<|<scss>...</scss> possible (and avoids the closing parentheses yeah!
-- Or also @bold<|"""Hello""" We only parse a simple expression after
addRightApplications: Parser (WS -> Exp) -> Parser (WS -> Exp)
addRightApplications parser =
  succeed (\simpExp apps wsBefore ->
     let finalExps: List Exp
         finalExps = simpExp wsBefore :: apps in
     case Utils.maybeInitLast finalExps of
       Just (functions, argument) ->
         List.foldr (\simpExp argExp ->
           withInfo (exp_ <| EApp space0 simpExp [argExp] (LeftApp space0) space0) simpExp.start argExp.end
         ) argument functions
       _ -> Debug.crash "Finally, P = NP."
    )
  |= parser
  |= repeat zeroOrMore (
      succeed (\sp spToExp -> spToExp sp)
      |. symbol "<|"
      |= spaces
      |= (simpleExpression 0 NoSpace |> addParenthesizedParameters))

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
        { attributevalue = inContext "HTML attribute value" << wrapWithSyntax ElmSyntax << (\apparg -> map always <| expressionWithoutGreater nospace 0 NoSpace)
        , attributelist = inContext "HTML special attribute list" <| wrapWithSyntax ElmSyntax <| simpleExpression 0 NoSpace
        , childlist = inContext "HTML special child list" << wrapWithSyntax ElmSyntax << (\spaceapparg ->
              oneOf [
                variableExpression |> addSelections |> addParenthesizedParameters |> addRightApplications,
                letBinding 0 NoSpace,
                lazy <| \_ -> multiLineInterpolatedString,
                baseValueExpression,
                tuple,
                list
              ]
            )
        , tagName = inContext "HTML special tag name" <| wrapWithSyntax ElmSyntax <| simpleExpression 0 NoSpace
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
tnamePattern : ParserI Ident -> Parser (WS -> TPat)
tnamePattern ident =
  mapWSInfo <|
    transferInfo (\name wsBefore -> TPatVar wsBefore name) ident


typePattern : Parser (WS -> TPat)
typePattern =
  inContext "type pattern" <|
    tnamePattern littleIdentifier

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
              pattern spaces 0
          , tailItem =
              pattern spaces 0
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
              pattern spaces 0
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
                |> List.map (\p -> (Just space0, p))
                |> Utils.indexedMapFrom 1 Lang.numericalEntry

            argsEntry =
              ( Just space0
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
              |= repeat zeroOrMore (pattern spaces 0)
              |= getPos
          )



simplePattern : Parser (WS -> Pat)
simplePattern =
  inContext "simple pattern" <| lazy <| \_ ->
    oneOf <|
      [ listPattern
      , recordPattern
      , tuplePattern
      , constantPattern
      , baseValuePattern
      , dataConstructorPattern
      , variablePattern
      , wildcardPattern
      ]

simplePatternWithMaybeColonType: Parser (WS -> Pat)
simplePatternWithMaybeColonType =
  inContext "simplePatternWithMaybeColonType" <| lazy <| \_ ->
  delayedCommitMap (\pos posToRes -> posToRes pos)
  getPos
  (
  succeed (\wsToP mbT startPos wsBefore ->
    case mbT of
      Just (ws, typ) ->
        withInfo (pat_ <| PColonType wsBefore (wsToP space0) ws typ) startPos typ.end
      Nothing -> wsToP wsBefore
  )
  |= simplePattern
  |= ParserUtils.optional (spaceColonType 0 MinIndentSpace))

--------------------------------------------------------------------------------
-- General Patterns
--------------------------------------------------------------------------------

pattern : Parser WS -> MinStartCol -> Parser Pat
pattern spFirst minStartCol =
  inContext "pattern" <|
    lazy <| \_ ->
      let operatorSpace = spaceSameLineOrNextAfter minStartCol MinIndentSpace in
      delayedCommitMap (\wsFront binaryWSPat -> binaryWSPat wsFront)
        spFirst <| binaryOperator
        { greedySpaceParser =
            spaces
        , precedenceTable =
            builtInPatternPrecedenceTable
        , minimumPrecedence =
            0
        , expression =
            simplePatternWithMaybeColonType
        , withZeroSpace =  \wsPat ->
            let finalPat = wsPat space0 in
            mapPrecedingWhitespacePatWS (\ws -> withInfo ws.val finalPat.start finalPat.start) finalPat
        , operator =
            patternOperator operatorSpace
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
                    withInfo
                      (
                        pat_ <| PAs wsBeforeEverything left wsBefore right
                      ) left.start right.end
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

dataType : Parser (WS -> Type)
dataType =
  inContext "data type" <| unwrapInfo <|
    transferInfo (flip TVar) bigIdentifier

--------------------------------------------------------------------------------
-- Tuple Type
--------------------------------------------------------------------------------

tupleType : Parser (WS -> Type)
tupleType =
  inContext "tuple type" <|
    lazy <| \_ ->
      mapWSInfo <|
        let t:  { term : Parser Type
                   , tagger : String -> Type
                   , one : WS -> Type -> WS -> Type_
                   , record : WS -> List (Maybe WS, WS, Ident, WS, Type) -> WS -> Type_
                   , implicitFun: Maybe (Ident -> (TPat, Type), WS -> List TPat -> Type_ -> Type_)
                   }
                -> ParserI (WS -> Type_)
            t = genericTuple
        in t
          { term = typ spaces 0
          , tagger = -- TODO: TSingleton
              withDummyRange << TVar space0
          , record =
              \ws1 entries ws2 ->
                TRecord ws1 Nothing entries ws2
          , one =
              \wsBefore innerExpression wsBeforeEnd -> TParens wsBefore innerExpression wsBeforeEnd
          , implicitFun = -- The type (,) is equivalent to forall a b.(a, b)
              Just (\ident -> (withDummyRange (TPatVar space1 ident), withDummyRange <| TVar space1 ident),
                \wsBefore pvars body -> TForall wsBefore pvars (withDummyRange body) space0)
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

forallType : MinStartCol -> Parser (WS -> Type)
forallType minStartCol =
  let
    patVar =
      delayedCommitMap
        ( \ws name ->
            withInfo (TPatVar ws name.val) name.start name.end
        )
        spaces
        littleIdentifier
    quantifiers = inContext "forall type (one)" <| repeat zeroOrMore patVar
  in
    inContext "forall type" <|
      lazy <| \_ ->
        unwrapInfo <|
        parenBlock
          ( \wsBefore (qs, t) wsEnd ->
              TForall wsBefore qs t wsEnd
          )
          ( succeed (,)
              |. keywordWithSpace "forall"
              |= quantifiers
              |= typ spaces minStartCol
          )

--------------------------------------------------------------------------------
-- Union Type
--------------------------------------------------------------------------------

unionType : MinStartCol -> Parser (WS -> Type)
unionType minStartCol =
  inContext "union type" <|
      unwrapInfo <|
      parenBlock TUnion <|
        succeed identity
          |. keywordWithSpace "union"
          |= repeat oneOrMore (typ spaces minStartCol)

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



simpleType : MinStartCol -> Parser (WS -> Type)
simpleType minStartCol =
  inContext "type" <|
    lazy <| \_ ->
      (oneOf
        [ nullType
        , variableType
        , dataType
        , numType
        , boolType
        , stringType
        , wildcardType
        , lazy <| \_ -> tupleType
        , lazy <| \_ -> recordType
        , lazy <| \_ -> forallType minStartCol
        ])

simpleTypeWithPossibleArguments : MinStartCol -> Parser (WS -> Type)
simpleTypeWithPossibleArguments minStartCol =
  inContext "simple type with arguments" <|
    mapWSInfo <|
    trackInfo <| andThen (\(first, args) ->
        let f = first space0 in
        case f.val of
          TVar _ "List" -> case args of
            [arg] -> succeed <| \spApp -> TList spApp arg space0
            _ -> fail "List takes exactly one type argument"
          TVar _ "Dict" -> case args of
            [tkey, tvalue] -> succeed <| \spApp -> TDict spApp tkey tvalue space0
            _ -> fail "Dict takes exactly two type arguments"
          _ -> succeed <| \spApp -> TApp spApp f args SpaceApp
      ) <|
      (trackInfo (simpleType minStartCol) |> andThen (\wsToMainTypeI ->
         let appargSpace = spaceSameLineOrNextAfter (min minStartCol wsToMainTypeI.start.col) MinIndentSpace in
         map ((,) wsToMainTypeI.val) <|
           repeat zeroOrMore (delayedCommitMap (\ws wsType -> wsType ws)
             appargSpace
             (simpleType minStartCol))))

typ: Parser WS -> MinStartCol -> Parser Type
typ spFirst minStartCol =
  inContext "type" <|
      lazy <| \_ ->
        let b: { greedySpaceParser : Parser WS
               , precedenceTable : PrecedenceTable
               , minimumPrecedence : Int
               , expression : Parser (WS -> Type)
               , withZeroSpace: (WS -> Type) -> Type
               , operator : Parser (WithInfo Operator)
               , representation : WithInfo Operator -> String
               , combine : WS -> Type -> (WithInfo Operator)-> Type -> Type
               }
               -> Parser (WS -> Type)
            b = binaryOperator
        in
        let operatorSpace = spaceSameLineOrNextAfter minStartCol MinIndentSpace in
        delayedCommitMap (\ws wsToType -> wsToType ws)
         spFirst <| b
          { withZeroSpace = \wsTyp ->
             let finalTyp = wsTyp space0 in
             mapPrecedingWhitespaceTypeWS (\ws -> withInfo ws.val finalTyp.start finalTyp.start) finalTyp
          , greedySpaceParser =
              spaces
          , precedenceTable =
              builtInTypePrecedenceTable
          , minimumPrecedence =
              0
          , expression =
              simpleTypeWithPossibleArguments minStartCol
          , operator =
              typeOperator operatorSpace
          , representation =
              .val >> Tuple.second
          , combine =
              \wsBefore left operator right ->
                let
                  (wsBeforeOp, identifier) =
                    operator.val
                in
                withInfo
                  (TApp wsBefore (replaceInfo operator <| TVar space1 identifier) [left, right] InfixApp)
                    left.start right.end
          }

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

builtInTypePrecedenceList: List (Int, List String, List String)
builtInTypePrecedenceList =
 [ ( 3
   , []
   , ["&"]
   )
 , ( 2
   , []
   , ["|"]
   )
 , ( 1
   , ["->"]
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

builtInPatternPrecedenceTable : PrecedenceTable
builtInPatternPrecedenceTable =
  buildPrecedenceTable builtInPatternPrecedenceList

builtInTypePrecedenceTable: PrecedenceTable
builtInTypePrecedenceTable =
  buildPrecedenceTable builtInTypePrecedenceList

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
    "^" ->
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

typeOperator: Parser WS -> ParserI Operator
typeOperator appargSpace =
  paddedBefore (,) appargSpace typeSymbolIdentifier

--==============================================================================
-- Modules
--==============================================================================

-- TODO: Remove so that we can define new modules;
moduleNames : Set String
moduleNames =
  Set.fromList
    -- preludeLeo.elm
    [ "Core"
    , "Basics"
    , "List"
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
    , "Result"
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

function : MinStartCol -> SpaceConstraint -> Parser (WS -> Exp)
function minStartCol spConstraint =
  inContext "function" <|
      mapWSExp_ <|
          transferInfo ( \(parameters, body) wsBefore ->
              EFun wsBefore parameters body space0
          )
          ( trackInfo <|
              succeed (,)
                |. symbol "\\"
                |= repeat oneOrMore (pattern spaces minStartCol)
                |. spaces
                |. symbol "->"
                |= expression spaces minStartCol spConstraint
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
              expression spaces 0 MinIndentSpace
          , tailItem =
              expression spaces 0 MinIndentSpace
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
        delayedCommitMap (\startPos f -> f startPos)
        getPos
        (succeed (\mbInit decls wsEnd endPos startPos wsBefore ->
          withInfo (exp_ <| ERecord wsBefore mbInit decls wsEnd) startPos endPos
        )
        |. symbol "{"
        |= optional (delayedCommitMap always
          (succeed (,)
           |= (let customSpace = LangParserUtils.spacesWithoutNewline in
             expression customSpace 0 MinIndentSpace)
           |= spaces)
          (symbol "|")
          )
        |= oneOf[
          declarations 0,
          succeed (Declarations [] ([], []) [] ([], []))]
        |= spaces
        |. symbol "}"
        |= getPos)

--------------------------------------------------------------------------------
-- Conditionals
--------------------------------------------------------------------------------

conditional : MinStartCol -> SpaceConstraint -> Parser (WS -> Exp)
conditional minStartCol spConstraint =
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
                  |= expression spaces minStartCol MinIndentSpace
                  |= spaces
                  |. keywordWithSpace "then"
                  |= expression spaces minStartCol MinIndentSpace
                  |= spaces
                  |. keywordWithSpace "else"
                  |= expression spaces minStartCol spConstraint
          )

--------------------------------------------------------------------------------
-- Case Expressions
--------------------------------------------------------------------------------

caseExpression : MinStartCol -> SpaceConstraint -> Parser (WS -> Exp)
caseExpression minStartCol spConstraint =
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
                  |= (pattern nospace minStartCol
                     |> andThen (\p ->
                        let newMinStartCol = p.start.col + 1 in
                        let spPolicy = spaceSameLineOrNextAfter newMinStartCol MinIndentSpace in
                        succeed (\wsBeforeArrow e -> (p, wsBeforeArrow, e))
                        |= spaces
                        |. symbol "->"
                        |= expression spaces newMinStartCol spConstraint
                        |. optional (delayedCommit spPolicy (symbol ";"))
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
      mapWSExp_ <| transferInfo
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
                       expression spaces 0 MinIndentSpace,
                       succeed (withDummyExpInfo <| EVar space1 implicitVarName) -- Unparsable var
                     ]
                  |= spaces
                  |. keyword "of"
                  |= (
                      branch spaces |>
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

-- In let or def context
letExpOrAnnotation : MinStartCol -> Parser (OptCommaSpace -> WS -> Declaration)
letExpOrAnnotation minStartCol =
  inContext "let/def binding" <| andThen identity <|
     delayedCommitAndThen (\(name, parameters, wsBeforeEq) final ->
       final name parameters wsBeforeEq
     )
     (succeed (,,)
     |= pattern nospace minStartCol
     |= repeat zeroOrMore (pattern spaces minStartCol)
     |= spaces)
     (\(p1, ps, sps) ->
      oneOf [source <| symbol "=", source <| symbol ":"] |>
      andThen (\eqSymbol ->
       if eqSymbol == "=" then
         expression spaces (p1.start.col + 1) MinIndentSpace
         |> map (\binding_ ->
           \pat parameters wsBeforeEq ->
             let (binding, funArgStyle) =
               if List.isEmpty parameters then
                  (binding_, FunArgsAfterEqual)
               else
                  (withInfo
                    (exp_ <| EFun space0 parameters binding_ space0)
                    binding_.start
                    binding_.end
                  ,FunArgAsPats)
             in
             succeed <| \optCommaSpace wsBeforePat -> DeclExp <|
               LetExp optCommaSpace wsBeforePat pat funArgStyle wsBeforeEq binding
         )
       else
         typ spaces minStartCol
         |> map (\binding_ ->
           \pat parameters wsBeforeEq ->
              let mbBindingsFunArgStyles =
                if List.isEmpty parameters then
                   Ok (binding_, FunArgsAfterEqual)
                else
                   case parameters |> List.map patToTPat |> Utils.projJusts of
                    Just tpats -> Ok (withInfo
                       (TForall space0 tpats binding_ space0)
                        binding_.start
                        binding_.end
                       ,FunArgAsPats)
                    Nothing -> Err "Could not interpret this pattern as a type pattern"
              in
              case mbBindingsFunArgStyles of
                Ok (binding, funArgStyle) ->
                  succeed <| \optCommaSpace wsBeforePat -> DeclAnnotation <| LetAnnotation optCommaSpace wsBeforePat pat funArgStyle wsBeforeEq binding
                Err msg -> fail msg
         )
     ))

computePrintOrder: List Int -> List Int
computePrintOrder evaluationOrder =
  (Utils.foldLeft (Array.initialize (List.length evaluationOrder) (\_ -> 0)) (Utils.zipWithIndex evaluationOrder) <|
                \array (n, i) ->
                  Array.set n i array
  ) |> Array.toList

reorderDefinitions: List Declaration -> Result String Declarations
reorderDefinitions letExps =
  -- We put types at the top
  -- We put expressions which are not EFuns at the top, keeping their order if it is possible
  -- Lastly, we keep all remaining functions in their given order.
  -- It does not matter since functions will be mutually recursive
  let (revTypesWithIndex, revAnnWithIndex, revExpWithIndex, _) =
    Utils.foldLeft ([], [], [], 0) letExps <|
       \(accType, accAnn, accExp, index) def -> case def of
        DeclType t -> ((t, index)::accType, accAnn, accExp, index + 1)
        DeclAnnotation a -> (accType, (a, index)::accAnn, accExp, index + 1)
        DeclExp e -> (accType, accAnn, (e, index)::accExp, index + 1)
  in
  let (typesWithIndex, annWithIndex, expWithIndex) = (
    List.reverse revTypesWithIndex,
    List.reverse revAnnWithIndex,
    List.reverse revExpWithIndex) in
  let canBeRecursive = eFunUnapply >> Utils.maybeIsEmpty >> not in
  let expDefsWithDependencies = List.map (\(LetExp _ _ pats _ _ e as def, index) ->
        ((def, index), (identifiersSetInPat pats, freeIdentifiers e, canBeRecursive e))
       ) expWithIndex
  in
  let typeDefsWithDependencies = List.map(\(LetType _ _ _ pats _ _ t as def, index) ->
        ((def, index), (identifiersSetInPat pats, {-freeIdentifiers e-}Set.empty, False)) -- TODO: Set of free identifiers in types, and recursive types
       ) typesWithIndex
  in
  let takeIndices = List.map (Tuple.first >> Tuple.second) in
  case Utils.orderWithDependencies expDefsWithDependencies Tuple.second of
    Err msg -> Err msg
    Ok expsGrouped ->
      case Utils.orderWithDependencies typeDefsWithDependencies Tuple.second  of
          Err msg -> Err msg
          Ok typesGrouped ->
            let reorderingTypes = typesGrouped |> List.concatMap takeIndices in
            let (finalAnnotations, reorderingAnnotations) = List.unzip annWithIndex in
            let reorderingExp = expsGrouped |> List.concatMap takeIndices in
            let evaluationOrder = reorderingTypes ++ reorderingAnnotations ++ reorderingExp in
            let finalTypes = extractGroupInfo (Tuple.first >> Tuple.first) typesGrouped in
            let finalExps = extractGroupInfo (Tuple.first >> Tuple.first) expsGrouped in
            let printOrder = computePrintOrder evaluationOrder in
            Ok <| Declarations printOrder finalTypes finalAnnotations finalExps

headDeclaration: MinStartCol -> Parser Declaration
headDeclaration minStartCol =
  inContext "Declaration #1" <| lazy <| \_ ->
    delayedCommitMap (\sp declBuilder -> declBuilder Nothing sp)
    spaces
    (oneOf [
      typeDefOrAlias minStartCol,
      letExpOrAnnotation minStartCol
      ])

tailDeclarations: MinStartCol -> List Declaration -> Parser (List Declaration)
tailDeclarations minStartCol revPrevDeclarations =
  let spaceBeforeNewDeclaration: Parser (OptCommaSpace, WS)
      spaceBeforeNewDeclaration = oneOf [
       try <|
        (succeed (\a b -> (Just a, b))
          |= spaces
          |. symbol ","
          |= spaces)
       , succeed ((,) Nothing) |= (spaceSameLineOrNextAfterOrTwoLines minStartCol)
    ]
  in
    succeed identity
    |= oneOf [
      delayedCommitAndThen (\_ -> identity)
        spaceBeforeNewDeclaration (\(optCommaSpace, wsBefore) ->
      let newMinStartCol = if wsBefore.start.line + 1 < wsBefore.end.line then
           wsBefore.end.col
        else minStartCol
      in
      oneOf [
         typeDefOrAlias newMinStartCol,
         letExpOrAnnotation newMinStartCol
      ] |>
      inContext ("Declaration #" ++ toString (List.length revPrevDeclarations + 1)) |>
      andThen (\final -> tailDeclarations newMinStartCol (final optCommaSpace wsBefore :: revPrevDeclarations)))
    , succeed (List.reverse revPrevDeclarations)]

-- Non-empty declarations
declarations: MinStartCol -> Parser Declarations
declarations minStartCol =
  inContext "Declarations" <| lazy <| \_ ->
  (headDeclaration minStartCol |> andThen (\headDecl ->
     let firstWS = precedingWhitespaceDeclarationWithInfo headDecl in
     tailDeclarations firstWS.end.col [headDecl]) |>
    andThen (\definitions -> case reorderDefinitions definitions of
         Ok r -> succeed r
         Err msg -> fail msg)
  )

genericLetBinding : Parser Exp -> Parser (WS -> Exp)
genericLetBinding bodyParser =
  lazy <| \_ ->
    delayedCommitMap (\startPos (decls, wsBeforeIn, endPos, body) wsBefore ->
       withInfo (exp_ <| ELet wsBefore Let decls wsBeforeIn body) startPos endPos
    )
    getPos
    (succeed (,,,)
    |. keyword "let"
    |= (getPos |> andThen (\pos -> declarations (pos.col - 3)))
    |= spaces
    |. keywordWithSpace "in"
    |= getPos
    |= bodyParser
    )


letBinding : MinStartCol -> SpaceConstraint -> Parser (WS -> Exp)
letBinding minStartCol spConstraint =
  inContext ("let binding") <|
  genericLetBinding <| expression spaces minStartCol spConstraint

--------------------------------------------------------------------------------
-- Tuples, parentheses, implicit tupling functions
--------------------------------------------------------------------------------

tuple : Parser (WS -> Exp)
tuple =
  inContext "tuple" <|
    lazy <| \_ ->
      mapWSExp_ <|
        genericTuple
          { term = expression spaces 0 MinIndentSpace
          , tagger =
              withDummyExpInfo << EBase space0 << EString defaultQuoteChar
          , record =
              \ws1 entries ws2 ->
                eRecord__ ws1 Nothing entries ws2
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
-- General Expressions
--------------------------------------------------------------------------------

selection : Parser ((WS -> Exp) -> (WS -> Exp))
selection =
  lazy <| \_ ->
  delayedCommitMap (\wsBeforeDot (wsAfterDot,idWithInfo) ws2Exp ->
      \wsBefore ->
        let exp = ws2Exp space0 in
        withInfo (exp_ <| ESelect wsBefore exp wsBeforeDot wsAfterDot idWithInfo.val)
           exp.start idWithInfo.end
    )
    ( succeed (\x -> x)
      |= (trackInfo <| source <| symbol "")
      |. symbol "."
    )
    (( succeed (\wsAfterDot x -> (wsAfterDot, x)))
      |= (trackInfo <| source <| symbol "")
      |= (trackInfo <| identifier)
    )

-- Add all following .identifier1.identifiers2 as wrappers to the original expression
addSelections : Parser (WS -> Exp) -> Parser (WS -> Exp)
addSelections parser =
  succeed (\simpExp selections ->
      List.foldl (\sel updatedExp -> sel updatedExp) simpExp selections
      )
    |= parser
    |= repeat zeroOrMore selection

implicitSelectionFun: Parser (WS -> Exp)
implicitSelectionFun =
  succeed (\{val,start,end} initSpace ->
    withInfo (exp_ <| EFun initSpace [withInfo (pat_ <| PVar space0 implicitVarName noWidgetDecl) start start]
    (List.foldl (\sel updatedExp -> sel updatedExp) (\wsBefore -> withInfo (exp_ <| EVar wsBefore implicitVarName) start start) val space0) space0) start end
  )
  |= (trackInfo <| repeat oneOrMore selection)

implicitOp: Parser (WS -> Exp)
implicitOp =
  lazy <| \_ ->
  operator nospace |> andThen (\op ->
    let (ws0, identifier) = op.val in
    case opFromIdentifier identifier of
      Just op_ -> succeed <| \wsBefore -> withInfo (exp_ <| EOp wsBefore space0 (withInfo op_ op.start op.end) [] space0) op.start op.end
      Nothing ->  succeed <| \w -> withInfo (exp_ <| EVar w identifier) op.start op.end
  )

-- Not a function application nor a binary operator
simpleExpression : MinStartCol -> SpaceConstraint -> Parser (WS -> Exp)
simpleExpression minStartCol spConstraint =
    oneOf
      [ addSelections variableExpression
      , constantExpression
      , lazy <| \_ -> htmlliteral
      , conditional minStartCol spConstraint
      , caseExpression minStartCol spConstraint
      , letBinding minStartCol spConstraint
      , lazy <| \_ -> multiLineInterpolatedString
      , baseValueExpression
      , lazy <| \_ -> function minStartCol spConstraint
      , lazy <| \_ -> implicitSelectionFun
      , lazy <| \_ -> list
      , lazy <| \_ -> addSelections record
      , lazy <| \_ -> addSelections tuple
      , lazy <| \_ -> addSelections hole
    ]

spaceColonType: MinStartCol -> SpaceConstraint -> Parser (WS, Type)
spaceColonType minStartCol spConstraint =
  lazy <| \_ ->
     try ( succeed (,)
          |= (spaceSameLineOrNextAfter minStartCol spConstraint)
          |. symbol ":"
          |= typ spaces minStartCol
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
-- Arguments must not be aligned with the main expression, AND indented by at least minStartCol
simpleExpressionWithPossibleArguments : MinStartCol -> SpaceConstraint -> Parser (WS -> Exp)
simpleExpressionWithPossibleArguments minStartCol spConstraint =
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
                      ( Just space0
                      , space0
                      , Lang.ctorArgs
                      , space0
                      , withDummyExpInfo <|
                          eRecord__ space0 Nothing [] space0
                      )
                  in
                    withInfo
                      ( exp_ <|
                          eRecord__
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
                      |> List.map (\e -> (Just space0, e))
                      |> Utils.indexedMapFrom 1 Lang.numericalEntry

                  argsEntry =
                    ( Just space0
                    , space0
                    , Lang.ctorArgs
                    , space0
                    , withDummyExpInfo <|
                        eRecord__ space0 Nothing insideArgsEntries space0
                    )
                in
                  Just << exp_ <|
                    eRecord__
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
      ( trackInfo (oneOf [simpleExpression minStartCol spConstraint, implicitOp])
      |> andThen (\wsToMainExp ->
        let appargSpace = spaceSameLineOrNextAfter (min minStartCol wsToMainExp.start.col) spConstraint in
        map (combine wsToMainExp.val) <|
          repeat zeroOrMore (delayedCommitMap (\ws wsExp -> wsExp ws)
          (appargSpace |> andThen (\sp ->
            if sp.val == "" then
              succeed sp
              |. negativeLookAhead (symbol "-")
            else
              succeed sp
          ))
          (simpleExpression minStartCol spConstraint))))

simpleExpressionWithPossibleArgumentsMaybeTyped : MinStartCol -> SpaceConstraint -> Parser (WS -> Exp)
simpleExpressionWithPossibleArgumentsMaybeTyped minStartCol spConstraint =
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
      |= simpleExpressionWithPossibleArguments minStartCol spConstraint
      |= ParserUtils.optional (spaceColonType minStartCol spConstraint))
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
  LangParserUtils.spacesCustom <|
    LangParserUtils.SpaceCheck (\start end -> start.line == end.line || end.col - 1 >= nSpaces) <|
      \() -> "I expect that to be on the same line or indented by at least " ++ toString nSpaces ++ " spaces."

sameLineOrIndentedByExactly: String -> Int -> Parser WS
sameLineOrIndentedByExactly msg nSpaces =
  LangParserUtils.spacesCustom <|
    LangParserUtils.SpaceCheck (\start end -> start.line == end.line || end.col - 1 == nSpaces) <|
      \() -> "I expect that to be on the same line or indented by exactly " ++ toString nSpaces ++ " spaces."

-- A next declaration should be on the next line aligned with the previous one, or anywhere else after two lines
nextDeclarationSpace: String -> Int -> Parser WS
nextDeclarationSpace msg nSpaces =
  LangParserUtils.spacesCustom <|
    LangParserUtils.SpaceCheck (\start end -> start.line + 1 == end.line && end.col - 1 == nSpaces
      || start.line + 1 < end.line
      ) <|
      \() -> "I expect that to be on a different line and indented by exactly " ++ toString nSpaces ++ " spaces."

sameLineOrIndentedByDifferentThan: String -> Int -> Parser WS
sameLineOrIndentedByDifferentThan msg nSpaces =
  LangParserUtils.spacesCustom <|
      LangParserUtils.SpaceCheck (\start end -> start.line == end.line || end.col - 1 /= nSpaces) <|
        \() -> "I expect that to be on the same line or not indented by " ++ toString nSpaces ++ " spaces."

expressionGeneral : Bool -> Parser WS ->  MinStartCol -> SpaceConstraint -> Parser Exp
expressionGeneral isHtmlAttribute spFirst minStartCol    spConstraint =
  inContext "expression" <|
    lazy <| \_ ->
      let operatorSpace = spaceSameLineOrNextAfter minStartCol spConstraint in
      delayedCommitMap (\wsFront binaryExp -> binaryExp wsFront)
        spFirst <| binaryOperator
        { greedySpaceParser =
            spaces
        , precedenceTable =
            builtInPrecedenceTable
        , minimumPrecedence =
            0
        , expression =
            simpleExpressionWithPossibleArgumentsMaybeTyped minStartCol spConstraint
        , withZeroSpace = \wsExp ->
            let finalExp = wsExp space0 in
            mapPrecedingWhitespaceWS (\ws -> withInfo ws.val finalExp.start finalExp.start) finalExp
        , operator =
            if isHtmlAttribute then htmlAttributeOperator operatorSpace else operator operatorSpace
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

expression : Parser WS -> MinStartCol -> SpaceConstraint -> Parser Exp
expression firstSpace minStartCol spConstraint= expressionGeneral False firstSpace minStartCol spConstraint

expressionWithoutGreater : Parser WS -> MinStartCol -> SpaceConstraint -> Parser Exp
expressionWithoutGreater firstSpace minStartCol spConstraint = expressionGeneral True firstSpace minStartCol spConstraint


--==============================================================================
-- Top-Level Expressions
--=============================================================================

-- The semicolon is now optional. Maybe remove that in the future?
optionalTopLevelSemicolon = optional (paddedBefore (\_ _ _ -> ()) spaces (trackInfo <| symbol ";"))

--------------------------------------------------------------------------------
-- Type Aliases
--------------------------------------------------------------------------------

typeDefOrAlias : MinStartCol -> Parser (OptCommaSpace -> WS -> Declaration)
typeDefOrAlias minStartCol =
  inContext "type or type alias" <|
    succeed
      ( \spAfterType mbSpaceAfterAlias wsToName parameters spEq binding_ optCommaSpace wsBeforeType ->
         let
           (binding, funArgStyle) =
             if List.isEmpty parameters then
               (binding_, FunArgsAfterEqual)
             else
               (withInfo
                 (TForall space0 parameters binding_ space0)
                 binding_.start
                 binding_.end, FunArgAsPats)
         in
         let (mbSpaceBeforeAlias, spaceBeforePattern) = case mbSpaceAfterAlias of
           Nothing -> (Nothing, spAfterType)
           Just spPattern -> (Just spAfterType, spPattern)
         in
         DeclType <| LetType optCommaSpace wsBeforeType mbSpaceBeforeAlias (wsToName spaceBeforePattern) funArgStyle spEq binding
      )
      |. keyword "type"
      |= spaces
      |= optional (
         succeed identity
        |. keyword "alias"
        |= spaces)
      |= simplePattern
      |= repeat zeroOrMore (
         delayedCommitMap (\ws wsPat -> wsPat ws)
         spaces
         typePattern)
      |= spaces
      |. symbol "="
      |= typ spaces minStartCol
      |. optionalTopLevelSemicolon
      --|= getPos

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
          ELet newline2 Let (Declarations [0] ([], []) [] ([LetExp Nothing space1 name FunArgAsPats space1 binding], [0])) space1 body
  in
    succeed builder
      |= getPos

mainExpression : Parser Exp
mainExpression =
  inContext "Main expression" <|
  oneOf
    [ expression spaces 0 MinIndentSpace
    , implicitMain
    ]

program : Parser Exp
program =
  succeed (\declsOpt mainExp ->
    case declsOpt of
      Nothing -> mainExp.val
      Just decls ->
        withInfo (exp_ <| ELet space0 Def decls.val space0 mainExp.val) decls.start mainExp.val.end
  )
  |= optional (trackInfo (declarations 0))
  |= trackInfo mainExpression
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
  run (typ spaces 0)



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
        Err err -> --(Just "Prelude did not parse and 0 either", freshenClean 1 (withDummyExpInfo <| EConst space0 0 dummyLoc noWidgetDecl))
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

         ELet ws1 kind (Declarations po (tpes, got) ann (exps, goe)) wsIn body ->
           let (newRevTpes, newK) = Utils.foldLeft ([], k) tpes <|
             \(revAcc, k) (LetType sp1 spType mbSpAlias pat funPolicy spEq tp) ->
                let (newPat, newK) = freshenPatPreserving idsToPreserve (pat, k) in
                (LetType sp1 spType mbSpAlias newPat funPolicy spEq tp :: revAcc, newK)
           in
           let (newRevAnn, newK2) = Utils.foldLeft ([], newK) ann <|
             \(revAcc, k) (LetAnnotation sp1 sp0 pat funPolicy spEq e1) ->
                let (newPat, newK) = freshenPatPreserving idsToPreserve (pat, k) in
                (LetAnnotation sp1 sp0 newPat funPolicy spEq e1::revAcc, newK)
           in
           let (newRevExps, newK3) = Utils.foldLeft ([], newK2) exps <|
             \(revAcc, k)  (LetExp sp1 sp0 p funPolicy spEq e1) ->
                 let (newP, newK) = freshenPatPreserving idsToPreserve (p, k) in
                 let newE1 = recordIdentifiers (newP, e1) in
                 (LetExp sp1 sp0 newP funPolicy spEq newE1 :: revAcc, newK)
           in
           (ELet ws1 kind (Declarations po (List.reverse newRevTpes, got) (List.reverse newRevAnn) (List.reverse newRevExps, goe)) wsIn body, newK3)

         EFun ws1 pats body ws2 ->
           let (newPats, newK) = freshenPatsPreserving idsToPreserve (pats, k) in
           (EFun ws1 newPats body ws2, newK)

         ECase ws1 scrutinee branches ws2 ->
           let (newBranches, newK) =
             branches
             |> List.foldl
                 (\branch (newBranches, k) ->
                   let (Branch_ bws1 pat ei bws2) = branch.val in
                   let (newPi, newK) = freshenPatPreserving idsToPreserve (pat, k) in
                   (newBranches ++ [{ branch | val = Branch_ bws1 newPi ei bws2 }], newK)
                 )
                 ([], k)
           in
           (ECase ws1 scrutinee newBranches ws2, newK)
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
freshenPatsPreserving : Set.Set Int -> (List Pat, Int) -> (List Pat, Int)
freshenPatsPreserving idsToPreserve (pats, initK) =
  pats
  |> List.foldl
      (\pat (finalPats, k) ->
        let (newPat, newK) = freshenPatPreserving idsToPreserve (pat, k) in
        (finalPats ++ [newPat], newK)
      )
      ([], initK)


-- Reassign any id not in idsToPreserve
freshenPatPreserving : Set.Set Int -> (Pat, Int) -> (Pat, Int)
freshenPatPreserving idsToPreserve (p, initK) =
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
            ELet _ kind (Declarations _ _ _ (defs, _)) _ e2     ->
              defs |> List.concatMap (\(LetExp _ _ p _ _ e1) -> pidsInPat p)
            EFun ws1 pats body ws2                -> pidsInPats pats
            ECase ws1 scrutinee branches ws2      -> pidsInPats (branchPats branches)
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

  (PAs _ p1 _ p2, _) ->
    recordIdentifiers (p1,e)

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

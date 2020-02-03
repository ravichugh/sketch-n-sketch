module HTMLParser exposing (parseHTMLString,
  HTMLNode, HTMLEndOpeningStyle(..), HTMLClosingStyle(..), HTMLCommentStyle(..), HTMLAttribute, HTMLAttributeValue,
  HTMLNode_(..),
  HTMLAttribute_(..),
  HTMLAttributeValue_(..),
  HTMLAttributeStringElem_(..),
  HTMLAttributeStringElem,
  HTMLTag(..),
  ParsingMode(..),
  isVoidElement, isForeignElement,
  NameSpace(..),
  unparseHtmlNodes,
  unparseTagName,
  unparseNode,
  unparseHtmlNodesDiffs,
  parseOneNode,
  parseNode,
  entityToString,
  interpretAttrValueContent,
  AtPresence(..),
  printAttrValueRaw)

import Char
import Set exposing (Set)
import Dict
import Pos
import Parser as P exposing (..)
import Parser.LanguageKit as LK
import Lang exposing (..)
import Info exposing (..)

import ParserUtils exposing (..)
import LangParserUtils exposing (..)
import BinaryOperatorParser exposing (..)
import Utils
import Regex
import Parser
import Parser.LanguageKit as LanguageKit
import Info exposing (WithInfo)
import Either

import ImpureGoodies

type NameSpace  = HTML | Foreign

type alias HTMLAttributeStringElem = WithInfo HTMLAttributeStringElem_
type HTMLAttributeStringElem_ =
      HTMLAttributeStringRaw String
    | HTMLAttributeEntity {-Entity rendered-} String {-Entity raw -} String

type alias HTMLAttributeValue = WithInfo HTMLAttributeValue_
type HTMLAttributeValue_ =
    HTMLAttributeUnquoted WS {-Equal sign-} WS (List HTMLAttributeStringElem)
  | HTMLAttributeString WS {-Equal sign-} WS String {-Delimiter char-}  (List HTMLAttributeStringElem)
  | HTMLAttributeNoValue
  | HTMLAttributeExp WS {-Equal sign-} WS AtPresence (WithInfo Exp_)

type AtPresence = AtPresent | AtAbsent

type alias HTMLAttribute = WithInfo HTMLAttribute_
type HTMLAttribute_ =
    HTMLAttribute WS (WithInfo String) HTMLAttributeValue
  | HTMLAttributeListExp WS (WithInfo Exp_)
type HTMLCommentStyle = Less_Greater String {- The string should start with a ? -}
                      | LessSlash_Greater {- The string should start with a space -} String
                      | LessBang_Greater String
                      | LessBangDashDash_DashDashGreater String

type HTMLClosingStyle = RegularClosing WS | VoidClosing | AutoClosing | ForgotClosing | ImplicitElem
type HTMLEndOpeningStyle = RegularEndOpening {- usually > -} | SlashEndOpening {- add a slash before the '>' of the opening, does not mark the element as ended in non-void HTML elements -}
-- HTMLInner may have unmatched closing tags inside it. You have to remove them to create a real innerHTML
-- HTMLInner may have unescaped chars (e.g. <, >, & etc.)

type alias HTMLNode = WithInfo HTMLNode_
type HTMLNode_ = HTMLInner String
               | HTMLEntity {-Entity rendered-} String {-Entity raw -} String
               | HTMLElement HTMLTag (List HTMLAttribute) WS HTMLEndOpeningStyle (List HTMLNode) HTMLClosingStyle
               | HTMLComment HTMLCommentStyle
               | HTMLListNodeExp (WithInfo Exp_)
               | HTMLDoctype {-DOCTYPE itself-} String
                   WS {-Name-} (WithInfo String) WS
                   {-public-} (Maybe (String, WS, HTMLDoctypeValue, WS))
                   {-system-} (Maybe (Maybe (String, WS), HTMLDoctypeValue, WS))

type alias HTMLDoctypeValue = ({-quote char-}String, {-content-}String)

type HTMLTag = HTMLTagString (WithInfo String) | HTMLTagExp (WithInfo Exp_)

type ParsingMode = Raw |
  Interpolation {
    tagName: ParserI Exp_,
    attributevalue: Parser WS -> ParserI Exp_,
    attributerawvalue: String -> List HTMLAttributeStringElem -> Exp,
    attributelist: ParserI Exp_,
    childlist: Parser WS -> ParserI Exp_}

entityToString: String -> String
entityToString entity =
  case Regex.find (Regex.AtMost 1) (Regex.regex "^&#x0*([a-fA-F0-9]+);?$") entity of
    [m] -> List.head m.submatches
           |> Maybe.andThen identity
           |> Maybe.andThen (\hex-> ImpureGoodies.fromHtmlEntity ("x" ++ hex))
           |> Maybe.withDefault entity
    _ ->
  case Regex.find (Regex.AtMost 1) (Regex.regex "^&#0*([0-9]+);?$") entity of
     [m] -> List.head m.submatches
            |> Maybe.andThen identity
            |> Maybe.andThen (\dec -> ImpureGoodies.fromHtmlEntity dec)
            |> Maybe.withDefault entity
     _ ->
  case Regex.find (Regex.AtMost 1) (Regex.regex "^(&\\w+;?)$") entity of
      [m] ->
         List.head m.submatches
         |> Maybe.andThen identity
         |> Maybe.andThen (\name ->  ImpureGoodies.fromHtmlEntity name)
         |> Maybe.withDefault entity
      _ ->
  case Regex.find (Regex.AtMost 1) (Regex.regex "^</[\\s\\S]*>?$") entity of
       [m] -> ""
       _ ->
        entity

voidElements: Set String
voidElements =
  Set.fromList ["area", "base", "br", "col", "embed", "hr", "img", "input",
                "link", "meta", "param", "source", "track", "wbr"]

isVoidElement: String -> Bool
isVoidElement s =
  Set.member (String.toLower s) voidElements

isForeignElement: String -> Bool
isForeignElement s = let s2 = String.toLower s in s == "math" || s == "svg"

svgTagNames = Set.fromList ["a","altGlyph","altGlyphDef","altGlyphItem","animate","animateColor","animateMotion",
  "animateTransform","circle","clipPath","color-profile","cursor","defs","desc","discard","ellipse","feBlend",
  "feColorMatrix","feComponentTransfer","feComposite","feConvolveMatrix","feDiffuseLighting","feDisplacementMap",
  "feDistantLight","feDropShadow","feFlood","feFuncA","feFuncB","feFuncG","feFuncR","feGaussianBlur","feImage",
  "feMerge","feMergeNode","feMorphology","feOffset","fePointLight","feSpecularLighting","feSpotLight","feTile",
  "feTurbulence","filter","font","font-face","font-face-format","font-face-name","font-face-src","font-face-uri",
  "foreignObject","g","glyph","glyphRef","hatch","hatchpath","hkern","image","line","linearGradient","marker","mask",
  "mesh","meshgradient","meshpatch","meshrow","metadata","missing-glyph","mpath","path","pattern","polygon","polyline",
  "radialGradient","rect","script","set","solidcolor","stop","style","svg","switch","symbol","text","textPath","title",
  "tref","tspan","unknown","use","view","vkern"]

possiblyAutoClosingElements = svgTagNames
isPossiblyAutoClosing s = Set.member (String.toLower s) svgTagNames

{-- HTML reference parsing: https://www.w3.org/TR/html5/syntax.html --}

{-- To see how HTML is parsed, we had a look at the method .innerHTML

function parsed(s) {
  var d = document.createElement("div");
  d.innerHTML = s
  return d.innerHTML;
}

parsed("<?Hello>") == "<!--Hello-->"
parsed("<!Hello>") == "<!--Hello-->"
parsed("<!Hello-->") == "<!--Hello---->"
parsed("<!-Hello-->") == "<!---Hello---->"
parsed("<!--Hello-->") == "<!--Hello---->"
parsed("</ Hello >") == "<!-- Hello -->"
--}
parseHTMLComment: Parser HTMLNode
parseHTMLComment =
  inContext "Parsing HTML comment" <|
  trackInfo <|
  delayedCommitMap (\_ -> HTMLComment)
    (symbol "<")
    (oneOf
    [ succeed Less_Greater
      |. lookAhead (symbol "?")
      |= keep oneOrMore (\c -> c /= '>')
      |. symbol ">"
    , succeed identity
      |. symbol "!"
      |= oneOf [
          succeed LessBangDashDash_DashDashGreater
          |. symbol "--"
          |= keepUntilRegex (Regex.regex "-->")
          |. symbol "-->"
          , succeed LessBang_Greater
          |= keep oneOrMore (\c -> c /= '>')
          |. symbol ">"
        ]
    , succeed LessSlash_Greater
      |. lookAhead (delayedCommitMap (\_ _ -> ()) (symbol "/") (
            keep (Exactly 1) (\c -> not (Char.isUpper c) && not (Char.isLower c) && c /= '@')))
      |. symbol "/"
      |= keep oneOrMore (\c -> c /= '>')
      |. symbol ">"
    ])

-- Special attribute space handling: whitespaces can contain / if they are not followed by
attributeSpaces: Parser WS
attributeSpaces =
   trackInfo <|
     succeed (\chars -> String.join "" chars)
     |= repeat zeroOrMore (
       oneOf [
         keep (Exactly 1) isSpace,
         delayedCommit
           (negativeLookAhead (symbol "/>"))
           (symbol "/" |> source)
       ]
     )

-- parse("<i ====>Hello</i>") == "<i =="==">Hell</i>" (= is a valid start for an attribute name.
parseHtmlAttributeName: ParserI String
parseHtmlAttributeName = inContext "HTML attribute name" <| trackInfo <|
  LanguageKit.variable
    (\c -> c /= '>' && c /= '<' && c /= ',' && c /= '/' && not (isSpace c))
    (\c -> c /= '>' && c /= '<' && c /= ',' && c /= '/' && c /= '=' && not (isSpace c)) (Set.fromList [])

parseHTMLAttributeStringContent: (Char -> Bool) -> Parser (List HTMLAttributeStringElem)
parseHTMLAttributeStringContent isEndChar =
  let aux: List HTMLAttributeStringElem -> Parser (List HTMLAttributeStringElem)
      aux revPreviousElems =
    oneOf [
       keep (AtLeast 1) (\c -> not (isEndChar c) && c /= '&') |> trackInfo |> andThen (\s ->
         aux (replaceInfo s (HTMLAttributeStringRaw s.val) :: revPreviousElems)
       )
    ,  parseHTMLEntity True [] |> andThen (\s -> case s.val of
        Err x -> aux (replaceInfo s (HTMLAttributeStringRaw x) :: revPreviousElems)
        Ok (rendered, raw) ->
          aux (replaceInfo s (HTMLAttributeEntity rendered raw) :: revPreviousElems))
    ,  succeed (List.reverse revPreviousElems |> mergeHTMLAttributeStringRaw)
    ]
  in aux []

parseHTMLAttributeString: Parser (String, List HTMLAttributeStringElem)
parseHTMLAttributeString =
  oneOf [
      source (symbol "\"")
    , source (symbol "\'")
  ] |> andThen (\quoteCharStr ->
    let quoteChar = ImpureGoodies.stringCharAt 0 quoteCharStr |> Maybe.withDefault '\"' in
    parseHTMLAttributeStringContent (\c -> c == quoteChar)
    |> andThen (\l ->
      succeed identity
      |. symbol quoteCharStr
      |= succeed (quoteCharStr, l)
    ))

-- parse("<div> i<j =b / =hello=abc /a/ div / /> d") == "<div> i<j =b="" =hello="abc" a="" div=""> d</j></div>"
parseHtmlAttributeValue: ParsingMode -> Parser HTMLAttributeValue
parseHtmlAttributeValue parsingMode =
  case parsingMode of
    Raw ->
      inContext "html raw attribute value" <|
      oneOf [
        delayedCommitMap (\sp1 (sp2, builder) -> builder sp1 sp2)
          spaces
          (  succeed (\sp2 builder -> (sp2, builder))
             |. (symbol "=")
             |= spaces
             |= oneOf [
                 parseHTMLAttributeString |> trackInfo |> map (\attrString sp1 sp2 ->
                   let (quoteChar, content) = attrString.val in
                     replaceInfo attrString <| HTMLAttributeString sp1 sp2 quoteChar content)
                 , parseHTMLAttributeStringContent (\c -> isSpace c || c == '>') |> trackInfo |>
                   map (\contentInfo sp1 sp2 ->
                     replaceInfo contentInfo <| HTMLAttributeUnquoted sp1 sp2 contentInfo.val)
             ]
          ),
        trackInfo <| succeed HTMLAttributeNoValue
      ]
    Interpolation {attributevalue, attributerawvalue} ->
      inContext "html interpolatable attribute value" <|
      trackInfo <| oneOf [
          delayedCommitMap (\spBeforeEq builder -> builder spBeforeEq)
          spaces
          (succeed (\spAfterEq (atPresence, value) spBeforeEq -> HTMLAttributeExp spBeforeEq spAfterEq atPresence value)
            |. symbol "="
            |= spaces
            |= (optional (symbol "@") |> andThen (\maybeAt ->
              let atPresence = Maybe.map (always AtPresent) maybeAt |> Maybe.withDefault AtAbsent in
              map ((,) atPresence) <|
              oneOf ((if atPresence == AtAbsent then [
                  delayedCommitMap (\x y -> y)
                    (lookAhead (oneOf [symbol "\"", symbol "'"]))
                    (succeed (\attrString ->
                             let (quoteChar, content) = attrString.val in
                             let (Expr x) = attributerawvalue quoteChar content in
                             { x | start = attrString.start, end = attrString.end}
                        )
                     |. negativeLookAhead (symbol "\"\"\"")
                     |= (parseHTMLAttributeString |> trackInfo)) ] else []) ++
                [attributevalue nospace
                ])
            ))
           )
        , succeed HTMLAttributeNoValue
      ]


letterRegex = Regex.regex "^[a-zA-Z]$"

isLetter: Char -> Bool
isLetter c = Regex.contains letterRegex (String.fromChar c)

-- parse(""<div/> d") == "<div> d</div>"
-- parse(""<br/> d") == "<br> d"
-- parse(""<img/> d") == "<img> d"

parseTagName: Parser String
parseTagName =
  succeed (\a b -> a ++ b)
  |= keep (Exactly 1) isLetter
  |= keep zeroOrMore (\c -> not (isSpace c) && c /= '/' && c /= '>')

nodeElementStart: ParsingMode -> Parser (HTMLTag, String)
nodeElementStart parsingMode =
  let defaultParser =
    succeed (\x -> (HTMLTagString x, x.val))
    |= trackInfo parseTagName
  in
  delayedCommit
    (symbol "<") <|
    case parsingMode of
      Raw -> defaultParser
      Interpolation {tagName} ->
        oneOf [
          defaultParser,
          succeed (\s x -> (HTMLTagExp x, "@"))
          |= symbol "@"
          |= tagName
        ]

nodeStart: Parser String
nodeStart =
  delayedCommit
    (symbol "<")
    (keep oneOrMore (\c -> not (isSpace c) && c /= '/' && c /= '>' && c /= '!' && c /= '?'))

parseHTMLEntity: Bool -> List String -> ParserI (Result String (String, String)) -- Errs are raw strings containing the "&", whereas Ok contain the interpreted and raw entity
parseHTMLEntity insideAttribute untilEndTagNames =
  inContext "HTML entity" <|
  trackInfo <|
    oneOf <| (if not insideAttribute then [
      delayedCommitAndThen (\_ wrongClosingTag ->
        Ok ("", wrongClosingTag))
      (source <| (
           symbol "</"
        |. parseTagName -- No space here, because else it would be a comment.
        |. keep zeroOrMore (\c -> c /= '>')
        |. optional (symbol ">")))
      (\endTagName ->
        let tagNamesRegex= untilEndTagNames |> List.map (\et -> "</" ++ Regex.escape et ++ "\\s*>") |> String.join "|" in
        if Regex.contains (Regex.regex tagNamesRegex) endTagName then
          fail "it's a true closing tag"
        else
          succeed endTagName
      ) ] else []) ++
      [succeed identity
    |. symbol "&"
    |= (lookAhead (source (
         oneOf [
           symbol "#x"
           |. ignore (AtLeast 1) Char.isHexDigit
         , symbol "#"
           |. ignore (AtLeast 1) Char.isDigit
         , ignore (AtLeast 1) (\x -> Char.isLower x || Char.isUpper x || Char.isDigit x)
         , succeed ()
         ]
         |. optional (symbol ";"))) |> andThen
      (\result -> -- Find out which part we have to consume.
        let default matched = map (\x -> Ok (entityToString ("&" ++ x), "&" ++ x))  (source (symbol matched)) in
        if result |> String.startsWith "#" then
          default result
        else -- Let's find out the named entities.
          let aux matched =
            if matched == "" then succeed (Err "&") else
            case ImpureGoodies.fromHtmlEntity ("&" ++ matched) of
               Nothing -> aux (String.left (String.length matched - 1) matched)
               Just x ->
                 let lastChar = String.right 1 matched in
                 if insideAttribute && lastChar /= ";" then
                   oneOf [
                       succeed identity
                       |. lookAhead (
                         delayedCommitMap (\a b -> ())
                           (symbol matched)
                           (keep (Exactly 1) (\c ->
                             Char.isUpper c || Char.isLower c || Char.isDigit c ||
                               c == '=')))
                       |= succeed (Err "&")
                     , default matched]
                 else
                   default matched
          in aux result
      ))]

{--
parsed("a</j  >b") == "ab"
parsed("<j>a<:b></j>b") == "<j>a&lt;:b&gt;</j>b"
parsed("<j>a</b></j>b") == "<j>a</j>b"
--}
-- Always succeed if not empty
-- parse the content until a node starts (either a comment, a node start element, or the end tag of this node)
parseHTMLInner: ParsingMode -> List String -> Parser HTMLNode
parseHTMLInner parsingMode untilEndTagNames =
  let maybeBreakOnAt = case parsingMode of
    Raw -> ""
    _ -> "|@"
  in
  let tagNameStarts = case parsingMode of
    Raw -> "[a-zA-Z]"
    _ -> "[a-zA-Z@]"
  in
  let regexToParseUntil =
        Regex.regex <|
        if List.head untilEndTagNames == Just "script" then
        "</script>|&" ++ maybeBreakOnAt
        else if List.head untilEndTagNames == Just "style" then
        "</style>|&" ++ maybeBreakOnAt
        else   "<" ++ tagNameStarts++"|&|<\\?|<!|</|$" ++ maybeBreakOnAt ++
            (untilEndTagNames |> List.map (\et -> "|</" ++ Regex.escape et ++ "\\s*>") |> String.join "")
   in
  inContext "Inner HTML" <|
  trackInfo <|
  oneOf [
    try <| (keepUntilRegex regexToParseUntil |> andThen (\s ->
      if s == "" then fail "[internal failure]"
      else succeed (HTMLInner s)
    ))
    , fail "No innerHTML starting here, there is probably a node or the end of the string"
  ]

parseHTMLAttribute: ParsingMode -> Parser HTMLAttribute
parseHTMLAttribute parsingMode =
  trackInfo <|
  delayedCommitMap (\sp attrBuilder -> attrBuilder sp)
    attributeSpaces <|
      let defaultAttributeParser =
         ( succeed (\name st -> \sp -> HTMLAttribute sp name st)
           |= parseHtmlAttributeName
           |= parseHtmlAttributeValue parsingMode)
      in
      case parsingMode of
        Raw -> defaultAttributeParser
        Interpolation {attributelist} ->
          oneOf [
            (succeed (flip HTMLAttributeListExp)
            |. symbol "@"
            |= attributelist),
            defaultAttributeParser]

mergeHTMLAttributeStringRaw: List HTMLAttributeStringElem -> List HTMLAttributeStringElem
mergeHTMLAttributeStringRaw children = case children of
  h1 :: h2 :: tail ->
    case (h1.val, h2.val) of
      (HTMLAttributeStringRaw s1, HTMLAttributeStringRaw s2) ->
        mergeHTMLAttributeStringRaw (withInfo (HTMLAttributeStringRaw (s1 ++ s2)) h1.start h2.end :: tail)
      (_, HTMLAttributeStringRaw s2) -> h1 :: mergeHTMLAttributeStringRaw (h2 :: tail)
      _ -> h1 :: h2 :: mergeHTMLAttributeStringRaw tail
  _ -> children

mergeInners: List HTMLNode -> List HTMLNode
mergeInners children = case children of
  h1 :: h2 :: tail ->
    case (h1.val, h2.val) of
      (HTMLInner s1, HTMLInner s2) -> mergeInners (withInfo (HTMLInner (s1++s2)) h1.start h2.end :: tail)
      (_, HTMLInner s2) -> h1 :: mergeInners (h2 :: tail)
      _ -> h1 :: h2 :: mergeInners tail
  _ -> children

wrapImplicitElems: String -> List HTMLNode -> List HTMLNode
wrapImplicitElems tagName children =
  case tagName of
    "table" -> -- There could be an implicit <tbody> as first child
      let firstChildTag childList = case childList of
            [] -> Nothing
            head :: tail ->
              case head.val of
                HTMLInner s1 -> firstChildTag tail
                HTMLElement (HTMLTagString t) _ _ _ _ _ -> Just t.val
                HTMLElement _ _ _ _ _ _ -> Nothing
                HTMLComment _ -> firstChildTag tail
                HTMLEntity _ _ -> firstChildTag tail
                HTMLListNodeExp _ -> Nothing -- We don't know what's generated
                HTMLDoctype _ _ _ _ _ _ -> Nothing
      in
      case firstChildTag children of
        Just "tr" ->
          [withDummyInfo <| HTMLElement (HTMLTagString (withDummyInfo "tbody")) [] space0 RegularEndOpening children ImplicitElem]
        _ -> children
    _ -> children


parseDoctype: Parser HTMLNode
parseDoctype =
  inContext "HTML Doctype" <| trackInfo <| (
    delayedCommitMap (\doctype withDoctype -> withDoctype doctype) (
      succeed (\d o c t y p e -> d ++ o ++ c ++ t ++ y ++ p ++ e)
      |. symbol "<!"
      |= source (oneOf [symbol "d", symbol "D"])
      |= source (oneOf [symbol "o", symbol "O"])
      |= source (oneOf [symbol "c", symbol "C"])
      |= source (oneOf [symbol "t", symbol "T"])
      |= source (oneOf [symbol "y", symbol "Y"])
      |= source (oneOf [symbol "p", symbol "P"])
      |= source (oneOf [symbol "e", symbol "E"])
    ) (
      succeed (\spName name sp0 mbPub mbSys doctypeName -> HTMLDoctype doctypeName spName name sp0 mbPub mbSys)
      |= spaces
      |= trackInfo (keep (AtLeast 0) (\c -> not (isSpace c) && c /= '>'))
      |= spaces
      |= optional (
           succeed (\p u b l i c sp0 value sp1 -> (p ++ u ++ b ++ l ++ i ++ c, sp0, value, sp1))
           |= source (oneOf [symbol "p", symbol "P"])
           |= source (oneOf [symbol "u", symbol "U"])
           |= source (oneOf [symbol "b", symbol "B"])
           |= source (oneOf [symbol "l", symbol "L"])
           |= source (oneOf [symbol "i", symbol "I"])
           |= source (oneOf [symbol "c", symbol "C"])
           |= spaces
           |= parseDoctypeValue
           |= spaces
           )
      |= optional (succeed (\mbSystemSpace value sp1 -> (mbSystemSpace, value, sp1))
            |= optional (succeed (\s y s2 t e m sp0 -> (s ++ y ++ s2 ++ t ++ e ++ m, sp0))
               |= source (oneOf [symbol "s", symbol "S"])
               |= source (oneOf [symbol "y", symbol "Y"])
               |= source (oneOf [symbol "s", symbol "S"])
               |= source (oneOf [symbol "t", symbol "T"])
               |= source (oneOf [symbol "e", symbol "E"])
               |= source (oneOf [symbol "m", symbol "M"])
               |= spaces)
            |= parseDoctypeValue
            |= spaces
            )
      |. symbol ">"
      ))

parseDoctypeValue: Parser HTMLDoctypeValue
parseDoctypeValue =
  inContext "HTML Doctype value" <| (oneOf [
      source (symbol "\"")
    , source (symbol "\'")
  ] |> andThen (\quoteCharStr ->
    let quoteChar = ImpureGoodies.stringCharAt 0 quoteCharStr |> Maybe.withDefault '\"' in
    parseDoctypeValueContent (\c -> c == quoteChar || c == '>')
    |> andThen (\l ->
      succeed identity
      |. oneOf [source (symbol quoteCharStr), succeed ""] -- Error recovery
      |= succeed (quoteCharStr, l)
    )))

parseDoctypeValueContent: (Char -> Bool) -> Parser String
parseDoctypeValueContent isEndChar =
  keep (AtLeast 0) (\c -> not (isEndChar c))


-- Always succeed if the string starts with <(letter)
parseHTMLElement: ParsingMode -> List String -> NameSpace -> Parser HTMLNode
parseHTMLElement parsingMode surroundingTagNames namespace =
  inContext "HTML Element" <| trackInfo <| (
    nodeElementStart parsingMode
    |> andThen (\(tagNode, tagName) ->
       let newParsingMode = case tagNode of
         HTMLTagString {val} -> if val == "raw" then Raw else parsingMode
         _ -> parsingMode
       in
       succeed (\attrs sp1 (endOpeningStyle, children, closingStyle)  ->
             HTMLElement tagNode attrs sp1 endOpeningStyle (wrapImplicitElems tagName (mergeInners children)) closingStyle)
       |= repeat zeroOrMore (parseHTMLAttribute parsingMode)
       |= attributeSpaces
       |= oneOf ((
            if namespace == HTML && isVoidElement tagName then
              [
                symbol "/>" |> map (\_ -> (RegularEndOpening, [], AutoClosing)),
                symbol ">" |> map (\_ -> (RegularEndOpening, [], VoidClosing))
              ]
            else []) ++
            ( if namespace /= HTML || Set.member tagName possiblyAutoClosingElements then
              -- Foreign elements can have autoclose tags.
              -- Limitation: if we parse '<a/>Hello', because 'a' can be autoclosing in SVG but not HTML,
              -- it would result in two different parse trees. For HTML, it will give <a>Hello</a>. For SVG it will return the same.
              -- Since HTML literals do not have context, if it is possible we allow auto-closing. This is ergonomics.
              [symbol "/>" |> map (\_  -> (RegularEndOpening, [], AutoClosing))]
              else []
            ) ++
            [ succeed (\optSlash children ending ->
                (optSlash |> Maybe.map (\_ -> SlashEndOpening) |> Maybe.withDefault RegularEndOpening,
                 children,
                 ending |> Maybe.withDefault ForgotClosing)
              )
            |= optional (symbol "/")
            |. symbol ">"
            |= repeat zeroOrMore (
                 succeed identity
                 |. negativeLookAhead (childrenIncompatibleWith tagName)
                 |= parseNode newParsingMode (tagName::surroundingTagNames) (if isForeignElement tagName then Foreign else namespace))
            |= optional (try <|
                 succeed (\sp -> RegularClosing sp)
                 |. symbol "</"
                 |. symbol tagName
                 |= spaces
                 |. symbol ">"
               )
          ])
     )
    )

incompatible_p_children =
  oneOf <| List.map symbol ["address", "article", "aside", "blockquote", "details", "div", "dl", "fieldset", "figcaption", "figure", "footer", "form", "h1", "h2", "h3", "h4", "h5", "h6", "header", "hgroup", "hr", "main", "menu", "nav", "ol", "p", "pre", "section", "table", "ul"]

childrenIncompatibleWith: String -> Parser ()
childrenIncompatibleWith tagName =
  case tagName of
    "p" -> succeed ()
           |. symbol "<"
           |. incompatible_p_children
           |. spaces
    _ -> fail "No child incompatible with this"

parseNode: ParsingMode -> List String -> NameSpace -> Parser HTMLNode
parseNode parsingMode surroundingTagNames namespace =
  oneOf <|
    let defaultParsers =
      (if List.head surroundingTagNames == Just "script" || List.head surroundingTagNames == Just "style"  then
         [] else (if List.isEmpty surroundingTagNames || surroundingTagNames == ["raw"] then [parseDoctype] else []) ++
           [ parseHTMLElement parsingMode surroundingTagNames namespace,
           parseHTMLComment,
           parseHTMLEntity False surroundingTagNames |> map (\entity -> case entity.val of
             Err x -> replaceInfo entity <| HTMLInner x
             Ok (rendered, raw) -> replaceInfo entity <| HTMLEntity rendered raw)
         ]) ++ [
      parseHTMLInner parsingMode surroundingTagNames
    ] in
    case parsingMode of
      Raw -> defaultParsers
      Interpolation {childlist} ->
        (trackInfo <| oneOf [
          succeed (\_ -> HTMLInner "@")
          |= symbol "@@"
        , succeed HTMLListNodeExp
          |. symbol "@"
          |= childlist nospace])::defaultParsers



parseOneNode: ParsingMode -> Parser HTMLNode
parseOneNode parsingMode = parseNode parsingMode [] HTML

parseTopLevelNodez : Parser (List HTMLNode)
parseTopLevelNodez =
  repeat zeroOrMore (parseOneNode Raw)

parseHTMLString: String -> Result Parser.Error (List HTMLNode)
parseHTMLString s =
  run parseTopLevelNodez s


unparseAttrValueStringElem: HTMLAttributeStringElem -> String
unparseAttrValueStringElem elem = case elem.val of
  HTMLAttributeStringRaw x -> x
  HTMLAttributeEntity rendered x -> x

interpretAttrValueStringElem: HTMLAttributeStringElem -> String
interpretAttrValueStringElem elem = case elem.val of
  HTMLAttributeStringRaw x -> x
  HTMLAttributeEntity rendered x -> rendered


unparseAttrValueContent: List HTMLAttributeStringElem -> String
unparseAttrValueContent content = content |> List.map unparseAttrValueStringElem |> String.join ""

interpretAttrValueContent: List HTMLAttributeStringElem -> String
interpretAttrValueContent content = content |> List.map interpretAttrValueStringElem |> String.join ""

unparseAttrValue: HTMLAttributeValue -> String
unparseAttrValue value =
  case value.val of
    HTMLAttributeUnquoted ws1 ws2 content -> ws1.val ++ "=" ++ ws2.val ++ unparseAttrValueContent content
    HTMLAttributeString ws1 ws2 delimiter content ->
      ws1.val ++ "=" ++ ws2.val ++ delimiter ++ unparseAttrValueContent content ++ delimiter
    HTMLAttributeNoValue -> ""
    HTMLAttributeExp _ _ _ _ -> "[Internal error] Don't know how to unparse an HTMLAttributeExp here"

unparseAttr: HTMLAttribute -> String
unparseAttr a = case a.val of
  HTMLAttribute ws0 name value -> ws0.val ++ name.val ++ unparseAttrValue value
  HTMLAttributeListExp _ _ -> "[Internal error] Don't know how to unparse an HTMLAttributeListExp here"

unparseEndOp: HTMLEndOpeningStyle -> HTMLClosingStyle -> String
unparseEndOp endOp closing = case endOp of
  RegularEndOpening -> case closing of
    AutoClosing -> "/"
    _ -> ""
  SlashEndOpening -> "/"

unparseClosing: HTMLTag -> HTMLClosingStyle -> String
unparseClosing htmlTag closing =
  case htmlTag of
    HTMLTagString tagName ->
      case closing of
        RegularClosing sp -> if isVoidElement tagName.val then "" else "</" ++ tagName.val ++ sp.val ++ ">"
        VoidClosing -> ""
        AutoClosing -> ""
        ForgotClosing -> ""
        ImplicitElem -> ""
    _ -> "Don't know how to unparse tag " ++ toString htmlTag

unparseCommentStyle: HTMLCommentStyle -> String
unparseCommentStyle style = case style of
   Less_Greater content -> if String.startsWith "?" content && not (String.contains ">" content)
     then "<" ++ content ++ ">"
     else  "<!--" ++ Regex.replace (Regex.All) (Regex.regex "-->") (\_ -> "~~>") content ++ "-->"
   LessSlash_Greater {- The string should start with a space -} content ->
     if String.startsWith " " content &&  not (String.contains ">" content)
     then "</" ++ content ++ ">"
     else  "<!--" ++ Regex.replace (Regex.All) (Regex.regex "-->") (\_ -> "~~>") content ++ "-->"
   LessBang_Greater content ->
     if String.startsWith "--" content && not (String.contains "-->" content)
     then "<!" ++ content ++ ">"
     else "<!--" ++ Regex.replace (Regex.All) (Regex.regex "-->") (\_ -> "~~>") content ++ "-->"
   LessBangDashDash_DashDashGreater content ->
     if not (String.contains "-->" content)
     then "<!--" ++ content ++ "-->"
     else "<!--" ++ Regex.replace (Regex.All) (Regex.regex "-->") (\_ -> "~~>") content ++ "-->"

unparseTagName: HTMLTag -> String
unparseTagName tagName =
  case tagName of
    HTMLTagString s -> s.val
    _ -> "[internal error] Don't know how to unparse " ++ toString tagName ++ " as a tagname"

unparseNode: HTMLNode -> String
unparseNode node = case node.val of
  HTMLInner s -> s
  HTMLEntity entityRendered entity -> entity
  HTMLElement tagName attrs ws1 endOp children closing ->
    let tagNameStr = unparseTagName tagName in
    "<" ++ tagNameStr  ++ String.join "" (List.map unparseAttr attrs) ++ ws1.val ++ (unparseEndOp endOp closing) ++ ">" ++
      String.join "" (List.map unparseNode children) ++ unparseClosing tagName closing
  HTMLComment style -> unparseCommentStyle style
  HTMLListNodeExp _ -> "[Internal error] Don't know how to unparse an HTMLListNodeExp here"
  HTMLDoctype doctype sp0 name sp1 mbPub mbSys ->
    "<!" ++ doctype ++ sp0.val ++ name.val ++ sp1.val ++ (case mbPub of
      Nothing -> ""
      Just (public, sp1, (quote, content), sp2) ->
        public ++ sp1.val ++ quote ++ content ++ quote ++ sp2.val
    ) ++ (case mbSys of
      Nothing -> ""
      Just (mbSystem, (quote, content), sp2) ->
        (case mbSystem of
          Just (system, sp) -> system ++ sp.val
          Nothing -> "") ++ quote ++ content ++ quote ++ sp2.val
     ) ++ ">"

unparseHtmlNodes: List HTMLNode -> String
unparseHtmlNodes nodes =
  List.map unparseNode nodes |> String.join ""


{-
  Methods to unparse an HTML node to a string by propagating back strings diffs.
-}


type alias Unparser = Int -> Maybe VDiffs -> Result String (String, Int, List StringDiffs)

type HTMLUnparserDiff = UnparseArgument Unparser |
                        UnparseSymbol String

contructorVDiffs: VDiffs -> Result String (TupleDiffs VDiffs)
contructorVDiffs vdiffs =
  --Debug.log ("constructorVDiffs " ++ toString vdiffs ++ " = ") <|
  case vdiffs of
  VConstDiffs ->
    Ok []
  VRecordDiffs d ->
    case Dict.get Lang.ctorArgs d of
      Nothing ->
        Err <| Lang.ctorArgs ++ " not found in " ++ toString d
      Just (VRecordDiffs dArgs) ->
        Dict.toList dArgs |> List.map (\(argKey, argValue) ->
          nameToArg argKey |> Result.map (\i -> (i - 1, argValue))
        ) |> Utils.projOk
      _ -> Err <| "Expected a datatype constructor vdiffs (nested VRecordDiffs), got " ++ toString vdiffs
  _ -> Err <| "Expected a datatype constructor vdiffs (nested VRecordDiffs), got " ++ toString vdiffs


unparseConstructor: String -> Int -> TupleDiffs VDiffs -> List HTMLUnparserDiff -> Result String (String, Int, List StringDiffs)
unparseConstructor tagName    offset tDiffs unparsers =
  --let _ = Debug.log ("unparseConstructor " ++ tagName ++ " " ++ toString offset ++ " " ++ toString tDiffs ++ " " ++ toString unparsers) () in
  let aux: Int -> TupleDiffs VDiffs -> List HTMLUnparserDiff -> (String, Int, List StringDiffs) -> Result String (String, Int, List StringDiffs)
      aux i tDiffs unparsers (strAcc, offset, listDiffs) =
    --let _ = Debug.log ("unparseConstructor.aux " ++ toString i ++ " " ++ toString tDiffs ++ " " ++ toString unparsers ++ " " ++ toString (strAcc, offset, listDiffs)) () in
    case tDiffs of
       [] ->
         case unparsers of
           [] -> Ok (strAcc, offset, listDiffs)
           UnparseSymbol s :: tail ->
             (strAcc ++ s, offset + String.length s, listDiffs) |>
             aux i tDiffs tail
           UnparseArgument unparser::tail ->
             case unparser offset Nothing of
               Err msg -> Err msg
               Ok (strArg, newOffset, diffsArgs) ->
                  (strAcc ++ strArg, newOffset, listDiffs ++ diffsArgs) |>
                  aux (i + 1) [] tail
       (j, subd)::diffTail ->
         case unparsers of
           [] -> Err <| "Unexpected end in unparseConstructor " ++ toString offset ++ toString tDiffs
           UnparseSymbol s :: tail ->
              (strAcc ++ s, offset + String.length s, listDiffs) |>
              aux i tDiffs tail
           UnparseArgument unparser::tail ->
             if j >= i then
               case unparser offset <| if j == i then Just subd else Nothing of
                 Err msg -> Err msg
                 Ok (strArg, newOffset, diffsArgs) ->
                   (strAcc ++ strArg, newOffset, listDiffs ++ diffsArgs) |>
                   aux (i + 1) (if j == i then diffTail else tDiffs) tail
             else Err <| "[internal error] HTMLParser j is < than i, we are missing something "
  in aux 0 tDiffs unparsers ("", offset, [])

unparseList: (a -> a -> Unparser) -> (a -> String) -> List a -> List a -> Unparser
unparseList subUnparserDiff defaultUnparser list1 list2 offset mbdiffs =
  --let _ = Debug.log ("unparseList " ++ toString mbdiffs) () in
  -- Things that did not change
  let default: (String, Int, List StringDiffs) -> List a -> List a -> (String, Int, List StringDiffs)
      default (strAcc, offset, listDiffs) list1 list2 =
    List.foldl (\(a1, a2) (str, offset, strDiffs) ->
       let str2 = defaultUnparser a2 in
       (str ++ str2, offset + String.length str2, strDiffs)
       ) (strAcc, offset, listDiffs) (Utils.zip list1 list2)
  in
  case mbdiffs of
    Nothing -> Ok <| default ("", offset, []) list1 list2
    Just (VListDiffs ds) ->
      let aux: Int -> ListDiffs VDiffs -> List a -> List a -> (String, Int, List StringDiffs) -> Result String (String, Int, List StringDiffs)
          aux i ds remaining1 remaining2 (strAcc, offset, listDiffs) =
        --let _ = Debug.log ("unparseList.aux " ++toString i++ " " ++ toString ds ++ " " ++ toString remaining1 ++ " " ++ toString remaining2 ++ " "  ++ " " ++ toString (strAcc, offset, listDiffs)) () in
        case ds of
        [] -> Ok <| default (strAcc, offset, listDiffs) remaining1 remaining2
        (j, listElem)::dsTail ->
           if i < j then
            let count = j - i in
            let (r1c, r1t) = Utils.split count remaining1 in
            let (r2c, r2t) = Utils.split count remaining2 in
            default (strAcc, offset, listDiffs) r1c r2c |>
            aux j ds r1t r2t
           else if i > j then Err "Unexpected mismatch in HTMLParser.unparseList"
           else --if i == j then
            case listElem of
              ListElemInsert count ->
                let (newElems, remaining22) = Utils.split count remaining2 in
                let newStr = List.map defaultUnparser newElems |> String.join "" in
                aux i dsTail remaining1 remaining22 (strAcc ++ newStr, offset, listDiffs ++ [StringUpdate offset offset <| String.length newStr])

              ListElemDelete count ->
                let (oldElems, remaining12) = Utils.split count remaining1 in
                let oldStr = List.map defaultUnparser oldElems |> String.join "" in
                let lengthOldStr = String.length oldStr in
                aux (i + count) dsTail remaining12 remaining2 (strAcc, offset + lengthOldStr, listDiffs ++ [StringUpdate offset (offset + lengthOldStr) 0])

              ListElemUpdate vd ->
                --let _ = Debug.log ("unparseList.ListElemUpdate " ++ toString vd) () in
                case (remaining1, remaining2) of
                  (a1::a1tail, a2::a2tail) ->
                    --let _ = Debug.log ("unparseList.subUnparserDiff ") (a1, a2, offset, (Just vd)) in
                    case subUnparserDiff a1 a2 offset (Just vd) of
                      Err msg -> Err msg
                      Ok (newStr, newOffset, newDiffs)  -> aux (i + 1) dsTail a1tail a2tail (strAcc ++ newStr, newOffset, listDiffs ++ newDiffs)
                  _ -> Err <| "Expected non-empty lists, got " ++ toString (remaining1, remaining2)
      in aux 0 ds list1 list2 ("", offset, [])
    Just ds -> Err <| "Expected VListDiffs, got " ++ toString ds

unparseStr: String -> String -> Unparser
unparseStr  oldStr newStr offset diffs =
  --let _ = Debug.log ("unparseStr " ++ toString oldStr ++ " " ++ toString newStr ++ " " ++ toString offset) in
  case diffs of
    Nothing -> Ok (newStr, offset + String.length oldStr, [])
    Just (VStringDiffs l) -> Ok (newStr, offset + String.length oldStr, offsetStr offset l)
    Just d -> Err <| "Expected VStringDiffs for a string, got " ++ toString d

unparseTagNameDiff: HTMLTag -> HTMLTag -> Unparser
unparseTagNameDiff oldTag newTag offset diffs =
  case (oldTag, newTag) of
    (HTMLTagString oldStr, HTMLTagString newStr) -> unparseStr oldStr.val newStr.val offset diffs
    _ -> Err <| "Don't know how to unparse " ++ toString newTag ++ " from " ++ toString oldTag

-- Here the diffs are on the string content, but we want them on the string as displayed in the editor.
unparseStrContent: String -> String -> String -> Unparser
unparseStrContent quoteChar  content1  content2  offset mbvdiffs =
  --Debug.log ("unparseStrContent " ++ toString quoteChar ++ " " ++ toString content1 ++ " " ++ toString content2 ++ " " ++ toString offset ++ " " ++ toString mbvdiffs) <|
  let unparsedContent1 = ParserUtils.unparseStringContent quoteChar content1 in
  let unparsedContent2 = ParserUtils.unparseStringContent quoteChar content2 in
  case mbvdiffs of
    Nothing -> Ok (unparsedContent2, offset + String.length unparsedContent1, [])
    Just (VStringDiffs stringDiffs) ->
      let find = Regex.find Regex.All (Regex.regex <| "\\\\|" ++ quoteChar ++ "|\r|\n|\t") in
      let content1splitted = find content1 in
      let aux:Int ->       List StringDiffs -> List Regex.Match -> List StringDiffs -> Result String (String, Int, List StringDiffs)
          aux updateOffset stringDiffs matches2 revAcc =
           --Debug.log ("unparseStrContent.aux " ++ toString updateOffset ++ " " ++ toString stringDiffs ++ " " ++ toString matches2 ++ " " ++ toString revAcc) <|
           case stringDiffs of
        [] -> Ok (unparsedContent2, offset + String.length unparsedContent1, List.reverse revAcc)
        StringUpdate start end replaced :: tail ->
           let newReplaced =
            let newSubtr = String.slice (start + updateOffset) (start + replaced + updateOffset) content2 in
            let newSubstrUnparsed = ParserUtils.unparseStringContent quoteChar newSubtr in
            String.length newSubstrUnparsed
           in
           let newUpdateOffset = updateOffset + replaced - (end - start) in
           case matches2 of
            [] ->
              StringUpdate (offset + start + updateOffset) (offset + end + updateOffset) newReplaced :: revAcc |>
              aux newUpdateOffset tail []
            m :: mtail ->
              if m.index >= end then
                StringUpdate (offset + start + updateOffset) (offset + end + updateOffset) newReplaced :: revAcc |>
                aux newUpdateOffset tail matches2
              else if m.index < start then -- The match is already before, it only increments the updateOffset by 1 because of the escaping.
                aux (newUpdateOffset + 1) stringDiffs mtail revAcc
              else
                --if m.index >= start && m.index < end then -- The replaced char is in the middle
                --nd is 1 char longer in the original string, so actually we are more replacing than expected.
                --However, we are adding updateOffset after all, so, in order that the start does not change,
                --we offset it here.
                aux (updateOffset + 1) (StringUpdate start (end + 1) replaced :: tail) mtail revAcc
      in aux 0 stringDiffs content1splitted []
    Just d -> Err <| "Expected VStringDiffs for a string, got " ++ toString d

unparseAttrValueStringElemDiff: HTMLAttributeStringElem -> HTMLAttributeStringElem -> Unparser
unparseAttrValueStringElemDiff oldattrelem newattrelem offset mbd =
  case mbd of
    Nothing ->
      let str = unparseAttrValueStringElem newattrelem in
      Ok (str, offset + String.length str, [])
    Just d ->
      case (oldattrelem.val, newattrelem.val, contructorVDiffs d) of
        (_, _, Err msg) -> Err msg
        (HTMLAttributeStringRaw content, HTMLAttributeStringRaw content2, Ok ds) ->
          unparseConstructor "HTMLAttributeStringRaw" offset ds [
            UnparseArgument <| unparseStr content content2
          ]
        (HTMLAttributeEntity rendered content, HTMLAttributeEntity rendered2 content2, Ok ds) ->
          unparseConstructor "HTMLAttributeEntity" offset ds [
            UnparseArgument <| unparseStr content content2
          ]
        (HTMLAttributeEntity rendered content, HTMLAttributeStringRaw content2, Ok ds) ->
          unparseConstructor "HTMLAttributeStringRaw" offset ds [
            UnparseArgument <| unparseStr content content2
          ]
        (HTMLAttributeStringRaw content, HTMLAttributeEntity rendered2 content2, Ok ds) ->
          unparseConstructor "HTMLAttributeEntity" offset ds [
            UnparseArgument <| unparseStr content content2
          ]

unparseAttrValueDiff: HTMLAttributeValue -> HTMLAttributeValue -> Unparser
unparseAttrValueDiff oldAttrVal newAttrVal offset mbd =
  case mbd of
    Nothing ->
      let str = unparseAttrValue newAttrVal in
      Ok (str, offset + String.length str, [])
    Just d ->
      case (oldAttrVal.val, newAttrVal.val, contructorVDiffs d) of
        (_, _, Err msg) -> Err msg
        (HTMLAttributeUnquoted ws1 ws2 content,  HTMLAttributeUnquoted ws12 ws22 content2, Ok ds) ->
          unparseConstructor "HTMLAttributeUnquoted" offset ds [
            UnparseArgument <| unparseStr ws1.val ws12.val,
            UnparseSymbol "=",
            UnparseArgument <| unparseStr ws2.val ws22.val,
            UnparseArgument <| unparseList unparseAttrValueStringElemDiff unparseAttrValueStringElem content content2
          ]
        (HTMLAttributeString ws1 ws2 delimiter content, HTMLAttributeString ws12 ws22 delimiter2 content2, Ok ds) ->
          unparseConstructor "HTMLAttributeString" offset ds [
            UnparseArgument <| unparseStr ws1.val ws12.val,
            UnparseSymbol "=",
            UnparseArgument <| unparseStr ws2.val ws22.val,
            UnparseArgument <| unparseStr delimiter delimiter2,
            UnparseArgument <| unparseList unparseAttrValueStringElemDiff unparseAttrValueStringElem content content2,
            UnparseSymbol delimiter2
          ]
        (HTMLAttributeNoValue, HTMLAttributeNoValue, Ok ds) ->
          Ok ("", offset, [])
        _ ->
          let oldStr = unparseAttrValue oldAttrVal in
          let newStr = unparseAttrValue newAttrVal in
          Ok (newStr, offset + String.length oldStr, [StringUpdate offset (offset + String.length oldStr) (String.length newStr)])

unparseAttrDiffs: HTMLAttribute -> HTMLAttribute -> Unparser
unparseAttrDiffs oldAttr newAttr offset mbd =
  case mbd of
    Nothing ->
      let str = unparseAttr newAttr in
      Ok (str, offset + String.length str, [])
    Just d ->
      case (oldAttr.val, newAttr.val, contructorVDiffs d) of
      (_, _, Err msg) -> Err msg
      (HTMLAttribute ws0 name value, HTMLAttribute ws02 name2 value2, Ok ds) ->
         unparseConstructor "HTMLAttribute" offset ds [
           UnparseArgument <| unparseStr ws0.val ws02.val,
           UnparseArgument <| unparseStr name.val name2.val,
           UnparseArgument <| unparseAttrValueDiff value value2
         ]
      (_, _, _) -> Err <| "[internal error] Don't know how to unparseAttrDiffs " ++ toString oldAttr.val ++ " " ++ toString newAttr.val

unparseCommentStyleDiffs: HTMLCommentStyle -> HTMLCommentStyle -> Unparser
unparseCommentStyleDiffs oldStyle newStyle offset mbvdiffs =
 case mbvdiffs of
     Nothing ->
       let styleStr = unparseCommentStyle newStyle in
       Ok (styleStr, offset + String.length styleStr, [])
     Just vdiffs ->
        let default () =
          let comment1 = unparseCommentStyle oldStyle in
          let comment2 = unparseCommentStyle newStyle in
          Ok (comment2, offset + String.length comment1, [StringUpdate offset (offset + String.length comment1) (String.length comment2)])
        in
        case (oldStyle, newStyle, contructorVDiffs vdiffs) of
          (_, _, Err msg) -> Err msg
          (Less_Greater oldContent, Less_Greater newContent, Ok ds) ->
            if String.startsWith "?" newContent && not (String.contains ">" newContent)
            then
              unparseConstructor "Less_Greater" offset ds [
                UnparseSymbol "<",
                UnparseArgument <| unparseStr oldContent newContent,
                UnparseSymbol ">"
              ]
            else default ()
          (LessSlash_Greater oldContent, LessSlash_Greater newContent, Ok ds) ->
            if String.startsWith " " newContent &&  not (String.contains ">" newContent)
            then
              unparseConstructor "LessSlash_Greater" offset ds [
                UnparseSymbol "</",
                UnparseArgument <| unparseStr oldContent newContent,
                UnparseSymbol ">"
              ]
            else default ()
          (LessBang_Greater oldContent, LessBang_Greater newContent, Ok ds) ->
            if String.startsWith "--" newContent && not (String.contains "-->" newContent)
            then unparseConstructor "LessBang_Greater" offset ds [
                UnparseSymbol "<!",
                UnparseArgument <| unparseStr oldContent newContent,
                UnparseSymbol ">"
              ]
            else default ()
          (LessBangDashDash_DashDashGreater oldContent,LessBangDashDash_DashDashGreater newContent, Ok ds)  ->
            if not (String.contains "-->" newContent)
            then unparseConstructor "LessBangDashDash_DashDashGreater" offset ds [
                UnparseSymbol "<!--",
                UnparseArgument <| unparseStr oldContent newContent,
                UnparseSymbol ">"
              ]
            else default ()
          _ -> default ()

unparseNodeDiffs: HTMLNode -> HTMLNode -> Unparser
unparseNodeDiffs oldNode newNode offset mbvdiffs =
  --let _ = Debug.log ("unparseNodeDiffs " ++ toString oldNode ++ " " ++ toString newNode ++ " " ++ toString offset ++ " " ++ toString mbvdiffs) () in
  case mbvdiffs of
    Nothing ->
      let nodeStr = unparseNode newNode in
      Ok (nodeStr, offset + String.length nodeStr, [])
    Just vdiffs ->
      case (oldNode.val, newNode.val, contructorVDiffs vdiffs) of
      (_, _, Err msg) -> Err msg
      (HTMLInner s1, HTMLInner s2, Ok ds) ->
        unparseConstructor "HTMLInner" offset ds [
           UnparseArgument <| unparseStr s1 s2
          ]

      (HTMLElement tagName attrs ws1 endOp children closing, HTMLElement tagName2 attrs2 ws12 endOp2 children2 closing2, Ok ds) ->
        unparseConstructor "HTMLElement" offset ds [
          UnparseSymbol "<",
          UnparseArgument <| unparseTagNameDiff tagName tagName2,
          UnparseArgument <| unparseList unparseAttrDiffs unparseAttr attrs attrs2,
          UnparseArgument <| unparseStr ws1.val ws12.val,
          UnparseArgument <| \offset mbdiffs ->
             -- Let's completely ignore changes made to the opening style.
             let endOpFinal1 = unparseEndOp endOp closing in
             let endOpFinal2 = unparseEndOp endOp2 closing2 in
             let diff = mbdiffs |> Maybe.map (\d ->
                [StringUpdate offset (offset + String.length endOpFinal1) (String.length endOpFinal2)]) |>
                Maybe.withDefault [] in
             Ok (endOpFinal2, offset + String.length endOpFinal1, diff),
          UnparseSymbol ">",
          UnparseArgument <| unparseList unparseNodeDiffs unparseNode children children2,
          UnparseArgument <| \offset mbdiffs ->
            let closingFinal1 = unparseClosing tagName closing in
            let closingFinal2 = unparseClosing tagName2 closing2 in
            let diff = mbdiffs |> Maybe.map (\d ->
              [StringUpdate offset (offset + String.length closingFinal1) (String.length closingFinal2)]) |>
              Maybe.withDefault [] in
            Ok (closingFinal2, offset + String.length closingFinal1, diff)
        ]
      (HTMLComment style1, HTMLComment style2, Ok ds) ->
        unparseConstructor "HTMLComment" offset ds [
          UnparseArgument <| unparseCommentStyleDiffs style1 style2
        ]
      _ ->
        let oldNodeStr = unparseNode oldNode in
        let newNodeStr = unparseNode newNode in
        let lengthOldode = String.length oldNodeStr in
        Ok (newNodeStr, offset + lengthOldode, [StringUpdate offset (offset + lengthOldode) (String.length newNodeStr)])

-- Top-level function
unparseHtmlNodesDiffs: Maybe VDiffs-> List HTMLNode -> List HTMLNode -> Result String (String, List StringDiffs)
unparseHtmlNodesDiffs diffs oldNodes newNodes =
  unparseList unparseNodeDiffs unparseNode oldNodes newNodes 0 diffs |> Result.map (\(s, _, l) -> (s, l))
  --|>  Debug.log ("unparseHtmlNodesDiffs " ++toString diffs ++ " " ++ toString oldNodes ++ " " ++ toString newNodes)


printAttrValueRaw prettyPrint v =  Utils.delimit "\"" "\"" (Regex.replace Regex.All (Regex.regex "\\\\|\"|'|\n|\r|\t|&") (\m ->
  case m.match of
    "\\" -> if prettyPrint then "\\\\" else "\\"
    "'" -> if prettyPrint then "'" else "&#39;"
    "\"" -> if prettyPrint then "\\\"" else "&quot;"
    "\n" -> if prettyPrint then "\\n" else "&#10;"
    "\r" -> if prettyPrint then "\\r" else "&#13;"
    "\t" -> if prettyPrint then "\\t" else "\t"
    "&" -> if prettyPrint then "&" else "&amp;"
    e -> e
  ) v)
module HTMLParser exposing (parseHTMLString,
  HTMLNode(..), HTMLEndOpeningStyle(..), HTMLClosingStyle(..), HTMLCommentStyle(..), HTMLAttribute(..), HTMLAttributeValue(..),
  unparseHtmlNodes,
  unparseNode)

import Char
import Set exposing (Set)
import Dict
import Pos
import Parser as P exposing (..)
import Parser.LanguageKit as LK
import Lang exposing (..)
import Info exposing (..)
import ElmLang
import Syntax

import ParserUtils exposing (..)
import LangParserUtils exposing (..)
import BinaryOperatorParser exposing (..)
import Utils
import Regex
import Parser

type NameSpace  = HTML | Foreign

type HTMLAttributeValue = HTMLAttributeUnquoted WS WS String | HTMLAttributeString WS WS String {-Delimiter char-}  String | HTMLAttributeNoValue
type HTMLAttribute = HTMLAttribute WS String HTMLAttributeValue
type HTMLCommentStyle = Less_Greater String {- The string should start with a ? -}
                      | LessSlash_Greater {- The string should start with a space -} String
                      | LessBang_Greater String
                      | LessBangDashDash_DashDashGreater String

type HTMLClosingStyle = RegularClosing WS | VoidClosing | AutoClosing | ForgotClosing
type HTMLEndOpeningStyle = RegularEndOpening {- usually > -} | SlashEndOpening {- add a slash before the '>' of the opening, does not mark the element as ended in non-void HTML elements -}
-- HTMLInner may have unmatched closing tags inside it. You have to remove them to create a real innerHTML
-- HTMLInner may have unescaped chars (e.g. <, >, & etc.)
type HTMLNode = HTMLInner String
              | HTMLElement String (List HTMLAttribute) WS HTMLEndOpeningStyle (List HTMLNode) HTMLClosingStyle
              | HTMLComment HTMLCommentStyle

voidElements: Set String
voidElements =
  Set.fromList ["area", "base", "br", "col", "embed", "hr", "img", "input",
                "link", "meta", "param", "source", "track", "wbr"]

isVoidElement: String -> Bool
isVoidElement s =
  Set.member (String.toLower s) voidElements

isForeignElement: String -> Bool
isForeignElement s = let s2 = String.toLower s in s == "math" || s == "svg"

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
  delayedCommitMap (\_ -> HTMLComment)
    (symbol "<")
    (oneOf
    [ succeed Less_Greater
      |. lookAhead (symbol "?")
      |= keep oneOrMore (\c -> c /= '>')
      |. symbol ">"
    , succeed identity
      |. lookAhead (symbol "!")
      |= oneOf [
          succeed LessBangDashDash_DashDashGreater
          |. lookAhead (symbol "--")
          |= keepUntilRegex (Regex.regex "-->")
          |. symbol "-->"
          , succeed LessBang_Greater
          |= keep oneOrMore (\c -> c /= '>')
          |. symbol ">"
        ]
    , succeed LessSlash_Greater
      |. lookAhead (symbol "/ ") -- Requires a space.
      |. symbol "."
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

parseHtmlAttributeName: Parser String
parseHtmlAttributeName = keep oneOrMore (\c -> c /= '>' && c /= '/' && not (isSpace c))

-- parse("<div> i<j =b / =hello=abc /a/ div / /> d") == "<div> i<j =b="" =hello="abc" a="" div=""> d</j></div>"
parseHtmlAttributeValue: Parser HTMLAttributeValue
parseHtmlAttributeValue =
  oneOf [
    delayedCommitMap (\sp1 (sp2, builder) -> builder sp1 sp2)
      spaces
      (  succeed (\sp2 builder -> (sp2, builder))
         |. (symbol "=")
         |= spaces
         |= oneOf [
           singleLineString |> map (\(quoteChar, content) sp1 sp2 -> HTMLAttributeString sp1 sp2 quoteChar content),
           ignore oneOrMore (\c -> not (isSpace c) && c /= '>') |> source |> map (\content sp1 sp2 -> HTMLAttributeUnquoted sp1 sp2 content)
         ]
      ),
    succeed HTMLAttributeNoValue
  ]

letterRegex = Regex.regex "^[a-zA-Z]$"

isLetter: Char -> Bool
isLetter c = Regex.contains letterRegex (String.fromChar c)

-- parse(""<div/> d") == "<div> d</div>"
-- parse(""<br/> d") == "<br> d"
-- parse(""<img/> d") == "<img> d"

nodeElementStart: Parser String
nodeElementStart =
  delayedCommit
    (symbol "<")
    (succeed (\a b -> a ++ b)
     |= keep (Exactly 1) isLetter)
     |= keep zeroOrMore (\c -> not (isSpace c) && c /= '/' && c /= '>')

nodeStart: Parser String
nodeStart =
  delayedCommit
    (symbol "<")
    (keep oneOrMore (\c -> not (isSpace c) && c /= '/' && c /= '>' && c /= '!' && c /= '?'))
{--
parsed("a</j  >b") == "ab"
parsed("<j>a<:b></j>b") == "<j>a&lt;:b&gt;</j>b"
parsed("<j>a</b></j>b") == "<j>a</j>b"
--}
-- Always succeed if not empty
-- parse the content until a node starts (either a comment, a node start element, or the end tag of this node)
parseHTMLInner: Maybe String -> Parser HTMLNode
parseHTMLInner untilEndTagName =
  let regexToParseUntil = Regex.regex <| "<[a-zA-Z]|<?|<!|</ " ++
    (untilEndTagName |> Maybe.map (\et -> "|</" ++ Regex.escape et ++ "\\s*>") |> Maybe.withDefault "") in
  oneOf [
    try <| (keepUntilRegex regexToParseUntil |> andThen (\s ->
      if s == "" then fail "[internal failure]"
      else succeed (HTMLInner s)
    ))
    , fail "No innerHTML starting here, there is probably a node or the end of the string"
  ]

parseHTMLAttribute: Parser HTMLAttribute
parseHTMLAttribute =
  delayedCommitMap (\sp (name, st) -> HTMLAttribute sp name st)
    attributeSpaces
    ( succeed (\name st -> (name, st))
      |= parseHtmlAttributeName
      |= parseHtmlAttributeValue)


-- Always succeed if the string starts with <(letter)
parseHTMLElement: NameSpace -> Parser HTMLNode
parseHTMLElement namespace =
  inContext "HTML Element" <| (
    nodeElementStart
    |> andThen (\tagName ->
       succeed (\attrs sp1 (endOpeningStyle, children, closingStyle)  ->
             HTMLElement tagName attrs sp1 endOpeningStyle children closingStyle)
       |= repeat zeroOrMore parseHTMLAttribute
       |= attributeSpaces
       |= oneOf ((
            if namespace == HTML && isVoidElement tagName then
              [
                symbol "/>" |> map (\_ -> (RegularEndOpening, [], AutoClosing)),
                symbol ">" |> map (\_ -> (RegularEndOpening, [], VoidClosing))
              ]
            else []) ++
            ( if namespace /= HTML then -- Foreign elements can have autoclose tags.
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
            |= repeat zeroOrMore (parseNode (Just tagName) (if isForeignElement tagName then Foreign else namespace))
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


parseNode: Maybe String -> NameSpace -> Parser HTMLNode
parseNode surroundingTagName namespace =
  inContext "HTML Node" <|
    oneOf [
      parseHTMLElement namespace,
      parseHTMLComment,
      parseHTMLInner surroundingTagName
    ]

parseTopLevelNode : Parser (List HTMLNode)
parseTopLevelNode =
  repeat zeroOrMore (parseNode Nothing HTML)

parseHTMLString: String -> Result Parser.Error (List HTMLNode)
parseHTMLString s =
  run parseTopLevelNode s

unparseAttr: HTMLAttribute -> String
unparseAttr a = case a of
  HTMLAttribute ws0 name value -> ws0.val ++ name ++ (case value of
     HTMLAttributeUnquoted ws1 ws2 content -> ws1.val ++ "=" ++ ws2.val ++ content
     HTMLAttributeString ws1 ws2 delimiter content -> ws1.val ++ "=" ++ ws2.val ++ Syntax.unparser Syntax.Elm (withDummyExpInfo <| EBase space0 <| EString delimiter content)
     HTMLAttributeNoValue -> ""
  )

unparseNode: HTMLNode -> String
unparseNode node = case node of
  HTMLInner s -> s
  HTMLElement tagName attrs ws1 endOp children closing ->
    "<" ++ tagName ++ String.join "" (List.map unparseAttr attrs) ++ ws1.val ++ (case endOp of
      RegularEndOpening -> case closing of
        AutoClosing -> "/"
        _ -> ""
      SlashEndOpening -> "/"
    ) ++ ">" ++ String.join "" (List.map unparseNode children) ++ (case closing of
      RegularClosing sp -> "</" ++ tagName ++ sp.val ++ ">"
      VoidClosing -> ""
      AutoClosing -> ""
      ForgotClosing -> ""
    )
  HTMLComment style -> case style of
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

unparseHtmlNodes: List HTMLNode -> String
unparseHtmlNodes nodes =
  List.map unparseNode nodes |> String.join ""

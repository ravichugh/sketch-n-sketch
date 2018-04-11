module HTMLParser exposing (parseHTMLString,
  HTMLNode(..), HTMLEndOpeningStyle(..), HTMLClosingStyle(..), HTMLCommentStyle(..), HTMLAttribute(..), HTMLAttributeValue(..),
  isVoidElement, isForeignElement,
  NameSpace(..),
  unparseHtmlNodes,
  unparseNode,
  unparseHtmlNodesDiffs)

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
import Parser.LanguageKit as LanguageKit
import ElmUnparser
import UpdateUtils

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

-- parse("<i ====>Hello</i>") == "<i =="==">Hell</i>" (= is a valid start for an attribute name.
parseHtmlAttributeName: Parser String
parseHtmlAttributeName =
  LanguageKit.variable (\c -> c /= '>' && c /= '/' && not (isSpace c)) (\c -> c /= '>' && c /= '/' && c /= '=' && not (isSpace c)) (Set.fromList [])

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
parseHTMLInner: List String -> Parser HTMLNode
parseHTMLInner untilEndTagNames =
  let regexToParseUntil = Regex.regex <| "<[a-zA-Z]|<\\?|<!|</ |$" ++
    (untilEndTagNames |> List.map (\et -> "|</" ++ Regex.escape et ++ "\\s*>") |> String.join "") in
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
parseHTMLElement: List String -> NameSpace -> Parser HTMLNode
parseHTMLElement surroundingTagNames namespace =
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
            |= repeat zeroOrMore (parseNode (tagName::surroundingTagNames) (if isForeignElement tagName then Foreign else namespace))
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


parseNode: List String -> NameSpace -> Parser HTMLNode
parseNode surroundingTagNames namespace =
  inContext "HTML Node" <|
    oneOf [
      parseHTMLElement surroundingTagNames namespace,
      parseHTMLComment,
      parseHTMLInner surroundingTagNames
    ]

parseTopLevelNode : Parser (List HTMLNode)
parseTopLevelNode =
  repeat zeroOrMore (parseNode [] HTML)

parseHTMLString: String -> Result Parser.Error (List HTMLNode)
parseHTMLString s =
  run parseTopLevelNode s

unparseAttrValue: HTMLAttributeValue -> String
unparseAttrValue value =
  case value of
    HTMLAttributeUnquoted ws1 ws2 content -> ws1.val ++ "=" ++ ws2.val ++ content
    HTMLAttributeString ws1 ws2 delimiter content -> ws1.val ++ "=" ++ ws2.val ++ Syntax.unparser Syntax.Elm (withDummyExpInfo <| EBase space0 <| EString delimiter content)
    HTMLAttributeNoValue -> ""


unparseAttr: HTMLAttribute -> String
unparseAttr a = case a of
  HTMLAttribute ws0 name value -> ws0.val ++ name ++ unparseAttrValue value

unparseEndOp: HTMLEndOpeningStyle -> HTMLClosingStyle -> String
unparseEndOp endOp closing = case endOp of
  RegularEndOpening -> case closing of
    AutoClosing -> "/"
    _ -> ""
  SlashEndOpening -> "/"

unparseClosing: String -> HTMLClosingStyle -> String
unparseClosing tagName closing = case closing of
  RegularClosing sp -> if isVoidElement tagName then "" else "</" ++ tagName ++ sp.val ++ ">"
  VoidClosing -> ""
  AutoClosing -> ""
  ForgotClosing -> ""

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

unparseNode: HTMLNode -> String
unparseNode node = case node of
  HTMLInner s -> s
  HTMLElement tagName attrs ws1 endOp children closing ->
    "<" ++ tagName ++ String.join "" (List.map unparseAttr attrs) ++ ws1.val ++ (unparseEndOp endOp closing) ++ ">" ++
      String.join "" (List.map unparseNode children) ++ unparseClosing tagName closing
  HTMLComment style -> unparseCommentStyle style

unparseHtmlNodes: List HTMLNode -> String
unparseHtmlNodes nodes =
  List.map unparseNode nodes |> String.join ""

type alias Unparser = Int -> Maybe VDiffs -> Result String (String, Int, List StringDiffs)

type HTMLUnparserDiff = UnparseArgument Unparser |
                        UnparseSymbol String

contructorVDiffs: VDiffs -> Maybe (TupleDiffs VDiffs)
contructorVDiffs vdiffs = case vdiffs of
  VRecordDiffs d ->
    case Dict.get Lang.ctorArgs d of
      Nothing -> Just []
      Just (VRecordDiffs dArgs) ->
        List.range 1 (Dict.size dArgs) |> List.filterMap (\i ->
          Dict.get (argName i) dArgs |> Maybe.map (\d -> (i, d))
        ) |> Just
      _ -> Nothing
  _ -> Nothing

unparseConstructor: Int -> TupleDiffs VDiffs -> List HTMLUnparserDiff -> Result String (String, Int, List StringDiffs)
unparseConstructor offset tDiffs unparsers =
  let aux i offset tDiffs unparsers =
    case tDiffs of
       [] ->
         case unparsers of
           [] -> Ok ("", offset, [])
           UnparseSymbol s :: tail -> aux i (offset + String.length s) tDiffs tail
           UnparseArgument unparser::tail -> unparser offset Nothing |> Result.andThen (\(s, newOffset, l) ->
             aux (i + 1) newOffset [] tail |> Result.map (\(sTail, finalOffset, lTail) ->
               (s ++ sTail, finalOffset, l ++ lTail)
             )
             )
       (j, subd)::diffTail ->
         case unparsers of
           [] -> Err <| "Unexpected end in unparseConstructor " ++ toString offset ++ toString tDiffs
           UnparseSymbol s :: tail -> aux i (offset + String.length s) tDiffs tail
           UnparseArgument unparser::tail ->
             if j > i then
                 unparser offset Nothing |> Result.andThen (\(s, newOffset, l) ->
                   aux (i + 1) newOffset tDiffs tail |> Result.map (\(sTail, finalOffset, lTail) ->
                     (s ++ sTail, finalOffset, l ++ lTail)
                   )
                   )
             else if j == i then
               unparser offset (Just subd) |> Result.andThen (\(s, newOffset, l) ->
                   aux (i + 1) newOffset diffTail tail |> Result.map (\(sTail, finalOffset, lTail) ->
                     (s ++ sTail, finalOffset, l ++ lTail)
                   )
                   )
             else Err <| "[internal error] HTMLParser j is < than i, we are missing something "
  in aux 1 offset tDiffs unparsers

unparseList: (a -> a -> Unparser) -> (a -> String) -> List a -> List a -> Unparser
unparseList subUnparserDiff defaultUnparser list1 list2 offset mbdiffs =
  let default (strAcc, offset, listDiffs) list1 list2 =
    List.foldl (\(a1, a2) unparserRes ->
       case unparserRes of
         Err msg -> Err msg
         Ok (strAcc, offset, strDiffs) ->
           subUnparserDiff a1 a2 offset Nothing |> Result.map (
             \(newStr, newOffset, newDiffs) -> (strAcc ++ newStr, newOffset, strDiffs ++ newDiffs))
       ) (Ok (strAcc, offset, listDiffs)) (Utils.zip list1 list2)
  in
  case mbdiffs of
    Nothing -> default ("", offset, []) list1 list2
    Just (VListDiffs ds) ->
      let aux i ds remaining1 remaining2 (strAcc, offset, listDiffs)= case ds of
        [] -> default (strAcc, offset, listDiffs) remaining1 remaining2
        (j, listElem)::dsTail ->
          if i < j then
            let count = j - i in
            let (r1c, r1t) = Utils.split count remaining1 in
            let (r2c, r2t) = Utils.split count remaining2 in
            case default (strAcc, offset, listDiffs) r1c r2c of
              Err msg -> Err msg
              Ok x -> aux j ds r1t r2t x
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
                case (remaining1, remaining2) of
                  (a1::a1tail, a2::a2tail) ->
                    case subUnparserDiff a1 a2 offset (Just vd) of
                      Err msg -> Err msg
                      Ok (newStr, newOffset, newDiffs)  -> aux (i + 1) dsTail a1tail a2tail (strAcc ++ newStr, newOffset, listDiffs ++ newDiffs)
                  _ -> Err <| "Expected non-empty lists, got " ++ toString (remaining1, remaining2)
      in aux 0 ds list1 list2 ("", offset, [])
    Just ds -> Err <| "Expected VListDiffs, got " ++ toString ds

unparseStr: String -> String -> Unparser
unparseStr  oldStr newStr offset diffs =
  case diffs of
    Nothing -> Ok (newStr, offset + String.length oldStr, [])
    Just (VStringDiffs l) -> Ok (newStr, offset + String.length oldStr, UpdateUtils.offsetStr offset l)
    Just d -> Err <| "Expected VStringDiffs for a string, got " ++ toString d

-- Here the diffs are on the string content, but we want them on the string as displayed in the editor.
unparseStrContent: String -> String -> String -> Unparser
unparseStrContent quoteChar  content1  content2  offset mbvdiffs =
  let unparsedContent1 = ElmUnparser.unparseStringContent quoteChar content1 in
  let unparsedContent2 = ElmUnparser.unparseStringContent quoteChar content2 in
  case mbvdiffs of
    Nothing -> Ok (unparsedContent2, offset + String.length unparsedContent1, [])
    Just (VStringDiffs stringDiffs) ->
      let find = Regex.find Regex.All (Regex.regex <| "\\\\|" ++ quoteChar ++ "|\r|\n|\t") in
      let content1splitted = find content1 in
      let aux:Int ->       Int ->             List StringDiffs -> List Regex.Match -> List StringDiffs -> Result String (String, Int, List StringDiffs)
          aux updateOffset currentStringDiffOffset stringDiffs matches2 revAcc = case stringDiffs of
        [] -> Ok (unparsedContent2, offset + String.length unparsedContent1, List.reverse revAcc)
        StringUpdate start end replaced :: tail ->
          let newReplaced =
            let newSubtr = String.slice (start + updateOffset) (start + replaced + updateOffset) content2 in
            let newSubstrUnparsed = ElmUnparser.unparseStringContent quoteChar newSubtr in
            String.length newSubstrUnparsed
          in
          case matches2 of
            [] -> StringUpdate (offset + start + updateOffset) (offset + end + updateOffset) newReplaced :: revAcc |>
              aux updateOffset 0 tail []
            m :: mtail ->
              if m.index >= end then
                StringUpdate (offset + start + updateOffset) (offset + end + updateOffset) newReplaced :: revAcc |>
                aux updateOffset 0 tail matches2
              else if m.index < start then -- The match is already before, it only increments the updateOffset by 1 because of the escaping.
                aux (updateOffset + 1) currentStringDiffOffset stringDiffs mtail revAcc
              else
                --if m.index >= start && m.index < end then -- The replaced char is in the middle
                --nd is 1 char longer in the original string, so actually we are more replacing than expected.
                --However, we are adding updateOffset after all, so, in order that the start does not change,
                --we offset it here.
                aux (updateOffset + 1) (currentStringDiffOffset - 1) (StringUpdate start (end + 1) replaced :: tail) mtail revAcc
      in aux 0 0 stringDiffs content1splitted []
    Just d -> Err <| "Expected VStringDiffs for a string, got " ++ toString d

unparseAttrValueDiff: HTMLAttributeValue -> HTMLAttributeValue -> Unparser
unparseAttrValueDiff oldAttrVal newAttrVal offset mbd =
  case mbd of
    Nothing ->
      let str = unparseAttrValue newAttrVal in
      Ok (str, offset + String.length str, [])
    Just d ->
      case (oldAttrVal, newAttrVal, contructorVDiffs d) of
        (HTMLAttributeUnquoted ws1 ws2 content,  HTMLAttributeUnquoted ws12 ws22 content2, Just ds) ->
          unparseConstructor offset ds [
            UnparseArgument <| unparseStr ws1.val ws12.val,
            UnparseSymbol "=",
            UnparseArgument <| unparseStr ws2.val ws22.val,
            UnparseArgument <| unparseStr content content2
          ]
        (HTMLAttributeString ws1 ws2 delimiter content, HTMLAttributeString ws12 ws22 delimiter2 content2, Just ds) ->
          unparseConstructor offset ds [
            UnparseArgument <| unparseStr ws1.val ws12.val,
            UnparseSymbol "=",
            UnparseArgument <| unparseStr ws2.val ws22.val,
            UnparseSymbol delimiter,
            UnparseArgument <| unparseStrContent delimiter content content2,
            UnparseSymbol delimiter
          ]
        (HTMLAttributeNoValue, HTMLAttributeNoValue, Just ds) ->
          Ok ("", offset, [])
        (a, b, _) ->
          let oldStr = unparseAttrValue a in
          let newStr = unparseAttrValue b in
          Ok (newStr, offset + String.length oldStr, [StringUpdate offset (offset + String.length oldStr) (String.length newStr)])

unparseAttrDiffs: HTMLAttribute -> HTMLAttribute -> Unparser
unparseAttrDiffs oldAttr newAttr offset mbd =
  case mbd of
    Nothing ->
      let str = unparseAttr newAttr in
      Ok (str, offset + String.length str, [])
    Just d ->
      case (newAttr, oldAttr, contructorVDiffs d) of
      (HTMLAttribute ws0 name value, HTMLAttribute ws02 name2 value2, Just ds) ->
         unparseConstructor offset ds [
           UnparseArgument <| unparseStr ws0.val ws02.val,
           UnparseArgument <| unparseStr name name2,
           UnparseArgument <| unparseAttrValueDiff value value2
         ]
      _ ->
         let oldStr = unparseAttr oldAttr in
         let newStr = unparseAttr newAttr in
         Ok (newStr, offset + String.length oldStr, [StringUpdate offset (offset + String.length oldStr) (String.length newStr)])

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
          (Less_Greater content, Less_Greater content2, Just ds) ->
            if String.startsWith "?" content && not (String.contains ">" content)
            then
              unparseConstructor offset ds [
                UnparseSymbol "<",
                UnparseArgument <| unparseStr content content2,
                UnparseSymbol ">"
              ]
            else default ()
          (LessSlash_Greater content, LessSlash_Greater content2, Just ds) ->
            if String.startsWith " " content &&  not (String.contains ">" content)
            then
              unparseConstructor offset ds [
                UnparseSymbol "</",
                UnparseArgument <| unparseStr content content2,
                UnparseSymbol ">"
              ]
            else default ()
          (LessBang_Greater content, LessBang_Greater content2, Just ds) ->
            if String.startsWith "--" content && not (String.contains "-->" content)
            then unparseConstructor offset ds [
                UnparseSymbol "<!",
                UnparseArgument <| unparseStr content content2,
                UnparseSymbol ">"
              ]
            else default ()
          (LessBangDashDash_DashDashGreater content,LessBangDashDash_DashDashGreater content2, Just ds)  ->
            if not (String.contains "-->" content)
            then unparseConstructor offset ds [
                UnparseSymbol "<!--",
                UnparseArgument <| unparseStr content content2,
                UnparseSymbol ">"
              ]
            else default ()
          _ -> default ()

unparseNodeDiffs: HTMLNode -> HTMLNode -> Unparser
unparseNodeDiffs oldNode newNode offset mbvdiffs =
  Debug.log ("unparseNodeDiffs " ++ toString oldNode ++ " " ++ toString newNode ++ " " ++ toString offset ++ " " ++ toString mbvdiffs) <|
  case mbvdiffs of
    Nothing ->
      let nodeStr = unparseNode newNode in
      Ok (nodeStr, offset + String.length nodeStr, [])
    Just vdiffs ->
      case (oldNode, newNode, contructorVDiffs vdiffs) of
      (HTMLInner s1, HTMLInner s2, Just ds) ->
        unparseConstructor offset ds [
           UnparseArgument <| unparseStr s1 s2
          ]

      (HTMLElement tagName attrs ws1 endOp children closing, HTMLElement tagName2 attrs2 ws12 endOp2 children2 closing2, Just ds) ->
        unparseConstructor offset ds [
          UnparseSymbol "<",
          UnparseArgument <| unparseStr tagName tagName2,
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
      (HTMLComment style1, HTMLComment style2, Just ds) ->
        unparseConstructor offset ds [
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

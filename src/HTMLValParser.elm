module HTMLValParser exposing (htmlValParser, htmlNodeToElmViewInLeo)

import HTMLParser exposing (..)
import Lang exposing (..)
import ParserUtils exposing (..)
import LangUtils exposing (valToString)
import UpdateUtils
import Utils
import Results exposing (Results(..))
import ValUnbuilder as Vu
import ValBuilder as Vb
import Regex

htmlValParser: Val
htmlValParser = builtinVal "(Native)HTMLValParser.htmlValParser" <| VFun "parseHTML" ["html"] (\args ->
  case args of
    [v] ->
      case v.v_ of
      VBase (VString s) ->
        case parseHTMLString s of
          Err pe -> Err (showError pe)
          Ok nodes -> Ok (Vb.list htmlNodeToVal (Vb.fromVal v) nodes, [])
      _ -> Err <| "parseHTML expects a string, got " ++ valToString v
    _ -> Err ("parseHTML expects exactly one argument, got " ++ toString (List.length args))
  ) <| Just <| \oldArgs oldOut newOut vdiffs -> -- Update means just parsing the old
    --let _ = Debug.log (valToString newOut) "<-- newOut in htmlValParser" in
     let newOutHtml = newOut |> Vu.list valToHtmlNode in
     let oldOutHtml = oldOut |> Vu.list valToHtmlNode in
     Result.map2 (unparseHtmlNodesDiffs <| Just vdiffs) oldOutHtml newOutHtml
     |> Result.andThen identity
     |> Result.map (\(s, sdiffs) ->
       let finalSDiffs = case sdiffs of
         [] -> []
         _ -> [(0, VStringDiffs sdiffs)]
       in
       ([replaceV_ newOut <| VBase (VString s)], finalSDiffs))
     |> Results.fromResult

htmlNodeToVal: Vb.Vb -> HTMLNode -> Val
htmlNodeToVal vb n = case n of
  HTMLInner s -> Vb.constructor vb "HTMLInner" [Vb.string vb s]
  HTMLElement tagName attrs ws1 endOp children closing ->
    Vb.constructor vb "HTMLElement" [
      Vb.string vb tagName,
      Vb.list (\vb a -> case a of
        HTMLAttribute ws0 name value -> Vb.constructor vb "HTMLAttribute" [Vb.string vb ws0.val, Vb.string vb name, case value of
          HTMLAttributeUnquoted ws1 ws2 content -> Vb.constructor vb "HTMLAttributeUnquoted" [Vb.string vb ws1.val, Vb.string vb ws2.val, Vb.string vb content]
          HTMLAttributeString ws1 ws2 delimiter content -> Vb.constructor vb "HTMLAttributeString" [Vb.string vb ws1.val, Vb.string vb ws2.val, Vb.string vb delimiter, Vb.string vb content]
          HTMLAttributeNoValue -> Vb.constructor vb "HTMLAttributeNoValue" []
      ]) vb attrs,
      Vb.string vb ws1.val,
      case endOp of
        RegularEndOpening -> Vb.constructor vb "RegularEndOpening" []
        SlashEndOpening -> Vb.constructor vb "SlashEndOpening" []
      ,
      Vb.list htmlNodeToVal vb children
      ,
      case closing of
        RegularClosing wsc -> Vb.constructor vb "RegularClosing" [Vb.string vb wsc.val]
        VoidClosing ->Vb.constructor vb "VoidClosing" []
        AutoClosing -> Vb.constructor vb "AutoClosing" []
        ForgotClosing ->Vb.constructor vb "ForgotClosing" []
    ]
  HTMLComment style -> Vb.constructor vb "HTMLComment" [case style of
      Less_Greater content -> Vb.constructor vb "Less_Greater" [Vb.string vb content]
      LessSlash_Greater {- The string should start with a space -} content -> Vb.constructor vb "LessSlash_Greater" [Vb.string vb content]
      LessBang_Greater content -> Vb.constructor vb "LessBang_Greater" [Vb.string vb content]
      LessBangDashDash_DashDashGreater content -> Vb.constructor vb "LessBangDashDash_DashDashGreater" [Vb.string vb content]
    ]

valToHtmlNode: Val -> Result String HTMLNode
valToHtmlNode v =
  case Vu.constructor Ok v of
  Ok ("HTMLInner", [i]) -> Vu.string i |> Result.map HTMLInner
  Ok ("HTMLElement", [tagNameV, attrsV, ws1V, endOpV, childrenV, closingV]) ->
    Result.map3
      (\tagName attrs ws1 ->
        (tagName, attrs, ws1)
      )
    (Vu.string tagNameV)
    (Vu.list (\attrV -> case Vu.constructor Ok attrV of
       Ok ("HTMLAttribute", [sp0v, namev, valuev]) ->
         Result.map3 (\sp0 name value -> HTMLAttribute (ws sp0) name value)
         (Vu.string sp0v)
         (Vu.string namev)
         (case Vu.constructor Ok valuev of
           Ok ("HTMLAttributeUnquoted", [ws1v, ws2v, contentv]) ->
             Result.map3 (\ws1 ws2 content -> HTMLAttributeUnquoted (ws ws1) (ws ws2) content)
             (Vu.string ws1v)
             (Vu.string ws2v)
             (Vu.string contentv)
           Ok ("HTMLAttributeString", [ws1v, ws2v, delimiterv, contentv]) ->
             Result.map4 (\ws1 ws2 delimiter content -> HTMLAttributeString (ws ws1) (ws ws2) delimiter content)
             (Vu.string ws1v)
             (Vu.string ws2v)
             (Vu.string delimiterv)
             (Vu.string contentv)
           Ok ("HTMLAttributeNoValue", []) -> Ok HTMLAttributeNoValue
           _ -> Err <| "Expected HTMLAttributeUnquoted(3), HTMLAttributeString(4), HTMLAttributeNoValue(0), got " ++ valToString valuev
         )
       _ -> Err <| "Expected HTMLAttribute, got " ++ valToString attrV
        ) attrsV)
     (Vu.string ws1V)
     |> Result.andThen (\(tagName, attrs, ws1) ->
       Result.map3
         (\endOp children closing -> HTMLElement tagName attrs (ws ws1) endOp children closing)
         (case Vu.constructor Ok endOpV of
            Ok ("RegularEndOpening", []) -> Ok RegularEndOpening
            Ok ("SlashEndOpening", []) -> Ok SlashEndOpening
            _ -> Err <| "Expected RegularEndOpening or SlashEndOpening, got " ++ valToString endOpV)
         (Vu.list valToHtmlNode childrenV)
         (case Vu.constructor Ok closingV of
           Ok ("RegularClosing", [wsc])  -> Vu.string wsc |> Result.map (RegularClosing << ws)
           Ok ("VoidClosing", [])        -> Ok VoidClosing
           Ok ("AutoClosing", [])        -> Ok AutoClosing
           Ok ("ForgotClosing", [])      -> Ok ForgotClosing
           _ -> Err <| "Expected RegularClosing space, VoidClosing, AutoClosing, ForgotClosing, got " ++ valToString closingV
         )
        )
  Ok ("HTMLComment", [styleV]) ->
      (case Vu.constructor Ok styleV of
        Ok ("Less_Greater", [v])      -> Vu.string v |> Result.map Less_Greater
        Ok ("LessSlash_Greater", [v]) -> Vu.string v |> Result.map LessSlash_Greater
        Ok ("LessBang_Greater", [v])  -> Vu.string v |> Result.map LessBang_Greater
        Ok ("LessBangDashDash_DashDashGreater", [v]) -> Vu.string v |> Result.map LessBangDashDash_DashDashGreater
        s -> Err <| "Expected Less_Greater, LessSlash_Greater, LessBang_Greater or LessBangDashDash_DashDashGreater but got " ++ valToString styleV
      ) |> Result.map (\style ->
          HTMLComment style
        )
  _ -> Err <| "Expected HTMLInner, HTMLElement or HTMLComment, got " ++ valToString v


-- Conversion between HTML nodes and true leo values

styleAttrToElmViewInLeo: Vb.Vb -> (String, String) -> Val
styleAttrToElmViewInLeo vb (name, content) =
  if name /= "style" then
    Vb.viewtuple2 Vb.string Vb.string vb (name, content)
  else
    Vb.viewtuple2 Vb.string (Vb.list (Vb.viewtuple2 Vb.string Vb.string)) vb (name,
          Regex.split Regex.All (Regex.regex "(?=;\\s*\\S)") content
       |> List.filterMap (\s ->
            case Regex.find (Regex.AtMost 1) (Regex.regex "^;?([\\s\\S]*):([\\s\\S]*);?\\s*$") s of
              [m] -> case m.submatches of
                [Just name, Just value] -> Just (name, value)
                _ ->Nothing
              _ ->Nothing
          )
       )

filterHTMLInnerWhitespace: List HTMLNode -> List HTMLNode
filterHTMLInnerWhitespace nodes =
  let aux revAcc nodes =
    case nodes of
       [] -> List.reverse revAcc
       HTMLInner txt :: tail -> -- CAREFUL: These are non-breaking spaces, used for indentation only (see Lang.tab and LangSVG.printHTML)
         let newTxt = Regex.replace Regex.All (Regex.regex "(^\r?\n +|\r?\n *$|\r?\n? +$)") (\_ -> "") txt in
         if newTxt /= txt && newTxt == "" then -- We remove this node alltogether
             aux revAcc tail
         else
             aux (HTMLInner newTxt :: revAcc) tail
       node :: tail -> aux (node :: revAcc) tail
  in aux [] nodes

htmlNodeToElmViewInLeo: Vb.Vb -> HTMLNode -> Val
htmlNodeToElmViewInLeo vb tree =
  case tree of
    HTMLInner inner -> Vb.viewtuple2 Vb.string  Vb.string vb ("TEXT", Regex.replace Regex.All
       (Regex.regex "&nbsp;|&amp;|&lt;|&gt;|</[^>]*>")
       (\{match} -> case match of
         "&nbsp;" -> " "
         "&amp;" -> "&"
         "&lt;" -> "<"
         "&gt;" -> ">"
         _ -> "") inner)
    HTMLElement tagName attrs ws1 endOp children closing ->
        Vb.viewtuple3 Vb.string (Vb.list styleAttrToElmViewInLeo) (Vb.list htmlNodeToElmViewInLeo) vb (
          tagName
        , List.map (\attr -> case attr of
          HTMLAttribute ws0 name value -> case value of
            HTMLAttributeUnquoted _ _ content -> (name, content)
            HTMLAttributeString _ _ _ content -> (name, content)
            HTMLAttributeNoValue -> (name, "")) attrs
        , filterHTMLInnerWhitespace children)
    HTMLComment commentStyle ->
       let contentToVal content =
         Vb.viewtuple3 Vb.string (Vb.list styleAttrToElmViewInLeo) (Vb.list htmlNodeToElmViewInLeo) vb (
            "comment",
            [("display", "none")],
            [HTMLInner content])
       in
       case commentStyle of
         Less_Greater content -> contentToVal content
         LessSlash_Greater content -> contentToVal content
         LessBang_Greater content -> contentToVal content
         LessBangDashDash_DashDashGreater content -> contentToVal content

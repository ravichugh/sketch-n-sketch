module HTMLValParser exposing (htmlValParser)

import HTMLParser exposing (..)
import Lang exposing (..)
import ParserUtils exposing (..)
import LangUtils exposing (valToString)
import UpdateUtils
import Utils
import Results exposing (Results(..))
import ValUnbuilder as Vu
import ValBuilder as Vb

htmlValParser: Val
htmlValParser = builtinVal "(Native)HTMLValParser.htmlValParser" <| VFun "parseHTML" ["html"] (\args ->
  case args of
    [v] ->
      case v.v_ of
      VBase (VString s) ->
        case parseHTMLString s of
          Err pe -> Err (showError pe)
          Ok nodes -> Ok (Vb.list v (htmlNodeToVal v) nodes, [])
      _ -> Err <| "parseHTML expects a string, got " ++ valToString v
    _ -> Err ("parseHTML expects exactly one argument, got " ++ toString (List.length args))
  ) (Just (\oldArgs oldOut newOut -> -- Update means just parsing the old
    --let _ = Debug.log (valToString newOut) "<-- newOut in htmlValParser" in
     newOut
     |> Vu.list valToHtmlNode
     |> Result.map unparseHtmlNodes
     |> Result.map (\s -> [replaceV_ newOut <| VBase (VString s)])
     |> Results.fromResult
  ))

htmlNodeToVal: Val -> HTMLNode -> Val
htmlNodeToVal v n = case n of
  HTMLInner s -> Vb.constructor v "HTMLInner" [Vb.string v s]
  HTMLElement tagName attrs ws1 endOp children closing ->
    Vb.constructor v "HTMLElement" [
      Vb.string v tagName,
      Vb.list v (\a -> case a of
        HTMLAttribute ws0 name value -> Vb.constructor v "HTMLAttribute" [Vb.string v ws0.val, Vb.string v name, case value of
          HTMLAttributeUnquoted ws1 ws2 content -> Vb.constructor v "HTMLAttributeUnquoted" [Vb.string v ws1.val, Vb.string v ws2.val, Vb.string v content]
          HTMLAttributeString ws1 ws2 delimiter content -> Vb.constructor v "HTMLAttributeString" [Vb.string v ws1.val, Vb.string v ws2.val, Vb.string v delimiter, Vb.string v content]
          HTMLAttributeNoValue -> Vb.constructor v "HTMLAttributeNoValue" []
      ]) attrs,
      Vb.string v ws1.val,
      case endOp of
        RegularEndOpening -> Vb.constructor v "RegularEndOpening" []
        SlashEndOpening -> Vb.constructor v "SlashEndOpening" []
      ,
      Vb.list v (htmlNodeToVal v) children
      ,
      case closing of
        RegularClosing wsc -> Vb.constructor v "RegularClosing" [Vb.string v wsc.val]
        VoidClosing ->Vb.constructor v "VoidClosing" []
        AutoClosing -> Vb.constructor v "AutoClosing" []
        ForgotClosing ->Vb.constructor v "ForgotClosing" []
    ]
  HTMLComment style -> Vb.constructor v "HTMLComment" [case style of
      Less_Greater content -> Vb.constructor v "Less_Greater" [Vb.string v content]
      LessSlash_Greater {- The string should start with a space -} content -> Vb.constructor v "LessSlash_Greater" [Vb.string v content]
      LessBang_Greater content -> Vb.constructor v "LessBang_Greater" [Vb.string v content]
      LessBangDashDash_DashDashGreater content -> Vb.constructor v "LessBangDashDash_DashDashGreater" [Vb.string v content]
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
           _ -> Err <| "Expected RegularClosing, VoidClosing, AutoClosing, ForgotClosing, got " ++ valToString closingV
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

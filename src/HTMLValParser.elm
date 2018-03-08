module HTMLValParser exposing (htmlValParser)

import HTMLParser exposing (..)
import Lang exposing (..)
import ParserUtils exposing (..)
import LangUtils exposing (valToString)
import Utils
import Results exposing (Results(..))

htmlValParser: Val
htmlValParser = Val (VFun "parseHTML" ["html"] (\env args ->
  case args of
    [v] ->
      let vList x = replaceV_ v <| VList x in
      let vStr x = replaceV_ v <| VBase <| VString x in
      let vConstructor name x = vList ([vStr name] ++ x) in
      case v.v_ of
      VBase (VString s) ->
        case parseHTMLString s of
          Err pe -> Err (showError pe)
          Ok nodes -> Ok ((vList <|
            List.map (htmlNodeToVal vList vStr vConstructor) nodes, []), env)

      _ -> Err <| "parseHTML expects a string, got " ++ valToString v
    _ -> Err ("parseHTML expects exactly one argument, got " ++ toString (List.length args))
  ) (Just (\oldArgs oldOut newOut -> -- Update means just parsing the old
    let _ = Debug.log (valToString newOut) "<-- newOut in htmlValParser" in
    case getList newOut of
      Err msg -> Errs msg
      Ok newOutL ->
        List.map valToHtmlNode newOutL
        |> Utils.projOk
        |> Result.map unparseHtmlNodes
        |> Result.map (\s -> [replaceV_ newOut <| VBase (VString s)])
        |> Results.fromResult
  ))) (Provenance [] (withDummyExpInfo (EVar space0 "(Native)HTMLValParser.htmlValParser" )) []) (Parents [])

htmlNodeToVal: (List Val -> Val) -> (String -> Val) -> (String -> List Val -> Val) -> HTMLNode -> Val
htmlNodeToVal vList vStr vConstructor n = case n of
  HTMLInner s -> vConstructor "HTMLInner" [vStr s]
  HTMLElement tagName attrs ws1 endOp children closing ->
    vConstructor "HTMLElement" [vStr tagName,
      vList (List.map (\a -> case a of
        HTMLAttribute ws0 name value -> vConstructor "HTMLAttribute" [vStr ws0.val, vStr name, case value of
            HTMLAttributeUnquoted ws1 ws2 content -> vConstructor "HTMLAttributeUnquoted" [vStr ws1.val, vStr ws2.val, vStr content]
            HTMLAttributeString ws1 ws2 delimiter content -> vConstructor "HTMLAttributeString" [vStr ws1.val, vStr ws2.val, vStr delimiter, vStr content]
            HTMLAttributeNoValue -> vConstructor "HTMLAttributeNoValue" []
          ]) attrs),
      vStr ws1.val,
      case endOp of
        RegularEndOpening -> vConstructor "RegularEndOpening" []
        SlashEndOpening -> vConstructor "SlashEndOpening" [] 
      ,
      vList (List.map (htmlNodeToVal vList vStr vConstructor) children)
      ,
      case closing of
        RegularClosing wsc -> vConstructor "RegularClosing" [vStr wsc.val]
        VoidClosing ->vConstructor "VoidClosing" []
        AutoClosing -> vConstructor "AutoClosing" []
        ForgotClosing ->vConstructor "ForgotClosing" []
    ]
  HTMLComment style -> vConstructor "HTMLComment" (case style of
    Less_Greater content -> [vStr "Less_Greater", vStr content]
    LessSlash_Greater {- The string should start with a space -} content -> [vStr "LessSlash_Greater", vStr content]
    LessBang_Greater content -> [vStr "LessBang_Greater", vStr content]
    LessBangDashDash_DashDashGreater content -> [vStr "LessBangDashDash_DashDashGreater", vStr content]
      )

type MBCase a b = Case a b | NoCase

deconstruct: Val -> MBCase String (List Val)
deconstruct v = case v.v_ of
  VList (head::tail) -> case head.v_ of
    VBase (VString s) -> Case s tail
    _ -> NoCase
  _ -> NoCase

getString: Val -> Result String String
getString v = case v.v_ of
  VBase (VString s) -> Ok s
  _ -> Err <| "Expected a string while recovering HTML element, got " ++ valToString v

getList: Val -> Result String (List Val)
getList v = case v.v_ of
  VList vs -> Ok vs
  _ -> Err <| "Expected a list while recovering HTML element, got " ++ valToString v

valToHtmlNode: Val -> Result String HTMLNode
valToHtmlNode v =
  case deconstruct v of
  Case "HTMLInner" [i] -> getString i |> Result.map HTMLInner 
  Case "HTMLElement" [tagNameV, attrsV, ws1V, endOpV, childrenV, closingV] ->
    getString tagNameV |> Result.andThen (\tagName ->
      getList attrsV |> Result.andThen (\attrsList ->
        attrsList |> List.map (\attrV -> case deconstruct attrV of
          Case "HTMLAttribute" [sp0v, namev, valuev] ->
            getString sp0v |> Result.andThen (\sp0 ->
              getString namev |> Result.andThen (\name ->
                (case deconstruct valuev of
                  Case "HTMLAttributeUnquoted" [ws1v, ws2v, contentv] -> 
                    getString ws1v |> Result.andThen (\ws1 ->
                    getString ws2v |> Result.andThen (\ws2 ->
                    getString contentv |> Result.map (\content ->
                      HTMLAttributeUnquoted (ws ws1) (ws ws2) content
                    ) ) )
                  Case "HTMLAttributeString" [ws1v, ws2v, delimiterv, contentv] ->
                    getString ws1v |> Result.andThen (\ws1 ->
                    getString ws2v |> Result.andThen (\ws2 ->
                    getString delimiterv |> Result.andThen (\delimiter ->
                    getString contentv |> Result.map (\content ->
                      HTMLAttributeString (ws ws1) (ws ws2) delimiter content
                    ) ) ) )
                  Case "HTMLAttributeNoValue" [] -> Ok HTMLAttributeNoValue
                  _ -> Err <| "Expected HTMLAttributeUnquoted(3), HTMLAttributeString(4), HTMLAttributeNoValue(0), got " ++ valToString valuev
                ) |> Result.map (\value ->
                  HTMLAttribute (ws sp0) name value
                )
              )
            )
          _ -> Err "Expected HTMLAttribute, got something else"
        ) |> Utils.projOk |> Result.andThen (\attrs ->
          getString ws1V |> Result.andThen (\ws1 ->
            (case deconstruct endOpV of
              Case "RegularEndOpening" [] -> Ok RegularEndOpening
              Case "SlashEndOpening" [] -> Ok SlashEndOpening
              _ -> Err <| "Expected RegularEndOpening or SlashEndOpening, got " ++ valToString endOpV) |> Result.andThen (\endOp ->
                getList childrenV |> Result.andThen (\childrenList ->
                  List.map valToHtmlNode childrenList |> Utils.projOk |> Result.andThen (\children ->
                    (case deconstruct closingV of
                      Case "RegularClosing" [wsc]  -> getString wsc |> Result.map (RegularClosing << ws)
                      Case "VoidClosing" []        -> Ok VoidClosing
                      Case "AutoClosing" []        -> Ok AutoClosing
                      Case "ForgotClosing" []      -> Ok ForgotClosing
                      _ -> Err <| "Expected RegularClosing, VoidClosing, AutoClosing, ForgotClosing, got " ++ valToString closingV
                    ) |> Result.map (\closing ->
                       HTMLElement tagName attrs (ws ws1) endOp children closing
                    )
                  )
                )
              )
          )
        )
      )
    )
  Case "HTMLComment" [styleV, contentV] ->
    getString contentV |> Result.andThen (\content ->
      (case getString styleV of
        Ok "Less_Greater" -> Ok <| Less_Greater content
        Ok "LessSlash_Greater" -> Ok <| LessSlash_Greater content
        Ok "LessBang_Greater" -> Ok <| LessBang_Greater content
        Ok "LessBangDashDash_DashDashGreater" -> Ok <| LessBangDashDash_DashDashGreater content
        Ok s -> Err <| "Expected Less_Greater, LessSlash_Greater, LessBang_Greater or LessBangDashDash_DashDashGreater but got " ++ s
        Err msg -> Err msg) |> Result.map (\style ->
          HTMLComment style
        ))
  _ -> Err <| "Expected HTMLInner, HTMLElement or HTMLComment, got " ++ valToString v

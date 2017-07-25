module UserStudyLog exposing (log, logModelUpdate)

import InterfaceModel exposing (..)

import Native.UserStudyLog

import String


log : String -> String -> ()
log eventName info =
  Native.UserStudyLog.log eventName info


logModelUpdate updateFunc msg model =
  let (Msg msgName _) = msg in
  let _ =
    case msgInfo msg model of
      Just info -> log msgName <| "{ " ++ info ++ " }"
      Nothing   -> ()
  in
  let (newModel, cmd) =
    updateFunc msg model
  in
  let _ =
    let nonEmptySelections model =
      model.codeBoxInfo.selections
      |> List.filter (\range -> range.start /= range.end)
    in
    let rangeToJson {start, end} =
      let posToJson {row, column} =
        "{ " ++ "\"row\" : " ++ toString row ++ ", \"column\" : " ++ toString column ++ " }"
      in
      "{ " ++ "\"start\" : " ++ posToJson start ++ ", \"end\" : " ++ posToJson end ++ " }"
    in
    let rangeListToJson ranges =
      "[ " ++ String.join ", " (List.map rangeToJson ranges) ++ " ]"
    in
    let somethingSelected model = nonEmptySelections model /= [] in
    if model.codeBoxInfo.selections /= newModel.codeBoxInfo.selections && (somethingSelected model || somethingSelected newModel) then
      log "Text Selection Changed" (rangeListToJson (nonEmptySelections newModel))
    else if model.codeBoxInfo.cursorPos /= newModel.codeBoxInfo.cursorPos && model.code == newModel.code then
      log "Text Cursor Moved" ""
    else
      ()
  in
  let _ =
    if model.code /= newModel.code
    then log "New Code" (toString newModel.code)
    else ()
  in
  let _ =
    let deuceToolJsons model =
      List.concat model.deuceToolsAndResults
      |> List.map
          (\(deuceTool, results, disabled) ->
            let impossible = List.any predicateImpossible deuceTool.reqs in
            "{ " ++
            "\"name\" : " ++ toString deuceTool.name ++
            ", \"results\" : [ " ++ String.join ", " (List.map (resultDescription >> toString) results) ++ " ] " ++
            ", \"disabled\" : " ++ String.toLower (toString disabled) ++
            ", \"impossible\" : " ++ String.toLower (toString impossible) ++
            " }"
          )
    in
    if deuceToolJsons model /= deuceToolJsons newModel then
      log "New Deuce Tools and Results List" ("[ " ++ String.join ", " (deuceToolJsons newModel) ++ " ]")
    else
      ()
  in
  let _ =
    case (msgName, newModel.errorBox) of
      ("Run", Nothing)     -> log "Run Success" ""
      ("Run", Just errMsg) -> log "Run Error" (toString errMsg)
      _                    -> ()
  in
  (newModel, cmd)


msgInfo (Msg msgName updater) model =
  case msgName of
    "Ace Update" -> Nothing

    _ ->
      let info =
        "\"deuceSelectionsCount\" : " ++ toString (List.length model.deuceState.selectedWidgets)
      in
      if String.startsWith "MousePosition" msgName
      then Nothing
      else Just info

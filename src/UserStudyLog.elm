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
    let somethingSelected model =
      model.codeBoxInfo.selections |> List.any (\range -> range.start /= range.end)
    in
    if model.codeBoxInfo.selections /= newModel.codeBoxInfo.selections && (somethingSelected model || somethingSelected newModel) then
      log "Text Selection Changed" (if somethingSelected newModel then "Non-empty" else "Empty")
    else if model.codeBoxInfo.cursorPos /= newModel.codeBoxInfo.cursorPos && model.code == newModel.code then
      log "Text Cursor Moved" ""
    else
      ()
  in
  let _ =
    if model.code /= newModel.code then
      log "New Code" (toString newModel.code)
    else
      ()
  in
  (newModel, cmd)


msgInfo (Msg msgName updater) model =
  case msgName of
    "Ace Update" -> Nothing

    _ ->
      let info = "\"deuceSelectionsCount\" : " ++ toString (List.length model.deuceState.selectedWidgets) in
      if String.startsWith "MousePosition" msgName
      then Nothing
      else Just info

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
  updateFunc msg model


msgInfo (Msg msgName updater) model =
  case msgName of
    "Ace Update" -> Nothing

    _ ->
      let info = "\"deuceSelectionsCount\" : " ++ toString (List.length model.deuceState.selectedWidgets) in
      if String.startsWith "MousePosition" msgName
      then Nothing
      else Just info

port module FileHandler exposing (showAlert, alertDone)

port showAlert : () -> Cmd msg

port alertDone : (() -> msg) -> Sub msg

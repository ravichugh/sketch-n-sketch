port module AnimationLoop exposing (requestFrame, receiveFrame)

port requestFrame : () -> Cmd msg

port receiveFrame : (Float -> msg) -> Sub msg

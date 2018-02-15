port module OutputCanvas exposing
  ( initialize
  , resetScroll
  , receiveOutputCanvasState
  , receiveAttributeValueUpdate
  , receiveTextValueUpdate
  )

import InterfaceModel as Model exposing (Model, OutputCanvasInfo)

--------------------------------------------------------------------------------
-- Ports

-- Outgoing

port outputCanvasCmd : OutputCanvasCmd -> Cmd msg

type alias OutputCanvasCmd =
  { message : String
  }

initialize  = sendCmd "initialize"
resetScroll = sendCmd "resetScroll"

sendCmd message =
  outputCanvasCmd <|
    { message = message
    }

-- Incoming

port receiveOutputCanvasState : (OutputCanvasInfo -> msg) -> Sub msg

port receiveAttributeValueUpdate : ((Int, String, String) -> msg) -> Sub msg

port receiveTextValueUpdate : ((Int, String) -> msg) -> Sub msg

port module OutputCanvas exposing
  ( initialize
  , resetScroll
  , receiveOutputCanvasState
  , receiveValueUpdate
  , maybeAutoSync
  , enableAutoSync
  , setAutoSyncDelay
  )

import InterfaceModel as Model exposing (Model, OutputCanvasInfo)
import Json.Decode as JSDecode

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

port enableAutoSync: Bool -> Cmd msg

port setAutoSyncDelay : Int -> Cmd msg

-- Incoming

port receiveOutputCanvasState : (OutputCanvasInfo -> msg) -> Sub msg

port receiveValueUpdate : ((List Int, JSDecode.Value)-> msg) -> Sub msg

port maybeAutoSync : (Int -> msg) -> Sub msg
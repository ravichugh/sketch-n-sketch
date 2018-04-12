port module OutputCanvas exposing
  ( initialize
  , resetScroll
  , receiveOutputCanvasState
  , receiveValueUpdate
  , maybeAutoSync
  , enableAutoSync
  , setAutoSyncDelay
  , setPreviewMode
  , setDomNumAttribute
  , setDiffTimer
  , DiffTimer
  , clearPreviewDiff
  , setCaretPosition
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

type alias DiffTimer = { delay: Int, activate: Bool}

initialize  = sendCmd "initialize"
resetScroll = sendCmd "resetScroll"

sendCmd message =
  outputCanvasCmd <|
    { message = message
    }

port enableAutoSync: Bool -> Cmd msg

port setAutoSyncDelay : Int -> Cmd msg

port setPreviewMode: Bool -> Cmd msg

port setDomNumAttribute : {nodeId:Int, attrName:String, attrValue:Float} -> Cmd msg

port setDiffTimer: DiffTimer-> Cmd msg

port setCaretPosition: Int -> Cmd msg

-- Incoming

port receiveOutputCanvasState : (OutputCanvasInfo -> msg) -> Sub msg

port receiveValueUpdate : ((List Int, JSDecode.Value)-> msg) -> Sub msg

port maybeAutoSync : (Int -> msg) -> Sub msg

port clearPreviewDiff : (Int -> msg) -> Sub msg
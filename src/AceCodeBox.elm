port module AceCodeBox exposing
  ( initializeAndDisplay, display, resize
  , receiveEditorState
  )

import InterfaceModel as Model exposing (Model, AceCodeBoxInfo)


--------------------------------------------------------------------------------
-- Ports

-- Outgoing

port aceCodeBoxCmd : AceCodeBoxCmd -> Cmd msg

type alias AceCodeBoxCmd =
  { message : String
  , info : AceCodeBoxInfo
  }

initializeAndDisplay  = sendCmd "initializeAndDisplay"
display               = sendCmd "display"
resize                = sendCmd "resize"

sendCmd message model =
  aceCodeBoxCmd <|
    { message = message
    , info =
        { code = model.code
        , codeBoxInfo = model.codeBoxInfo
        }
    }

-- Incoming

port receiveEditorState : (AceCodeBoxInfo -> msg) -> Sub msg

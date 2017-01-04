port module AceCodeBox exposing
  ( initializeAndDisplay, display
  , requestEditorState, receiveEditorState
  , aceUpdate
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

sendCmd message model =
  aceCodeBoxCmd <|
    { message = message
    , info =
        { code = model.code
        , codeBoxInfo = model.codeBoxInfo
        }
    }

-- Outgoing

port aceCodeBoxPoll : () -> Cmd msg

requestEditorState = aceCodeBoxPoll

-- Incoming

port aceCodeBoxMsg : (AceCodeBoxInfo -> msg) -> Sub msg

receiveEditorState = aceCodeBoxMsg

port aceUpdate : (AceCodeBoxInfo -> msg) -> Sub msg

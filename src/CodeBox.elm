-- This is the elm file responsible for returning the completed code box given
-- the Model and the appropriate dimensions.

module CodeBox (interpretAceEvents, packageModel, 
                AceMessage, CodeBoxInfo) where

import Graphics.Element as GE
import InterfaceModel exposing (Event, sampleModel)

-- So we can crash correctly
import Debug

-- The mailbox/port which relays models to Native/codeBox.js
-- Not needed, as we simply sample on the sigModel
--toAce : Signal.Mailbox Model
--toAce = Signal.mailbox sampleModel

-- The mailbox/port that recieves JsHtml from Native/codeBox.js that needs to be
-- turned into an Element for interfaceView2 to display
--htmlFromAce : Signal.Mailbox JsHtml
--htmlFromAce = Signal.mailbox {}

-- The mailbox/port that recieves AceMessages from Native/codeBox.js that needs
-- to be turned into an Event for upstate to interpret
--eventsFromAce : Signal.Mailbox AceMessage
--eventsFromAce = Signal.mailbox { noyet}

-- Investigation required to see what exactly comes through here. Probably going
-- to have to do some JSON decoding business, hopefully it can be coerced into
-- a VirtualDom/Elm-Html type without too much trouble, from which conversion to
-- an Element is easy.
-- type alias JsHtml = {}
type alias CodeBoxInfo = { code : String 
                         , cursorPos : { row : Int, column : Int }
                         }
type alias AceMessage = { evt : String 
                        , strArg  : String 
                        , cursorArg : { row : Int, column : Int }
                        } 

-- Ultimately what is exposed to InterfaceView2
-- Not really, we end up getting a Signal... Hmm...
-- Could we want a Signal Event, where the Events are
--  UpdateModel (oldModel -> withNewRenderedCodebox)?
-- That would necessitate changing the model to keep track of the rendered
-- codebox, which is not ideal. We could change the view to take an Element as
-- part of rendering - I like that.
-- Typing into the code box will presumably send both a CodeEvent and
-- RenderedCode; hopefully there's no race condition that we have to worry
-- about by sending both over the codebox signal. If there is, splitting into
-- two ports (one for CodeEvent Events and the other for RenderedCode
-- GE.Element) should solve the problem, as Signals entering the program for the
-- first time are guaranteed to be slightly offset.
-- codebox : Signal AceMessage
-- codebox = Signal.map interpret fromAce.signal

--interpretAceHtml : JsHtml -> GE.Element
--interpretAceHtml ahtml = GE.spacer 0 0

interpretAceEvents : AceMessage -> Event
interpretAceEvents amsg = case Debug.log "got\n" amsg.evt of
    "AceCodeUpdate" -> InterfaceModel.UpdateModel <|
        \m -> { m | code <- amsg.strArg
                  , cursorPos <- amsg.cursorArg 
              }
    "init" -> InterfaceModel.Noop
    _ -> Debug.crash "Malformed update sent to Elm"

packageModel : InterfaceModel.Model -> CodeBoxInfo
packageModel model = { code = model.code 
                     , cursorPos = model.cursorPos 
                     }

import Storage exposing (..)

import Html exposing (text, div, input, form, button)
import Html.Attributes exposing (type')
import Html.Events exposing (on, targetValue, onClick)
import Json.Encode exposing (string, Value)
import Json.Decode as Decode

import Signal exposing (Signal, Mailbox, Message, mailbox, message, send)
import Task exposing (Task, succeed, andThen, mapError, onError, fail)

import Debug

--This is just a standard Event mailbox
eventMailbox : Mailbox Event
eventMailbox = mailbox (TextInput "")

--To have buttons/other HTML events cause tasks to occur, they can send a Task
-- to a task mailbox. The tasks then send a message to the eventMailbox
taskMailbox : Mailbox (Task String ())
taskMailbox = mailbox (succeed ())

--These are the two tasks that get sent to the taskMailbox by the buttons
sendInputToStorage : String -> String -> Task String ()
sendInputToStorage key value = setItem key <| string <| Debug.log "send" value

getInputFromStorage : String -> Task String ()
getInputFromStorage key =
  getItem (Debug.log "get" key) Decode.string
  `andThen` \val -> send eventMailbox.address <| GotVal val

--Standard model/event business
type alias Model = 
    { textInputStr   : String
    , keyInputStr    : String
    , curDisplayText : String
    }

initModel = 
    { textInputStr   = ""
    , keyInputStr    = ""
    , curDisplayText = ""    
    }

type Event = GotVal String | TextInput String | KeyInput String

--Upstate as usual, but be sure to keep in mind that tasks can only be fired
-- things that can send messages to mailboxes (e.g. buttons and other
-- Html.Events things)
upstate : Event -> Model -> Model
upstate evt oldmodel = case Debug.log "evt" evt of
    GotVal str -> { oldmodel | curDisplayText <- str }
    TextInput str -> { oldmodel | textInputStr <- str }
    KeyInput str -> { oldmodel | keyInputStr <- str }

--Usual view, but pay attention to the events associated with the buttons. They
-- send Tasks to the taskMailbox, which in turn (maybe) send messages to the
-- eventMailbox, which in turn updates the state.
view : Model -> Html
view model =
  div []
  [ input
    [ on "input" targetValue (message eventMailbox.address << KeyInput)
    ] []
  , input
    [ on "input" targetValue (message eventMailbox.address << TextInput) ]
    []
  , text model.curDisplayText
  , button
    [ onClick taskMailbox.address 
      (getInputFromStorage model.keyInputStr) ]
    [ text "GetVal" ]
  , button
    [ onClick taskMailbox.address 
      (sendInputToStorage model.keyInputStr model.textInputStr) ]
    [ text "PutVal" ]
  ]

--The name doesn't matter, it seems that there must exist this port to support
-- functioning, though I'm not sure if it's a part of elm-storage or tasks
port randotando : Signal (Task String ())
port randotando = taskMailbox.signal

--Main as usual, but note that there's no need to do anything with taskMailbox
-- if the tasks end up sending events through the eventMailbox as usual.
main : Signal Html
main = Signal.map view (Signal.foldp upstate initModel eventMailbox.signal)

import InterfaceModel as Model exposing (events)
import InterfaceView2 as View
import InterfaceController as Controller
import InterfaceStorage exposing (taskMailbox)
import CodeBox exposing (htmlFromAce, eventsFromAce, interpretAceEvents,
                         interpretAceHtml, AceMessage, JsHtml)

import Graphics.Element exposing (Element)
import Mouse 
import Window 

import Task exposing (Task)

--TEMP FOR DEVELOPMENT
import Html
import Debug

--------------------------------------------------------------------------------
-- Main

sigModel : Signal Model.Model
sigModel =
  Signal.foldp Controller.upstate Model.sampleModel <|
    Signal.mergeMany
      [ events.signal
      , Signal.map2 (,) Mouse.isDown Mouse.position
          |> Signal.filter (\(x,y) -> x) (False, (0,0))
          |> Signal.map (\(x,y) -> y)
          |> Signal.map2 adjustCoords Window.dimensions
          |> Signal.map Model.MousePos
      , Signal.map interpretAceEvents theTurn --eventsFromAce.signal
      ]

sigCodeBox : Signal Element
sigCodeBox = Signal.map (interpretAceHtml << Debug.log "h") theRiver --htmlFromAce.signal

main : Signal Element
main = Debug.log "Html type" (Html.div [] []) |> \_ -> Signal.map3 View.view Window.dimensions sigModel sigCodeBox

adjustCoords : (Int, Int) -> (Int, Int) -> (Int, Int)
adjustCoords (w,h) (mx, my) = (mx - (w // 2), my)

-- The necessary port for Tasks/Storage
-- Due to current Elm limitations, this must be in the Main module
port taskPort : Signal (Task String ())
port taskPort = taskMailbox.signal

-- Port for messages to the code box
-- The model (will) contain all the information needed to deduce highlights and such
port aceInTheHole : Signal String
port aceInTheHole = Signal.dropRepeats (Signal.map (\m -> m.code) sigModel)

-- Port for Event messages from the code box
-- port theTurn : Signal AceMessage (type not exposed)
port theTurn : Signal CodeBox.AceMessage
--port theTurn = eventsFromAce.signal

-- Port for Html messages from the code box (e.g. the rendered code box)
-- port theRiver : Signal JsHtml (type not exposed)
port theRiver : Signal CodeBox.JsHtml
--port theRiver = htmlFromAce.signal

import InterfaceModel as Model exposing (events)
import InterfaceView2 as View
import InterfaceController as Controller
import InterfaceStorage exposing (taskMailbox)
import CodeBox exposing (interpretAceEvents, packageModel,
                         AceMessage, CodeBoxInfo)

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

--sigCodeBox : Signal Element
--sigCodeBox = Signal.map (interpretAceHtml << Debug.log "h") theRiver --htmlFromAce.signal

main : Signal Element
main = Signal.map2 View.view Window.dimensions sigModel

adjustCoords : (Int, Int) -> (Int, Int) -> (Int, Int)
adjustCoords (w,h) (mx, my) = (mx - (w // 2), my)

-- The necessary port for Tasks/Storage
-- Due to current Elm limitations, this must be in the Main module
port taskPort : Signal (Task String ())
port taskPort = taskMailbox.signal

-- Port for messages to the code box
-- The model (will) contain all the information needed to deduce highlights and such
-- Note that we don't want to drop repeats, as we need to rerender the Ace
-- editor whenever the rest of the window is rendered
--
-- Current 'race condition issue' - Whenever a Task is used to update, it seems
-- that the order of rendering (Element in view vs codeBox.js replacing it) is
-- switched from when it would otherwise be. So, the order of events is:
--   Model Update -> View Renders empty div -> codeBox.js replaces it
--    Now is:
--   Event -> Task to update model is created -> View Renders empty div ->
--     codebox.js replaces it -> Task returns? -> View is rerendered, codebox
--       does not replace?
--
-- But when the task returns, that should update the below signal anyways, which
-- should trigger a rerender. Hmm.
--
port aceInTheHole : Signal CodeBoxInfo
port aceInTheHole = Signal.map2 (\a b -> packageModel b) Window.dimensions sigModel

-- Port for Event messages from the code box
-- port theTurn : Signal AceMessage (type not exposed)
port theTurn : Signal AceMessage
--port theTurn = eventsFromAce.signal

-- Port for Html messages from the code box (e.g. the rendered code box)
-- port theRiver : Signal JsHtml (type not exposed)
-- port theRiver : Signal CodeBox.JsHtml
--port theRiver : Signal CodeBoxInfo
--port theRiver = htmlFromAce.signal

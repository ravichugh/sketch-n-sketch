
import InterfaceModel as Model exposing (events)
import InterfaceView2 as View
import InterfaceController as Controller
import InterfaceStorage exposing (taskMailbox)

import Graphics.Element exposing (Element)
import Mouse 
import Window 

import Task exposing (Task)

--------------------------------------------------------------------------------
-- Main

main : Signal Element
main =
  let sigModel =
      Signal.foldp Controller.upstate Model.sampleModel <|
        Signal.mergeMany
          [ events.signal
          , Signal.map2 (,) Mouse.isDown Mouse.position
              |> Signal.filter (\(x,y) -> x) (False, (0,0))
              |> Signal.map (\(x,y) -> y)
              |> Signal.map2 adjustCoords Window.dimensions
              |> Signal.map Model.MousePos
          ]
   in
   Signal.map2 View.view Window.dimensions sigModel

adjustCoords : (Int, Int) -> (Int, Int) -> (Int, Int)
adjustCoords (w,h) (mx, my) = (mx - (w // 2), my)

-- The necessary port for Tasks/Storage
-- Due to current Elm limitations, this must be in the Main module
port taskPort : Signal (Task String ())
port taskPort = taskMailbox.signal

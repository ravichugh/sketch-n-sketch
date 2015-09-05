import InterfaceModel as Model exposing (events)
import InterfaceView2 as View
import InterfaceController as Controller
import InterfaceStorage exposing (taskMailbox)
import CodeBox exposing (interpretAceEvents, packageModel,
                         AceMessage, AceCodeBoxInfo, tripRender,
                         initAceCodeBoxInfo)

import Graphics.Element exposing (Element)
import Mouse 
import Window 
import Keyboard
import Set

import Task exposing (Task, andThen)

--TEMP FOR DEVELOPMENT
import Html
import Debug

--------------------------------------------------------------------------------
-- Main

sigModel : Signal Model.Model
sigModel =
  Signal.foldp Controller.upstate Model.sampleModel combinedEventSig

combinedEventSig : Signal Model.Event
combinedEventSig = 
  Signal.mergeMany
    [ events.signal
    , Signal.map2 (,) Mouse.isDown Mouse.position
      |> Signal.filter (\(x,y) -> x) (False, (0,0))
      |> Signal.map (\(x,y) -> y)
      |> Signal.map2 adjustCoords Window.dimensions
      |> Signal.map Model.MousePos
    , Signal.map
      (Model.KeysDown << List.sort << Set.toList)
      Keyboard.keysDown
    ]

main : Signal Element
main = Signal.map2 View.view Window.dimensions sigModel

adjustCoords : (Int, Int) -> (Int, Int) -> (Int, Int)
adjustCoords (w,h) (mx, my) = (mx - (w // 2), my)

-- The necessary port for Tasks/Storage
-- Due to current Elm limitations, this must be in the Main module
port taskPort : Signal (Task String ())
port taskPort = Signal.mergeMany
    [ taskMailbox.signal
    , Signal.map2 interpretAceEvents theTurn 
        <| Signal.sampleOn theTurn sigModel
    ]

-- Port for messages to the code box
port aceInTheHole : Signal AceCodeBoxInfo
port aceInTheHole =
    let pickAsserts (m,e) = case m.editingMode of
          Nothing -> True
          Just _ -> case e of
              -- All events let through here that aren't already let through
              -- 'poke' Ace, rerendering if necessary
              Model.WaitRun -> True
              Model.WaitSave _ -> True
              Model.MousePos _ -> True
--              Model.KeysDown _ -> False
--              Model.CodeUpdate _ -> False
--              Model.UpdateModel _ -> False
              Model.SwitchOrient -> True
              Model.Noop -> True
--              Model.Noop -> False
--              Model.SelectObject _ _ _ -> False 
--              Model.MouseUp -> False
--              Model.MousePos _ -> False
--              Model.Sync -> False
--              Model.TraverseOption _ -> False
--              Model.SelectOption -> False
--              Model.SwitchMode _ -> False
--              Model.SelectExample _ _ -> False
              Model.Edit -> True
--              Model.Run -> False
--              Model.ToggleOutput -> False
--              Model.ToggleZones -> False
              Model.InstallSaveState -> True
              Model.RemoveDialog _ _ -> True
--              Model.SetBasicCodeBox _ -> False
--              Model.StartResizingMid -> False
--              Model.Undo -> False
--              Model.Redo -> False
--              Model.KeysDown _ -> False
--              Model.MultiEvent _ -> False
              --TODO distinguish installState
              _ -> False
    in
        Signal.map fst
                      <| Signal.foldp packageModel initAceCodeBoxInfo
                      <| Signal.filter 
                            (\a -> not (fst a).basicCodeBox
                                   && pickAsserts a )
                            (Model.sampleModel, Model.Noop)
                      <| Signal.map2 (,) sigModel combinedEventSig

-- Port for Event messages from the code box
port theTurn : Signal AceMessage

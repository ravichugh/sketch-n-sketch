module Main exposing (main)

import InterfaceModel as Model exposing (Msg, Model)
import InterfaceView3 as View
import InterfaceController as Controller
import AceCodeBox
import Config

import Html exposing (Html)
import Mouse
import Window
import Keyboard
import Time
import Set

import Task exposing (Task, andThen)


--------------------------------------------------------------------------------
-- Main

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init = (Model.initModel, initCmd)

view : Model -> Html Msg
view = View.view

update : Msg -> Model -> (Model, Cmd Msg)
update = Controller.update

initCmd : Cmd Msg
initCmd =
  Cmd.batch
    [ Task.perform Model.WindowDimensions Window.size
    , AceCodeBox.initializeAndDisplay Model.initModel
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes Model.WindowDimensions
    , Mouse.downs (always (Model.MouseIsDown True))
    , Mouse.ups (always (Model.MouseIsDown False))
    , Mouse.moves Model.MousePosition
    , Keyboard.presses Model.KeyPress
    , Keyboard.downs Model.KeyDown
    , Keyboard.ups Model.KeyUp
    , AceCodeBox.receiveEditorState Model.AceMsg
    ]


{----

sigModel : Signal Model.Model
sigModel =
  let foo initVal =
    case Config.debugLog Config.debugController "initVal" initVal of
      Model.WindowDimensions wh -> { sampleModel | dimensions = wh }
      _                         ->   sampleModel
  in
  Signal.Extra.foldp' Controller.upstate foo combinedEventSig

  -- Signal.foldp Controller.upstate Model.sampleModel combinedEventSig

combinedEventSig : Signal Model.Event
combinedEventSig =
  Signal.mergeMany
   -- Window.dimensions first, so that foldp' gets initial value...
    [ Window.dimensions |> Signal.map Model.WindowDimensions
    , Mouse.isDown |> Signal.map Model.MouseIsDown
    , Mouse.position |> Signal.map Model.MousePosition
    , Keyboard.keysDown |> Signal.map (Model.KeysDown << List.sort << Set.toList)
    , Time.fpsWhen 60 animateSignal |> Signal.map Model.TickDelta
    , events.signal
    ]

main : Signal Element
main = Signal.map2 View.view Window.dimensions sigModel

{-
adjustCoords : (Int, Int) -> (Int, Int) -> (Int, Int)
adjustCoords (w,h) (mx, my) = (mx - (w // 2), my)
-}

-- To have a feedback loop in signals, it appears we need to use ports.
port animateSignal : Signal Bool

port modelRunAnimation : Signal Bool
port modelRunAnimation = Signal.dropRepeats <| Signal.map (.runAnimation) sigModel

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
    let pickAsserts (m,e) =
          case e of
              -- All events let through here that aren't already let through
              -- 'poke' Ace, rerendering if necessary
              Model.WaitRun -> True
              Model.WaitSave _ -> True
              Model.WaitClean -> True
              Model.MultiEvent [ _, Model.CleanCode ] -> True
              Model.WaitCodeBox -> True
--              Model.MousePos _ -> True
--              Model.KeysDown _ -> False
--              Model.CodeUpdate _ -> False
              Model.UpdateModel _ -> True
              Model.SwitchOrient -> True
              Model.Noop -> True
--              Model.Noop -> False
--              Model.SelectObject _ _ _ -> False
--              Model.MouseUp -> False
--              Model.MousePos _ -> False
--              Model.Sync -> False
--              Model.SelectOption -> False
--              Model.SwitchMode _ -> False
              Model.SelectExample _ _ -> True
--              Model.Run -> False
--              Model.ToggleOutput -> False
--              Model.ToggleZones -> False
              Model.InstallSaveState -> True
              Model.RemoveDialog _ _ -> True
              Model.ToggleBasicCodeBox -> True
--              Model.StartResizingMid -> False
--              Model.Undo -> False
--              Model.Redo -> False
--              Model.KeysDown _ -> False
--              Model.MultiEvent _ -> False
              --TODO distinguish installState
              _ -> False
    in
        Signal.filterMap (\(notifyAce, aceCodeBoxInfo, _) -> if notifyAce then Just aceCodeBoxInfo else Nothing) initAceCodeBoxInfo
                      <| Signal.foldp packageModel initFoldpAceCodeBoxInfo
                      -- <| Signal.map (\(m, e) -> let _ = Debug.log "Ace Event:" e in (m,e))
                      -- <| Signal.filter
                      --       (\a -> not (fst a).basicCodeBox
                      --               || snd a == Model.ToggleBasicCodeBox
                      --                   && (fst a).basicCodeBox)
                      --       (sampleModel, Model.Noop)
                      <| Signal.map2 (,) sigModel combinedEventSig

-- Port for Event messages from the code box
port theTurn : Signal AceMessage

----}

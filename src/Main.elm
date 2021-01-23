module Main exposing (main)

import InterfaceModel as Model exposing (Msg(..), Model)
import SleekView as View
import InterfaceController as Controller
import AceCodeBox
import OutputCanvas
import AnimationLoop
import FileHandler
import DeucePopupPanelInfo
import ColorScheme
import Solver
import ImpureGoodies
import WindowOnLoad

import Html exposing (Html)
import Mouse
import Window
import Keyboard
import Time
import PageVisibility
import Update

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
init = (Model.initModel, Cmd.none)

view : Model -> Html Msg
view = View.view

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    WindowOnLoad ->
      (model, onLoadCmd)
    Msg _ _ ->
      Controller.update msg model

-- Initial command runs too fast. Explicitly wait for window.onload event.
onLoadCmd : Cmd Msg
onLoadCmd =
  Cmd.batch <|
    [ Task.perform Controller.msgWindowDimensions Window.size
    , AceCodeBox.initializeAndDisplay Model.initModel
    , OutputCanvas.initialize
    , FileHandler.sendMessage FileHandler.RequestFileIndex
    , Cmd.batch <|
        List.map
          (FileHandler.sendMessage << FileHandler.RequestIcon)
          Model.iconNames
    , ColorScheme.updateColorScheme Model.initColorScheme
    , Task.perform Controller.msgNew (Task.succeed Model.initTemplate)
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ WindowOnLoad.windowOnLoad (\_ -> WindowOnLoad)
    , Window.resizes Controller.msgWindowDimensions
    , PageVisibility.visibilityChanges Controller.msgVisibilityChange
    , Mouse.downs (always (Controller.msgMouseIsDown True))
    , Mouse.ups (always (Controller.msgMouseIsDown False))
    , Mouse.moves Controller.msgMousePosition
    , Keyboard.presses Controller.msgKeyPress
    , Keyboard.downs Controller.msgKeyDown
    , Keyboard.ups Controller.msgKeyUp
    , AceCodeBox.receiveEditorState Controller.msgAceUpdate
    , AceCodeBox.userHasTyped (always Controller.msgUserHasTyped)
    , OutputCanvas.receiveOutputCanvasState Controller.msgOutputCanvasUpdate
    , AnimationLoop.receiveFrame Controller.msgTickDelta
    , FileHandler.receiveMessage
        Controller.fileMessageHandler
        Controller.fileMessageError
    , DeucePopupPanelInfo.receiveDeucePopupPanelInfo
        Controller.msgReceiveDeucePopupPanelInfo
    -- , DependenceGraph.receiveImage Controller.msgReceiveDotImage
    ]

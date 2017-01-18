module Main exposing (main)

import InterfaceModel as Model exposing (Msg, Model)
import InterfaceView3 as View
import InterfaceController as Controller
import AceCodeBox
import AnimationLoop
import FileHandler
import DependenceGraph

import Html exposing (Html)
import Mouse
import Window
import Keyboard
import Time

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
    [ Task.perform Controller.msgWindowDimensions Window.size
    , AceCodeBox.initializeAndDisplay Model.initModel
    , Task.perform Controller.msgNew (Task.succeed "BLANK")
    ]

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes Controller.msgWindowDimensions
    , Mouse.downs (always (Controller.msgMouseIsDown True))
    , Mouse.ups (always (Controller.msgMouseIsDown False))
    , Mouse.moves Controller.msgMousePosition
    , Keyboard.presses Controller.msgKeyPress
    , Keyboard.downs Controller.msgKeyDown
    , Keyboard.ups Controller.msgKeyUp
    , AceCodeBox.receiveEditorState Controller.msgAceUpdate
    , AnimationLoop.receiveFrame Controller.msgTickDelta
    , FileHandler.writeConfirmation Controller.msgConfirmWrite
    , FileHandler.receiveFile Controller.msgReadFile
    , FileHandler.receiveFileFromInput Controller.msgReadFileFromInput
    , FileHandler.receiveFileIndex Controller.msgUpdateFileIndex
    , DependenceGraph.receiveImage Controller.msgReceiveDotImage
    ]

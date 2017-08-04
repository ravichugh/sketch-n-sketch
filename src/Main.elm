module Main exposing (main)

import InterfaceModel as Model exposing (Msg, Model)
import SleekView as View
import InterfaceController as Controller
import AceCodeBox
import AnimationLoop
import FileHandler
import DeucePopupPanelInfo
-- import DependenceGraph

import UserStudyLog
import UserStudy

import Html exposing (Html)
import Mouse
import Window
import Keyboard
import Time
import PageVisibility

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
update msg model =
  UserStudyLog.logModelUpdate Controller.update msg model

initCmd : Cmd Msg
initCmd =
  Cmd.batch <|
    [ Task.perform Controller.msgWindowDimensions Window.size
    , AceCodeBox.initializeAndDisplay Model.initModel
    , Cmd.batch <| List.map FileHandler.requestIcon Model.iconNames
    , Task.perform Controller.msgLoadIcon (Task.succeed (Model.starLambdaToolIcon))
    ] ++
    -- Fixes model not correctly handling initial user study step
    ( if UserStudy.enabled then
        [ Task.perform (Controller.msgUserStudyStep "") (Task.succeed 0)
        ]
      else
        [ Task.perform Controller.msgNew (Task.succeed Model.initTemplate)
        ]
    )

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes Controller.msgWindowDimensions
    , PageVisibility.visibilityChanges Controller.msgVisibilityChange
    , Mouse.downs (always (Controller.msgMouseIsDown True))
    , Mouse.ups (always (Controller.msgMouseIsDown False))
    , Mouse.moves Controller.msgMousePosition
    , Keyboard.presses Controller.msgKeyPress
    , Keyboard.downs Controller.msgKeyDown
    , Keyboard.ups Controller.msgKeyUp
    , AceCodeBox.receiveEditorState Controller.msgAceUpdate
    , AceCodeBox.userHasTyped (always Controller.msgUserHasTyped)
    , AnimationLoop.receiveFrame Controller.msgTickDelta
    , FileHandler.writeConfirmation Controller.msgConfirmWrite
    , FileHandler.deleteConfirmation Controller.msgConfirmDelete
    , FileHandler.receiveFile Controller.msgReadFile
    , FileHandler.receiveIcon Controller.msgLoadIcon
    , FileHandler.receiveFileFromInput Controller.msgReadFileFromInput
    , FileHandler.receiveFileIndex Controller.msgUpdateFileIndex
    , DeucePopupPanelInfo.receiveDeucePopupPanelInfo
        Controller.msgReceiveDeucePopupPanelInfo
    -- , DependenceGraph.receiveImage Controller.msgReceiveDotImage
    ]

module Main exposing (main)

import Model exposing (Msg(..), Model)
import View
import Controller
import AceCodeBox
import OutputCanvas
import AnimationLoop
import FileHandler
import DeucePopupPanelInfo
import ColorScheme
import Solver
import SolverServer
import ImpureGoodies

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
init = (Model.initModel, initCmd)

view : Model -> Html Msg
view = View.view

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  -- let _ = Debug.log "msg" msg in
  case msg of
    ResponseFromSolver str ->
      SolverServer.handleReduceResponse str model
    Msg _ _ ->
      ImpureGoodies.tryCatch "NeedSolution"
        (\()                            -> Controller.update msg model)
        (\(Solver.NeedSolution problem) -> SolverServer.askForSolution problem msg model)
    NewModelAndCmd _ _ ->
      ImpureGoodies.tryCatch "NeedSolution"
        (\()                            -> Controller.update msg model)
        (\(Solver.NeedSolution problem) -> SolverServer.askForSolution problem msg model)

initCmd : Cmd Msg
initCmd =
  Cmd.batch <|
    [ Task.perform Controller.msgWindowDimensions Window.size
    , AceCodeBox.initializeAndDisplay Model.initModel
    , OutputCanvas.initialize
    , FileHandler.sendMessage FileHandler.RequestFileIndex
    , Cmd.batch <|
        List.map
          (FileHandler.sendMessage << FileHandler.RequestIcon)
          Model.iconNames
    , Task.perform
        Controller.msgLoadIcon
        (Task.succeed (Model.starLambdaToolIcon))
    , ColorScheme.updateColorScheme Model.initColorScheme
    -- , Task.perform Controller.msgNew (Task.succeed Model.initTemplate)
    ]

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
    , OutputCanvas.receiveOutputCanvasState Controller.msgOutputCanvasUpdate
    , OutputCanvas.receiveValueUpdate Controller.msgValuePathUpdate
    , OutputCanvas.maybeAutoSync Controller.msgAutoSync
    , OutputCanvas.clearPreviewDiff Controller.msgClearPreviewDiff
    , OutputCanvas.setExampleByName Controller.msgSetExampleByName
    , AnimationLoop.receiveFrame Controller.msgTickDelta
    , FileHandler.receiveMessage
        Controller.fileMessageHandler
        Controller.fileMessageError
    , DeucePopupPanelInfo.receiveDeucePopupPanelInfo
        Controller.msgReceiveDeucePopupPanelInfo
    -- , DependenceGraph.receiveImage Controller.msgReceiveDotImage
    , SolverServer.reduceResponse ResponseFromSolver
    ]

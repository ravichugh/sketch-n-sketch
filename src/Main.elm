module Main exposing (main)

import Keyboard

import Html exposing (Html)
import Html.Attributes as A
import Html.Events as E

import ElmParser

--------------------------------------------------------------------------------
-- Model
--------------------------------------------------------------------------------

type alias Model =
  { code : String
  , oldCode : String
  , output : String
  }

type Msg
  = UpdateCode String
  | Run

init : (Model, Cmd Msg)
init =
  ( { code =
        ""
    , oldCode =
        ""
    , output =
        ""
    }
  , Cmd.none
  )

-- Helpers

isDirty : Model -> Bool
isDirty model =
  model.code /= model.oldCode

--------------------------------------------------------------------------------
-- View
--------------------------------------------------------------------------------

-- Header

header : Html Msg
header =
  Html.h1
    [ A.id "header"
    ]
    [ Html.text "elm-elm"
    ]

-- Work Area

editor : Model -> Html Msg
editor model =
  Html.div
    [ A.id "editor"
    ]
    [ Html.div
        []
        [ Html.h2
            []
            [ Html.text "Input"
            ]
        , Html.div
            [ A.id "run-button"
            , E.onClick Run
            ]
            [ Html.text "Run ▸"
            ]
        ]
    , Html.textarea
        [ A.autofocus True
        , E.onInput UpdateCode
        ]
        []
    ]

output : Model -> Html Msg
output model =
  let
    dirtyFlag =
      if isDirty model then
        "dirty"
      else
        ""
  in
    Html.div
      [ A.id "output"
      , A.class dirtyFlag
      ]
      [ Html.h2
          []
          [ Html.text "Output"
          ]
      , Html.pre
          []
          [ Html.text model.output
          ]
      ]

workArea : Model -> Html Msg
workArea model =
  Html.div
    [ A.id "work-area"
    ]
    [ editor model
    , output model
    ]

-- View

view : Model -> Html Msg
view model =
  Html.div
    [ A.id "view"
    ]
    [ header
    , workArea model
    ]

--------------------------------------------------------------------------------
-- Controller
--------------------------------------------------------------------------------

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    UpdateCode newCode ->
      ( { model | code = newCode }
      , Cmd.none
      )
    Run ->
      let
        oldCode =
          model.code
        newOutput =
          toString <| ElmParser.parse model.code
      in
        ( { model
              | oldCode =
                  oldCode
              , output =
                  newOutput
          }
        , Cmd.none
        )

--------------------------------------------------------------------------------
-- Subscriptions
--------------------------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Keyboard.downs (always Run)
    ]

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

--module Main exposing (main)
--
--import InterfaceModel as Model exposing (Msg, Model)
--import SleekView as View
--import InterfaceController as Controller
--import AceCodeBox
--import AnimationLoop
--import FileHandler
--import DeucePopupPanelInfo
--import ColorScheme
---- import DependenceGraph
--
--import UserStudyLog
--import UserStudy
--
--import Html exposing (Html)
--import Mouse
--import Window
--import Keyboard
--import Time
--import PageVisibility
--
--import Task exposing (Task, andThen)
--
--
----------------------------------------------------------------------------------
---- Main
--
--main : Program Never Model Msg
--main =
--  Html.program
--    { init = init
--    , view = view
--    , update = update
--    , subscriptions = subscriptions
--    }
--
--init : (Model, Cmd Msg)
--init = (Model.initModel, initCmd)
--
--view : Model -> Html Msg
--view = View.view
--
--update : Msg -> Model -> (Model, Cmd Msg)
--update msg model =
--  UserStudyLog.logModelUpdate Controller.update msg model
--
--initCmd : Cmd Msg
--initCmd =
--  Cmd.batch <|
--    [ Task.perform Controller.msgWindowDimensions Window.size
--    , AceCodeBox.initializeAndDisplay Model.initModel
--    , FileHandler.requestFileIndex ()
--    , Cmd.batch <| List.map FileHandler.requestIcon Model.iconNames
--    , Task.perform Controller.msgLoadIcon (Task.succeed (Model.starLambdaToolIcon))
--    , ColorScheme.updateColorScheme Model.initColorScheme
--    ] ++
--    -- Fixes model not correctly handling initial user study step
--    ( if UserStudy.enabled then
--        [ Task.perform (Controller.msgUserStudyStep "") (Task.succeed 0)
--        ]
--      else
--        [ Task.perform Controller.msgNew (Task.succeed Model.initTemplate)
--        ]
--    )
--
--subscriptions : Model -> Sub Msg
--subscriptions model =
--  Sub.batch
--    [ Window.resizes Controller.msgWindowDimensions
--    , PageVisibility.visibilityChanges Controller.msgVisibilityChange
--    , Mouse.downs (always (Controller.msgMouseIsDown True))
--    , Mouse.ups (always (Controller.msgMouseIsDown False))
--    , Mouse.moves Controller.msgMousePosition
--    , Keyboard.presses Controller.msgKeyPress
--    , Keyboard.downs Controller.msgKeyDown
--    , Keyboard.ups Controller.msgKeyUp
--    , AceCodeBox.receiveEditorState Controller.msgAceUpdate
--    , AceCodeBox.userHasTyped (always Controller.msgUserHasTyped)
--    , AnimationLoop.receiveFrame Controller.msgTickDelta
--    , FileHandler.writeConfirmation Controller.msgConfirmWrite
--    , FileHandler.deleteConfirmation Controller.msgConfirmDelete
--    , FileHandler.receiveFile Controller.msgReadFile
--    , FileHandler.receiveIcon Controller.msgLoadIcon
--    , FileHandler.receiveFileFromInput Controller.msgReadFileFromInput
--    , FileHandler.receiveFileIndex Controller.msgUpdateFileIndex
--    , DeucePopupPanelInfo.receiveDeucePopupPanelInfo
--        Controller.msgReceiveDeucePopupPanelInfo
--    -- , DependenceGraph.receiveImage Controller.msgReceiveDotImage
--    , Time.every Time.second Controller.msgUserStudyEverySecondTick
--    ]

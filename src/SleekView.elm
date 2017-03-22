module SleekView exposing (view)

import Html exposing (Html)
import Html.Attributes as Attr

import InterfaceModel as Model exposing (Model, Msg)

--------------------------------------------------------------------------------
-- Menu Bar
--------------------------------------------------------------------------------

menuBar : Model -> Html Msg
menuBar model =
  Html.div
    [ Attr.class "menu-bar"
    ]
    [ Html.img
        [ Attr.class "logo-image"
        , Attr.src "img/logo.png"
        , Attr.width 20
        , Attr.height 20
        ]
        []
    , Html.div
        [ Attr.class "menu"
        ]
        [ Html.div
          [ Attr.class "menu-heading"
          ]
          [ Html.text "File"
          ]
        ]
    , Html.div
        [ Attr.class "menu"
        ]
        [ Html.div
          [ Attr.class "menu-heading"
          ]
          [ Html.text "Edit"
          ]
        ]
    ]

--------------------------------------------------------------------------------
-- Code Panel
--------------------------------------------------------------------------------

codePanel : Model -> Html Msg
codePanel model =
  let
    actionBar =
      Html.div
        [ Attr.class "action-bar"
        ]
        [ Html.div
            [ Attr.class "action"
            ]
            [ Html.text "Undo"
            ]
        , Html.div
            [ Attr.class "action"
            ]
            [ Html.text "Redo"
            ]
        , Html.div
            [ Attr.class "action"
            ]
            [ Html.text "Clean Up"
            ]
        , Html.div
            [ Attr.class "action special run"
            ]
            [ Html.text "Run ▸"
            ]
        ]
    editor =
      Html.div
        [ Attr.id "editor"
        ]
        []
    statusBar =
      Html.div
        [ Attr.class "status-bar"
        ]
        [ Html.text "status-bar"
        ]
  in
    Html.div
      [ Attr.class "panel code-panel"
      ]
      [ actionBar
      , editor
      , statusBar
      ]

--------------------------------------------------------------------------------
-- Resizer
--------------------------------------------------------------------------------

resizer : Model -> Html Msg
resizer model =
  Html.div
    [ Attr.class "resizer"
    ]
    []

--------------------------------------------------------------------------------
-- Output Panel
--------------------------------------------------------------------------------

outputPanel : Model -> Html Msg
outputPanel model =
  Html.div
    [ Attr.class "panel output-panel"
    ]
    []

--------------------------------------------------------------------------------
-- Tool Panel
--------------------------------------------------------------------------------

toolPanel : Model -> Html Msg
toolPanel model =
  Html.div
    [ Attr.class "panel tool-panel"
    ]
    [ Html.div
        [ Attr.class "tool"
        ]
        [ Html.text "tool"
        ]
    , Html.div
        [ Attr.class "tool"
        ]
        [ Html.text "tool"
        ]
    , Html.div
        [ Attr.class "tool"
        ]
        [ Html.text "tool"
        ]
    , Html.div
        [ Attr.class "tool"
        ]
        [ Html.text "tool"
        ]
    , Html.div
        [ Attr.class "tool"
        ]
        [ Html.text "tool"
        ]
    , Html.div
        [ Attr.class "tool"
        ]
        [ Html.text "tool"
        ]
    , Html.div
        [ Attr.class "tool"
        ]
        [ Html.text "tool"
        ]
    ]

--------------------------------------------------------------------------------
-- Work Area
--------------------------------------------------------------------------------

workArea : Model -> Html Msg
workArea model =
  Html.div
    [ Attr.class "work-area"
    ]
    [ codePanel model
    , resizer model
    , outputPanel model
    , toolPanel model
    ]

--------------------------------------------------------------------------------
-- Main View
--------------------------------------------------------------------------------

view : Model -> Html Msg
view model =
  Html.div
    []
    [ menuBar model
    , workArea model
    ]

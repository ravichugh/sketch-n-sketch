module SleekView exposing (view)

import List

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as E

import InterfaceModel as Model exposing (Model, Msg)
import InterfaceController as Controller

--------------------------------------------------------------------------------
-- Menu Bar
--------------------------------------------------------------------------------

menuBar : Model -> Html Msg
menuBar model =
  let
    menu heading options =
      let
        activeFlag =
          if model.viewState.currentMenu == Just heading then
            " active"
          else
            ""
        menuHeading =
          Html.div
            [ Attr.class "menu-heading"
            , E.onClick (Controller.msgToggleMenu heading)
            ]
            [ Html.text heading
            ]
        menuOptions =
          let
            menuOptionDivider =
              Html.div
                [ Attr.class "menu-option-divider"
                ]
                []
            menuOption option =
              Html.div
                [ Attr.class "menu-option"
                ]
                [ option
                ]
          in
            Html.div
              [ Attr.class "menu-options"
              ]
              ( options
                  |> List.map (List.map menuOption)
                  |> List.intersperse [ menuOptionDivider ]
                  |> List.concat
              )
      in
        Html.div
          [ Attr.class <| "menu" ++ activeFlag
          ]
          [ menuHeading
          , menuOptions
          ]
  in
    Html.div
      [ Attr.class "menu-bar"
      ]
      [ Html.div
          [ Attr.class "main-bar"
          ]
          [ Html.img
              [ Attr.class "logo-image"
              , Attr.src "img/logo.png"
              , Attr.width 20
              , Attr.height 20
              ]
              []
          , menu "File"
              [ [ Html.text "New"
                , Html.text "Save As"
                , Html.text "Save"
                ]
              , [ Html.text "Open"
                ]
              , [ Html.text "Export Code"
                , Html.text "Export SVG"
                ]
              , [ Html.text "Import Code"
                , Html.text "Import SVG"
                ]
              ]
          , menu "Edit"
              [ [ Html.text "Dig Hole"
                , Html.text "Make Equal"
                , Html.text "Relate"
                , Html.text "Indexed Relate"
                ]
              , [ Html.text "Dupe"
                , Html.text "Merge"
                , Html.text "Group"
                , Html.text "Abstract"
                ]
              , [ Html.text "Repeat Right"
                , Html.text "Repeat To"
                , Html.text "Repeat Around"
                ]
              ]
          ]
      , Html.div
          [ Attr.class "quick-action-bar"
          ]
          [ Html.div
              [ Attr.class "quick-action-bar-label"
              ]
              [ Html.text "Quick Actions"
              ]
          , Html.div
              [ Attr.class "quick-action"
              ]
              [ Html.text "Save"
              ]
          , Html.div
              [ Attr.class "quick-action"
              ]
              [ Html.text "Open"
              ]
          , Html.div
              [ Attr.class "quick-action"
              ]
              [ Html.text "Make Equal"
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

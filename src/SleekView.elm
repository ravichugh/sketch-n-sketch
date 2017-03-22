module SleekView exposing (view)

import List
import Dict

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as E

import Utils
import HtmlUtils exposing (handleEventAndStop)

import InterfaceModel as Model exposing
  ( Msg(..)
  , Model
  , Tool(..)
  , ShapeToolKind(..)
  , Mode(..)
  , ReplicateKind(..)
  , LambdaTool(..)
  , Caption(..)
  , MouseMode(..)
  , mkLive_
  , DialogBox(..)
  )

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

showRawShapeTools = False

type ButtonKind = Regular | Selected | Unselected

buttonRegularColor = "white"
buttonSelectedColor = "lightgray"

iconButton model iconName onClickHandler btnKind disabled =
  iconButtonExtraAttrs model iconName [] onClickHandler btnKind disabled

iconButtonExtraAttrs model iconName extraAttrs onClickHandler btnKind disabled =
  let
    color =
      case btnKind of
        Regular    -> buttonRegularColor
        Unselected -> buttonRegularColor
        Selected   -> buttonSelectedColor
    iconHtml =
      case Dict.get (String.toLower iconName) model.icons of
        Just h -> h
        Nothing -> Html.text ""
  in
  let commonAttrs =
    [ Attr.disabled disabled
    , Attr.style [ ("width", "40px")
                 , ("height", "40px")
                 , ("background", color)
                 , ("cursor", "pointer")
                 ]
    ]
  in
  Html.button
    (commonAttrs ++
      [ handleEventAndStop "mousedown" Controller.msgNoop
      , E.onClick onClickHandler
      , Attr.title iconName
      ] ++
      extraAttrs)
    [ iconHtml ]

toolButton model tool =
  let capStretchy s = if showRawShapeTools then "BB" else s in
  let capSticky = Utils.uniPlusMinus in -- Utils.uniDelta in
  let capRaw = "(Raw)" in
  let cap = case tool of
    Cursor        -> "Cursor"
    Line Raw      -> "Line"
    Rect Raw      -> "Rect"
    Rect Stretchy -> capStretchy "Rect" -- "Box"
    Oval Raw      -> "Ellipse"
    Oval Stretchy -> capStretchy "Ellipse" -- "Oval"
    Poly Raw      -> "Polygon"
    Poly Stretchy -> capStretchy "Polygon"
    Poly Sticky   -> capSticky
    Path Raw      -> "Path"
    Path Stretchy -> capStretchy "Path"
    Path Sticky   -> capSticky
    Text          -> "Text"
    HelperLine    -> "(Rule)"
    HelperDot     -> "(Dot)"
    Lambda _      -> "Lambda" -- Utils.uniLambda
    _             -> Debug.crash ("toolButton: " ++ toString tool)
  in
  -- TODO temporarily disabling a couple tools
  let (btnKind, disabled) =
    case (model.tool == tool, tool) of
      (True, _)            -> (Selected, False)
      (False, Path Sticky) -> (Regular, True)
      (False, _)           -> (Unselected, False)
  in
    Html.div
      [ Attr.class "tool"
      ]
      [ iconButton
          model cap (Msg cap (\m -> { m | tool = tool })) btnKind disabled
      ]

lambdaTools : Model -> List (Html Msg)
lambdaTools model =
  let buttons =
    Utils.mapi1 (\(i, lambdaTool) ->
      let
        iconName = Model.strLambdaTool lambdaTool
      in
        Html.div
          [ Attr.class "tool"
          ]
          [ iconButton model iconName
              (Msg iconName (\m -> { m | tool = Lambda i }))
              (if model.tool == Lambda i then Selected else Unselected)
              False
          ]
      ) model.lambdaTools
  in
    buttons

toolPanel : Model -> Html Msg
toolPanel model =
  Html.div
    [ Attr.class "panel tool-panel"
    ]
    ( [ toolButton model Cursor
      , toolButton model Text
      , toolButton model (Line Raw)
      , toolButton model (Rect Raw)
      , toolButton model (Oval Raw)
      , toolButton model (Poly Raw)
      , toolButton model (Path Raw)
      ] ++ (lambdaTools model)
    )

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

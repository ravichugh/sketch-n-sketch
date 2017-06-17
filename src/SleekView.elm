module SleekView exposing (view)

import List
import Dict
import Set

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as E
import Json.Decode as Json

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

import SleekLayout exposing (px, half)
import Canvas

import Debug

--------------------------------------------------------------------------------
-- Buttons
--------------------------------------------------------------------------------

textButton : String -> Msg -> Html Msg
textButton text onClickHandler =
  disableableTextButton text onClickHandler False

disableableTextButton : String -> Msg -> Bool -> Html Msg
disableableTextButton text onClickHandler disabled =
  htmlTextButton [Html.text text] onClickHandler disabled False

htmlTextButton : (List (Html Msg)) -> Msg -> Bool -> Bool -> Html Msg
htmlTextButton content onClickHandler disabled stopPropatation =
  let
    disabledFlag =
      if disabled then " disabled" else ""
  in
    Html.span
      [ Attr.class <| "text-button" ++ disabledFlag
      , E.onWithOptions
          "click"
          { stopPropagation = stopPropatation
          , preventDefault = False
          }
          (Json.succeed onClickHandler)
      ]
      content

relateTextButton : Model -> String -> Msg -> Html Msg
relateTextButton model text onClickHandler =
  let
    noFeatures =
      Set.isEmpty model.selectedFeatures
  in
    disableableTextButton text onClickHandler noFeatures

groupTextButton : Model -> String -> Msg -> Bool -> Html Msg
groupTextButton model text onClickHandler disallowSelectedFeatures =
  let
    noFeatures =
      Set.isEmpty model.selectedFeatures
    noBlobs =
      Dict.isEmpty model.selectedBlobs
  in
    disableableTextButton
      text
      onClickHandler
      (noBlobs || (disallowSelectedFeatures && (not noFeatures)))

--------------------------------------------------------------------------------
-- Menu Bar
--------------------------------------------------------------------------------

menuBar : Model -> Html Msg
menuBar model =
  let
    activeFlag =
      if model.viewState.menuActive then
        " active"
      else
        ""
    menu heading options =
      let
        menuHeading =
          Html.div
            [ Attr.class "menu-heading"
            , E.onWithOptions
                "click"
                { stopPropagation = True
                , preventDefault = False
                }
                (Json.succeed <| Controller.msgToggleMenu)
            , Attr.style
                [ ("height", (px << .height) SleekLayout.menuBar)
                , ("line-height", (px << .height) SleekLayout.menuBar)
                , ("padding", "0 " ++
                    (px << half << .height) SleekLayout.menuBar)
                ]
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
          in
            Html.div
              [ Attr.class "menu-options"
              , Attr.style
                  [ ("top", (px << .height) SleekLayout.menuBar) ]
              ]
              ( options
                  |> List.intersperse [ menuOptionDivider ]
                  |> List.concat
              )
      in
        Html.div
          [ Attr.class "menu"
          ]
          [ menuHeading
          , menuOptions
          ]
    hoverMenu title dropdownContent =
      Html.div
        [ Attr.class "hover-menu"
        ]
        [ Html.div
            [ Attr.class "hover-menu-title"
            ]
            [ htmlTextButton
                [ Html.span
                    []
                    [ Html.text title
                    ]
                , Html.span
                    [ Attr.class "hover-menu-indicator"
                    ]
                    [ Html.text "▸"
                    ]
                ]
                Controller.msgNoop
                False
                True
            ]
        , Html.div
            [ Attr.class "dropdown-content" ]
            dropdownContent
        ]
  in
    Html.div
      [ Attr.class "menu-bar"
      , Attr.style
          [ ("height", (px << .height) SleekLayout.menuBar)
          ]
      ]
      [ Html.div
          [ Attr.class <| "main-bar" ++ activeFlag
          ]
          [ Html.img
              [ Attr.class "logo-image"
              , Attr.src "img/light_logo.svg"
              , Attr.width 20
              , Attr.height 20
              ]
              []
          , menu "Sketch-n-Sketch"
              [ [ htmlTextButton
                    [ Html.a
                        [ Attr.href "https://github.com/ravichugh/sketch-n-sketch/blob/master/README.md"
                        , Attr.target "_blank"
                        ]
                        [ Html.text "Syntax Guide" ]
                    ]
                    Controller.msgNoop
                    False
                    False
                , htmlTextButton
                    [ Html.a
                        [ Attr.href "https://github.com/ravichugh/sketch-n-sketch/blob/master/examples/prelude.little"
                        , Attr.target "_blank"
                        ]
                        [ Html.text "Little Standard Library (Prelude)" ]
                    ]
                    Controller.msgNoop
                    False
                    False
                , htmlTextButton
                    [ Html.a
                        [ Attr.href "http://ravichugh.github.io/sketch-n-sketch/"
                        , Attr.target "_blank"
                        ]
                        [ Html.text "About Sketch-n-Sketch" ]
                    ]
                    Controller.msgNoop
                    False
                    False
                ]
              ]
          , menu "File"
              [ [ textButton "New" <|
                    Controller.msgOpenDialogBox New
                , textButton "Save As" <|
                    Controller.msgOpenDialogBox SaveAs
                , disableableTextButton "Save"
                    Controller.msgSave
                    (not model.needsSave)
                ]
              , [ textButton "Open" <|
                    Controller.msgOpenDialogBox Open
                ]
              , [ textButton "Export Code"
                    Controller.msgExportCode
                , textButton "Export SVG"
                    Controller.msgExportSvg
                ]
              , [ textButton "Import Code" <|
                    Controller.msgOpenDialogBox ImportCode
                , textButton "Import SVG"
                    Controller.msgNoop
                ]
              ]
          , menu "Edit Code"
              [ [ hoverMenu "Abstract"
                    [ textButton "All Constants" Controller.msgNoop
                    , textButton "Named Constants" Controller.msgNoop
                    , textButton "Unfrozen Constants" Controller.msgNoop
                    ]
                , textButton "Merge" Controller.msgNoop
                ]
              , [ hoverMenu "Long Hover Menu"
                    [ hoverMenu "Submenu 1"
                        [ textButton "Button 1.1" Controller.msgNoop
                        ]
                    , hoverMenu "Submenu 2"
                        [ hoverMenu "Submenu 2.1"
                            [ hoverMenu "Submenu 2.1.1"
                                [ textButton "Button 2.1.1.1" Controller.msgNoop
                                , hoverMenu "Submenu 2.1.1.2"
                                    [ textButton "Button 2.1.1.2.1" Controller.msgNoop
                                    ]
                                ]
                            , textButton "Button 2.1.2" Controller.msgNoop
                            ]
                        ]
                    ]
                ]
              , [ hoverMenu "Add Arguments"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Remove Arguments"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Reorder Arguments"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                ]
              , [ hoverMenu "Move Definitions"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Introduce Variable"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                ]
              , [ hoverMenu "Eliminate Common Subexpression"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Rename"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Swap Variable Names and Usages"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Inline Definition"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Duplicate Definition"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Make Single Line"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Make Multi-Line"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Align Expressions"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                ]
              , [ hoverMenu "Make Equal (Introduce Single Variable)"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Make Equal (Copy Expression)"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Reorder Expressions"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Swap Variable Usages"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                ]
              , [ hoverMenu "Thaw/Freeze Numbers"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Add/Remove Ranges"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Show/Hide Sliders"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Rewrite as Offsets"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Convert Color Strings"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Flip Boolean"
                    [ textButton "TODO" Controller.msgNoop
                    ]
                ]
              ]
          , menu "Edit Output"
              [ [ relateTextButton model "Dig Hole"
                    Controller.msgDigHole
                , relateTextButton model "Make Equal"
                    Controller.msgMakeEqual
                , relateTextButton model "Relate"
                    Controller.msgRelate
                , relateTextButton model "Indexed Relate"
                    Controller.msgIndexedRelate
                ]
              , [ groupTextButton model "Dupe"
                    Controller.msgDuplicateBlobs
                    True
                , groupTextButton model "Merge"
                    Controller.msgMergeBlobs
                    True
                , groupTextButton model "Group"
                    Controller.msgGroupBlobs
                    False
                , groupTextButton model "Abstract"
                    Controller.msgAbstractBlobs
                    True
                ]
              , [ groupTextButton model "Repeat Right"
                    (Controller.msgReplicateBlob HorizontalRepeat)
                    True
                , groupTextButton model "Repeat To"
                    (Controller.msgReplicateBlob LinearRepeat)
                    True
                , groupTextButton model "Repeat Around"
                    (Controller.msgReplicateBlob RadialRepeat)
                    True
                ]
              ]
          , menu "View" <|
              [ [ textButton "Main Layer (boolean...)" Controller.msgNoop
                , textButton "Widget Layer (boolean...)" Controller.msgNoop
                , textButton "Ghost Layer (boolean...)" Controller.msgNoop
                ]
              ]
          , menu "Options"
                -- TODO make radio buttons
              [ [ hoverMenu "Font Size"
                    [ textButton "8" (Controller.msgUpdateFontSize 8)
                    , textButton "10" (Controller.msgUpdateFontSize 10)
                    , textButton "12" (Controller.msgUpdateFontSize 12)
                    , textButton "14" (Controller.msgUpdateFontSize 14)
                    , textButton "16" (Controller.msgUpdateFontSize 16)
                    , textButton "18" (Controller.msgUpdateFontSize 18)
                    , textButton "20" (Controller.msgUpdateFontSize 20)
                    , textButton "22" (Controller.msgUpdateFontSize 22)
                    , textButton "24" (Controller.msgUpdateFontSize 24)
                    ]
                -- TODO make radio buttons
                , hoverMenu "Auto-Run"
                    [ textButton "Every second" Controller.msgNoop
                    , textButton "Every 2 seconds" Controller.msgNoop
                    , textButton "Every 3 seconds" Controller.msgNoop
                    ]
                -- TODO make radio buttons
                , hoverMenu "Color Scheme"
                    [ disableableTextButton "Light" Controller.msgNoop True
                    , disableableTextButton "Dark" Controller.msgNoop True
                    ]
                ]
                -- TODO make checkboxes
              , [ hoverMenu "Edit Code UI Mode"
                    [ disableableTextButton
                        "Text Select" Controller.msgNoop True
                    , disableableTextButton
                        "Nested Boxes" Controller.msgNoop True
                    , disableableTextButton
                        "Both" Controller.msgNoop True
                    ]
                -- TODO make boolean
                , hoverMenu "Pin Context-Sensitive Menu"
                    [ disableableTextButton "Pin" Controller.msgNoop True
                    , disableableTextButton "Unpin" Controller.msgNoop True
                    ]
                ]
                -- TODO make radio buttons
              , [ hoverMenu "Shape Code Templates"
                    [ textButton "Raw" Controller.msgNoop
                    , textButton "Stretchy" Controller.msgNoop
                    , textButton "Sticky" Controller.msgNoop
                    ]
                ]
                -- TODO make boolean
              , [ hoverMenu "Automatically Suggest Code Changes"
                    [ textButton "On" Controller.msgNoop
                    , textButton "Off" Controller.msgNoop
                    ]
                -- TODO make radio buttons
                , hoverMenu "Live Update Heuristics"
                    [ textButton "Biased" Controller.msgNoop
                    , textButton "Fair" Controller.msgNoop
                    ]
                ]
                -- TODO make radio buttons
              , [ hoverMenu "Output Type"
                    [ textButton "Graphics" Controller.msgNoop
                    , textButton "Text" Controller.msgNoop
                    ]
                ]
              ]
          ]
      -- Quick Action Bar disabled for now
      -- , Html.div
      --     [ Attr.class "quick-action-bar"
      --     ]
      --     [ Html.div
      --         [ Attr.class "quick-action-bar-label"
      --         ]
      --         [ Html.text "Quick Actions"
      --         ]
      --     ]
      ]

--------------------------------------------------------------------------------
-- Code Panel
--------------------------------------------------------------------------------

codePanel : Model -> Html Msg
codePanel model =
  let
    runButton =
      Html.div
        [ Attr.class "run"
        , E.onClick Controller.msgRun
        ]
        [ Html.text "Run ▸"
        ]
    actionBar =
      Html.div
        [ Attr.class "action-bar"
        ]
        [ textButton "Undo" Controller.msgUndo
        , textButton "Redo" Controller.msgRedo
        , textButton "Clean Up" Controller.msgCleanCode
        , runButton
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
    , Attr.style
        [ ("flex", "0 0 " ++ (px << .width) SleekLayout.spacing) ]
    ]
    []

--------------------------------------------------------------------------------
-- Output Panel
--------------------------------------------------------------------------------

textArea text attrs =
  let innerPadding = 4 in
  -- NOTE: using both Attr.value and Html.text seems to allow read/write...
  let commonAttrs =
    [ Attr.spellcheck False
    , Attr.value text
    , Attr.style
        [ ("font-family", "monospace")
        , ("font-size", "14px")
        , ("whiteSpace", "pre")
        , ("height", "100%")
        , ("resize", "none")
        , ("overflow", "auto")
        -- Horizontal Scrollbars in Chrome
        , ("word-wrap", "normal")
        -- , ("background-color", "whitesmoke")
        , ("background-color", "white")
        , ("padding", toString innerPadding ++ "px")
        -- Makes the 100% for width/height work as intended
        , ("box-sizing", "border-box")
        ]
    ]
  in
  Html.textarea (commonAttrs ++ attrs) [ Html.text text ]

pixels n = toString n ++ "px"

outputPanel : Model -> Html Msg
outputPanel model =
  let
    dim =
      SleekLayout.outputPanelBox model
    output =
      case (model.errorBox, model.mode, model.preview) of
        (_, _, Just (_, Err errorMsg)) ->
          textArea errorMsg
            [ Attr.style [ ("width", pixels dim.width) ] ]
        (_, _, Just (_, Ok _)) ->
          Canvas.build dim.width dim.height model
        (Just errorMsg, _, Nothing) ->
          textArea errorMsg
            [ Attr.style [ ("width", pixels dim.width) ] ]
        (Nothing, Print svgCode, Nothing) ->
          textArea svgCode
            [ Attr.style [ ("width", pixels dim.width) ] ]
        (Nothing, _, _) ->
          Canvas.build dim.width dim.height model
  in
    Html.div
      [ Attr.class "panel output-panel"
      ]
      [ output
      ]

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
    -- HelperDot     -> "(Dot)"
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
      , Attr.style
          [ ("width", (px << .width) SleekLayout.toolPanel)
          , ("height", (px << .width) SleekLayout.toolPanel)
          ]
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
    , Attr.style
        [ ("flex", "0 0 " ++ (px << .width) SleekLayout.toolPanel)
        , ("margin-left", (px << .marginLeft) SleekLayout.toolPanel)
        ]
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
    , Attr.style
        [ ("margin", px <| .width SleekLayout.spacing)
        , ("top", px <| .height SleekLayout.menuBar)
        ]
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
    [ E.onClick Controller.msgHideMenu ]
    [ menuBar model
    , workArea model
    ]

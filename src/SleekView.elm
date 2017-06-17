module SleekView exposing (view)

import List
import Dict
import Set
import Regex

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
import ExamplesGenerated as Examples

import Layout
import SleekLayout exposing (px, half)
import Canvas
import LangTools

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

relateDisabled : Model -> Bool
relateDisabled model =
  Set.isEmpty model.selectedFeatures

groupDisabled : Bool -> Model -> Bool
groupDisabled disallowSelectedFeatures model =
  let
    noFeatures =
      Set.isEmpty model.selectedFeatures
    noBlobs =
      Dict.isEmpty model.selectedBlobs
  in
    noBlobs || (disallowSelectedFeatures && (not noFeatures))

--------------------------------------------------------------------------------
-- Buttons
--------------------------------------------------------------------------------

-- Text Button Options

type alias TextButtonOptions =
  { attributes : List (Html.Attribute Msg)
  , content : (List (Html Msg))
  , onClick : Msg
  , disabled : Bool
  , stopPropagation : Bool
  }

defaultTb : TextButtonOptions
defaultTb =
  { attributes = []
  , content = []
  , onClick = Controller.msgNoop
  , disabled = False
  , stopPropagation = False
  }

-- Main Text Button

textButton : TextButtonOptions -> Html Msg
textButton tb =
  let
    disabledFlag =
      if tb.disabled then " disabled" else ""
  in
    Html.span
      ( [ Attr.class <| "text-button" ++ disabledFlag
        , E.onWithOptions
            "click"
            { stopPropagation = tb.stopPropagation
            , preventDefault = False
            }
            (Json.succeed tb.onClick)
        ] ++ tb.attributes
      )
      tb.content

-- Convenience Button Functions

simpleHtmlTextButton : (List (Html Msg)) -> Html Msg
simpleHtmlTextButton content =
  textButton
    { defaultTb
        | content = content
    }

disableableTextButton : Bool -> String -> Msg -> Html Msg
disableableTextButton disabled title onClick =
  textButton
    { defaultTb
        | content = [Html.text title]
        , onClick = onClick
        , disabled = disabled
    }

simpleTextButton : String -> Msg -> Html Msg
simpleTextButton =
  disableableTextButton False

relateTextButton : Model -> String -> Msg -> Html Msg
relateTextButton model text onClickHandler =
  let
    noFeatures =
      Set.isEmpty model.selectedFeatures
  in
    disableableTextButton noFeatures text onClickHandler

groupTextButton : Model -> String -> Msg -> Bool -> Html Msg
groupTextButton model text onClickHandler disallowSelectedFeatures =
  let
    noFeatures =
      Set.isEmpty model.selectedFeatures
    noBlobs =
      Dict.isEmpty model.selectedBlobs
  in
    disableableTextButton
      (noBlobs || (disallowSelectedFeatures && (not noFeatures)))
      text
      onClickHandler

-- UI Buttons

uiButton : String -> Msg -> Html Msg
uiButton =
  styledUiButton ""

styledUiButton : String -> String -> Msg -> Html Msg
styledUiButton =
  generalUiButton False

generalUiButton : Bool -> String -> String -> Msg -> Html Msg
generalUiButton disabled userClass title onClickHandler =
  let
    disabledFlag =
      if disabled then
        "disabled "
      else
        ""
  in
    Html.span
      [ Attr.class <| "uiButton " ++ disabledFlag ++ userClass
      , E.onClick onClickHandler
      ]
      [ Html.text title ]

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
    generalHoverMenu title onMouseEnter onMouseLeave disabled dropdownContent =
      let
        (disabledFlag, realOnMouseEnter, realOnMouseLeave) =
          if disabled then
            (" disabled", Controller.msgNoop, Controller.msgNoop)
          else
            ("", onMouseEnter, onMouseLeave)
      in
        Html.div
          [ Attr.class <| "hover-menu" ++ disabledFlag
          , E.onMouseEnter realOnMouseEnter
          , E.onMouseLeave realOnMouseLeave
          ]
          [ Html.div
              [ Attr.class "hover-menu-title"
              ]
              [ textButton
                  { defaultTb
                      | content =
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
                      , disabled = disabled
                      , stopPropagation = True
                  }
              ]
          , Html.div
              [ Attr.class "dropdown-content" ]
              dropdownContent
          ]
    hoverMenu title dropdownContent =
      generalHoverMenu
        title
        Controller.msgNoop
        Controller.msgNoop
        False
        dropdownContent
    synthesisHoverMenu title onMouseEnter disabled =
      generalHoverMenu
        title
        onMouseEnter
        Controller.msgClearSynthesisResults
        disabled
        (synthesisResultsSelect model)
    relateHoverMenu title onMouseEnter =
      synthesisHoverMenu
        title
        onMouseEnter
        (relateDisabled model)
    groupHoverMenu title onMouseEnter disallowSelectedFeatures =
      synthesisHoverMenu
        title
        onMouseEnter
        (groupDisabled disallowSelectedFeatures model)
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
              [ [ simpleHtmlTextButton
                    [ Html.a
                        [ Attr.href "https://github.com/ravichugh/sketch-n-sketch/blob/master/README.md"
                        , Attr.target "_blank"
                        ]
                        [ Html.text "Syntax Guide" ]
                    ]
                , simpleHtmlTextButton
                    [ Html.a
                        [ Attr.href "https://github.com/ravichugh/sketch-n-sketch/blob/master/examples/prelude.little"
                        , Attr.target "_blank"
                        ]
                        [ Html.text "Little Standard Library (Prelude)" ]
                    ]
                , simpleHtmlTextButton
                    [ Html.a
                        [ Attr.href "http://ravichugh.github.io/sketch-n-sketch/"
                        , Attr.target "_blank"
                        ]
                        [ Html.text "About Sketch-n-Sketch" ]
                    ]
                ]
              ]
          , menu "File"
              [ [ simpleTextButton "New" <|
                    Controller.msgOpenDialogBox New
                , simpleTextButton "Save As" <|
                    Controller.msgOpenDialogBox SaveAs
                , disableableTextButton
                    (not model.needsSave)
                    "Save"
                    Controller.msgSave
                ]
              , [ simpleTextButton "Open" <|
                    Controller.msgOpenDialogBox Open
                ]
              , [ simpleTextButton "Export Code"
                    Controller.msgExportCode
                , simpleTextButton "Export SVG"
                    Controller.msgExportSvg
                ]
              , [ simpleTextButton "Import Code" <|
                    Controller.msgOpenDialogBox ImportCode
                , disableableTextButton
                    True
                    "Import SVG"
                    Controller.msgNoop
                ]
              ]
          , menu "Edit Code"
              [ [ hoverMenu "Abstract"
                    [ simpleTextButton "All Constants" Controller.msgNoop
                    , simpleTextButton "Named Constants" Controller.msgNoop
                    , simpleTextButton "Unfrozen Constants" Controller.msgNoop
                    ]
                , simpleTextButton "Merge" Controller.msgNoop
                ]
              , [ hoverMenu "Long Hover Menu"
                    [ hoverMenu "Submenu 1"
                        [ simpleTextButton "Button 1.1" Controller.msgNoop
                        ]
                    , hoverMenu "Submenu 2"
                        [ hoverMenu "Submenu 2.1"
                            [ hoverMenu "Submenu 2.1.1"
                                [ simpleTextButton "Button 2.1.1.1" Controller.msgNoop
                                , hoverMenu "Submenu 2.1.1.2"
                                    [ simpleTextButton "Button 2.1.1.2.1" Controller.msgNoop
                                    ]
                                ]
                            , simpleTextButton "Button 2.1.2" Controller.msgNoop
                            ]
                        ]
                    ]
                ]
              , [ hoverMenu "Add Arguments"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Remove Arguments"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Reorder Arguments"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                ]
              , [ hoverMenu "Move Definitions"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Introduce Variable"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                ]
              , [ hoverMenu "Eliminate Common Subexpression"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Rename"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Swap Variable Names and Usages"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Inline Definition"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Duplicate Definition"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Make Single Line"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Make Multi-Line"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Align Expressions"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                ]
              , [ hoverMenu "Make Equal (Introduce Single Variable)"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Make Equal (Copy Expression)"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Reorder Expressions"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Swap Variable Usages"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                ]
              , [ hoverMenu "Thaw/Freeze Numbers"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Add/Remove Ranges"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Show/Hide Sliders"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Rewrite as Offsets"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Convert Color Strings"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                , hoverMenu "Flip Boolean"
                    [ simpleTextButton "TODO" Controller.msgNoop
                    ]
                ]
              ]
          , menu "Edit Output"
              [ [ relateTextButton
                    model
                    "Dig Hole"
                    Controller.msgDigHole
                , relateHoverMenu
                    "Make Equal"
                    Controller.msgMakeEqual
                , relateHoverMenu
                   "Relate"
                    Controller.msgRelate
                , relateHoverMenu
                    "Indexed Relate"
                    Controller.msgIndexedRelate
                ]
              , [ groupTextButton
                    model
                    "Dupe"
                    Controller.msgDuplicateBlobs
                    True
                , groupTextButton
                    model
                    "Merge"
                    Controller.msgMergeBlobs
                    True
                , groupTextButton
                    model
                    "Group"
                    Controller.msgGroupBlobs
                    False
                , groupTextButton
                    model
                    "Abstract"
                    Controller.msgAbstractBlobs
                    True
                ]
              , [ groupTextButton
                    model
                    "Repeat Right"
                    (Controller.msgReplicateBlob HorizontalRepeat)
                    True
                , groupTextButton
                    model
                    "Repeat To"
                    (Controller.msgReplicateBlob LinearRepeat)
                    True
                , groupTextButton
                    model
                    "Repeat Around"
                    (Controller.msgReplicateBlob RadialRepeat)
                    True
                ]
              ]
          , menu "View" <|
                -- TODO make booleans
              [ [ disableableTextButton True "Main Layer" Controller.msgNoop
                , disableableTextButton True "Widget Layer" Controller.msgNoop
                , hoverMenu "Ghost Layer"
                    [ simpleTextButton "On" <| Controller.msgSetGhostsShown True
                    , simpleTextButton "Off" <| Controller.msgSetGhostsShown False
                    ]
                ]
              ]
          , menu "Options"
                -- TODO make radio buttons
              [ [ hoverMenu "Font Size"
                    [ simpleTextButton "8" (Controller.msgUpdateFontSize 8)
                    , simpleTextButton "10" (Controller.msgUpdateFontSize 10)
                    , simpleTextButton "12" (Controller.msgUpdateFontSize 12)
                    , simpleTextButton "14" (Controller.msgUpdateFontSize 14)
                    , simpleTextButton "16" (Controller.msgUpdateFontSize 16)
                    , simpleTextButton "18" (Controller.msgUpdateFontSize 18)
                    , simpleTextButton "20" (Controller.msgUpdateFontSize 20)
                    , simpleTextButton "22" (Controller.msgUpdateFontSize 22)
                    , simpleTextButton "24" (Controller.msgUpdateFontSize 24)
                    ]
                -- TODO make radio buttons
                , hoverMenu "Auto-Run"
                    [ disableableTextButton True "Every second" Controller.msgNoop
                    , disableableTextButton True "Every 2 seconds" Controller.msgNoop
                    , disableableTextButton True "Every 3 seconds" Controller.msgNoop
                    ]
                -- TODO make radio buttons
                , hoverMenu "Color Scheme"
                    [ disableableTextButton True "Light" Controller.msgNoop
                    , disableableTextButton True "Dark" Controller.msgNoop
                    ]
                ]
                -- TODO make checkboxes
              , [ hoverMenu "Edit Code UI Mode"
                    [ disableableTextButton
                        True "Text Select" Controller.msgNoop
                    , disableableTextButton
                        True "Nested Boxes" Controller.msgNoop
                    , disableableTextButton
                        True "Both" Controller.msgNoop
                    ]
                -- TODO make boolean
                , hoverMenu "Pin Context-Sensitive Menu"
                    [ disableableTextButton True "Pin" Controller.msgNoop
                    , disableableTextButton True "Unpin" Controller.msgNoop
                    ]
                ]
                -- TODO make radio buttons
              , [ hoverMenu "Shape Code Templates"
                    [ simpleTextButton "Raw" <|
                        Controller.msgSetToolMode Raw
                    , simpleTextButton "Stretchy" <|
                        Controller.msgSetToolMode Stretchy
                    , disableableTextButton True "Sticky" Controller.msgNoop
                    --, simpleTextButton "Sticky" <|
                    --    Controller.msgSetToolMode Sticky
                    ]
                ]
                -- TODO make boolean
              , [ hoverMenu "Automatically Suggest Code Changes"
                    [ simpleTextButton "On" <| Controller.msgSetAutoSynthesis True
                    , simpleTextButton "Off" <| Controller.msgSetAutoSynthesis False
                    ]
                -- TODO make radio buttons
                , hoverMenu "Live Update Heuristics"
                    [ simpleTextButton "Biased" Controller.msgSetHeuristicsBiased
                    , simpleTextButton "None" Controller.msgSetHeuristicsNone
                    , simpleTextButton "Fair" Controller.msgSetHeuristicsFair
                    ]
                ]
                -- TODO make radio buttons
              , [ hoverMenu "Output Type"
                    [ simpleTextButton "Graphics" Controller.msgSetOutputLive
                    , simpleTextButton "Text" Controller.msgSetOutputPrint
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
-- Synthesis Results
--------------------------------------------------------------------------------

synthesisResultsSelectBox model =
  let
    desc description exp isSafe sortKey =
      (if isSafe then "" else "[UNSAFE] ") ++
      (Regex.replace Regex.All (Regex.regex "^Original -> | -> Cleaned$") (\_ -> "") description) ++
      " (" ++ toString (LangTools.nodeCount exp) ++ ")" ++ " " ++ toString sortKey
    resultButtonList priorPathByIndices remainingPathByIndices results =
      let buttons =
        results
        |> Utils.mapi0
            (\(i, Model.SynthesisResult {description, exp, isSafe, sortKey, children}) ->
              let thisElementPath = priorPathByIndices ++ [i] in
              let (isHovered, nextMenu) =
                case remainingPathByIndices of
                  nexti::is ->
                    if i == nexti then
                      case children of
                        Just childResults -> (True, [resultButtonList thisElementPath is childResults])
                        Nothing           -> (True, [])
                    else
                      (False, [])
                  [] ->
                    (False, [])
              in
              nextMenu ++
              [ textButton
                  { defaultTb
                      | attributes =
                          [ E.onMouseEnter (Controller.msgHoverSynthesisResult thisElementPath)
                          , E.onMouseLeave (Controller.msgHoverSynthesisResult [])
                          ]
                      , content =
                          [Html.text <| desc description exp isSafe sortKey]
                      , onClick =
                          Controller.msgSelectSynthesisResult exp
                  }
              ]
            )
        |> List.concat
    in
      Html.div
          [ Attr.style <|
            [ ("position", if priorPathByIndices == [] then "relative" else "absolute")
            , ("left", if priorPathByIndices == [] then "0" else "-325px")
            ]
          ]
          buttons
  in
    resultButtonList [] model.hoveredSynthesisResultPathByIndices model.synthesisResults

synthesisResultsSelect model =
  if List.length model.synthesisResults > 0
  then [ synthesisResultsSelectBox model ]
  else []

--------------------------------------------------------------------------------
-- Code Panel
--------------------------------------------------------------------------------

fileIndicator : Model -> Html Msg
fileIndicator model =
  let
    filenameHtml =
      Html.text <| Model.prettyFilename model
    wrapper =
      if model.needsSave then
        Html.i
          []
          [ filenameHtml
          , Html.text " *"
          ]
      else
        filenameHtml
  in
    Html.span
      [ Attr.class "file-indicator"
      ]
      [ wrapper
      ]


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
        [ simpleTextButton "Undo" Controller.msgUndo
        , simpleTextButton "Redo" Controller.msgRedo
        , simpleTextButton "Clean Up" Controller.msgCleanCode
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
        [ Html.span
            []
            [ Html.b
                []
                [ Html.text "Current file: "
                ]
            , fileIndicator model
            ]
        , Html.span
            [ Attr.class "needs-run-indicator"
            ]
            [
              Html.span
                [ Attr.class "needs-run-light-vertical-spacer"
                ]
                [ Html.text "|"
                ]
            , Html.span
                [ Attr.class "needs-run-light"
                ]
                []
            ]
        ]
  in
    Html.div
      [ Attr.class "panel code-panel"
      ]
      [ statusBar
      , actionBar
      , editor
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
strInterfaceColor = "rgba(52,73,94,1.0)"
strButtonTopColor = "rgba(231,76,60,1.0)" -- from InterfaceButtons example
buttonHeight = 25

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
      case Dict.get (Utils.naturalToCamelCase iconName) model.icons of
        Just h -> h
        Nothing -> Html.text ""
  in
  let commonAttrs =
    [ Attr.disabled disabled
    , Attr.style [ ("width", (px << .width) SleekLayout.toolPanel)
                 , ("height", (px << .width) SleekLayout.toolPanel)
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
  let
    cap = case tool of
      Cursor ->
        "Cursor"
      Line _ ->
        "Line"
      Rect _ ->
        "Rect"
      Oval _  ->
        "Ellipse"
      Poly _ ->
        "Polygon"
      Path _ ->
        "Path"
      Text ->
        "Text"
      HelperLine ->
        "(Rule)"
      Lambda _ ->
        "Lambda" -- Utils.uniLambda
      _ ->
        Debug.crash ("toolButton: " ++ toString tool)
    -- TODO temporarily disabling a couple tools
    (btnKind, disabled) =
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
  let
    separator =
      Html.div
        [ Attr.class "tool-separator" ]
        []
  in
    Html.div
      [ Attr.class "panel tool-panel"
      , Attr.style
          [ ("flex", "0 0 " ++ (px << .width) SleekLayout.toolPanel)
          , ("margin-left", (px << .marginLeft) SleekLayout.toolPanel)
          ]
      ]
      ( [ toolButton model Cursor
        , toolButton model Text
        , toolButton model (Line model.toolMode)
        , toolButton model (Rect model.toolMode)
        , toolButton model (Oval model.toolMode)
        , toolButton model (Poly model.toolMode)
        , toolButton model (Path model.toolMode)
        , separator
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
-- Dialog Boxes
--------------------------------------------------------------------------------

dialogBox
  zIndex
  width
  height
  closable
  db
  model
  headerStyles
  headerElements
  parentStyles
  elements =
    let
      closeDialogBoxButton =
        styledUiButton
          "circle"
          "×"
          (Controller.msgCloseDialogBox db)
      closeButton =
        if closable then
          [ closeDialogBoxButton ]
        else
          []
      activeFlag =
        if (Model.isDialogBoxShowing db model) then
          " active"
        else
          ""
    in
      Html.div
        [ Attr.class <| "dialog-box" ++ activeFlag
        , Attr.style
            [ ("width", width)
            , ("height", height)
            , ("z-index", zIndex)
            ]
        ] <|
        [ Html.h1
            [ Attr.style headerStyles
            ]
            [ Html.div [] headerElements
            , Html.div [] closeButton
            ]
        , Html.div
            [ Attr.class "content"
            , Attr.style parentStyles
            ]
            elements
        ]

bigDialogBox = dialogBox "100" "85%" "85%"
smallDialogBox = dialogBox "101" "35%" "35%"

fileNewDialogBox model =
  let
    viewTemplate (name, _) =
      Html.div
        [ Attr.class "file-listing"
        ]
        [ styledUiButton
            "wide"
            name
            (Controller.msgAskNew name model.needsSave)
        ]
    viewCategory (categoryName, templates) =
      Html.div
        []
        ( [ Html.h2
              []
              [ Html.text categoryName ]
          ] ++ List.map viewTemplate templates
        )
  in
    bigDialogBox
      True
      New
      model
      []
      [Html.text "New..."]
      []
      (List.map viewCategory Examples.templateCategories)

fileSaveAsDialogBox model =
  let
    saveAsInput =
      Html.div
        [ Attr.class "save-as-input" ]
        [ Html.input
            [ Attr.type_ "text"
            , E.onInput Controller.msgUpdateFilenameInput
            ]
            []
        , Html.text ".little"
        , Html.span
            [ Attr.class "save-as-button"
            ]
            [ uiButton
                "Save"
                Controller.msgSaveAs
            ]
        ]
    currentFilesHeader =
      Html.h2
        []
        [ Html.text "Current Files"
        ]
  in
    bigDialogBox
      True
      SaveAs
      model
      []
      [Html.text "Save As..."]
      []
      ([saveAsInput, currentFilesHeader] ++ (List.map viewFileIndexEntry model.fileIndex))

fileOpenDialogBox model =
  let fileOpenRow filename =
        Html.div
          [ Attr.class "file-listing"
          ]
          [ Html.span []
              [ Html.b [] [ Html.text filename ]
              , Html.text ".little"
              ]
          , Html.span
              [ Attr.class "file-open-delete-buttons"
              ]
              [ uiButton
                  "Open"
                   (Controller.msgAskOpen filename model.needsSave)
              , Html.span
                  [ Attr.class "file-delete-button"
                  ]
                  [ uiButton
                      "Delete"
                      (Controller.msgDelete filename)
                  ]
              ]
          ]
  in
    bigDialogBox
      True
      Open
      model
      []
      [Html.text "Open..."]
      []
      (List.map fileOpenRow model.fileIndex)

viewFileIndexEntry filename =
  Html.div
    [ Attr.class "file-listing"
    ]
    [ Html.span []
        [ Html.b [] [ Html.text filename ]
        , Html.text ".little"
        ]
    ]

alertSaveDialogBox model =
  smallDialogBox
    False
    AlertSave
    model
    []
    [ Html.span
        [ Attr.style [("color", "#FF3300")] ]
        [ Html.text "Warning" ]
    ]
    [ ("display", "flex") ]
    [ Html.div
        [ Attr.style
            [ ("padding", "20px")
            , ("flex-grow", "1")
            , ("display", "flex")
            , ("flex-direction", "column")
            , ("justify-content", "space-between")
            ]
        ]
        [ Html.div
            []
            [ Html.i []
                [ Html.text <| Model.prettyFilename model ]
            , Html.text
                " has unsaved changes. Would you like to continue anyway?"
            ]
        , Html.div
            [ Attr.style
                [ ("text-align", "right")
                ]
            ]
            [ uiButton
                "Cancel"
                Controller.msgCancelFileOperation
            , Html.span
                [ Attr.style
                    [ ("margin-left", "30px")
                    ]
                ]
                [ uiButton
                    "Yes (Discard Changes)"
                    Controller.msgConfirmFileOperation
                ]
            ]
        ]
    ]

importCodeDialogBox model =
  smallDialogBox
    True
    ImportCode
    model
    []
    [ Html.text "Import Code..." ]
    []
    [ Html.div
        [ Attr.class "centered"
        ]
        [ Html.input
            [ Attr.type_ "file"
            , Attr.id Model.importCodeFileInputId
            ]
            []
        , Html.br [] []
        , Html.br [] []
        , uiButton
            "Import"
            (Controller.msgAskImportCode model.needsSave)
        ]
    ]

dialogBoxes : Model -> (List (Html Msg))
dialogBoxes model =
  [ fileNewDialogBox model
  , fileSaveAsDialogBox model
  , fileOpenDialogBox model
  , alertSaveDialogBox model
  , importCodeDialogBox model
  ]

subtleBackground : Html Msg
subtleBackground =
  Html.div
    [ Attr.class "subtle-background" ]
    []

--------------------------------------------------------------------------------
-- Onbeforeunload Data Element (save confirmation)
--------------------------------------------------------------------------------

onbeforeunloadDataElement : Model -> Html Msg
onbeforeunloadDataElement model =
  let
    needsSaveString =
      if model.needsSave then
        "true"
      else
        "false"
  in
    Html.input
      [ Attr.type_ "hidden"
      , Attr.id "onbeforeunload-data"
      , Attr.attribute "data-needs-save" needsSaveString
      , Attr.attribute "data-filename" (Model.prettyFilename model)
      ]
      []

--------------------------------------------------------------------------------
-- Main View
--------------------------------------------------------------------------------

view : Model -> Html Msg
view model =
  let
    needsRunFlag =
      if Model.needsRun model then
        " needs-run"
      else
        ""
    hasDialogFlag =
      if Model.anyDialogShown model then
         " has-dialogs"
      else
        ""
  in
    Html.div
      [ Attr.class <| "main" ++ needsRunFlag ++ hasDialogFlag
      , E.onClick Controller.msgHideMenu
      ]
      ( [ onbeforeunloadDataElement model
        , menuBar model
        , workArea model
        , subtleBackground
        ]
        ++ (dialogBoxes model)
      )

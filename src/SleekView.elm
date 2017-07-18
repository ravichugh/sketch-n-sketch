module SleekView exposing (view)

import List
import Dict
import Set
import Regex
import String

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as E
import Json.Decode as Json
import Svg
import Svg.Attributes as SAttr

import Utils
import HtmlUtils exposing (handleEventAndStop, styleListToString)
import Either exposing (..)

import InterfaceModel as Model exposing (..)

import InterfaceController as Controller
import ExamplesGenerated as Examples

import Deuce
import DeuceTools

import SleekLayout exposing (px, half)
import Canvas
import LangTools
import Sync
import Lang exposing (Exp)
import LangUnparser

import DeuceWidgets exposing (..)
import Config exposing (params)

import UserStudy

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

allButLast : List a -> List a
allButLast xs =
  let
    len = List.length xs
  in
    List.take (len - 1) xs

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

onClickWithoutPropagation : msg -> Html.Attribute msg
onClickWithoutPropagation handler =
  E.onWithOptions
    "click"
    { stopPropagation = True
    , preventDefault = False
    }
    (Json.succeed handler)

onRightClick : msg -> Html.Attribute msg
onRightClick handler =
  E.onWithOptions
    "contextmenu"
    { stopPropagation = True
    , preventDefault = True
    }
    (Json.succeed handler)

-- Derived from (under "keyCode"):
--   http://package.elm-lang.org/packages/elm-lang/html/2.0.0/Html-Events
onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
  E.on "keydown" (Json.map tagger E.keyCode)

enterKeyCode : Int
enterKeyCode =
  13

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
    (disabledFlag, realOnClick, realStopPropagation) =
      if tb.disabled then
        (" disabled"
        , Controller.msgNoop
        , True
        )
      else
        (""
        , tb.onClick
        , tb.stopPropagation
        )
  in
    Html.span
      ( [ Attr.class <| "text-button" ++ disabledFlag
        , E.onWithOptions
            "click"
            { stopPropagation = realStopPropagation
            , preventDefault = False
            }
            (Json.succeed realOnClick)
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
        | content = [ Html.text title ]
        , onClick = onClick
        , disabled = disabled
    }

simpleTextButton : String -> Msg -> Html Msg
simpleTextButton =
  disableableTextButton False

simpleTextRadioButton : Bool -> String -> Msg -> Html Msg
simpleTextRadioButton active title onClick =
  let
    (activeFlag, radioButtonIcon) =
      if active then
        ("radio-button-active", "●")
      else
        ("", "○")
  in
    textButton
      { defaultTb
          | content =
              [ Html.span
                  [ Attr.class "radio-button-icon"
                  ]
                  [ Html.text radioButtonIcon
                  ]
              , Html.span
                  [ Attr.class activeFlag
                  ]
                  [ Html.text title
                  ]
              ]
          , onClick = onClick
      }

booleanOption : Bool -> String -> String -> (Bool -> Msg) -> List (Html Msg)
booleanOption test onString offString handler =
  [ simpleTextRadioButton
      test
      onString
      (handler True)
  , simpleTextRadioButton
      (not test)
      offString
      (handler False)
  ]

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
      [ Attr.class <| "ui-button " ++ disabledFlag ++ userClass
      , E.onClick onClickHandler
      ]
      [ Html.text title ]

--------------------------------------------------------------------------------
-- Menu Bar
--------------------------------------------------------------------------------

generalHtmlHoverMenu
  : String -> List (Html Msg) -> Msg -> Msg -> Msg -> Bool -> List (Html Msg) -> Html Msg
generalHtmlHoverMenu
  class titleHtml onMouseEnter onMouseLeave onClick disabled dropdownContent =
    let
      (disabledFlag, realOnMouseEnter, realOnMouseLeave, realOnClick) =
        if disabled then
          ("disabled "
          , Controller.msgNoop
          , Controller.msgNoop
          , Controller.msgNoop
          )
        else
          (""
          , onMouseEnter
          , onMouseLeave
          , onClick
          )
    in
      Html.div
        [ Attr.class <| "hover-menu " ++ disabledFlag ++ class
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
                            [ Attr.class "hover-menu-content"
                            ]
                            titleHtml
                        , Html.span
                            [ Attr.class "hover-menu-indicator"
                            ]
                            [ Html.text "▸"
                            ]
                        ]
                    , disabled = disabled
                    , stopPropagation = True
                    , onClick = realOnClick
                }
            ]
        , Html.div
            [ Attr.class "dropdown-content" ]
            dropdownContent
        ]

generalHoverMenu
  : String -> Msg -> Msg -> Msg -> Bool -> List (Html Msg) -> Html Msg
generalHoverMenu titleString =
  generalHtmlHoverMenu "" [ Html.text titleString ]

hoverMenu : String -> (List (Html Msg)) -> Html Msg
hoverMenu title dropdownContent =
  generalHoverMenu
    title
    Controller.msgNoop
    Controller.msgNoop
    Controller.msgNoop
    False
    dropdownContent

synthesisHoverMenu : Model -> String -> Msg -> Bool -> Html Msg
synthesisHoverMenu model title onMouseEnter disabled =
  generalHoverMenu
    title
    onMouseEnter
    Controller.msgClearSynthesisResults
    Controller.msgNoop
    disabled
    [ synthesisResultsSelect model ]

relateHoverMenu : Model -> String -> Msg -> Html Msg
relateHoverMenu model title onMouseEnter =
  synthesisHoverMenu
    model
    title
    onMouseEnter
    (relateDisabled model)

groupHoverMenu : Model -> String -> Msg -> Bool -> Html Msg
groupHoverMenu model title onMouseEnter disallowSelectedFeatures =
  synthesisHoverMenu
    model
    title
    onMouseEnter
    (groupDisabled disallowSelectedFeatures model)

deuceSynthesisResult : Model -> List Int -> Bool -> SynthesisResult -> Html Msg
deuceSynthesisResult model path isRenamer (SynthesisResult result) =
  let
    (preview, class) =
      case (result.isSafe, runAndResolve model result.exp) of
        (True, Ok (val, widgets, slate, code)) ->
          (Just (code, Ok (val, widgets, slate)), "expected-safe")
        (True, Err err) ->
          let _ = Debug.log "not safe after all!" () in
          (Just (LangUnparser.unparse result.exp, Err err), "unexpected-unsafe")
        (False, Ok (val, widgets, slate, code)) ->
          (Just (code, Ok (val, widgets, slate)), "unexpected-safe")
        (False, Err err) ->
          (Just (LangUnparser.unparse result.exp, Err err), "expected-unsafe")
    maybePreview =
      -- TODO make renaming dynamically appear in the code
      if isRenamer then
        Nothing
      else
        Just preview
    renameInput =
      if isRenamer then
        [ Html.input
            [ Attr.type_ "text"
            , Attr.class "rename-input"
            , E.onInput Controller.msgUpdateRenameVarTextBox
            , onClickWithoutPropagation Controller.msgNoop
            , onKeyDown <|
                \code ->
                  if code == enterKeyCode then -- Enter button
                    Controller.msgChooseDeuceExp result.description result.exp
                  else
                    Controller.msgNoop
            ]
            []
        ]
      else
        []
    additionalInputs =
      renameInput
  in
    generalHtmlHoverMenu class
      ( [ Html.span
            []
            [ Html.text
                result.description
            ]
        ] ++ additionalInputs
      )
      (Controller.msgHoverDeuceResult result.description path maybePreview)
      (Controller.msgLeaveDeuceResult result.description path maybePreview)
      (Controller.msgChooseDeuceExp result.description result.exp)
      False
      []

deuceSynthesisResults
  : Model -> List Int -> Bool -> List SynthesisResult -> List (Html Msg)
deuceSynthesisResults model path isRenamer results =
  if List.isEmpty results then
    [ generalHtmlHoverMenu "transformation-oops"
        [ Html.span
            []
            [ Html.text "Oops! Can't apply transformation after all."
            ]
        ]
        Controller.msgNoop
        Controller.msgNoop
        Controller.msgNoop
        True
        []
    ]
  else
    List.map
      (deuceSynthesisResult model path isRenamer)
      results

deuceHoverMenu : Model -> (Int, CachedDeuceTool) -> Html Msg
deuceHoverMenu model (index, (deuceTool, results, disabled)) =
  let
    path =
      [ index ]
    isRenamer =
      DeuceTools.isRenamer deuceTool
    title =
      deuceTool.name
  in
    generalHoverMenu
      title
      Controller.msgNoop
      Controller.msgNoop
      Controller.msgNoop
      disabled
      [ Html.div
          [ Attr.class "synthesis-results"
          ] <|
          deuceSynthesisResults model path isRenamer results
      ]

editCodeEntry : Model -> (Int, CachedDeuceTool) -> Html Msg
editCodeEntry model (_, ((deuceTool, _, _) as cachedDeuceTool)) =
  let
    title =
      deuceTool.name ++ "..."
    disabled =
      List.any Model.predicateImpossible deuceTool.reqs
  in
    disableableTextButton
      disabled
      title
      (Controller.msgSetSelectedDeuceTool cachedDeuceTool)

menuHeading : String -> Html Msg
menuHeading heading =
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

menuOptions : List (List (Html Msg)) -> Html Msg
menuOptions options =
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

menu : String -> List (List (Html Msg)) -> Html Msg
menu heading options =
  Html.div
    [ Attr.class "menu"
    ]
    [ menuHeading heading
    , menuOptions options
    ]

menuBar : Model -> Html Msg
menuBar model =
  let
    activeFlag =
      if model.viewState.menuActive then
        " active"
      else
        ""
  in
    Html.div
      [ Attr.class "menu-bar"
      , Attr.style
          [ ("height", (px << .height) SleekLayout.menuBar)
          , ("borderBottomWidth", (px << .borderWidth) SleekLayout.menuBar)
          ]
      ]
      [ Html.div
          [ Attr.class <| "main-bar" ++ activeFlag
          ] <|
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
                , disableableTextButton
                    True
                    params.strVersion
                    Controller.msgNoop
                ]
              ]
          , menu "File"
              [ [ simpleTextButton "New..." <|
                    Controller.msgOpenDialogBox New
                , simpleTextButton "Save As..." <|
                    Controller.msgOpenDialogBox SaveAs
                , disableableTextButton
                    (not model.needsSave)
                    "Save"
                    Controller.msgSave
                ]
              , [ simpleTextButton "Open..." <|
                    Controller.msgOpenDialogBox Open
                ]
              , [ simpleTextButton "Export Code"
                    Controller.msgExportCode
                , simpleTextButton "Export SVG"
                    Controller.msgExportSvg
                ]
              , [ simpleTextButton "Import Code..." <|
                    Controller.msgOpenDialogBox ImportCode
                , disableableTextButton
                    True
                    "Import SVG"
                    Controller.msgNoop
                ]
              ]
          ] ++
          ( if model.showDeuceInMenuBar then
              [ menu "Deuce" <|
                  List.map
                    (Utils.mapi1 <| deuceHoverMenu model)
                    model.deuceToolsAndResults
              ]
            else
              []
          ) ++
          ( if model.showEditCodeInMenuBar then
              [ menu "Edit Code" <|
                  List.map
                    (Utils.mapi1 <| editCodeEntry model)
                    model.deuceToolsAndResults
              ]
            else
              []
          ) ++
          [ menu "Edit Output"
              [ [ relateTextButton
                    model
                    "Dig Hole"
                    Controller.msgDigHole
                , relateHoverMenu
                    model
                    "Make Equal"
                    Controller.msgMakeEqual
                , relateHoverMenu
                    model
                   "Relate"
                    Controller.msgRelate
                , relateHoverMenu
                    model
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
              [ [ disableableTextButton True "Main Layer" Controller.msgNoop
                , disableableTextButton True "Widget Layer" Controller.msgNoop
                , hoverMenu "Ghost Layer" <|
                    booleanOption
                      model.showGhosts
                      "On"
                      "Off"
                      Controller.msgSetGhostsShown
                ]
              ]
          , menu "Options"
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
                , hoverMenu "Auto-Run"
                    [ disableableTextButton
                        True "Every second" Controller.msgNoop
                    , disableableTextButton
                        True "Every 2 seconds" Controller.msgNoop
                    , disableableTextButton
                        True "Every 3 seconds" Controller.msgNoop
                    ]
                , hoverMenu "Color Scheme"
                    [ disableableTextButton True "Light" Controller.msgNoop
                    , disableableTextButton True "Dark" Controller.msgNoop
                    ]
                ]
              , [ hoverMenu "Enable Deuce Box Selection" <|
                    booleanOption
                      model.enableDeuceBoxSelection
                      "True"
                      "False"
                      Controller.msgSetEnableDeuceBoxSelection
                , hoverMenu "Enable Deuce Text Selection" <|
                    booleanOption
                      model.enableDeuceTextSelection
                      "True"
                      "False"
                      Controller.msgSetEnableDeuceTextSelection
                ]
              , [ hoverMenu "Text Selection Mode"
                    [ simpleTextRadioButton
                        ( case model.textSelectMode of
                            Strict ->
                              True
                            _ ->
                              False
                        )
                        "Strict"
                        (Controller.msgSetTextSelectMode Strict)
                    , simpleTextRadioButton
                        ( case model.textSelectMode of
                            Superset ->
                              True
                            _ ->
                              False
                        )
                        "Superset"
                        (Controller.msgSetTextSelectMode Superset)
                    , simpleTextRadioButton
                        ( case model.textSelectMode of
                            Subset ->
                              True
                            _ ->
                              False
                        )
                        "Subset"
                        (Controller.msgSetTextSelectMode Subset)
                    , simpleTextRadioButton
                        ( case model.textSelectMode of
                            SubsetExtra ->
                              True
                            _ ->
                              False
                        )
                        "SubsetExtra"
                        (Controller.msgSetTextSelectMode SubsetExtra)
                    ]
                ]
              , [ hoverMenu "Show Deuce in Menu Bar" <|
                    booleanOption
                      model.showDeuceInMenuBar
                      "True"
                      "False"
                      Controller.msgSetShowDeuceInMenuBar
                , hoverMenu "Show Edit Code in Menu Bar" <|
                    booleanOption
                      model.showEditCodeInMenuBar
                      "True"
                      "False"
                      Controller.msgSetShowEditCodeInMenuBar
                , hoverMenu "Show Deuce Panel" <|
                    booleanOption
                      model.showDeucePanel
                      "True"
                      "False"
                      Controller.msgSetShowDeucePanel
                , hoverMenu "Show Deuce Right Click Menu" <|
                    booleanOption
                      model.showDeuceRightClickMenu
                      "True"
                      "False"
                      Controller.msgSetShowDeuceRightClickMenu
                ]
              , [ hoverMenu "Shape Code Templates"
                    [ simpleTextRadioButton
                        (model.toolMode == Raw)
                        "Raw"
                        (Controller.msgSetToolMode Raw)
                    , simpleTextRadioButton
                        (model.toolMode == Stretchy)
                        "Stretchy"
                        (Controller.msgSetToolMode Stretchy)
                    , simpleTextRadioButton
                        (model.toolMode == Sticky)
                        "Sticky"
                        (Controller.msgSetToolMode Sticky)
                    ]
                ]
              , [ hoverMenu "Automatically Suggest Code Changes"
                    [ simpleTextRadioButton
                        model.autoSynthesis
                        "On"
                        Controller.msgStartAutoSynthesis
                    , simpleTextRadioButton
                        (not model.autoSynthesis)
                        "Off"
                        Controller.msgStopAutoSynthesisAndClear
                    ]
                , hoverMenu "Live Update Heuristics"
                    [ simpleTextRadioButton
                        ( model.syncOptions.feelingLucky ==
                            Sync.heuristicsBiased
                        )
                        "Biased"
                        Controller.msgSetHeuristicsBiased
                    , simpleTextRadioButton
                        ( model.syncOptions.feelingLucky ==
                            Sync.heuristicsNone
                        )
                        "None"
                        Controller.msgSetHeuristicsNone
                    , simpleTextRadioButton
                        ( model.syncOptions.feelingLucky ==
                            Sync.heuristicsFair
                        )
                        "Fair"
                        Controller.msgSetHeuristicsFair
                    ]
                ]
              , [ hoverMenu "Output Type"
                    [ simpleTextRadioButton
                        ( case model.mode of
                            Live _ ->
                              True
                            _ ->
                              False
                        )
                        "Graphics"
                        Controller.msgSetOutputLive
                    , simpleTextRadioButton
                        ( case model.mode of
                            Print _ ->
                              True
                            _ ->
                              False
                        )
                        "Text"
                        Controller.msgSetOutputPrint
                    ]
                ]
              ]
          , Html.div
              [ Attr.class "user-study-info"
              ]
              [ textButton
                  { defaultTb
                      | content =
                          [ Html.span
                              [ Attr.class "flip"
                              ]
                              [ Html.text "▸"
                              ]
                          , Html.text " Previous Step"
                          ]
                      , onClick =
                          Controller.msgUserStudyPrev
                      , disabled =
                          UserStudy.disablePreviousStep
                            model.userStudyStateIndex
                  }
              , disableableTextButton
                  True
                  "USER STUDY"
                  Controller.msgNoop
              , disableableTextButton
                  (UserStudy.disableNextStep model.userStudyStateIndex)
                  "Next Step ▸"
                  Controller.msgUserStudyNext
              ]
          , menu "Help"
              [ [ simpleTextButton
                    "Text Select Mode"
                    (Controller.msgOpenDialogBox (Help HelpTextSelectMode))
                , simpleTextButton
                    "Box Select Mode"
                    (Controller.msgOpenDialogBox (Help HelpBoxSelectMode))
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

synthesisResultHoverMenu
  : String -> (List Int) -> Exp -> (List (Html Msg)) -> Html Msg
synthesisResultHoverMenu description elementPath exp nextMenu =
  generalHoverMenu
    description
    (Controller.msgHoverSynthesisResult elementPath)
    (Controller.msgHoverSynthesisResult <| allButLast elementPath)
    (Controller.msgSelectSynthesisResult exp)
    False
    nextMenu

synthesisResultsSelect : Model -> Html Msg
synthesisResultsSelect model =
  let
    desc description exp isSafe sortKey =
      (if isSafe then "" else "[UNSAFE] ") ++
      (Regex.replace Regex.All (Regex.regex "^Original -> | -> Cleaned$") (\_ -> "") description) ++
      " (" ++ toString (LangTools.nodeCount exp) ++ ")" ++ " " ++ toString sortKey
    resultButtonList priorPathByIndices remainingPathByIndices results =
      results
        |> Utils.mapi0
             ( \( i
                , Model.SynthesisResult
                    { description, exp, isSafe, sortKey, children }
                ) ->
                  let
                    thisElementPath =
                      priorPathByIndices ++ [i]
                    nextMenu =
                      case remainingPathByIndices of
                        nexti::is ->
                          if i == nexti then
                            case children of
                              Just childResults ->
                                resultButtonList
                                  thisElementPath
                                  is
                                  childResults
                              Nothing ->
                                []
                          else
                            []
                        [] ->
                          []
                  in
                    [ synthesisResultHoverMenu
                        (desc description exp isSafe sortKey)
                        thisElementPath
                        exp
                        nextMenu
                    ]
              )
        |> List.concat
  in
    Html.div
      [ Attr.class "synthesis-results"
      ]
      ( resultButtonList
          []
          model.hoveredSynthesisResultPathByIndices
          model.synthesisResults
      )

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
    undoButton =
      let
        past =
          Tuple.first model.history
        attributes =
          case past of
            _ :: prevCode :: _ ->
              [ E.onMouseEnter <| Controller.msgPreview (Right prevCode)
              , E.onMouseLeave Controller.msgClearPreview
              ]
            _ ->
             []
      in
        textButton
          { defaultTb
              | attributes = attributes
              , content = [Html.text "Undo"]
              , onClick = Controller.msgUndo
              , disabled = List.length past <= 1
          }
    redoButton =
      let
        future =
          Tuple.second model.history
        attributes =
          case future of
            futureCode :: _ ->
              [ E.onMouseEnter <| Controller.msgPreview (Right futureCode)
              , E.onMouseLeave Controller.msgClearPreview
              ]
            _ ->
             []
      in
        textButton
          { defaultTb
              | attributes = attributes
              , content = [Html.text "Redo"]
              , onClick = Controller.msgRedo
              , disabled = List.length future == 0
          }
    cleanButton =
      let
        disabled =
          case model.mode of
            Live _ -> False
            _      -> True
      in
        disableableTextButton disabled "Clean Up" Controller.msgCleanCode
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
        [ undoButton
        , redoButton
        , cleanButton
        , runButton
        ]
    editor =
      Html.div
        [ Attr.id "editor"
        , onRightClick <|
            Controller.msgDeuceRightClick ShowPossible
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
        , Html.div
            [ Attr.class "needs-run-light"
            ]
            []
        ]
  in
    Html.div
      [ Attr.class "panel code-panel"
      , Attr.style
          [ ("left", (px << .x) <| SleekLayout.codePanel model)
          , ("top", (px << .y) <| SleekLayout.codePanel model)
          , ("width", (px << .width) <| SleekLayout.codePanel model)
          , ("height", (px << .height) <| SleekLayout.codePanel model)
          ]
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
        [ ("width", (px << .width) SleekLayout.spacing) ]
    ]
    []

--------------------------------------------------------------------------------
-- Output Panel
--------------------------------------------------------------------------------

textOutput : String -> Html Msg
textOutput text =
  Html.textarea
    [ Attr.class "text-output"
    , Attr.readonly True
    ]
    [ Html.text text
    ]

outputPanel : Model -> Html Msg
outputPanel model =
  let
    dim =
      SleekLayout.outputCanvas model
    output =
      case (model.errorBox, model.mode, model.preview) of
        (_, _, Just (_, Err errorMsg)) ->
          textOutput errorMsg
        (_, _, Just (_, Ok _)) ->
          Canvas.build dim.width dim.height model
        (Just errorMsg, _, Nothing) ->
          textOutput errorMsg
        (Nothing, Print svgCode, Nothing) ->
          textOutput svgCode
        (Nothing, _, _) ->
          Canvas.build dim.width dim.height model
    outputPanelWarning =
      Html.div
        [ Attr.class "output-panel-warning"
        , Attr.style
            [ ("top", (px << negate) SleekLayout.panelBorderWidth)
            , ("right", (px << negate) SleekLayout.panelBorderWidth)
            , ("bottom", (px << negate) SleekLayout.panelBorderWidth)
            , ("left", (px << negate) SleekLayout.panelBorderWidth)
            ]
        ]
        []
  in
    Html.div
      [ Attr.class "panel output-panel"
      , Attr.style
          [ ("left", (px << .x) <| SleekLayout.outputPanel model)
          , ("top", (px << .y) <| SleekLayout.outputPanel model)
          , ("width", (px << .width) <| SleekLayout.outputPanel model)
          , ("height", (px << .height) <| SleekLayout.outputPanel model)
          ]
      ]
      [ output
      , outputPanelWarning
      ]

--------------------------------------------------------------------------------
-- Tool Panel
--------------------------------------------------------------------------------

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
      case Dict.get (Utils.naturalToCamelCase iconName) model.icons of
        Just h -> h
        Nothing -> Html.text ""
  in
  let commonAttrs =
    [ Attr.disabled disabled
    , Attr.class "icon-button"
    , Attr.style
        [ ("width", (px << .width) SleekLayout.iconButton)
        , ("height", (px << .height) SleekLayout.iconButton)
        , ("background", color)
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

toolModeIndicator : Model -> Html Msg
toolModeIndicator model =
  let
    toolModeDisplay mode modeText =
      let
        flag =
          if model.toolMode == mode then
             " active"
          else
            ""
      in
        Html.div
          [ Attr.class <| "tool-mode" ++ flag
          , E.onClick <| Controller.msgSetToolMode mode
          ]
          [ Html.text modeText
          ]
  in
    Html.div
      [ Attr.class "tool-mode-indicator"
      ]
      [ toolModeDisplay Raw "Raw"
      , toolModeDisplay Stretchy "Stretchy"
      , toolModeDisplay Sticky "Sticky"
      ]

toolPanel : Model -> Html Msg
toolPanel model =
  let
    toolSeparator =
      Html.div
        [ Attr.class "tool-separator" ]
        []
  in
    Html.div
      [ Attr.class "panel tool-panel"
      , Attr.style
          [ ("width", (px << .width) SleekLayout.toolPanel)
          , ("right", (px << .right) SleekLayout.toolPanel)
          , ("marginLeft", (px << .marginLeft) SleekLayout.toolPanel)
          ]
      ]
      ( [ toolButton model Cursor
        , toolButton model Text
        , toolButton model (Line model.toolMode)
        , toolButton model (Rect model.toolMode)
        , toolButton model (Oval model.toolMode)
        , toolButton model (Poly model.toolMode)
        , toolButton model (Path model.toolMode)
        ]
        ++ (lambdaTools model) ++
        [ toolModeIndicator model
        ]
      )

--------------------------------------------------------------------------------
-- Synthesis Panel
--------------------------------------------------------------------------------

synthesisAutoSearch : Model -> (List (Html Msg))
synthesisAutoSearch model =
  if List.length model.synthesisResults > 0 then
     [ Html.div
         [ Attr.class "synthesis-auto-search"
         ]
         [ synthesisResultsSelect model
         ]
     ]
  else
    []

synthesisPanel : Model -> Html Msg
synthesisPanel model =
  Html.div
    [ Attr.class "synthesis-panel-wrapper"
    , Attr.style
        [ ( "bottom", (px << .bottom) <| SleekLayout.synthesisPanel model)
        , ( "height", (px << .height) <| SleekLayout.synthesisPanel model)
        ]
    ]
    [ Html.div
        [ Attr.class "panel synthesis-panel"
        ]
        [ Html.div
            [ Attr.class "dropdown-content synthesis-menu-holder"
            ]
            [ synthesisResultsSelect model
            ]
        ]
    ]

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
    [ Html.div
        [ Attr.class "main-panels"
        ]
        [ codePanel model
        , outputPanel model
        , toolPanel model
        ]
    , synthesisPanel model
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
smallWideDialogBox = dialogBox "101" "50%" "35%"

fileNewDialogBox model =
  let
    viewTemplate (name, _) =
      styledUiButton
        "wide"
        name
        (Controller.msgAskNew name model.needsSave)
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
      [Html.text "New from Template..."]
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
          [ Attr.class "open-listing"
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

helpDialogBox model helpInfo =
  smallWideDialogBox
    True
    (Help helpInfo)
    model
    []
    [ case helpInfo of
        HelpTextSelectMode -> Html.text "Text Select Mode..."
        HelpBoxSelectMode  -> Html.text "Box Select Mode..."
    ]
    []
    [ Html.div
        [ Attr.class "centered"
        ]
        ( let makeList items =
            Html.ol [] (List.map (\s -> Html.li [] [Html.text s]) items)
          in
          case helpInfo of
            HelpTextSelectMode ->
              [ makeList
                  [ "Text select something in the code."
                  , "Select a tool from either the Edit Code menu or the right-click pop-up menu."
                  , "Follow any instructions and finish."
                  ]
              , Html.br [] []
              , Html.text "The Escape key resets all selections and tools."
              ]
            HelpBoxSelectMode ->
              [ makeList
                  [ "Hold down Shift, and hover and click boxes."
                  , "Select a tool from pop-up menu."
                  , "Follow any instructions and finish."
                  ]
              , Html.br [] []
              , Html.text "The Escape key deselects all selected boxes."
              ]
        )
    ]

dialogBoxes : Model -> (List (Html Msg))
dialogBoxes model =
  [ fileNewDialogBox model
  , fileSaveAsDialogBox model
  , fileOpenDialogBox model
  , alertSaveDialogBox model
  , importCodeDialogBox model
  , helpDialogBox model HelpTextSelectMode
  , helpDialogBox model HelpBoxSelectMode
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
-- Deuce
--------------------------------------------------------------------------------

deuceOverlay : Model -> Html Msg
deuceOverlay model =
  let
    pointerEvents =
      if Model.deuceActive model then
        "auto"
      else
        "none"
    disabledFlag =
      case model.preview of
        Just _ ->
          " disabled"
        Nothing ->
          ""
  in
    Html.div
      [ Attr.class <| "deuce-overlay-container" ++ disabledFlag
      , Attr.style
          [ ("pointer-events", pointerEvents)
          , ("top", px model.codeBoxInfo.scrollerTop)
          , ("left", px model.codeBoxInfo.scrollerLeft)
          , ("width", px model.codeBoxInfo.scrollerWidth)
          , ("height", px model.codeBoxInfo.scrollerHeight)
          ]
      ]
      [ Svg.svg
          [ SAttr.class "deuce-overlay"
          , SAttr.width "10000000"
          , SAttr.height "10000000"
          , SAttr.style << styleListToString <|
              [ ("top", px -model.codeBoxInfo.scrollTop)
              , ("left", px -model.codeBoxInfo.scrollLeft)
              ]
          ]
          [ Deuce.overlay model
          ]
      ]

--------------------------------------------------------------------------------
-- Deuce Right Click Menu
--------------------------------------------------------------------------------
-- NOTE: This is very similar to the "Edit Code" menu.
--------------------------------------------------------------------------------

deuceRightClickMenuEntry : Model -> (Int, CachedDeuceTool) -> List (Html Msg)
deuceRightClickMenuEntry model (_, ((deuceTool, _, _) as cachedDeuceTool)) =
  let
    title =
      deuceTool.name ++ "..."
    disabled =
      List.any Model.predicateImpossible deuceTool.reqs
  in
    if disabled then
      []
    else
      [ simpleTextButton
          title
          (Controller.msgSetSelectedDeuceTool cachedDeuceTool)
      ]

deuceRightClickMenu : Model -> Html Msg
deuceRightClickMenu model =
  let
    atLeastOneWidgetSelected =
      not <| List.isEmpty model.deuceState.selectedWidgets
    (disabledFlag, position) =
      case
        ( model.showDeuceRightClickMenu
        , model.deuceRightClickMenu
        )
      of
        (True, Just (pos, _)) ->
          ("", pos)
        _ ->
          (" disabled", { x = 0, y = 0 })
  in
    Html.div
      [ Attr.class <| "deuce-right-click-menu" ++ disabledFlag
      , Attr.style
          [ ("left", px <| position.x)
          , ("top", px <| position.y)
          ]
      ]
      [ menuOptions <|
          List.singleton <|
            List.concat <|
              List.concatMap
                (Utils.mapi1 <| deuceRightClickMenuEntry model)
                model.deuceToolsAndResults
      ]

--------------------------------------------------------------------------------
-- Popup Panels
--------------------------------------------------------------------------------

popupPanel
  :  { pos : (Int, Int)
     , disabled : Bool
     , dragHandler : Msg
     , class : String
     , title : String
     , content : List (Html Msg)
     }
  -> Html Msg
popupPanel args =
  let
    disabledFlag =
      if args.disabled then
        "disabled "
      else
        ""
    dragger =
      [ Html.div
          [ Attr.class "dragger"
          , E.onMouseDown args.dragHandler
          ]
          [ Html.text args.title
          ]
      ]
    (xString, yString) =
      Utils.mapBoth px args.pos
  in
    Html.div
      [ Attr.class <|
          "popup-panel panel " ++ disabledFlag ++ args.class
      , Attr.style
          [ ("left", xString)
          , ("top", yString)
          ]
      ] <|
      dragger ++ args.content

--------------------------------------------------------------------------------
-- Deuce Popup Panel
--------------------------------------------------------------------------------

deucePopupPanel : Model -> Html Msg
deucePopupPanel model =
  popupPanel
    { pos =
        model.popupPanelPositions.deuce
    , disabled =
        (not model.showDeucePanel) ||
        (Model.noWidgetsSelected model)
    , dragHandler =
        Controller.msgDragDeucePopupPanel
    , class =
        ""
    , title =
        "Deuce Menu"
    , content =
        [ Html.div
            [] <|
            List.concatMap
              (    List.filter (Utils.fst3 >> DeuceTools.isActive model)
                >> Utils.mapi1 (deuceHoverMenu model)
              )
              model.deuceToolsAndResults
        ]
    }

--------------------------------------------------------------------------------
-- Edit Code Panel
--------------------------------------------------------------------------------

editCodePopupPanel : Model -> Html Msg
editCodePopupPanel model =
  let
    (disabled, title, content) =
      case model.selectedDeuceTool of
        Nothing ->
          (True, "Edit Code Menu", [])
        Just (deuceTool, results, _) ->
          ( False
          , deuceTool.name
          , let
              path =
                [ 1 ] -- TODO, maybe?
              isRenamer =
                DeuceTools.isRenamer deuceTool
            in
              [ Html.h2
                  []
                  [ Html.text "Requirements" ]
              , Html.ul
                  [ Attr.class "requirements-list"
                  ]
                  ( List.map
                      ( \{description, value} ->
                          let
                            class =
                              case value of
                                FullySatisfied ->
                                  "fully-satisfied"
                                Satisfied ->
                                  "satisfied"
                                Possible ->
                                  "possible"
                                Impossible ->
                                  "impossible"
                          in
                            Html.li
                              [ Attr.class class
                              ]
                              [ Html.text description
                              ]
                      )
                      deuceTool.reqs
                  )
              ] ++
              ( if List.all Model.predicateSatisfied deuceTool.reqs then
                  [ Html.h2
                      []
                      [ Html.text "Code Updates" ]
                  , Html.div
                      [ Attr.class "synthesis-results"
                      ] <|
                      deuceSynthesisResults model path isRenamer results
                  ]
                else
                  []
              )
          )
  in
    popupPanel
      { pos =
          model.popupPanelPositions.editCode
      , disabled =
          disabled
      , dragHandler =
          Controller.msgDragEditCodePopupPanel
      , class =
          "edit-code-popup-panel"
      , title =
          title
      , content =
          content
      }

--------------------------------------------------------------------------------
-- All Popup Panels
--------------------------------------------------------------------------------

popupPanels : Model -> List (Html Msg)
popupPanels model =
  [ deucePopupPanel model
  , editCodePopupPanel model
  ]

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
        , deuceOverlay model
        , deuceRightClickMenu model
        ]
        ++ (popupPanels model)
        ++ [subtleBackground]
        ++ (dialogBoxes model)
      )

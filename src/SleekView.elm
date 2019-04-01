module SleekView exposing (view)

import List
import Dict
import Set
import Regex
import String
import Time

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events as E
import Json.Decode as Json
import Json.Encode as JsonE
import Svg
import Svg.Attributes as SAttr

import ImpureGoodies

import Utils
import HtmlUtils exposing (..)
import Either exposing (..)
import Updatable
import History

import InterfaceModel as Model exposing (..)

import InterfaceController as Controller
import Keys
import ExamplesGenerated as Examples

import Deuce
import DeuceTools

import OutputTools exposing (OutputTool)

import SleekLayout exposing (px, half)
import Canvas
import Draw
import Sync
import Lang exposing (Exp)
import LangTools
import Syntax
import File
import Eval

import DeuceWidgets exposing (..)
import Config exposing (params)

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
relateDisabled model = nothingSelectedInOutput model

groupDisabled : Bool -> Model -> Bool
groupDisabled disallowSelectedFeatures model =
  let
    noFeatures =
      List.isEmpty model.selectedFeatures
    noBlobs =
      Dict.isEmpty model.selectedBlobs
  in
    noBlobs || (disallowSelectedFeatures && (not noFeatures))

italicizeQuotes : String -> String -> List (Html Msg)
italicizeQuotes quoteString text =
  let
    splitString =
      String.split quoteString text
    mapper (i, s) =
      if Utils.isEven i then
        Html.text s
      else
        Html.i [] [ Html.text s ]
  in
    Utils.mapi0 mapper splitString

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

simpleHtmlTextButton : List (Html Msg) -> Html Msg
simpleHtmlTextButton content =
  textButton
    { defaultTb
        | content = content
    }

logMouseOver itemDescription =
  [ E.onMouseOver (Msg ("Hover " ++ itemDescription) identity)
  , E.onMouseOut  (Msg ("Leave " ++ itemDescription) identity)
  ]

disableableTextButton : Bool -> String -> Msg -> Html Msg
disableableTextButton disabled title onClick =
  textButton
    { defaultTb
        | content = [ Html.text title ]
        , onClick = onClick
        , disabled = disabled
        , attributes = logMouseOver ("Button \"" ++ title ++ "\"")
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
          , attributes = logMouseOver ("Radio Button \"" ++ radioButtonIcon ++ " " ++ title ++ "\"")
      }


simpleBooleanButton : Bool -> String -> (Bool -> Msg) -> Html Msg
simpleBooleanButton isOn string handler =
  simpleTextButton
    ((if isOn then "✔︎ " else "") ++ string)
    (handler (not isOn))


-- booleanOption : Bool -> String -> String -> (Bool -> Msg) -> List (Html Msg)
-- booleanOption test onString offString handler =
--   [ simpleTextRadioButton
--       test
--       onString
--       (handler True)
--   , simpleTextRadioButton
--       (not test)
--       offString
--       (handler False)
--   ]

-- Logic moved to OutputTools
-- relateTextButton : Model -> String -> Msg -> Html Msg
-- relateTextButton model text onClickHandler =
--   let
--     noFeatures =
--       Set.isEmpty model.selectedFeatures
--   in
--     disableableTextButton noFeatures text onClickHandler

-- Logic moved to OutputTools
-- groupTextButton : Model -> String -> Msg -> Bool -> Html Msg
-- groupTextButton model text onClickHandler disallowSelectedFeatures =
--   let
--     noFeatures =
--       Set.isEmpty model.selectedFeatures
--     noShapes =
--       Set.isEmpty model.selectedShapes
--     noBlobs =
--       Dict.isEmpty model.selectedBlobs
--   in
--     disableableTextButton
--       ((noBlobs && noShapes && noFeatures) || (disallowSelectedFeatures && (not noFeatures)))
--       text
--       onClickHandler

-- UI Buttons

closeUiButton : Msg -> Html Msg
closeUiButton =
  styledUiButton "close" ""

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

type alias RevealInfo
  = { shown : Int, extra : Int, max : Int }

type RevealAmount
  = Reveal RevealInfo
  | RevealAll

currentMenuAmount : RevealInfo -> Int
currentMenuAmount { shown, extra } =
  shown + extra

revealer : RevealInfo -> Html Msg
revealer revealInfo =
  let remainingCount = revealInfo.max - currentMenuAmount revealInfo in
  generalHtmlHoverMenu "revealer"
    ( [ Html.span
          []
          [ Html.text <| "Show " ++ toString remainingCount ++ " More" ]
      ]
    )
    RevealAll
    Controller.msgNoop
    Controller.msgNoop
    (Controller.msgIncreaseExtraMenuAmount 10)
    False
    []

generalHtmlHoverMenu
  : String -> List (Html Msg) -> RevealAmount -> Msg -> Msg -> Msg -> Bool -> List (Html Msg) -> Html Msg
generalHtmlHoverMenu
  class titleHtml revealAmount onMouseEnter onMouseLeave onClick disabled dropdownContent =
    let
      (disabledFlag, realOnMouseEnter, realOnMouseLeave, realOnClick) =
        if disabled then
          ( "disabled "
          , Controller.msgNoop
          , Controller.msgNoop
          , Controller.msgNoop
          )
        else
          ( ""
          , onMouseEnter
          , onMouseLeave
          , onClick
          )

      revealContent =
        case revealAmount of
          RevealAll ->
            []

          Reveal revealInfo ->
            if currentMenuAmount revealInfo < revealInfo.max then
              [ revealer revealInfo ]
            else
              []
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
            ( dropdownContent
                ++ revealContent
            )
        ]

generalHoverMenu
  : String -> RevealAmount -> Msg -> Msg -> Msg -> Bool -> List (Html Msg) -> Html Msg
generalHoverMenu titleString =
  generalHtmlHoverMenu "" [ Html.text titleString ]

hoverMenu : String -> (List (Html Msg)) -> Html Msg
hoverMenu title dropdownContent =
  generalHoverMenu
    title
    RevealAll
    Controller.msgNoop
    Controller.msgNoop
    Controller.msgNoop
    False
    dropdownContent

synthesisHoverMenu : Model -> String -> String -> Msg -> Bool -> Html Msg
synthesisHoverMenu model resultsKey title onMouseEnter disabled =
  let
    cached =
      Dict.member resultsKey model.synthesisResultsDict

    results =
      synthesisResultsSelect model resultsKey

    max =
      List.length results

    revealDisabled =
      [ "Reorder in List"
      ]

    shownCount =
      if List.member resultsKey revealDisabled then
        max
      else
        1

    revealInfo =
      { shown = shownCount
      , extra = model.extraMenuAmount
      , max = max
      }
  in
    generalHoverMenu
      ( if cached -- if desired, can indicate whether this tool has been run and cached
          then title -- ++ " ✓"
          else title
      )
      ( Reveal revealInfo
      )
      ( if cached
          then Controller.msgNoop
          else onMouseEnter
      )
      Controller.msgClearExtraMenuAmount
      Controller.msgNoop
      disabled
      [ wrapSynthesisResultsSelect (List.take (currentMenuAmount revealInfo) results)
      ]

-- relateHoverMenu : Model -> String -> String -> Msg -> Html Msg
-- relateHoverMenu model resultsKey title onMouseEnter =
--   synthesisHoverMenu
--     model
--     resultsKey
--     title
--     onMouseEnter
--     (relateDisabled model)

-- groupHoverMenu : Model -> String -> Msg -> Bool -> Html Msg
-- groupHoverMenu model title onMouseEnter disallowSelectedFeatures =
--   synthesisHoverMenu
--     model
--     ""
--     title
--     onMouseEnter
--     (groupDisabled disallowSelectedFeatures model)

deuceSynthesisResult : Model -> List Int -> Bool -> SynthesisResult -> Html Msg
deuceSynthesisResult model path isRenamer (SynthesisResult result) =
  let
    alreadyRun =
      Dict.member path model.deuceToolResultPreviews

    class =
      case Dict.get path model.deuceToolResultPreviews of
        Nothing -> -- tool result Exp has not yet been run and cached
          if isRenamer then ""
          else if result.isSafe then "expected-safe"
          else "expected-unsafe"

        Just (_, class) ->
          class

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
    description =
      if isRenamer then
        italicizeQuotes "'" result.description
      else
        [ Html.text <|
            -- if desired, can indicate whether this tool has been run and cached
            if alreadyRun
              then result.description -- ++ " ✓"
              else result.description
        ]
  in
    generalHtmlHoverMenu class
      ( [ Html.span
            []
            description
        ] ++ additionalInputs
      )
      RevealAll
      (Controller.msgHoverDeuceResult isRenamer (SynthesisResult result) path)
      (Controller.msgLeaveDeuceResult (SynthesisResult result) path)
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
        RevealAll
        Controller.msgNoop
        Controller.msgNoop
        Controller.msgNoop
        True
        []
    ]
  else
    Utils.mapi1
      (\(i, result) -> deuceSynthesisResult model (path ++ [i]) isRenamer result)
      results

deuceHoverMenu : Model -> (Int, CachedDeuceTool) -> Html Msg
deuceHoverMenu model (index, (deuceTool, results, disabled)) =
  let
    path =
      [ index ]
    isRenamer =
      DeuceTools.isRenamer deuceTool
    title =
      if isRenamer then
        italicizeQuotes "'" deuceTool.name
      else
        [ Html.text deuceTool.name
        ]
  in
    generalHtmlHoverMenu
      ""
      title
      RevealAll
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
    name =
      deuceTool.name ++ "..."
    isRenamer =
      DeuceTools.isRenamer deuceTool
    title =
      if isRenamer then
        italicizeQuotes "'" name
      else
        [ Html.text name
        ]
    disabled =
      (List.any Model.predicateImpossible deuceTool.reqs) ||
      (not <| Model.noCodeWidgetsSelected model)
  in
    textButton
      { defaultTb
          | content =
              title
          , disabled =
              disabled
          , onClick =
              (Controller.msgSetSelectedDeuceTool True cachedDeuceTool)
          , attributes = logMouseOver ("Edit Code Top Menu Item \"" ++ name ++ "\"")
      }

outputToolEntry : Model -> OutputTool -> Html Msg
outputToolEntry model tool =
  let
    prettyName =
      case tool.shortcut of
        Just shortcut ->
          tool.name ++ " (⌘" ++ shortcut ++ ")"
        _ ->
          tool.name

    (disabled, action) =
      case tool.func of
        Just msg ->
          (not <| List.all Model.predicateSatisfied tool.reqs, msg)

        Nothing ->
          (True, Controller.msgNoop)
  in
    case tool.kind of
      OutputTools.Single ->
        disableableTextButton
          disabled
          prettyName
          action

      OutputTools.Multi ->
        synthesisHoverMenu
          model
          tool.name
          prettyName
          action
          disabled

menuHeading : String -> Html Msg
menuHeading heading =
  let attributes =
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
    ] ++
    logMouseOver ("Menu \"" ++ heading ++ "\"")
  in
  Html.div
    attributes
    [ Html.text heading ]

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

    logo =
      Html.img
        [ Attr.class "logo-image"
        , Attr.src <|
            case model.colorScheme of
              Light ->
                "img/sketch-n-sketch-logo.png"
              Dark ->
                "img/light_logo.svg"
        , Attr.width 20
        , Attr.height 20
        ]
        []

    snsMenu =
      menu "Sketch-n-Sketch"
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

    fileMenu =
      menu "File"
        [ [ simpleTextButton "New" <|
              Controller.msgAskNew Model.blankTemplate model.needsSave
          , simpleTextButton "New From Template..." <|
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

    maybeCodeToolsMenu =
      let
        maybeEntry =
          case model.codeToolsMenuMode of
            CTAll ->
              Just <| editCodeEntry model
            CTActive ->
              Just <| deuceHoverMenu model
            CTDisabled ->
              Nothing
      in
        case maybeEntry of
          Just entry ->
            [ menu "Code Tools" <| -- "Edit Code"
                List.map
                  (Utils.mapi1 entry)
                  model.deuceToolsAndResults
            ]
          Nothing ->
            []

--    outputToolsMenu =
--      menu "Output Tools"
--        [ [ relateHoverMenu
--              model
--              "Make Equal"
--              "Make Equal (⌘E)"
--              Controller.msgMakeEqual
--          , relateHoverMenu
--              model
--              "Relate"
--              "Relate"
--              Controller.msgRelate
--          , relateHoverMenu
--              model
--              "Indexed Relate"
--              "Indexed Relate"
--              Controller.msgIndexedRelate
--          , relateHoverMenu
--              model
--              "Abstract"
--              "Abstract"
--              Controller.msgAbstract
--          ]
--        , [ groupTextButton
--              model
--              "Dupe (⌘D)"
--              Controller.msgDuplicate
--              False
--          , groupTextButton
--              model
--              "Merge"
--              Controller.msgMergeBlobs
--              True
--          , groupTextButton
--              model
--              "Group (⌘G)"
--              Controller.msgGroupBlobs
--              False
--          , groupTextButton
--              model
--              "Abstract"
--              Controller.msgAbstractBlobs
--              True
--          ]
--        ]

    outputToolsMenu =
      menu "Output Tools" <|
        List.map (List.map <| outputToolEntry model) <|
          OutputTools.tools model

    viewMenu =
      menu "View" <|
        [ [ hoverMenu "Font Size"
              [ simpleTextRadioButton
                  ( case model.codeBoxInfo.fontSize of
                      8 ->
                        True
                      _ ->
                        False
                  )
                  "8"
                  (Controller.msgUpdateFontSize 8)
              , simpleTextRadioButton
                  ( case model.codeBoxInfo.fontSize of
                      10 ->
                        True
                      _ ->
                        False
                  )
                  "10"
                  (Controller.msgUpdateFontSize 10)
              , simpleTextRadioButton
                  ( case model.codeBoxInfo.fontSize of
                      12 ->
                        True
                      _ ->
                        False
                  )
                  "12"
                  (Controller.msgUpdateFontSize 12)
              , simpleTextRadioButton
                  ( case model.codeBoxInfo.fontSize of
                      14 ->
                        True
                      _ ->
                        False
                  )
                  "14"
                  (Controller.msgUpdateFontSize 14)
              , simpleTextRadioButton
                  ( case model.codeBoxInfo.fontSize of
                      16 ->
                        True
                      _ ->
                        False
                  )
                  "16"
                  (Controller.msgUpdateFontSize 16)
              , simpleTextRadioButton
                  ( case model.codeBoxInfo.fontSize of
                      18 ->
                        True
                      _ ->
                        False
                  )
                  "18"
                  (Controller.msgUpdateFontSize 18)
              , simpleTextRadioButton
                  ( case model.codeBoxInfo.fontSize of
                      20 ->
                        True
                      _ ->
                        False
                  )
                  "20"
                  (Controller.msgUpdateFontSize 20)
              , simpleTextRadioButton
                  ( case model.codeBoxInfo.fontSize of
                      22 ->
                        True
                      _ ->
                        False
                  )
                  "22"
                  (Controller.msgUpdateFontSize 22)
              , simpleTextRadioButton
                  ( case model.codeBoxInfo.fontSize of
                      24 ->
                        True
                      _ ->
                        False
                  )
                  "24"
                  (Controller.msgUpdateFontSize 24)
              ]
          , hoverMenu "Color Scheme"
              [ simpleTextRadioButton
                  ( case model.colorScheme of
                      Light ->
                        True
                      _ ->
                        False
                  )
                  "Light"
                  (Controller.msgSetColorScheme Light)
              , simpleTextRadioButton
                  ( case model.colorScheme of
                      Dark ->
                        True
                      _ ->
                        False
                  )
                  "Dark"
                  (Controller.msgSetColorScheme Dark)
              ]
          ]
        , [ disableableTextButton True "Main Layer" Controller.msgNoop
          , disableableTextButton True "Widget Layer" Controller.msgNoop
          , simpleBooleanButton model.showGhosts "Show Widgets" Controller.msgSetGhostsShown
          , simpleBooleanButton model.showPreludeOffsets "Show Offset Widgets from Prelude" Controller.msgSetPreludeOffsetsShown
          , hoverMenu "Output Type"
              [ simpleTextRadioButton
                  ( case model.outputMode of
                      Live ->
                        True
                      _ ->
                        False
                  )
                  "Graphics"
                  Controller.msgSetOutputLive
              , simpleTextRadioButton
                  ( case model.outputMode of
                      Print _ ->
                        True
                      _ ->
                        False
                  )
                  "Text"
                  Controller.msgSetOutputPrint
              -- , simpleTextRadioButton
              --     ( case model.outputMode of
              --         ShowValue ->
              --           True
              --         _ ->
              --           False
              --     )
              --     "Value Editor"
              --     Controller.msgSetOutputShowValue
              ]
          ]
        , [ simpleTextButton
              "Reset Interface Layout"
              Controller.msgResetInterfaceLayout
          ]
        ]

    optionsMenu =
      menu "Options" <|
        [ [ hoverMenu "Auto-Run"
              [ disableableTextButton
                  True "Every second" Controller.msgNoop
              , disableableTextButton
                  True "Every 2 seconds" Controller.msgNoop
              , disableableTextButton
                  True "Every 3 seconds" Controller.msgNoop
              ]
          ]
        , [ simpleBooleanButton (Updatable.extract model.enableTextEdits) "Enable Text Edits" Controller.msgSetEnableTextEdits
        -- hoverMenu "Enable Text Edits" <|
        --       booleanOption
        --         (Updatable.extract model.enableTextEdits)
        --         "True"
        --         "False"
        --         Controller.msgSetEnableTextEdits
          , simpleBooleanButton model.enableDeuceBoxSelection "Enable Deuce Box Selection" Controller.msgSetEnableDeuceBoxSelection
          -- hoverMenu "Enable Deuce Box Selection" <|
          --     booleanOption
          --       model.enableDeuceBoxSelection
          --       "True"
          --       "False"
          --       Controller.msgSetEnableDeuceBoxSelection
          , simpleBooleanButton model.enableDeuceTextSelection "Enable Deuce Text Selection" Controller.msgSetEnableDeuceTextSelection
          -- hoverMenu "Enable Deuce Text Selection" <|
          --     booleanOption
          --       model.enableDeuceTextSelection
          --       "True"
          --       "False"
          --       Controller.msgSetEnableDeuceTextSelection
          ]
        , [ hoverMenu "Code Tools Menu Mode"
              [ simpleTextRadioButton
                  ( case model.codeToolsMenuMode of
                      CTAll ->
                        True
                      _ ->
                        False
                  )
                  "All"
                  (Controller.msgSetCodeToolsMenuMode CTAll)
              , simpleTextRadioButton
                  ( case model.codeToolsMenuMode of
                      CTActive ->
                        True
                      _ ->
                        False
                  )
                  "Active"
                  ( Controller.msgSetCodeToolsMenuMode CTActive)
              , simpleTextRadioButton
                  ( case model.codeToolsMenuMode of
                      CTDisabled ->
                        True
                      _ ->
                        False
                  )
                  "Disabled"
                  (Controller.msgSetCodeToolsMenuMode CTDisabled)
              ]
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
          , simpleBooleanButton model.allowMultipleTargetPositions "Allow Multiple Target Positions" Controller.msgSetAllowMultipleTargetPositions
          -- hoverMenu "Allow Multiple Target Positions" <|
          --     booleanOption
          --       (model.allowMultipleTargetPositions)
          --       "True"
          --       "False"
          --       Controller.msgSetAllowMultipleTargetPositions
          ]
        , [ simpleBooleanButton model.autoSynthesis "Automatically Suggest Code Changes" Controller.msgSetAutoSynthesis
        --
        -- hoverMenu "Automatically Suggest Code Changes"
        --       [ simpleTextRadioButton
        --           model.autoSynthesis
        --           "On"
        --           Controller.msgStartAutoSynthesis
        --       , simpleTextRadioButton
        --           (not model.autoSynthesis)
        --           "Off"
        --           Controller.msgStopAutoSynthesisAndClear
        --       ]
          , hoverMenu "Output Synchronization"
              [ simpleTextRadioButton
                  (model.liveSyncDelay == False)
                  "Live"
                  (Controller.msgSetLiveSyncDelay False)
              , simpleTextRadioButton
                  (model.liveSyncDelay == True)
                  "Delayed"
                  (Controller.msgSetLiveSyncDelay True)
              ]
          , hoverMenu "Live Update Heuristics"
              [ simpleTextRadioButton
                  ( model.syncOptions.heuristicsMode ==
                      Sync.HeuristicsBiased
                  )
                  "Biased"
                  Controller.msgSetHeuristicsBiased
              , simpleTextRadioButton
                  ( model.syncOptions.heuristicsMode ==
                      Sync.HeuristicsNone
                  )
                  "None"
                  Controller.msgSetHeuristicsNone
              , simpleTextRadioButton
                  ( model.syncOptions.heuristicsMode ==
                      Sync.HeuristicsFair
                  )
                  "Fair"
                  Controller.msgSetHeuristicsFair
              ]
          ]
          , [ hoverMenu "Syntax"
                [ simpleTextRadioButton
                    ( case model.syntax of
                        Syntax.Elm ->
                          True
                        _ ->
                          False
                    )
                    "Elm"
                    (Controller.msgSetSyntax Syntax.Elm)
                , simpleTextRadioButton
                    ( case model.syntax of
                        Syntax.Little ->
                          True
                        _ ->
                          False
                    )
                    "Little"
                    (Controller.msgSetSyntax Syntax.Little)
                ]
            ]
        ]

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
          (List.concat
            [ [logo]
            , [snsMenu]
            , [fileMenu]
            , maybeCodeToolsMenu
            -- , [outputToolsMenu]
            , [viewMenu]
            , [optionsMenu]
            -- temporary hack: just moving Output Tools menu farther to the right
            , [Html.span [ Attr.style [ ("width", "200px") ] ] [ ]]
            , [outputToolsMenu]
            ]
          )

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
  : String -> String -> (List Int) -> Exp -> (List (Html Msg)) -> Html Msg
synthesisResultHoverMenu resultsKey description elementPath exp nextMenu =
  generalHoverMenu
    description
    RevealAll
    (Controller.msgHoverSynthesisResult resultsKey elementPath)
    (Controller.msgHoverSynthesisResult resultsKey <| allButLast elementPath)
    (Controller.msgSelectSynthesisResult exp)
    False
    nextMenu


wrappedSynthesisResultsSelect : Model -> String -> Html Msg
wrappedSynthesisResultsSelect model resultsKey =
  synthesisResultsSelect model resultsKey
    |> wrapSynthesisResultsSelect

wrapSynthesisResultsSelect : List (Html Msg) -> Html Msg
wrapSynthesisResultsSelect =
  Html.div
    [ Attr.class "synthesis-results"
    ]

synthesisResultsSelect : Model -> String -> List (Html Msg)
synthesisResultsSelect model resultsKey =
  let
    desc description exp isSafe sortKey =
      (if isSafe then "" else "[UNSAFE] ") ++
      (Regex.replace Regex.All (Regex.regex "^Original → | → Cleaned$") (\_ -> "") description) -- ++
      -- " (" ++ toString (LangTools.nodeCount exp) ++ ")" ++ " " ++ toString sortKey

    resultButtonList priorPathByIndices remainingPathByIndices results =
      if results == [] then
        [ disableableTextButton True "No Results" Controller.msgNoop ]
      else
        results
          |> Utils.mapi0
               (\(i, SynthesisResult { description, exp, isSafe, sortKey, children }) ->
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
                        resultsKey
                        (desc description exp isSafe sortKey)
                        thisElementPath
                        exp
                        nextMenu
                    ]
                )
          |> List.concat
  in
    case Dict.get resultsKey model.synthesisResultsDict of
      Just results ->
        resultButtonList
            []
            model.hoveredSynthesisResultPathByIndices
            results

      Nothing ->
        [ disableableTextButton True "Synthesizing... ⏳ ⏰ 👵🏽 👴🏼 ⚰️ 🏁" Controller.msgNoop ]

--------------------------------------------------------------------------------
-- Code Panel
--------------------------------------------------------------------------------

fileIndicator : Model -> Html Msg
fileIndicator model =
  let
    filenameHtml =
      Html.text <| Model.prettyFilename WithExtension model
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
      textButton
        { defaultTb
            | attributes = logMouseOver "Undo"
            , content = [Html.text "⟲ Undo"]
            , onClick = Controller.msgUndo
            , disabled = not <| History.hasExtendedPast model.history
        }
    redoButton =
      textButton
        { defaultTb
            | attributes = logMouseOver "Redo"
            , content = [Html.text "⟳ Redo"]
            , onClick = Controller.msgRedo
            , disabled = not <| History.hasFuture model.history
        }
    cleanButton =
      let
        disabled =
          case model.outputMode of
            Live -> False
            _    -> True
      in
        disableableTextButton disabled "Clean Up" Controller.msgCleanCode
    emoji =
      let
        happinessEmoji =
          if      model.runFailuresInARowCount <= -40 then "😎"
          else if model.runFailuresInARowCount <= -20 then "😍"
          else if model.runFailuresInARowCount <= -10 then "😁"
          else if model.runFailuresInARowCount <=  -5 then "😃"
          else if model.runFailuresInARowCount <=   0 then "🙂"
          else if model.runFailuresInARowCount <=   1 then "😐"
          else if model.runFailuresInARowCount <=   2 then "😬"
          else if model.runFailuresInARowCount <=   3 then "😕"
          else if model.runFailuresInARowCount <=   4 then "🙁"
          else if model.runFailuresInARowCount <=   5 then "☹️"
          else if model.runFailuresInARowCount <=   6 then "😔"
          else if model.runFailuresInARowCount <=   7 then "😣"
          else if model.runFailuresInARowCount <=   8 then "😖"
          else if model.runFailuresInARowCount <=   9 then "😫"
          else                                             "😡"
        emoji =
          if model.runFailuresInARowCount <= 3 && Model.needsRun model
          then "🤔"
          else happinessEmoji
      in
      Html.div
        [ Attr.class "emoji"
        ]
        [ Html.text emoji
        ]
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
        ] <|
        [ undoButton
        , redoButton
        , cleanButton
        ] ++
        if Updatable.extract model.enableTextEdits then
          [ runButton
          , emoji
          ]
        else
          []
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
        ] <|
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
      [ Attr.class "panel outlined code-panel"
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
-- Main Resizer
--------------------------------------------------------------------------------

mainResizer : Model -> Html Msg
mainResizer model =
  let
    mainResizerBB =
      SleekLayout.mainResizer model
  in
    Html.div
      [ Attr.class "resizer main-resizer"
      , Attr.style
          [ ("width", (px << .width) mainResizerBB)
          , ("height", (px << .height) mainResizerBB)
          , ("line-height", (px << .height) mainResizerBB)
          , ("left", (px << .x) mainResizerBB)
          , ("top", (px << .y) mainResizerBB)
          ]
      , E.onMouseDown Controller.msgDragMainResizer
      , E.onMouseUp Controller.msgClearDrag
      ]
      [ Html.text "⦀"
      ]

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
    contextLabel =
      Html.b
        []
        [ Html.text "Context: "
        ]
    canvasDim =
      SleekLayout.outputCanvas model
    contextBreadCrumbs =
      case model.editingContext of
        Nothing       -> [ contextLabel, Html.text "Program" ]
        Just (eid, _) ->
          let breadCrumbs = "Program" :: LangTools.expDescriptionParts model.inputExp eid in
          [ contextLabel, Html.text <| String.join " > " breadCrumbs ]
    output =
      case (model.errorBox, model.outputMode, model.preview) of
        (_, _, Just (_, Err errorMsg)) ->
          [textOutput errorMsg]
        (_, _, Just (_, Ok _)) ->
          Canvas.build canvasDim model
        (Just errorMsg, _, Nothing) ->
          [textOutput errorMsg]
        (Nothing, Print svgCode, Nothing) ->
          [textOutput svgCode]
        (Nothing, ShowValue, _) ->
          [ Html.textarea
              [ E.onInput (\s -> Msg "Update Value Editor" (\m -> { m | valueEditorString = s } ))
              , Attr.style -- TODO
                  [ ("font-size", "24px")
                  , ("width", "100%")
                  , ("height", "80%")
                  ]
              ]
              -- [ Html.text (Lang.strVal model.inputVal) ]
              [ -- let _ = Debug.log "valueEditorString" model.valueEditorString in
                Html.text model.valueEditorString
              ]
          , Html.button
              [ E.onClick Controller.msgCallUpdate
              , Attr.style -- TODO
                  [ ("font-size", "16px") ]
              ]
              [ Html.text "Update (⌘Enter)" ]
          ]
        (Nothing, _, _) ->
          Canvas.build canvasDim model
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
      [ Attr.class "panel outlined output-panel"
      , Attr.style
          [ ("left", (px << .x) <| SleekLayout.outputPanel model)
          , ("top", (px << .y) <| SleekLayout.outputPanel model)
          , ("width", (px << .width) <| SleekLayout.outputPanel model)
          , ("height", (px << .height) <| SleekLayout.outputPanel model)
          ]
      ]
      [ Html.div
          [ Attr.class "context-bar"
          ]
          contextBreadCrumbs
      , Html.div
          --
          -- Always create this div --- even when it's just showing a
          -- text box and not HTML/SVG nodes --- because outputCanvas.js
          -- looks for, and installs an onscroll handler for, this
          -- element just once. So, don't want this element to
          -- disappear/re-appear when just a text box is displayed.
          --
          [ Attr.id "outputCanvas"
          , Attr.style
              [ ("width", px canvasDim.width)
              , ("height", px canvasDim.height)
              ]
          ]
          output
      , outputPanelWarning
      ]

--------------------------------------------------------------------------------
-- Tool Panel
--------------------------------------------------------------------------------

type ButtonKind = Regular | Selected | Unselected

buttonRegularColor = "#FFFFFF"
buttonSelectedColor = "#DDDDDD"

iconButton model iconName maybeTitle onClickHandler btnKind disabled =
  iconButtonExtraAttrs model iconName maybeTitle [] onClickHandler btnKind disabled

iconButtonExtraAttrs model iconName maybeTitle extraAttrs onClickHandler btnKind disabled =
  let
    color =
      case btnKind of
        Regular    -> buttonRegularColor
        Unselected -> buttonRegularColor
        Selected   -> buttonSelectedColor
    iconHtml =
      case (Dict.get (Utils.naturalToCamelCase iconName) model.icons, Dict.get iconName model.icons) of
        (Just html, _) -> html
        (_, Just html) -> html
        _              -> Html.text ""
  in
  let commonAttrs =
    [ Attr.disabled disabled
    , Attr.class "icon-button"
    , Attr.style
        [ ("word-wrap", "break-word")
        , ("background", color)
        , ("overflow", "hidden")
        ]
    ]
  in
  Html.button
    (commonAttrs ++
      [ handleEventAndStop "mousedown" Controller.msgNoop
      , E.onClick onClickHandler
      , Attr.title (Maybe.withDefault iconName maybeTitle)
      ] ++
      extraAttrs)
    [ iconHtml
    , Html.span
        [ Attr.class "icon-button-label"
        ]
        [ Html.text iconName
        ]
    ]

toolButton model tool =
  let
    cap = case tool of
      Cursor ->
        "Cursor"
      PointOrOffset ->
        "Point or Offset"
      -- Line ->
      --   "Line"
      -- Rect ->
      --   "Rect"
      -- Oval ->
      --   "Ellipse"
      Poly ->
        "Polygon"
      Path ->
        "Path"
      Text ->
        "Text"
      HelperLine ->
        "(Rule)"
      Function fName ->
        fName

    btnKind = if model.tool == tool then Selected else Unselected
  in
    Html.div
      [ Attr.class "tool"
      ]
      [ iconButton
          model cap Nothing (Msg cap (\m -> { m | mouseMode = MouseNothing, tool = tool })) btnKind False
      ]

toolHeader : String -> Html Msg
toolHeader title =
  Html.div
    [ Attr.class "tool-header" ]
    [ Html.text title
    ]

functionTools : Model -> List (Html Msg)
functionTools model =
  model.drawableFunctions
  |> List.concatMap
      (\(funcName, funcType) ->
        ( -- Sad hack :(
          if funcName == "vec2DPlus" then
            [ toolHeader "Standard Library Tools" ]
          else
            []
        ) ++
        [ Html.div
            [ Attr.class "tool"
            ]
            [ iconButton model funcName (Just <| funcName ++ " : " ++ Syntax.typeWithRolesUnparser Syntax.Elm funcType)
                (Msg (funcName ++ " Function Tool") (\m -> { m | tool = Function funcName }))
                (if model.tool == Function funcName then Selected else Unselected)
                False
            ]
        ]
      )

toolPanel : Model -> Html Msg
toolPanel model =
  let
    toolSeparator =
      Html.div
        [ Attr.class "tool-separator" ]
        []
  in
    Html.div
      [ Attr.class "panel outlined tool-panel"
      , Attr.style
          [ ("width", (px << .width) SleekLayout.toolPanel)
          -- +35 for height of context bar
          , ("maxHeight", (px << (+) 35 << .height) (SleekLayout.outputCanvas model))
          , ("right", (px << .right) SleekLayout.toolPanel)
          , ("marginLeft", (px << .marginLeft) SleekLayout.toolPanel)
          ]
      ]
      ( [ toolHeader "Built-In Tools"
        , toolButton model Cursor
        , toolButton model PointOrOffset
        -- , toolButton model Text
        -- , toolButton model Line
        -- , toolButton model Rect
        -- , toolButton model Oval
        , toolButton model Poly
        -- , toolButton model Path
        , toolHeader "User-Defined Tools"
        ] ++
        functionTools model
      )

--------------------------------------------------------------------------------
-- Synthesis Panel
--------------------------------------------------------------------------------

synthesisAutoSearch : Model -> (List (Html Msg))
synthesisAutoSearch model =
  if List.length (Utils.getWithDefault "Auto-Synthesis" [] model.synthesisResultsDict) > 0 then
     [ Html.div
         [ Attr.class "synthesis-auto-search"
         ]
         [ wrappedSynthesisResultsSelect model "Auto-Synthesis"
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
        [ Attr.class "panel outlined synthesis-panel"
        ]
        [ Html.div
            [ Attr.class "dropdown-content synthesis-menu-holder"
            ]
            [ wrappedSynthesisResultsSelect model "Auto-Synthesis"
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
    ] <|
    [ Html.div
        [ Attr.class "main-panels"
        ] <|
        [ codePanel model
        , mainResizer model
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
        closeUiButton <|
          Controller.msgCloseDialogBox db
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
smallWideDialogBox = dialogBox "101" "60%" "35%"

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
      [Html.text "New From Template..."]
      []
      (List.map viewCategory Examples.templateCategories)

fileSaveAsDialogBox model =
  let
    saveAsInputHeader =
      Html.h2
        []
        [ Html.text "Filename (default "
        , Html.code [] [ Html.text ".elm" ]
        , Html.text ")"
        ]
    saveAsInput =
      Html.div
        [ Attr.class "save-as-input" ]
        [ Html.input
            [ Attr.type_ "text"
            , E.onInput Controller.msgUpdateFilenameInput
            ]
            []
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
      ([saveAsInputHeader, saveAsInput, currentFilesHeader] ++ (List.map viewFileIndexEntry model.fileIndex))

fileOpenDialogBox model =
  let fileOpenRow filename =
        Html.div
          [ Attr.class "open-listing"
          ]
          [ Html.span []
              [ Html.b [] [ Html.text filename.name ]
              , Html.text <|
                  "." ++ File.fileExtensionToString filename.extension
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
        [ Html.b [] [ Html.text filename.name ]
        , Html.text <|
            "." ++ File.fileExtensionToString filename.extension
        ]
    ]

alertSaveDialogBox model =
  smallDialogBox
    False
    AlertSave
    model
    []
    [ Html.span
        [ Attr.class "alert-warning" ]
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
                [ Html.text <| Model.prettyFilename WithExtension model ]
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
      , Attr.attribute "data-filename" (Model.prettyFilename WithExtension model)
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
          [ ( "pointer-events"
            , pointerEvents
            )
          , ( "top"
            , px model.codeBoxInfo.scrollerTop
            )
          , ( "left"
            , px <|
                model.codeBoxInfo.scrollerLeft - SleekLayout.deuceOverlayBleed
            )
          , ( "width"
            , px <|
                model.codeBoxInfo.scrollerWidth + SleekLayout.deuceOverlayBleed
            )
          , ( "height"
            , px model.codeBoxInfo.scrollerHeight
            )
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
    name =
      deuceTool.name ++ "..."
    isRenamer =
      DeuceTools.isRenamer deuceTool
    title =
      if isRenamer then
        italicizeQuotes "'" name
      else
        [ Html.text name
        ]
    disabled =
      List.any Model.predicateImpossible deuceTool.reqs
  in
    if disabled then
      []
    else
      [ textButton
          { defaultTb
              | content =
                  title
              , onClick =
                  Controller.msgSetSelectedDeuceTool False cachedDeuceTool
              , attributes =
                  logMouseOver <|
                    "Button (Deuce Right-Click Menu) \"" ++ name ++ "\""
          }
      ]

deuceRightClickMenu : Model -> Html Msg
deuceRightClickMenu model =
  let
    disabled =
      (not model.enableDeuceTextSelection) ||
      (not <| Model.deuceRightClickMenuShown model)
    content =
      [ Html.div
          [ Attr.class <| "deuce-right-click-menu"
          ] <|
          List.concat <|
            List.concatMap
              (Utils.mapi1 <| deuceRightClickMenuEntry model)
              model.deuceToolsAndResults
      ]
  in
    popupPanel
      { pos =
          model.popupPanelPositions.deuceRightClickMenu
      , disabled =
          disabled
      , dragHandler =
          Controller.msgDragDeuceRightClickMenu
      , class =
          ""
      , title =
          [ Html.text "Code Tools"
          ]
      , content =
          content
      }

--------------------------------------------------------------------------------
-- Popup Panels
--------------------------------------------------------------------------------

popupPanel
  :  { pos : (Int, Int)
     , disabled : Bool
     , dragHandler : Msg
     , class : String
     , title : List (Html Msg)
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
          , E.onMouseUp Controller.msgClearDrag
          ] <|
          args.title ++ [closeUiButton <| Controller.msgKeyDown Keys.keyEsc]
      ]
    (xString, yString) =
      Utils.mapBoth px args.pos
  in
    Html.div
      [ Attr.class <|
          "popup-panel panel outlined " ++ disabledFlag ++ args.class
      , Attr.style
          [ ("left", xString)
          , ("top", yString)
          ]
      ] <|
        dragger ++ args.content

--------------------------------------------------------------------------------
-- No Available Tools Helper
--------------------------------------------------------------------------------

noAvailableTools : Html Msg
noAvailableTools =
  Html.div
    [ Attr.class "no-available-tools"
    ]
    [ Html.text
        "There are no available tools based on these selections. Press "
    , Html.i []
        [ Html.text "Escape" ]
    , Html.text
        " to clear."
    ]

--------------------------------------------------------------------------------
-- Deuce Popup Panel
--------------------------------------------------------------------------------

deucePopupPanel : Model -> Html Msg
deucePopupPanel model =
  let
    appearDirectionFlag =
      if model.deucePopupPanelAbove then
        "appear-above"
      else
        "appear-below"
  in
    popupPanel
      { pos =
          model.popupPanelPositions.deuce
      , disabled =
          not <| Model.deucePopupPanelShown model
      , dragHandler =
          Controller.msgDragDeucePopupPanel
      , class =
          "deuce-popup-panel " ++ appearDirectionFlag
      , title =
          [ Html.text "Code Tools" -- "Deuce Menu"
          ]
      , content =
          [ let
              activeTools =
                model.deuceToolsAndResults
                  |> List.concatMap (List.filter (Utils.fst3 >> DeuceTools.isActive))
                  |> Utils.mapi1 (deuceHoverMenu model)

              maybeTypeString =
                case model.deuceState.selectedWidgets of
                  [DeuceExp eid]  -> Just <| (Dict.get eid model.idToTypeAndContextThunk |> Maybe.map (\(tipe, _) -> Syntax.typeWithRolesUnparser model.syntax tipe) |> Maybe.withDefault "Type not found!")
                  [DeucePat ppid] -> Just <| (LangTools.pathedPatternIdToPId ppid model.inputExp |> Maybe.andThen (\pid -> Dict.get pid model.idToTypeAndContextThunk) |> Maybe.map (\(tipe, _) -> Syntax.typeWithRolesUnparser model.syntax tipe) |> Maybe.withDefault "Type not found!")
                  _               -> Nothing

              perhapsTypeMessage =
                maybeTypeString
                |> Maybe.map (\typeString -> disableableTextButton True typeString Controller.msgNoop)
                |> Utils.maybeToList

            in
              if List.isEmpty activeTools then
                noAvailableTools
              else
                Html.div
                  []
                  (perhapsTypeMessage ++ activeTools)

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
          ( True
          , [ Html.text "Configuration Panel" ]
          , []
          )
        Just (deuceTool, results, _) ->
          let
            path =
              [ 1 ] -- TODO, maybe?
            isRenamer =
              DeuceTools.isRenamer deuceTool
            title =
              if isRenamer then
                italicizeQuotes "'" deuceTool.name
              else
                [ Html.text deuceTool.name
                ]
            content =
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
          in
            ( False
            , title
            , content
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
-- Auto Output Tools Popup Panel
--------------------------------------------------------------------------------

autoOutputToolsPopupPanel : Model -> Html Msg
autoOutputToolsPopupPanel model =
  popupPanel
    { pos =
        model.popupPanelPositions.autoOutputTools
    , disabled =
        not <| Model.autoOutputToolsPopupPanelShown model
    , dragHandler =
        Controller.msgDragAutoOutputToolsPopupPanel
    , class =
        "auto-output-tools"
    , title =
        [ Html.text "Output Tools"
        ]
    , content =
        [ let
            activeTools =
              OutputTools.tools model
                |> List.concatMap
                     (List.filter <| List.all Model.predicateSatisfied << .reqs)

            (repetitionTools, otherTools) =
              activeTools
              |> List.partition (.name >> String.startsWith "Repeat ")

            menus =
              if List.length repetitionTools >= 2 then
                let repetitionMenu =
                  generalHoverMenu
                      "Repeat..."
                      RevealAll
                      Controller.msgNoop -- onMouseEnter
                      Controller.msgNoop
                      Controller.msgNoop
                      False -- disabled
                      [Html.div [Attr.class "repeat-tools-submenu"] (List.map (outputToolEntry model) repetitionTools)]
                in
                List.map (outputToolEntry model) otherTools ++ [repetitionMenu]
              else
                otherTools ++ repetitionTools
                |> List.map (outputToolEntry model)
          in
            if List.isEmpty activeTools then
              noAvailableTools
            else
              Html.div
                []
                menus
        ]
    }

--------------------------------------------------------------------------------
-- All Popup Panels
--------------------------------------------------------------------------------

popupPanels : Model -> List (Html Msg)
popupPanels model =
  [ deucePopupPanel model
  , editCodePopupPanel model
  , autoOutputToolsPopupPanel model
  ]

--------------------------------------------------------------------------------
-- Main View
--------------------------------------------------------------------------------

view : Model -> Html Msg
view model =
  -- ImpureGoodies.logTimedRun "view" <| \_ ->
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
      [ Attr.class <|
          "main"
            ++ needsRunFlag
            ++ hasDialogFlag
      , E.onClick Controller.msgHideMenu
      , onRightClick Controller.msgNoop
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

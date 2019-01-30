module View exposing (view)

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

import Utils
import HtmlUtils exposing (..)
import Either exposing (..)
import Updatable
import History

import Model exposing (..)

import Controller
import Keys
import ExamplesGenerated as Examples

import Deuce
import DeuceTools

import OutputTools exposing (OutputTool)

import Layout exposing (px, half)
import Canvas
import OutputCanvas
import Draw
import LangTools
import Sync
import Lang exposing (Exp, PredicateValue(..), SynthesisResult(..), ResultText(..), TransformationResult(..), DeuceTransformation(..), SpecialResult(..))
import LangSvg
import Syntax
import File
import Eval
import TriEval

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
      Set.isEmpty model.selectedFeatures
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

italicizeQuotesIfRenamer : DeuceTransformation -> String -> List (Html Msg) -> List (Html Msg)
italicizeQuotesIfRenamer deuceTransformation toItalicize otherwise =
  case deuceTransformation of
    RenameDeuceTransform _ -> italicizeQuotes "'" toItalicize
    _ -> otherwise

deuceTextInput onInput onKeyDn mbValue mbId =
  [ Html.input
    (
      [ Attr.type_ "text"
      , Attr.class "deuce-input"
      ] ++
      -- Id is apparently necessary if we wish to give automatic focus
      (Maybe.map Attr.id mbId |> flip Utils.maybeCons []) ++
      (Maybe.map Attr.value mbValue |> flip Utils.maybeCons []) ++
      [ E.onInput onInput
      , E.onFocus <| Controller.msgDeuceTextBoxSetFocus True
      , E.onBlur <| Controller.msgDeuceTextBoxSetFocus False
      , onClickWithoutPropagation Controller.msgNoop
      , onKeyDown onKeyDn
      ]
    )
    []
  ]

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
      ( [ Attr.class <| "ui-button " ++ disabledFlag ++ userClass
        ] ++
        ( if not disabled then
            [ E.onClick onClickHandler
            ]
          else
            []
        )
      )
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

-- Configuration flag to indicate whether Code and Output Tools
--   have been run and cached.
indicateWhetherToolIsCached = False

synthesisHoverMenu : Model -> String -> String -> Msg -> Bool -> Html Msg
synthesisHoverMenu model resultsKey title onMouseEnter disabled =
  let cached = Dict.member resultsKey model.synthesisResultsDict in
  generalHoverMenu
    ( if cached && indicateWhetherToolIsCached
        then title ++ " ✓"
        else title
    )
    ( if cached
        then Controller.msgNoop
        else onMouseEnter
    )
    Controller.msgNoop
    Controller.msgNoop
    disabled
    [ synthesisResultsSelect model resultsKey ]

relateHoverMenu : Model -> String -> String -> Msg -> Html Msg
relateHoverMenu model resultsKey title onMouseEnter =
  synthesisHoverMenu
    model
    resultsKey
    title
    onMouseEnter
    (relateDisabled model)

-- groupHoverMenu : Model -> String -> Msg -> Bool -> Html Msg
-- groupHoverMenu model title onMouseEnter disallowSelectedFeatures =
--   synthesisHoverMenu
--     model
--     ""
--     title
--     onMouseEnter
--     (groupDisabled disallowSelectedFeatures model)

deuceSynthesisResultInfo :
  Model -> List Int -> DeuceTransformation -> SynthesisResult
    -> { alreadyRun : Bool
       , class : String
       , additionalHTML : List (Html Msg)
       , onMouseEnter : Msg
       , onMouseLeave : Msg
       , onClick : Msg
       }
deuceSynthesisResultInfo
  model path deuceTransformation (SynthesisResult result) =
    let
      onKeyDn code =
        if code == enterKeyCode then -- Enter button
          Controller.msgChooseDeuceExp result.description result.exp
        else
          Controller.msgNoop

      (additionalHTML, canEval) =
        case deuceTransformation of
          RenameDeuceTransform _ ->
            ( deuceTextInput
                Controller.msgUpdateRenameVarTextBox onKeyDn Nothing Nothing
            , False
            )
          _ ->
            ([], True)
    in
      { alreadyRun =
          Dict.member path model.deuceToolResultPreviews
      , class =
          case Dict.get path model.deuceToolResultPreviews of
            Nothing -> -- tool result Exp has not yet been run and cached
              case deuceTransformation of
                NoInputDeuceTransform _ ->
                  if result.isSafe then "expected-safe" else "expected-unsafe"
                _ ->
                  ""
            Just (_, class) ->
              class
      , additionalHTML =
          additionalHTML
      , onMouseEnter =
          Controller.msgHoverDeuceResult
            (not canEval) (SynthesisResult result) path
      , onMouseLeave =
          Controller.msgLeaveDeuceResult
            (SynthesisResult result) path
      , onClick =
          Controller.msgChooseDeuceExp
            result.description result.exp
      }

viewResultText : ResultText -> Html Msg
viewResultText rt =
  let
    content t =
      Html.span
        [ Attr.class "result-text-content"
        ]
        [ Html.text t
        ]
  in
    case rt of
      PlainText t ->
        Html.p
          [ Attr.class "result-text-plain"
          ]
          [ content t
          ]
      HeaderText t ->
        Html.h1
          [ Attr.class "result-text-header"
          ]
          [ content t
          ]
      ErrorHeaderText t ->
        Html.h1
          [ Attr.class "result-text-header error"
          ]
          [ content t
          ]
      CodeText t ->
        Html.code
          [ Attr.class "result-text-code"
          ]
          [ content t
          ]
      TypeText t ->
        Html.code
          [ Attr.class "result-text-type"
          ]
          [ content t
          ]
      HintText t ->
        Html.p
          [ Attr.class "result-text-hint"
          ]
          [ Html.span
              [ Attr.class "result-text-hint-annotation"
              ]
              [ Html.text "Hint"
              ]
          , Html.text ": "
          , content t
          ]

deuceTransformationResult :
  Model -> List Int -> DeuceTransformation -> TransformationResult -> Html Msg
deuceTransformationResult model path deuceTransformation transformationResult =
  let
    (maybeSynthesisInfo, description) =
      case transformationResult of
        Basic synthesisResult ->
          let
            (SynthesisResult result) =
              synthesisResult

            info =
              deuceSynthesisResultInfo
                model path deuceTransformation synthesisResult
          in
            ( Just info
            , italicizeQuotesIfRenamer
                deuceTransformation
                result.description
                [ Html.text <|
                    if info.alreadyRun && indicateWhetherToolIsCached
                      then result.description ++ " ✓"
                      else result.description
                ]
            )

        Fancy synthesisResult resultText ->
          ( Just <|
              deuceSynthesisResultInfo
                model path deuceTransformation synthesisResult
          , [ viewResultText resultText ]
          )

        Label resultText ->
          ( Nothing
          , [ viewResultText resultText ]
          )

        Special sr ->
          case sr of
            ExampleProvider holeId ->
              case model.unExpOutput of
                Just output ->
                  ( Nothing
                  , [ viewExampleProvider holeId output
                    ]
                  )

                Nothing ->
                  ( Nothing
                  , []
                  )
  in
    case maybeSynthesisInfo of
      Nothing ->
        Html.div
          [ Attr.class "just-description"
          ]
          description

      Just synthesisInfo ->
        generalHtmlHoverMenu
          synthesisInfo.class
          ( [ Html.span
                []
                description
            ] ++ synthesisInfo.additionalHTML
          )
          synthesisInfo.onMouseEnter
          synthesisInfo.onMouseLeave
          synthesisInfo.onClick
          False
          []

viewExampleProvider : Lang.HoleId -> TriEval.UnExp -> Html Msg
viewExampleProvider holeId output =
  let
    holes =
      TriEval.findHoles holeId output

    viewBinding (identifier, (u, _)) =
      Html.li
        [ Attr.class "env-binding"
        ]
        [ Html.code
            []
            [ Html.text <|
                identifier ++ " → " ++ TriEval.unparse u
            ]
        ]

    viewHole (index, env) =
      Html.li
        []
        [ Html.div
            [ Attr.class "hole-index-label"
            ]
            [ Html.text <|
                "Hole " ++ toString index
            ]
        , Html.ul
            [ Attr.class "hole-env"
            ]
            ( List.map viewBinding env
            )
        , Html.div
            [ Attr.class "hole-example-input"
            ]
            [ Html.input
                [ Attr.type_ "text"
                , E.onInput (Controller.msgUpdateExampleInput holeId index)
                ]
                []
            ]
        ]

    synthesizeButton =
      Html.button
        [ Attr.class "synthesize-button"
        , E.onClick (Controller.msgSynthesizeFromExamples holeId)
        ]
        [ Html.text "Synthesize"
        ]
  in
    Html.ul
      [ Attr.class "example-provider"
      ] <|
      ( List.map viewHole holes
      ) ++
      [ synthesizeButton
      ]

deuceTransformationResults
  : Model -> List Int -> DeuceTransformation -> List TransformationResult -> List (Html Msg)
deuceTransformationResults model path deuceTransformation transformationResults =
  let
    mapResults () =
      Utils.mapi1
        ( \(i, result) ->
            deuceTransformationResult
            model
            (path ++ [i])
            deuceTransformation
            result
        )
        transformationResults

    results =
      Lang.synthesisResults transformationResults
  in
  case (deuceTransformation, transformationResults) of
    (SmartCompleteDeuceTransform _, _) ->
      let onKeyDn code =
        -- Enter button
        if code == enterKeyCode && not (List.isEmpty results) then
          let (SynthesisResult firstResult) = Utils.head_ results in
          Controller.msgChooseDeuceExp firstResult.description firstResult.exp
        else
          Controller.msgNoop
      in
      deuceTextInput Controller.msgUpdateSmartCompleteTextBox onKeyDn Nothing Nothing ++
      [ Html.ul
          []
          (mapResults ())
      ]

    (_, []) ->
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

    _ ->
      mapResults ()

deuceHoverMenu : Bool -> Model -> (Int, CachedDeuceTool) -> Html Msg
deuceHoverMenu alwaysShow model (index, (deuceTool, results, disabled)) =
  let
    showFlag =
      if alwaysShow then
        "always-show"
      else
        ""
    path =
      [ index ]
    title =
      italicizeQuotesIfRenamer
        deuceTool.func
        deuceTool.name
        [ Html.text deuceTool.name
        ]
  in
    generalHtmlHoverMenu
      showFlag
      title
      Controller.msgNoop
      Controller.msgNoop
      Controller.msgNoop
      disabled
      [ Html.div
          [ Attr.class "synthesis-results"
          ] <|
          deuceTransformationResults model path deuceTool.func results
      ]

editCodeEntry : Model -> (Int, CachedDeuceTool) -> Html Msg
editCodeEntry model (_, ((deuceTool, _, _) as cachedDeuceTool)) =
  let
    name =
      deuceTool.name ++ "..."
    title =
      italicizeQuotesIfRenamer
        deuceTool.func
        name
        [ Html.text name
        ]
    disabled =
      (List.any Lang.predicateImpossible deuceTool.reqs) ||
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
          (not <| List.all Lang.predicateSatisfied tool.reqs, msg)

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
        [ ("height", (px << .height) Layout.menuBar)
        , ("line-height", (px << .height) Layout.menuBar)
        , ("padding", "0 " ++
            (px << half << .height) Layout.menuBar)
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
          [ ("top", (px << .height) Layout.menuBar) ]
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
                  [ Attr.href "https://github.com/ravichugh/sketch-n-sketch/blob/master/examples/preludeLeo.elm"
                  , Attr.target "_blank"
                  ]
                  [ Html.text "Standard Library (Prelude)" ]
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
        [ [ simpleTextButton "New SVG" <|
              Controller.msgAskNew Examples.blankSvgTemplate model.needsSave
          , simpleTextButton "New HTML" <|
              Controller.msgAskNew Examples.blankHtmlTemplate model.needsSave
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
          , simpleTextButton "Export HTML"
              Controller.msgExportHtml
          ]
        , [ simpleTextButton "Import Code..." <|
              Controller.msgOpenDialogBox ImportCode
          , disableableTextButton
              True
              "Import HTML"
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
              Just <| deuceHoverMenu False model
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
--        [ [ relateTextButton
--              model
--              "Dig Hole"
--              Controller.msgDigHole
--          , relateHoverMenu
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
--              "Build Abstraction"
--              "Build Abstraction"
--              Controller.msgBuildAbstraction
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
--        , [ groupTextButton
--              model
--              "Repeat Right"
--              (Controller.msgReplicateBlob HorizontalRepeat)
--              True
--          , groupTextButton
--              model
--              "Repeat To"
--              (Controller.msgReplicateBlob LinearRepeat)
--              True
--          , groupTextButton
--              model
--              "Repeat Around"
--              (Controller.msgReplicateBlob RadialRepeat)
--              True
--          ]
--        ]

    outputToolsMenu =
      menu "Output Tools" <|
        List.map (List.map <| outputToolEntry model) <|
          OutputTools.tools model

    viewMenu =
      menu "View" <|
        [ [ hoverMenu "Output Type"
              [ simpleTextRadioButton
                  (model.outputMode == Graphics)
                  "Graphics"
                  Controller.msgSetOutputGraphics
              , simpleTextRadioButton
                  (isHtmlText model.outputMode)
                  "HTML (Text)"
                  Controller.msgSetOutputHtmlText
              , simpleTextRadioButton
                  (model.outputMode == ValueText)
                  "Value (Text)"
                  Controller.msgSetOutputValueText
              ]
          ]
        , [ disableableTextButton True "Main Layer" Controller.msgNoop
          , disableableTextButton True "Widget Layer" Controller.msgNoop
          , hoverMenu "Ghost Layer" <|
              booleanOption
                model.showGhosts
                "On"
                "Off"
                Controller.msgSetGhostsShown
          ]
        , [ simpleTextButton
              "Reset Interface Layout"
              Controller.msgResetInterfaceLayout
          ]
        ]

    optionsMenu =
      menu "Options" <|
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
          , hoverMenu "Auto-Run"
              [ disableableTextButton
                  True "Every second" Controller.msgNoop
              , disableableTextButton
                  True "Every 2 seconds" Controller.msgNoop
              , disableableTextButton
                  True "Every 3 seconds" Controller.msgNoop
              ]
          ]
        , [ hoverMenu "Enable Type Checking" <|
              booleanOption
                model.doTypeChecking
                "True"
                "False"
                Controller.msgSetDoTypeChecking
          ]
        , [ hoverMenu "Enable Text Edits" <|
              booleanOption
                (Updatable.extract model.enableTextEdits)
                "True"
                "False"
                Controller.msgSetEnableTextEdits
          , hoverMenu "Enable Deuce Box Selection" <|
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
        , [ hoverMenu "Code Tools Menu"
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
          , let toggle =
              Msg "Toggle Output Tools Menu Mode" <| \m ->
                { m | outputToolsMenuMode = not m.outputToolsMenuMode }
            in
            hoverMenu "Output Tools Menu"
              [ simpleTextRadioButton model.outputToolsMenuMode "Enabled" toggle
              , simpleTextRadioButton (not model.outputToolsMenuMode) "Disabled" toggle
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
          , hoverMenu "Allow Multiple Target Positions" <|
              booleanOption
                (model.allowMultipleTargetPositions)
                "True"
                "False"
                Controller.msgSetAllowMultipleTargetPositions
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
          , let msgSetSyncMode mode =
              NewModelAndCmd "Set Sync Mode" <| \m ->
                let
                  newModel =
                    { m | syncMode = mode }

                  cmd =
                    case (m.syncMode, mode) of
                      (ValueBackprop _, TracesAndTriggers _) ->
                        OutputCanvas.stopDomListener
                      (TracesAndTriggers _, ValueBackprop _) ->
                        OutputCanvas.startDomListener
                      _ ->
                        Cmd.none
                in
                (newModel, cmd)
            in
            hoverMenu "Output Synchronization"
              [ simpleTextRadioButton
                  (model.syncMode == ValueBackprop True)
                  "Auto (Value Backpropagation)"
                  (msgSetSyncMode (ValueBackprop True))
              , simpleTextRadioButton
                  (model.syncMode == ValueBackprop False)
                  "Manual (Value Backpropagation)"
                  (msgSetSyncMode (ValueBackprop False))
              , simpleTextRadioButton
                  (model.syncMode == TracesAndTriggers True)
                  "Live (Traces / Triggers)"
                  (msgSetSyncMode (TracesAndTriggers True))
              , simpleTextRadioButton
                  (model.syncMode == TracesAndTriggers False)
                  "Delayed (Traces / Triggers)"
                  (msgSetSyncMode (TracesAndTriggers False))
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
          {-
          , [ hoverMenu "Syntax"
                [ simpleTextRadioButton
                    ( case model.syntax of
                        Syntax.Leo ->
                          True
                        _ ->
                          False
                    )
                    "Leo"
                    (Controller.msgSetSyntax Syntax.Leo)
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
        -}
        ]

    templateNavigation =
      Html.div
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
                    , Html.text " Previous Example"
                    ]
                , onClick =
                    Controller.msgAskPreviousTemplate (reallyNeedsSave model)
                , disabled =
                    False
            }
        , disableableTextButton
            False
            "Next Example ▸"
            (Controller.msgAskNextTemplate (reallyNeedsSave model))
        ]

  in
    Html.div
      [ Attr.class "menu-bar"
      , Attr.style
          [ ("height", (px << .height) Layout.menuBar)
          , ("borderBottomWidth", (px << .borderWidth) Layout.menuBar)
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
            , if model.outputToolsMenuMode then [outputToolsMenu] else []
            , [viewMenu]
            , [optionsMenu]
            , [templateNavigation]
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
  generalHtmlHoverMenu
    "synthesisResult" [ Html.text description ]
    (Controller.msgHoverSynthesisResult resultsKey elementPath)
    (Controller.msgHoverSynthesisResult resultsKey <| allButLast elementPath)
    (Controller.msgSelectSynthesisResult exp)
    False
    nextMenu

synthesisResultsSelect : Model -> String -> Html Msg
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
    Html.div
      [ Attr.class "synthesis-results"
      ]
      ( case Dict.get resultsKey model.synthesisResultsDict of
          Just results ->
            resultButtonList
                []
                model.hoveredSynthesisResultPathByIndices
                results

          Nothing ->
            [ disableableTextButton True "Synthesizing... ⏳ ⏰ 👵🏽 👴🏼 ⚰️ 🏁" Controller.msgNoop ]
      )

--------------------------------------------------------------------------------
-- Code Panel
--------------------------------------------------------------------------------

fileIndicator : Model -> Html Msg
fileIndicator model =
  let
    filenameHtml =
      Html.text <| Model.prettyFilename WithExtension model
    wrapper =
      if reallyNeedsSave model then
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
          History.prior model.history
        attributes =
          if not <| List.member Keys.keyShift model.keysDown then
            []
          else
          case past of
            Just snapshot ->
              [ E.onMouseEnter <| Controller.msgPreview (Right snapshot.code)
              , E.onMouseLeave Controller.msgClearPreview
              ]

            Nothing ->
              []
      in
        textButton
          { defaultTb
              | attributes = attributes ++ logMouseOver "Undo"
              , content = [Html.text "⟲ Undo"]
              , onClick = Controller.msgUndo
              , disabled = not <| History.hasExtendedPast model.history
          }
    redoButton =
      let
        future =
          History.next model.history
        attributes =
          if not <| List.member Keys.keyShift model.keysDown then
            []
          else
          case future of
            Just snapshot ->
              [ E.onMouseEnter <| Controller.msgPreview (Right snapshot.code)
              , E.onMouseLeave Controller.msgClearPreview
              ]
            Nothing ->
             []
      in
        textButton
          { defaultTb
              | attributes = attributes ++ logMouseOver "Redo"
              , content = [Html.text "⟳ Redo"]
              , onClick = Controller.msgRedo
              , disabled = not <| History.hasFuture model.history
          }
    cleanButton =
      let
        disabled =
          case model.outputMode of
            Graphics -> False
            _        -> True
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
        [ -- Html.text emoji
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
        -- Suppress for Leo/Docs
        -- , cleanButton
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
    codePanelWarning =
      Html.div
        [ Attr.class "code-panel-warning"
        ]
        []
    modeIcon mode =
      let
        active =
          Model.modeActive model mode

        (modeName, imageName) =
          case mode of
            Model.CEText ->
              ("Text", "Text")
            Model.CEDeuceClick ->
              ("Deuce", "Deuce")
            Model.CETypeInspector ->
              ("Type Inspector", "TypeInspector")
      in
        Html.div
          [ Attr.classList
              [ ("mode-icon-wrapper", True)
              , ("active", active)
              ]
          ]
          [ Html.div
              [ Attr.class "mode-indicator" ]
              []
          , Html.div
              [ Attr.class "mode-icon" ]
              [ textButton
                  { defaultTb
                      | content =
                          [ Html.img
                              [ Attr.src <|
                                  "img/mode-icons/" ++ imageName ++ ".svg"
                              , Attr.title <|
                                  modeName ++ " Mode"
                              ]
                              []
                          ]
                      , onClick =
                          Controller.msgSetCodeEditorMode mode
                  }
              ]
          ]
    modeSeparator =
      Html.div
        [ Attr.class "mode-separator"
        ]
        []
    modeBar =
      Html.div
        [ Attr.class "mode-bar"
        ]
        [ modeIcon Model.CEText
        , modeSeparator
        , modeIcon Model.CEDeuceClick
        , modeSeparator
        , modeIcon Model.CETypeInspector
        , modeSeparator
        ]
  in
    Html.div
      [ Attr.class "panel code-panel"
      , Attr.style
          [ ("left", (px << .x) <| Layout.codePanel model)
          , ("top", (px << .y) <| Layout.codePanel model)
          , ("width", (px << .width) <| Layout.codePanel model)
          , ("height", (px << .height) <| Layout.codePanel model)
          ]
      ]
      [ statusBar
      , actionBar
      , editor
      , modeBar
      , codePanelWarning
      ]

--------------------------------------------------------------------------------
-- Main Resizer
--------------------------------------------------------------------------------

mainResizer : Model -> Html Msg
mainResizer model =
  let
    mainResizerBB =
      Layout.mainResizer model
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
    canvasDim =
      Layout.outputCanvas model
    output =
      case (model.errorBox, model.outputMode, model.preview) of
        (_, _, Just (_, _, Err errorMsg)) ->
          [textOutput errorMsg]
        (_, _, Just (_, _, Ok _)) ->
          Canvas.build canvasDim model
        (Just errorMsg, _, Nothing) ->
          [textOutput errorMsg]
        (Nothing, HtmlText rawHtml, Nothing) ->
          [ Html.textarea
              [ E.onInput Controller.msgUpdateHTMLEditor
              , Attr.class "text-output"
              ]
              [ Html.text (Maybe.withDefault rawHtml model.htmlEditorString) ]
          ]
        (Nothing, ValueText, _) ->
{-
          [ Html.div
              []
              [ Html.text <|
                  "Make some edits."
                    ++ if valueEditorNeedsCallUpdate model
                         then " Now pick a new program from the pop-up menu."
                         else ""
              ]
          , Html.textarea
-}
          [ Html.textarea
              [ E.onInput Controller.msgUpdateValueEditor
              , Attr.class "text-output"
              ]
              [ Html.text model.valueEditorString ]
          ]
        (Nothing, _, _) ->
          Canvas.build canvasDim model
    outputPanelWarning =
      Html.div
        [ Attr.class "output-panel-warning"
        , Attr.style
            [ ("top", (px << negate) Layout.panelBorderWidth)
            , ("right", (px << negate) Layout.panelBorderWidth)
            , ("bottom", (px << negate) Layout.panelBorderWidth)
            , ("left", (px << negate) Layout.panelBorderWidth)
            ]
        ]
        []
  in
    Html.div
      [ Attr.class "panel output-panel"
      , Attr.style
          [ ("left", (px << .x) <| Layout.outputPanel model)
          , ("top", (px << .y) <| Layout.outputPanel model)
          , ("width", (px << .width) <| Layout.outputPanel model)
          , ("height", (px << .height) <| Layout.outputPanel model)
          ]
      ]
      [ Html.div
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

          -- https://www.w3schools.com/tags/att_global_data.asp
          , Attr.attribute "data-canvas-count" (toString model.slateCount)

          -- allow right-clicks on canvas (e.g. to access Inspect Element)
          , onRightClickPreventDefault False Controller.msgNoop
          ]
          output
      , outputPanelWarning
      ]

--------------------------------------------------------------------------------
-- Tool Panel
--------------------------------------------------------------------------------

type ButtonKind = Regular | Selected | Unselected

buttonClass : ButtonKind -> String
buttonClass bk =
  case bk of
    Regular ->
      "regular"

    Selected ->
      "selected"

    Unselected ->
      "unselected"

iconButton model iconName onClickHandler btnKind disabled =
  iconButtonExtraAttrs model iconName [] onClickHandler btnKind disabled

iconButtonExtraAttrs model iconName extraAttrs onClickHandler btnKind disabled =
  let
    iconHtml =
      case Dict.get (Utils.naturalToCamelCase iconName) model.icons of
        Just h ->
          h

        Nothing ->
          Html.text iconName

    commonAttrs =
      [ Attr.title iconName
      , Attr.disabled disabled
      , Attr.class <|
          "icon-button " ++ buttonClass btnKind
      , Attr.style
          [ ("width", (px << .width) Layout.iconButton)
          , ("height", (px << .height) Layout.iconButton)
          ]
      , handleEventAndStop "mousedown" Controller.msgNoop
      , E.onClick onClickHandler
      ]
  in
    Html.button
      ( commonAttrs ++ extraAttrs )
      [ Html.div
          []
          [ iconHtml ]
      ]

toolButton model tool =
  let
    cap = case tool of
      Cursor ->
        "Cursor"
      PointOrOffset ->
        "Point or Offset"
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
      Function fName ->
        fName
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


customButton model name btnKind disabled onClickHandler =
  let cap = name
  in
    Html.div
      [ Attr.class "tool"
      ]
      [ iconButton
          model name (Msg cap onClickHandler) btnKind disabled
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

functionTools : Model -> List (Html Msg)
functionTools model =
  Draw.getDrawableFunctions model
  |> List.map
      (\(funcName, _) ->
        Html.div
          [ Attr.class "tool"
          ]
          [ iconButton model funcName
              (Msg (funcName ++ " Function Tool") (\m -> { m | tool = Function funcName }))
              (if model.tool == Function funcName then Selected else Unselected)
              False
          ]
      )

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

-- slight variation on toolModeDisplay above
modeDisplay modeText textStyles modePredicate updateModel =
  let
    flag =
      if modePredicate then
         " active"
      else
        ""
  in
    Html.div
      [ Attr.class <| "tool-mode larger-text" ++ flag
      , Attr.style textStyles
      , E.onClick (Msg modeText updateModel)
      ]
      [ Html.text modeText
      ]

outputModeIndicator : Model -> Html Msg
outputModeIndicator model =
  Html.div
    [ Attr.class "tool-mode-indicator"
    ]
    [ modeDisplay "GUI" []
        (model.outputMode == Graphics)
        (\m -> { m | outputMode = Graphics, slateCount = m.slateCount + 1 })
    , modeDisplay "HTML" []
        (isHtmlText model.outputMode)
        Controller.doSetOutputHtmlText
    , modeDisplay "Value" []
        (model.outputMode == ValueText)
        (\m -> { m | outputMode = ValueText, slateCount = m.slateCount + 1 })
    ]

syncModeIndicator model =
  -- not showing TracesAndTriggers modes, only ValueBackprop
  Html.div
    [ Attr.class "tool-mode-indicator"
    ]
    [ modeDisplay "Auto Sync"
        ( if model.outputMode == Graphics
            then []
            else [("text-decoration", "line-through")]
        )
        (model.syncMode == ValueBackprop True)
        (\m ->
          if m.outputMode == Graphics then
            { m | syncMode = case m.syncMode of
                    ValueBackprop True -> ValueBackprop False
                    _                  -> ValueBackprop True
            }
          else
           m
        )
    ]
{-
    [ modeDisplay "Auto Sync"
        ( if model.outputMode == Graphics
            then []
            else [("text-decoration", "line-through")]
        )
        (model.outputMode == Graphics && model.syncMode == ValueBackprop True)
        (\m -> { m | syncMode = ValueBackprop True})
    , modeDisplay "Manual" []
        (model.outputMode /= Graphics || model.syncMode == ValueBackprop False)
        (\m -> { m | syncMode = ValueBackprop False })
    ]
-}

toolPanel : Model -> Html Msg
toolPanel model =
  let
    toolSeparator =
      Html.div
        [ Attr.class "tool-separator" ]
        []

    toolButtons =
      if LangSvg.isSvg model.inputVal then
       (if Config.elmConfDemo then []
        else
        [ outputModeIndicator model
        , syncModeIndicator model
        ]
        ) ++
        [ toolButton model Cursor
        , toolButton model PointOrOffset
        , toolButton model Text
        , toolButton model (Line model.toolMode)
        , toolButton model (Rect model.toolMode)
        , toolButton model (Oval model.toolMode)
        , toolButton model (Poly model.toolMode)
        , toolButton model (Path model.toolMode)
        ] ++
        lambdaTools model ++
        functionTools model ++
        [ toolModeIndicator model
        ]

      else
        [ outputModeIndicator model
        , syncModeIndicator model
        ]
  in
    Html.div
      [ Attr.class "panel tool-panel"
      , Attr.style
          [ ("width", (px << .width) Layout.toolPanel)
          , ("right", (px << .right) Layout.toolPanel)
          , ("marginLeft", (px << .marginLeft) Layout.toolPanel)
          ]
      ]
      toolButtons

--------------------------------------------------------------------------------
-- Synthesis Panel
--------------------------------------------------------------------------------

synthesisAutoSearch : Model -> (List (Html Msg))
synthesisAutoSearch model =
  if List.length (Utils.getWithDefault "Auto-Synthesis" [] model.synthesisResultsDict) > 0 then
     [ Html.div
         [ Attr.class "synthesis-auto-search"
         ]
         [ synthesisResultsSelect model "Auto-Synthesis"
         ]
     ]
  else
    []

synthesisPanel : Model -> Html Msg
synthesisPanel model =
  Html.div
    [ Attr.class "synthesis-panel-wrapper"
    , Attr.style
        [ ( "bottom", (px << .bottom) <| Layout.synthesisPanel model)
        , ( "height", (px << .height) <| Layout.synthesisPanel model)
        ]
    ]
    [ Html.div
        [ Attr.class "panel synthesis-panel"
        ]
        [ Html.div
            [ Attr.class "dropdown-content synthesis-menu-holder"
            ]
            [ synthesisResultsSelect model "Auto-Synthesis"
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
        [ ("margin", px <| .width Layout.spacing)
        , ("top", px <| .height Layout.menuBar)
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
  let
    fileOpenRow filename =
      let
        -- Disable any operations if a backup file is present
        disabled =
          List.member (File.backupFilename filename) model.fileIndex
      in
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
              [ generalUiButton
                  disabled
                  ""
                  "Open"
                   (Controller.msgAskOpen filename model.needsSave)
              , Html.span
                  [ Attr.class "file-delete-button"
                  ]
                  [ generalUiButton
                      disabled
                      ""
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
      ( model.fileIndex
          |> List.sortBy .name
          |> List.map fileOpenRow
      )

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
  case model.deuceOverlayCache of
    Nothing ->
      Html.text ""

    Just overlay ->
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
                    model.codeBoxInfo.scrollerLeft - Layout.deuceOverlayBleed
                )
              , ( "width"
                , px <|
                    model.codeBoxInfo.scrollerWidth + Layout.deuceOverlayBleed
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
              [ overlay
              ]
          ]


diffOverlay : Model -> Html Msg
diffOverlay model =
  let
    pointerEvents =
      if Model.deuceActive model then
        "auto"
      else
        "none"
    (disabledFlag, exps) =
      case model.preview of
        Just (_, exps, _) ->
          ("", exps)
        Nothing ->
          case model.previewdiffs of
            Just exps ->
              ("", exps)
            Nothing ->
              (" disabled", [])
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
                model.codeBoxInfo.scrollerLeft - Layout.deuceOverlayBleed
            )
          , ( "width"
            , px <|
                model.codeBoxInfo.scrollerWidth + Layout.deuceOverlayBleed
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
          [ Deuce.diffOverlay model exps
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
    title =
      italicizeQuotesIfRenamer
      deuceTool.func
      name
      [ Html.text name
      ]
    disabled =
      List.any Lang.predicateImpossible deuceTool.reqs
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
          "popup-panel panel " ++ disabledFlag ++ args.class
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
-- Deuce Popup Panel For Hotkeys
--------------------------------------------------------------------------------

deuceKeyboardPopupPanel : Model -> Html Msg
deuceKeyboardPopupPanel model =
  let
    {title, text, textToTransformationResults, smartCompleteSelection} =
      model.mbDeuceKeyboardInfo |>
      Maybe.withDefault
        { title = "Totally broken!"
        , text = ""
        , textToTransformationResults = always []
        , smartCompleteSelection = ""
        }

    transformationResults = textToTransformationResults text
    synthesisResults = Lang.synthesisResults transformationResults
    mbSelectedResult =
      transformationResults |>
      Utils.findFirst (Lang.transformationResultToString >> (==) smartCompleteSelection)

    resultItem (SynthesisResult result) =
      let
        desc = result.description
        class =
          if desc == smartCompleteSelection then
            "deuce-menu-phony-hovered"
          else
            "expected-safe"
      in
      generalHtmlHoverMenu class
        ( [ Html.span
              []
              [ Html.text result.description ]
          ]
        )
        Controller.msgNoop
        Controller.msgNoop
        (Controller.msgChooseDeuceExp result.description result.exp)
        False
        []

    onKeyDn code =
      -- Enter button
      if code == enterKeyCode && not (List.isEmpty synthesisResults) then
        let
          sResultWrapped =
            case mbSelectedResult of
              Just selectedResult ->
                Lang.transformationToSynthesisResult selectedResult
                |> Utils.fromJust_ "Smart complete results must always have synthesis results"
              Nothing ->
                Utils.head_ synthesisResults
          (SynthesisResult sResult) = sResultWrapped
        in
        Controller.msgChooseDeuceExp sResult.description sResult.exp
      else
        Controller.msgNoop

    root = model.inputExp
    displayInfo =
      { lineHeight =
          model.codeBoxInfo.lineHeight
      , characterWidth =
          model.codeBoxInfo.characterWidth
      , colorScheme =
          model.colorScheme
      }
    toPos =
      Maybe.map (\withInfo -> (withInfo.start.col + 6, withInfo.start.line + 6))
      >> Maybe.withDefault (0, 0)
      >> Deuce.c2a displayInfo
      >> Utils.mapBoth floor
    pos =
      case model.deuceState.mbKeyboardFocusedWidget of
        Just (DeuceExp eId) ->
          Lang.findExpByEId root eId |> Maybe.map Lang.unExpr |> toPos
        Just (DeucePat ppid) ->
          LangTools.findPatByPathedPatternId ppid root |> toPos
        _ ->
          toPos Nothing

    id = Just deuceKeyboardPopupPanelTextBoxId
    value = Just text
  in
  popupPanel
    { pos = pos
    , disabled = not <| Model.deuceKeyboardPopupPanelShown model
    , dragHandler = Controller.msgNoop
    , class = "deuce-popup-panel appear-above"
    , title = [ Html.text title ]
    , content =
        [ Html.div
            [ Attr.class "deuce-popup-panel-content" ]
            ( [ Html.ul
                [ Attr.class "synthesis-results" ]
                (List.reverse synthesisResults |> List.map resultItem)
              ] ++
              deuceTextInput Controller.msgUpdateDeuceKeyboardTextBox onKeyDn value id
            )
        ]
    }

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

    activeTools =
      model.deuceToolsAndResults
        |> List.concatMap
             ( List.filter
                 ( \(tool, _, _) ->
                     DeuceTools.isActive model.codeEditorMode tool
                 )
             )

    alwaysShowFlag =
      case activeTools of
        [(tool, _, _)] ->
          model.codeEditorMode == CETypeInspector
            && tool.id == DeuceTools.typesToolId

        _ ->
          False

    noTools =
      List.isEmpty activeTools

    activeToolHtmls =
      Utils.mapi1 (deuceHoverMenu alwaysShowFlag model) activeTools
  in
    popupPanel
      { pos =
          model.popupPanelPositions.deuce
      , disabled =
          Utils.or
            [ not <| Model.deucePopupPanelShown model
            , model.codeEditorMode == CETypeInspector && noTools
            ]
      , dragHandler =
          Controller.msgDragDeucePopupPanel
      , class =
          "deuce-popup-panel " ++ appearDirectionFlag
      , title =
          [ Html.text "Code Tools" -- "Deuce Menu"
          ]
      , content =
          [ if noTools then
              noAvailableTools
            else
              Html.div
                [ Attr.class "deuce-popup-panel-content" ]
                activeToolHtmls
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
            title =
              italicizeQuotesIfRenamer
              deuceTool.func
              deuceTool.name
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
              ( if List.all Lang.predicateSatisfied deuceTool.reqs then
                  [ Html.h2
                      []
                      [ Html.text "Code Updates" ]
                  , Html.div
                      [ Attr.class "synthesis-results"
                      ] <|
                      deuceTransformationResults model path deuceTool.func results
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
        -- [ Html.text "Output Tools"
        [ Html.text <|
            case model.syncMode of
              ValueBackprop _ -> valueBackpropPopupMenuTitle
              _               -> "Output Tools"
        ]
    , content =
        [ let
            activeTools =
              OutputTools.tools model
                |> List.concatMap
                     (List.filter <| List.all Lang.predicateSatisfied << .reqs)
                |> List.map (outputToolEntry model)
          in
            if List.isEmpty activeTools then
              noAvailableTools
            else
              Html.div
                []
                activeTools
        ]
    }

--------------------------------------------------------------------------------
-- All Popup Panels
--------------------------------------------------------------------------------

popupPanels : Model -> List (Html Msg)
popupPanels model =
  [ deucePopupPanel model
  , deuceKeyboardPopupPanel model
  , editCodePopupPanel model
  , autoOutputToolsPopupPanel model
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
    needsValueBackpropFlag =
      if Model.needsValueBackprop model then
        " needs-value-backprop"
      else
        ""
  in
    Html.div
      [ Attr.class <|
          "main"
            ++ needsRunFlag
            ++ hasDialogFlag
            ++ needsValueBackpropFlag
      , E.onClick Controller.msgHideMenu
      , onRightClick Controller.msgNoop
      ]
      ( [ onbeforeunloadDataElement model
        , menuBar model
        , workArea model
        ] ++ (if model.enableDeuceBoxSelection then [deuceOverlay model] else []) ++ [
          diffOverlay model
        -- , deuceRightClickMenu model
        ]
        ++ (popupPanels model)
        ++ [subtleBackground]
        ++ (dialogBoxes model)
      )

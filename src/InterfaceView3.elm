module InterfaceView3 exposing (view)

-- Sketch-n-Sketch Libraries ---------------------------------------------------

import Config exposing (params) -- TODO remove
import ExamplesGenerated as Examples
import Utils
import HtmlUtils exposing (handleEventAndStop)
import Either exposing (..)
import Lang

import InterfaceModel as Model exposing
  ( Msg(..), Model, Tool(..), ShapeToolKind(..), Mode(..)
  , ReplicateKind(..), LambdaTool(..)
  , Caption(..), MouseMode(..)
  , mkLive_
  , DialogBox(..)
  )
import InterfaceController as Controller
import Layout
import Canvas
import LangUnparser exposing (unparse)
import LangSvg exposing (attr)
import LangTools
import Sync
import DependenceGraph
import CodeMotion
import DeuceWidgets exposing (..) -- TODO

-- Elm Libraries ---------------------------------------------------------------

import Set
import Dict
import Regex

import Svg exposing (Svg)
import Svg.Events exposing (onMouseDown, onMouseUp, onMouseOver, onMouseOut)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing
  ( onClick, onInput, on, onMouseEnter, onMouseLeave
  , onWithOptions, defaultOptions
  )
import Html.Lazy
import Json.Decode


--------------------------------------------------------------------------------

pixels n = toString n ++ "px"
unpixels str = Result.withDefault 0 <| String.toFloat (String.dropRight 2 str)

imgPath s = "img/" ++ s


--------------------------------------------------------------------------------
-- Configuration Parameters

showRawShapeTools = False


--------------------------------------------------------------------------------
-- Top-Level View Function

view : Model -> Html Msg
view model =
  let layout = Layout.computeLayout model in

  let fileTools = fileToolBox model layout in
  let codeTools = codeToolBox model layout in
  let drawTools = drawToolBox model layout in
  let stretchyDrawTools = stretchyDrawToolBox model layout in
  let lambdaDrawTools = lambdaDrawToolBoxWithIcons model layout in
  let attributeTools = attributeToolBox model layout in
  let blobTools = blobToolBox model layout in
  let moreBlobTools = moreBlobToolBox model layout in
  let outputTools = outputToolBox model layout in
  let textTools = textToolBox model layout in
  let synthesisResultsSelect = synthesisResultsSelectBox model layout in

  let
    dialogBoxes =
      [ fileNewDialogBox model
      , fileSaveAsDialogBox model
      , fileOpenDialogBox model
      , alertSaveDialogBox model
      , importCodeDialogBox model
      ]
  in

  let
    needsSaveString =
      if model.needsSave then
        "true"
      else
        "false"
    onbeforeunloadDataElement =
      Html.input
        [ Attr.type_ "hidden"
        , Attr.id "onbeforeunload-data"
        , Attr.attribute "data-needs-save" needsSaveString
        , Attr.attribute "data-filename" (Model.prettyFilename model)
        ]
        []
  in

  let animationTools =
    if model.slideCount > 1 || model.movieCount > 1
    then [ animationToolBox model layout ]
    else [] in

  let synthesisResultsSelect =
    if List.length model.synthesisResults > 0
    then [ synthesisResultsSelectBox model layout ]
    else [] in

  let codeBox =
    if model.basicCodeBox
      then basicCodeBox model layout.codeBox
      else aceCodeBox model layout.codeBox in

  let outputBox = outputArea model layout in

  let resizeCodeBox =
    resizeWidget "resizeCodeBox" model layout Layout.getPutCodeBox
       (layout.codeBox.left + layout.codeBox.width)
       (layout.codeBox.top + layout.codeBox.height) in

  let resizeCanvas =
    resizeWidget "resizeCanvas" model layout Layout.getPutCanvas
       layout.canvas.left
       layout.canvas.top in

  let caption = captionArea model layout in
  let deuce = [deuceLayer model layout] in
  let everything = -- z-order in decreasing order
     -- bottom-most
     [ onbeforeunloadDataElement
     , codeBox, outputBox

     -- toolboxes in reverse order
     , outputTools] ++ animationTools ++
     [ moreBlobTools, blobTools, attributeTools, lambdaDrawTools, stretchyDrawTools, drawTools
     , textTools
     , codeTools, fileTools
     ] ++ synthesisResultsSelect ++ deuce ++

     -- top-most
     [ resizeCodeBox
     , resizeCanvas
     -- , caption
     ] ++ dialogBoxes
  in

  Html.div
    [ Attr.id "containerDiv"
    , Attr.style
        [ ("position", "fixed")
        , ("width", pixels model.dimensions.width)
        , ("height", pixels model.dimensions.height)
        , ("background", "white")
        ]
    ]
    everything

--------------------------------------------------------------------------------
-- Tool Boxes

fileToolBox model layout =
  toolBox model "fileToolBox" [] Layout.getPutFileToolBox layout.fileTools
    [ fileIndicator model
    , fileNewDialogBoxButton
    , fileSaveAsDialogBoxButton
    , fileSaveButton model
    , fileOpenDialogBoxButton
    , exportCodeButton
    , exportSvgButton
    , importCodeButton
    , importSvgButton
    ]

codeToolBox model layout =
  toolBox model "codeToolBox" [] Layout.getPutCodeToolBox layout.codeTools
    [ runButton
    , undoButton model
    , redoButton model
    , cleanButton model
    ]

drawToolBox model layout =
  toolBox model "drawToolBox" [] Layout.getPutDrawToolBox layout.drawTools
    [ toolButton model Cursor
    , toolButton model Text
    , toolButton model (Line Raw)
    , toolButton model (Rect Raw)
    , toolButton model (Oval Raw)
    , toolButton model (Poly Raw)
    , toolButton model (Path Raw)
    ]

stretchyDrawToolBox model layout =
  toolBox model "stretchyDrawToolBox" [] Layout.getPutDrawToolBox layout.stretchyDrawTools
    [ toolButton model (Rect Stretchy)
    , toolButton model (Oval Stretchy)
    , toolButton model (Poly Stretchy)
    , toolButton model (Path Stretchy)
    ]

{-
lambdaDrawToolBox model layout =
  toolBox model "lambdaDrawToolBox" [] Layout.getPutDrawToolBox layout.lambdaDrawTools
    [ toolButton model Lambda
    , dropdownLambdaTool model
    ]
-}

attributeToolBox model layout =
  toolBox model "attributeToolBox" [] Layout.getPutAttributeToolBox layout.attributeTools
    [ relateButton model "Dig Hole" Controller.msgDigHole
    , relateButton model "Make Equal" Controller.msgMakeEqual
    , relateButton model "Relate" Controller.msgRelate
    , relateButton model "Indexed Relate" Controller.msgIndexedRelate
    ]

textToolBox model layout =
  toolBox model "textToolBox" [] Layout.getPutTextToolBox layout.textTools
    [ undoButton model
    , redoButton model
    , deuceMoveExpButton model
    , fontSizeButton model
    -- , deuceWidgetsButton model
    ]

blobToolBox model layout =
  toolBox model "blobToolBox" [] Layout.getPutBlobToolBox layout.blobTools
    [ groupButton model "Dupe" Controller.msgDuplicateBlobs True
    , groupButton model "Merge" Controller.msgMergeBlobs True
    , groupButton model "Group" Controller.msgGroupBlobs False
    , groupButton model "Abstract" Controller.msgAbstractBlobs True
    ]

moreBlobToolBox model layout =
  toolBox model "moreBlobToolBox" [] Layout.getPutMoreBlobToolBox layout.moreBlobTools
    [ groupButton model "Repeat Right" (Controller.msgReplicateBlob HorizontalRepeat) True
    , groupButton model "Repeat To" (Controller.msgReplicateBlob LinearRepeat) True
    , groupButton model "Repeat Around" (Controller.msgReplicateBlob RadialRepeat) True
    ]

outputToolBox model layout =
  toolBox model "outputToolBox" [] Layout.getPutOutputToolBox layout.outputTools
    [ -- codeBoxButton model
      heuristicsButton model
    , outputButton model
    , ghostsButton model
    , autoSynthesisButton model
    ]

animationToolBox model layout =
  toolBox model "animationToolBox" [] Layout.getPutAnimationToolBox layout.animationTools
    [ previousSlideButton model
    , previousMovieButton model
    , pauseResumeMovieButton model
    , nextMovieButton model
    , nextSlideButton model
    ]

synthesisResultsSelectBox model layout =
  let desc description exp sortKey =
    (Regex.replace Regex.All (Regex.regex "^Original -> | -> Cleaned$") (\_ -> "") description) ++
    " (" ++ toString (LangTools.nodeCount exp) ++ ")" ++ " " ++ toString sortKey
  in
  let extraButtonStyles =
    Attr.style
        [ ("width", "100%")
        , ("text-align", "left")
        , ("padding-top", "8px")
        , ("padding-bottom", "8px")
        ]
  in
  let cancelButton =
    htmlButtonExtraAttrs [extraButtonStyles, Attr.style [("text-align", "center")]] "Cancel" (Controller.msgClearSynthesisResults) Regular False
  in
  let extraStyles =
    [ ("display", "block")
    ]
  in
  let resultButtonList priorPathByIndices remainingPathByIndicies results =
    let buttons =
      results
      |> Utils.mapi0
          (\(i, Model.SynthesisResult {description, exp, sortKey, children}) ->
            let thisElementPath = priorPathByIndices ++ [i] in
            let (isHovered, nextMenu) =
              case remainingPathByIndicies of
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
            [ htmlButtonExtraAttrs
                  [ extraButtonStyles
                  , Attr.style [("background-color", if isHovered then buttonSelectedColor else buttonRegularColor)]
                  , Html.Events.onMouseEnter (Controller.msgHoverSynthesisResult thisElementPath)
                  , Html.Events.onMouseLeave (Controller.msgHoverSynthesisResult [])
                  ]
                  (desc description exp sortKey)
                  (Controller.msgSelectSynthesisResult exp)
                  Regular
                  False
            ]
          )
      |> List.concat
    in
    Html.div
        [ Attr.style <|
          [ ("position", if priorPathByIndices == [] then "relative" else "absolute")
          -- , ("max-height", "325px")
          , ("width", "325px")
          , ("overflow", "visible")
          -- , ("overflow-y", "auto")
          , ("left", if priorPathByIndices == [] then "0" else "-325px")
          ]
        ]
        buttons
  in
  let resultsMenuTree =
    resultButtonList [] model.hoveredSynthesisResultPathByIndicies model.synthesisResults
  in
  toolBox model "synthesisResultsSelect" extraStyles Layout.getPutSynthesisResultsSelectBox layout.synthesisResultsSelect [resultsMenuTree, cancelButton]


toolBox model id extraStyles (getOffset, putOffset) leftRightTopBottom elements =
  Html.div
    [ Attr.id id
    -- , Attr.title id
    , Attr.style <|
        [ ("position", "fixed")
        , ("padding", "0px 0px 0px 15px")
        , ("background", Layout.strInterfaceColor) -- "#444444")
        , ("border-radius", "10px 0px 0px 10px")
        , ("box-shadow", "6px 6px 3px #888888")
        , ("cursor", "move")
        ] ++ extraStyles ++ Layout.fixedPosition leftRightTopBottom
    , onMouseDown <| Layout.dragLayoutWidgetTrigger (getOffset model) putOffset
    ]
    elements


--------------------------------------------------------------------------------
-- Code Box

aceCodeBox model dim =
  Html.div
    [ Attr.id "editor"
    -- parent div as relative to have hover div on top
    , Attr.style [ ("position", "relative")
                 , ("width", pixels dim.width)
                 , ("height", pixels dim.height)
                 , ("left", pixels Layout.windowPadding)
                 , ("top", pixels Layout.windowPadding)
                 , ("pointer-events", "auto")
                 , ("z-index", "-1")
                 ]
    , onMouseEnter Controller.msgMouseEnterCodeBox
    , onMouseLeave Controller.msgMouseLeaveCodeBox
    , onClick Controller.msgMouseClickCodeBox
    ]
    [ ]
    {- Html.Lazy.lazy ... -}

basicCodeBox model dim =
  textArea (Model.codeToShow model) <|
    [ onInput Controller.msgCodeUpdate
    , Attr.style [ ("width", pixels dim.width)
                 ]
    ]

textArea text attrs =
  textArea_ [Html.text text] attrs

textArea_ children attrs =
  let innerPadding = 4 in
  -- NOTE: using both Attr.value and Html.text seems to allow read/write...
  let commonAttrs =
    [ Attr.spellcheck False
    , Attr.style
        [ ("font-family", params.mainSection.codebox.font)
        , ("font-size", params.mainSection.codebox.fontSize)
        , ("whiteSpace", "pre")
        , ("height", "100%")
        , ("resize", "none")
        , ("overflow", "auto")
        , ("border-radius", "0px 10px 10px 10px") -- like outputArea
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
  Html.div (commonAttrs ++ attrs) children

computePolygonPoints rcs model layout =
  let traverse leftSide =
    let maybeReverse = if leftSide then identity else List.reverse in
    let things = maybeReverse (Dict.toList (Tuple.second rcs)) in
    flip List.concatMap things <| \(k,(c1,c2)) ->
      let c = if leftSide then c1 else c2 in
      let topOffset = rowColToPixelPos {line = k, col = c} model in
      let top = {x=topOffset.x - model.codeBoxInfo.gutterWidth, y=topOffset.y - model.codeBoxInfo.offsetHeight} in
      let bottom = {x=top.x, y=top.y + model.codeBoxInfo.lineHeight} in
      if leftSide then [top, bottom] else [bottom, top]
  in
  let left = traverse True in
  let right = traverse False in
  let combine point acc =
    toString(point.x) ++ "," ++ toString(point.y) ++ " " ++ acc in
  List.foldr combine  "" (left ++ right)

expBoundingPolygonPoints =
  boundingPolygonPoints List.reverse
     (\exp -> (DeuceExp exp.val.eid, expBoundingPolygon exp))

patBoundingPolygonPoints =
  boundingPolygonPoints identity
     (\(pat,pid,_,_,_) -> (DeucePat pid, patBoundingPolygon pat))

boundingPolygonPoints maybeReverse deuceWidgetAndBoundingPolygonOf exps model layout =
  let calculate thing =
    let (deuceWidget, boundingPolygon) = deuceWidgetAndBoundingPolygonOf thing in
    let points = computePolygonPoints boundingPolygon model layout in
    let color =
      if List.member deuceWidget model.deuceState.selectedWidgets && not (needsRun model)
        then "orange"
      else if List.member deuceWidget model.deuceState.hoveredWidgets && showDeuceWidgets model
        then "yellow"
        else
          ""
    in
    let textPolygon =
        [ flip Svg.polygon []
            [LangSvg.attr "stroke" color
            , LangSvg.attr "stroke-width" "5"
            , Attr.style [("fill-opacity", "0")]
            , LangSvg.attr "points" points
            , onClick (Controller.msgMouseClickDeuceWidget deuceWidget)
            , onMouseOver (Controller.msgMouseEnterDeuceWidget deuceWidget)
            , onMouseLeave (Controller.msgMouseLeaveDeuceWidget deuceWidget)
            ]
          ] in
      textPolygon
  in
  let polygons = maybeReverse (List.concatMap calculate exps) in
  polygons

expTargetIndicator = targetIndicator DeuceExpTarget
patTargetIndicator = targetIndicator DeucePatTarget

targetIndicator deuceWidgetConstructor targets model layout =
  let drawTarget deuceWidget pixelPos color opacity numRings =
    let rDot = 4 in
    let (cx, cy) =
      ( toString <|
          pixelPos.x - model.codeBoxInfo.gutterWidth
                     - model.codeBoxInfo.offsetLeft
                     + model.codeBoxInfo.characterWidth / 2
      -- TODO should pixelPos calculations take care of all these offsets?
      , toString <|
          pixelPos.y - model.codeBoxInfo.offsetHeight
                     + model.codeBoxInfo.lineHeight / 2
      )
    in
    let rings =
      flip List.map (List.range 1 numRings) <| \i ->
        flip Svg.circle [] <|
          [ LangSvg.attr "fill" "none"
          , LangSvg.attr "fill-opacity" opacity
          , LangSvg.attr "stroke" color
          , LangSvg.attr "stroke-width" (toString (rDot//2))
          , LangSvg.attr "cx" cx, LangSvg.attr "cy" cy
          , LangSvg.attr "r" (toString ((i+1) * rDot))
          ]
    in
    let center =
      flip Svg.circle [] <|
        [ LangSvg.attr "fill" color
        , LangSvg.attr "fill-opacity" opacity
        , LangSvg.attr "cx" cx, LangSvg.attr "cy" cy
        , LangSvg.attr "r" (toString rDot)
        , onClick (Controller.msgMouseClickDeuceWidget deuceWidget)
        , onMouseOver (Controller.msgMouseEnterDeuceWidget deuceWidget)
        , onMouseLeave (Controller.msgMouseLeaveDeuceWidget deuceWidget)
        ]
    in
    center :: rings
  in
  flip List.concatMap targets <| \(id, start, end) ->
    let deuceWidget = deuceWidgetConstructor id in
    let pixelPos = rowColToPixelPos start model in
    if List.member deuceWidget model.deuceState.selectedWidgets && not (needsRun model) then
      drawTarget deuceWidget pixelPos "darkgreen" "1.0" 3
    else if List.member deuceWidget model.deuceState.hoveredWidgets && showDeuceWidgets model then
      drawTarget deuceWidget pixelPos "red" "1.0" 3
    else
      -- TODO: want to return [], but then targets are never drawn
      -- []
      drawTarget deuceWidget pixelPos "white" "0.0" 0

getBoxWidth start end m =
  let offSet = if start.line == end.line then 0 else 1 in
  let characters = end.col - start.col - offSet in
  toFloat(characters) * m.codeBoxInfo.characterWidth

getBoxHeight start end m =
  let lines = end.line - start.line + 1 in
  toFloat(lines) * m.codeBoxInfo.lineHeight


--------------------------------------------------------------------------------
-- Output Box

outputArea model layout =
  let output =
    case (model.errorBox, model.mode, model.preview) of

      (_, _, Just (_, Err errorMsg)) ->
        textArea errorMsg
          [ Attr.style [ ("width", pixels layout.canvas.width) ] ]

      (Just errorMsg, _, Nothing) ->
        textArea errorMsg
          [ Attr.style [ ("width", pixels layout.canvas.width) ] ]

      (Nothing, Print svgCode, Nothing) ->
        textArea svgCode
          [ Attr.style [ ("width", pixels layout.canvas.width) ] ]

      (_, _, Just (_, Ok _)) ->
        Canvas.build layout.canvas.width layout.canvas.height model

      (Nothing, Live _, Nothing) ->
        Canvas.build layout.canvas.width layout.canvas.height model

      (Nothing, PrintScopeGraph maybeImageData, Nothing) ->
        let srcAttr =
          case maybeImageData of
            Nothing   -> []
            Just data -> [Attr.src data]
        in
        textArea_
          [ Html.img (srcAttr ++
              [ Attr.alt "Dot rendering should appear here..."
              , Attr.style [ ("max-width", pixels layout.canvas.width)
                           , ("max-height", pixels layout.canvas.height) ]
              ]) []
          , Html.br [] [], Html.br [] []
          , DependenceGraph.printHtml model.scopeGraph
          ]
          [ Attr.style [ ("width", pixels layout.canvas.width) ] ]

  in
  let (border, boxShadow) =
    if Model.needsRun model
      then ("2px solid rgba(220,20,60,1)", "10px 10px 5px rgba(220,20,60,0.3)")
      else (params.mainSection.canvas.border, "10px 10px 5px #888888")
  in
  Html.div
     [ Attr.id "outputArea"
     , Attr.style
         [ ("width", pixels layout.canvas.width)
         , ("height", pixels layout.canvas.height)
         , ("position", "fixed")
         , ("border", border)
         , ("left", pixels layout.canvas.left)
         , ("top", pixels layout.canvas.top)
         , ("background", "white")
         , ("border-radius", "0px 10px 10px 10px")
         , ("box-shadow", boxShadow)
         , ("-ms-user-select", "none")
         , ("-moz-user-select", "none")
         , ("-webkit-user-select", "none")
         , ("user-select", "none")
         ]
     ]
     [ output ]


--------------------------------------------------------------------------------
-- Resizing Widgets

rResizeWidgetBall = 5

resizeWidget id model layout (getOffset, putOffset) left top =
  Svg.svg
    [ Attr.id id
    , Attr.style
        [ ("position", "fixed")
        , ("left", pixels (left - 2 * rResizeWidgetBall))
        , ("top", pixels (top - 2 * rResizeWidgetBall))
        , ("width", pixels (4 * rResizeWidgetBall))
        , ("height", pixels (4 * rResizeWidgetBall))
        ]
    ]
    [ flip Svg.circle [] <|
        [ attr "stroke" "black" , attr "stroke-width" "2px"
        , attr "fill" Layout.strButtonTopColor
        , attr "r" (pixels rResizeWidgetBall)
        , attr "cx" (toString (2 * rResizeWidgetBall))
        , attr "cy" (toString (2 * rResizeWidgetBall))
        , attr "cursor" "move"
        , onMouseDown <| Layout.dragLayoutWidgetTrigger (getOffset model) putOffset
        ]
    ]


--------------------------------------------------------------------------------
-- Buttons

type ButtonKind = Regular | Selected | Unselected

buttonRegularColor = "white"
buttonSelectedColor = "lightgray"

htmlButton text onClickHandler btnKind disabled =
  htmlButtonExtraAttrs [] text onClickHandler btnKind disabled

htmlButtonExtraAttrs extraAttrs text onClickHandler btnKind disabled =
  let color =
    case btnKind of
      Regular    -> buttonRegularColor
      Unselected -> buttonRegularColor
      Selected   -> buttonSelectedColor
  in
  -- let lineHeight = 1 + 1.1735 * unpixels params.mainSection.widgets.fontSize |> ((*) 2) |> round |> toFloat |> ((*) 0.5) in -- My best guess based on sampling Firefox's behavior.
  let commonAttrs =
    [ Attr.disabled disabled
    , Attr.style [ ("font", params.mainSection.widgets.font)
                 , ("fontSize", params.mainSection.widgets.fontSize)
                 , ("box-sizing", "border-box") -- Strangely, Firefox and Chrome on Mac differ on this default.
                 , ("min-height", pixels Layout.buttonHeight)
                 , ("background", color)
                 , ("cursor", "pointer")
                 , ("-ms-user-select", "none")
                 , ("-moz-user-select", "none")
                 , ("-webkit-user-select", "none")
                 , ("user-select", "none")
                 ] ]
  in
  Html.button
    (commonAttrs ++
      [ handleEventAndStop "mousedown" Controller.msgNoop
      , onClick onClickHandler
      ] ++
      extraAttrs)
    [ Html.text text ]

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
    , Attr.style [ ("font", params.mainSection.widgets.font)
                 , ("fontSize", params.mainSection.widgets.fontSize)
                 , ("height", pixels Layout.iconButtonHeight)
                 , ("background", color)
                 , ("cursor", "pointer")
                 , ("-ms-user-select", "none")
                 , ("-moz-user-select", "none")
                 , ("-webkit-user-select", "none")
                 , ("user-select", "none")
                 ] ]
  in
  Html.button
    (commonAttrs ++
      [ handleEventAndStop "mousedown" Controller.msgNoop
      , onClick onClickHandler
      , Attr.title iconName
      ] ++
      extraAttrs)
    [ iconHtml ]

runButton =
  htmlButton "Run" Controller.msgRun Regular False

cleanButton model =
  let disabled =
    case model.mode of
      Live _ -> False
      _      -> True
  in
  htmlButton "Clean Up" Controller.msgCleanCode Regular disabled

undoButton model =
  let past = Tuple.first model.history in
  let extraAttrs =
    case past of
      _ :: prevCode :: _ ->
        [ Html.Events.onMouseEnter (Controller.msgPreview (Right prevCode))
        , Html.Events.onMouseLeave Controller.msgClearPreview ]
      _ ->
       []
  in
  htmlButtonExtraAttrs extraAttrs
    "Undo" Controller.msgUndo Regular (List.length past <= 1)

redoButton model =
  let future = Tuple.second model.history in
  let extraAttrs =
    case future of
      futureCode :: _ ->
        [ Html.Events.onMouseEnter (Controller.msgPreview (Right futureCode))
        , Html.Events.onMouseLeave Controller.msgClearPreview ]
      _ ->
       []
  in
  htmlButtonExtraAttrs extraAttrs
    "Redo" Controller.msgRedo Regular (List.length future == 0)

heuristicsButton model =
  let foo old =
    let so = old.syncOptions in
    let so_ = { so | feelingLucky = Sync.toggleHeuristicMode so.feelingLucky } in
    case old.mode of
      Live _ ->
        case mkLive_ so_ old.slideNumber old.movieNumber old.movieTime old.inputExp of
          Ok m_ -> { old | syncOptions = so_, mode = m_ }
          Err s -> { old | syncOptions = so_, errorBox = Just s }
      _ -> { old | syncOptions = so_ }
  in
  let yesno =
    let hm = model.syncOptions.feelingLucky in
    if hm == Sync.heuristicsNone then "None"
    else if hm == Sync.heuristicsFair then "Fair"
    else "Biased"
  in
  htmlButton ("[Heuristics] " ++ yesno)
    (Msg "Toggle Heuristics" foo) Regular False

outputButton model =
  let cap =
     case model.mode of
       Print _ -> "[Out] SVG"
       Live _  -> "[Out] Canvas"
       PrintScopeGraph _ -> "[Out] Scope Graph"
  in
  htmlButton cap Controller.msgToggleOutput Regular False

ghostsButton model =
  let cap =
     case model.showGhosts of
       True  -> "[Ghosts] Shown"
       False -> "[Ghosts] Hidden"
  in
  let foo old =
    let showGhosts_ = not old.showGhosts in
    let mode_ =
      case old.mode of
        Print _ -> Print (LangSvg.printSvg showGhosts_ old.slate)
        _       -> old.mode
    in
    { old | showGhosts = showGhosts_, mode = mode_ }
  in
  htmlButton cap (Msg "Toggle Ghosts" foo) Regular False

autoSynthesisButton model =
  let cap =
     case model.autoSynthesis of
       True  -> "[Auto-Search] On"
       False -> "[Auto-Search] Off"
  in
  htmlButton cap
    (Msg "Toggle Auto-Search" (\m -> { m | autoSynthesis = not m.autoSynthesis }))
    Regular False

codeBoxButton model =
  let text = "[Code Box] " ++ if model.basicCodeBox then "Basic" else "Fancy" in
  htmlButton text Controller.msgToggleCodeBox Regular False

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
  --htmlButton cap (Msg cap (\m -> { m | tool = tool })) btnKind disabled
  iconButton model cap (Msg cap (\m -> { m | tool = tool })) btnKind disabled

relateButton model text handler =
  let noFeatures = Set.isEmpty model.selectedFeatures in
  htmlButton text handler Regular noFeatures

groupButton model text handler disallowSelectedFeatures =
  let noFeatures = Set.isEmpty model.selectedFeatures in
  let noBlobs = Dict.isEmpty model.selectedBlobs in
  htmlButton text handler Regular
    (noBlobs || (disallowSelectedFeatures && (not noFeatures)))

previousSlideButton model =
  htmlButton "◀◀" Controller.msgPreviousSlide Regular
    (model.slideNumber == 1 && model.movieNumber == 1)

nextSlideButton model =
  htmlButton "▶▶" Controller.msgNextSlide Regular
    (model.slideNumber == model.slideCount
      && model.movieNumber == model.movieCount)

previousMovieButton model =
  htmlButton "◀" Controller.msgPreviousMovie Regular
    (model.slideNumber == 1 && model.movieNumber == 1)

nextMovieButton model =
  htmlButton "▶" Controller.msgNextMovie Regular
    (model.slideNumber == model.slideCount
      && model.movieNumber == model.movieCount)

pauseResumeMovieButton model =
  let enabled = model.movieTime < model.movieDuration in
  let caption =
    if enabled && not model.runAnimation then "Play"
    else "Pause"
  in
  htmlButton caption Controller.msgPauseResumeMovie Regular (not enabled)

fileNewDialogBoxButton =
  htmlButton "New" (Controller.msgOpenDialogBox New) Regular False

fileSaveAsDialogBoxButton =
  htmlButton "Save As" (Controller.msgOpenDialogBox SaveAs) Regular False

fileSaveButton model =
  htmlButton "Save" Controller.msgSave Regular (not model.needsSave)

fileOpenDialogBoxButton =
  htmlButton "Open" (Controller.msgOpenDialogBox Open) Regular False

closeDialogBoxButton db model =
  htmlButton
    "X"
    (Controller.msgCloseDialogBox db)
    Regular
    (Model.isDialogBoxShowing AlertSave model)

exportCodeButton =
  htmlButton "Export Code" Controller.msgExportCode Regular False

importCodeButton =
    htmlButton "Import Code" (Controller.msgOpenDialogBox ImportCode) Regular False

exportSvgButton =
  htmlButton "Export SVG" Controller.msgExportSvg Regular False

importSvgButton =
   htmlButton "Import SVG" Controller.msgNoop Regular True

-- autosaveButton model =
--     let cap = case model.autosave of
--       True  -> "[Autosave] Yes"
--       False -> "[Autosave] No"
--     in
--       htmlButton cap Controller.msgToggleAutosave Regular True

fontSizeButton model =
  let cap = "Font Size " ++ Utils.bracks (toString model.codeBoxInfo.fontSize) in
  let msg = Msg "Update Font Size" <| \m ->
    let codeBoxInfo = m.codeBoxInfo in
    let (minSize, maxSize) = (10, 24) in
    let fontSize =
      if codeBoxInfo.fontSize + 2 <= maxSize
        then codeBoxInfo.fontSize + 2
        else minSize
    in
    { m | codeBoxInfo = { codeBoxInfo | fontSize = fontSize } }
  in
  htmlButton cap msg Regular False

{-
deuceWidgetsButton model =
  let text =
    if model.showAllDeuceWidgets
      then "[Show Widgets] Always"
      else "[Show Widgets] On Shift" in
  let handler = Msg "Toggle Ace Deuce" <| \m ->
    if m.showAllDeuceWidgets then
      { m | showAllDeuceWidgets = not m.showAllDeuceWidgets
            -- TODO: factor these three elsewhere for reuse
          , selectedEIds = Set.empty
          , selectedExpTargets = Set.empty
          , selectedPats = Set.empty
          , selectedPatTargets = Set.empty
          }
    else { m | showAllDeuceWidgets = not m.showAllDeuceWidgets }
  in
  htmlButton text handler Regular False
-}

deuceMoveExpButton model =
  htmlButton "Move Exp" Controller.msgMoveExp Regular (not model.showAllDeuceWidgets)


--------------------------------------------------------------------------------
-- Lambda Tool Dropdown Menu

{-
dropdownLambdaTool model =
  let options =
    let (selectedIdx, exps) = model.lambdaTools in
    Utils.mapi1 (\(i,lambdaTool) ->
      let s = strLambdaTool lambdaTool in
      Html.option
         [ Attr.value s, Attr.selected (i == selectedIdx) ]
         [ Html.text s ]
      ) exps
  in
  let handler selected =
    Msg "Select Lambda Option" <| \model ->
      let (_, exps) = model.lambdaTools in
      let indexedStrings = Utils.mapi1 (\(i,lt) -> (i, strLambdaTool lt)) exps in
      let newSelectedIdx =
        case Utils.findFirst ((==) selected << Tuple.second) indexedStrings of
          Just (i, _) -> i
          Nothing     -> Debug.crash "dropdownLambdaTools"
      in
      { model | tool = Lambda, lambdaTools = (newSelectedIdx, exps) }
  in
  Html.select
    [ on "change" (Json.Decode.map handler Html.Events.targetValue)
    , handleEventAndStop "mousedown" Controller.msgNoop
        -- to prevent underlying toolBox from starting dragLayoutWidgetTrigger
    , Attr.style
        [ ("pointer-events", "auto")
        , ("border", "0 solid")
        , ("width", "140px")
        , ("height", pixels Layout.buttonHeight)
        , ("font-family", params.mainSection.widgets.font)
        , ("font-size", params.mainSection.widgets.fontSize)

        -- https://stackoverflow.com/questions/24210132/remove-border-radius-from-select-tag-in-bootstrap-3
        , ("outline", "1px solid #CCC")
        , ("outline-offset", "-1px")
        , ("background-color", "white")
        ]
    ]
    options
-}

lambdaDrawToolBoxWithIcons model layout =
  let buttons =
    Utils.mapi1 (\(i, lambdaTool) ->
      let iconName = Model.strLambdaTool lambdaTool in
      iconButton model iconName
        (Msg iconName (\m -> { m | tool = Lambda i }))
        (if model.tool == Lambda i then Selected else Unselected)
        False
      ) model.lambdaTools
  in
  toolBox model "lambdaDrawToolBox" []
    Layout.getPutDrawToolBox layout.lambdaDrawTools
    buttons


--------------------------------------------------------------------------------
-- Hover Caption

captionArea model layout =
  let (text, color) =
    case (model.caption, model.mode, model.mouseMode) of
      (Just (Hovering zoneKey), Live info, MouseNothing) ->
        case Sync.hoverInfo zoneKey info of
          (line1, Nothing) ->
            (line1 ++ " (INACTIVE)", "red")
          (line1, Just line2) ->
            (line1 ++ " (ACTIVE)\n" ++ line2, "green")

      (Just (LangError err), _, _) ->
        (err, "black")

      _ ->
        if model.slideCount > 1 then
          let
            s1 = toString model.slideNumber ++ "/" ++ toString model.slideCount
            s2 = toString model.movieNumber ++ "/" ++ toString model.movieCount
            s3 = truncateFloat model.movieTime ++ "/" ++ truncateFloat model.movieDuration
          in
          (Utils.spaces ["Slide", s1, ":: Movie", s2, ":: Time", s3], "black")

        else
          ("", "white")

  in
  Html.span
    [ Attr.id "captionArea"
    , Attr.style <|
        [ ("color", color)
        , ("position", "fixed")
        ] ++ Layout.fixedPosition layout.captionArea
    ]
    [ Html.text text ]

truncateFloat : Float -> String
truncateFloat n =
  case String.split "." (toString n) of
    [whole]           -> whole ++ "." ++ String.padRight 1 '0' ""
    [whole, fraction] -> whole ++ "." ++ String.left 1 (String.padRight 1 '0' fraction)
    _                 -> Debug.crash "truncateFloat"

--------------------------------------------------------------------------------
-- Dialog Boxes

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
      closeButton =
        if closable then
          [ closeDialogBoxButton db model ]
        else
          []
      displayStyle =
        if (Model.isDialogBoxShowing db model) then
          "flex"
        else
          "none"
    in
      Html.div
        [ Attr.style
            [ ("position", "fixed")
            , ("top", "50%")
            , ("left", "50%")
            , ("width", width)
            , ("height", height)
            , ("font-family", "sans-serif")
            , ("background-color", "#F8F8F8")
            , ("border", "2px solid " ++ Layout.strInterfaceColor)
            , ("border-radius", "10px")
            , ("box-shadow", "0 0 10px 0 #888888")
            , ("transform", "translateY(-50%) translateX(-50%)")
            , ("margin", "auto")
            , ("z-index", zIndex)
            , ("display", displayStyle)
            , ("flex-direction", "column")
            ]
        ] <|
        [ Html.h2
            [ Attr.style <|
                [ ("margin", "0")
                , ("padding", "0 20px")
                , ("border-bottom", "1px solid black")
                , ("flex", "0 0 60px")
                , ("display", "flex")
                , ("justify-content", "space-between")
                , ("align-items", "center")
                ] ++ headerStyles
            ] <|
            [ Html.div [] headerElements
            , Html.div [] closeButton
            ]
        , Html.div
            [ Attr.style <|
                [ ("overflow", "scroll")
                , ("flex-grow", "1")
                ] ++ parentStyles
            ]
            elements
        ]

bigDialogBox = dialogBox "100" "85%" "85%"

smallDialogBox = dialogBox "101" "35%" "35%"

fileNewDialogBox model =
  let viewTemplate (name, _) =
        Html.div
          [ Attr.style
              [ ("font-family", "monospace")
              , ("font-size", "1.2em")
              , ("padding", "20px")
              , ("border-bottom", "1px solid black")
              ]
          ]
          [ htmlButton
              name
              (Controller.msgAskNew name model.needsSave)
              Regular
              False
          ]
  in
    bigDialogBox
      True
      New
      model
      []
      [Html.text "New..."]
      []
      (List.map viewTemplate Examples.list)

fileSaveAsDialogBox model =
  let saveAsInput =
        Html.div
          [ Attr.style
            [ ("font-family", "monospace")
            , ("font-size", "1.2em")
            , ("padding", "20px")
            , ("text-align", "right")
            ]
          ]
          [ Html.input
              [ Attr.type_ "text"
              , onInput Controller.msgUpdateFilenameInput
              ]
              []
          , Html.text ".little"
          , Html.span
              [ Attr.style
                  [ ("margin-left", "20px")
                  ]
              ]
              [ htmlButton "Save" Controller.msgSaveAs Regular False ]
          ]
  in
    bigDialogBox
      True
      SaveAs
      model
      []
      [Html.text "Save As..."]
      []
      ((List.map viewFileIndexEntry model.fileIndex) ++ [saveAsInput])

fileOpenDialogBox model =
  let fileOpenRow filename =
        Html.div
          [ Attr.style
            [ ("font-family", "monospace")
            , ("font-size", "1.2em")
            , ("padding", "20px")
            , ("border-bottom", "1px solid black")
            , ("overflow", "hidden")
            ]
          ]
          [ Html.span []
              [ Html.b [] [ Html.text filename ]
              , Html.text ".little"
              ]
          , Html.span
              [ Attr.style
                  [ ("float", "right")
                  ]
              ]
              [ htmlButton "Open"
                           (Controller.msgAskOpen filename model.needsSave)
                           Regular
                           False
              , Html.span
                  [ Attr.style
                    [ ("margin-left", "30px")
                    ]
                  ]
                  [ htmlButton "Delete"
                               (Controller.msgDelete filename)
                               Regular
                               False
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
    [ Attr.style
        [ ("font-family", "monospace")
        , ("font-size", "1.2em")
        , ("padding", "20px")
        , ("border-bottom", "1px solid black")
        ]
    ]
    [ Html.span []
        [ Html.b [] [ Html.text filename ]
        , Html.text ".little"
        ]
    ]

fileIndicator model =
  let
    filenameHtml =
      Html.text (Model.prettyFilename model)
    wrapper =
      if model.needsSave then
        Html.i [] [ filenameHtml, Html.text " *" ]
      else
        filenameHtml
  in
    Html.span
      [ Attr.style
          [ ("color", "white")
          , ("font-family", "sans-serif")
          , ("padding", "7px")
          ]
      ]
      [ wrapper ]
{-
      [ Html.u [] [ Html.text "File" ]
      , Html.text ": "
      , wrapper
      ]
-}

alertSaveDialogBox model =
  smallDialogBox
    False
    AlertSave
    model
    []
    [ Html.span
        [ Attr.style [("color", "#550000")] ]
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
            [ htmlButton "Cancel" Controller.msgCancelFileOperation Regular False
            , Html.span
                [ Attr.style
                    [ ("margin-left", "30px")
                    ]
                ]
                [ htmlButton "Yes (Discard Changes)" Controller.msgConfirmFileOperation Regular False ]
            ]
        ]
    ]

importCodeDialogBox model =
  bigDialogBox
    True
    ImportCode
    model
    []
    [ Html.text "Import Code..." ]
    []
    [ Html.div
        [ Attr.style
            [ ("padding", "20px")
            , ("text-align", "center")
            ]
        ]
        [ Html.input
            [ Attr.type_ "file"
            , Attr.id Model.importCodeFileInputId
            ]
            []
        , htmlButton
            "Import"
            (Controller.msgAskImportCode model.needsSave)
            Regular
            False
        ]
    ]


--------------------------------------------------------------------------------
-- Deuce Widgets

deuceLayer model layout =
  let find e acc =
    [e] ++ acc
  in
  let widgets =
    expBoundingPolygonPoints (Lang.foldExp find [] model.inputExp) model layout ++
    patBoundingPolygonPoints (findPats model.inputExp) model layout ++
    expTargetIndicator (computeExpTargets model.inputExp) model layout ++
    patTargetIndicator (findPatTargets model.inputExp) model layout
  in
  let svgWidgets =
    [Svg.svg [Attr.id "polygons",
         Attr.style
          [ ("position", "fixed")
          , ("left", pixels (round(model.codeBoxInfo.gutterWidth) + layout.codeBox.left))
          , ("top", pixels layout.codeBox.top)
          , ("width", pixels (layout.codeBox.width - round(model.codeBoxInfo.offsetLeft + model.codeBoxInfo.gutterWidth)))
          , ("height", pixels (layout.codeBox.height - round(model.codeBoxInfo.offsetHeight)))
          ]] widgets ]
  in
  let pointerEvents =
    if showDeuceWidgets model
    then "auto"
    else "none"
  in
  Html.div [ Attr.id "hoveredItem"
            , Attr.style
                -- child div as absolute to overlay on parent div
                -- https://stackoverflow.com/questions/2941189/how-to-overlay-one-div-over-another-div
                [ ("position", "absolute")
                , ("left", pixels layout.codeBox.left)
                , ("top", pixels layout.codeBox.top)
                -- TODO don't want this layer to block clicks to change Ace cursor
                -- , ("width", pixels layout.codeBox.width)
                -- , ("height", pixels layout.codeBox.height)
                , ("width", "0")
                , ("height", "0")
                , ("user-select", "none")
                , ("pointer-events", pointerEvents)
                ]
            -- TODO why are these events here and aceCodeBox?
            , onMouseEnter Controller.msgMouseEnterCodeBox
            , onMouseLeave Controller.msgMouseLeaveCodeBox
            , onClick Controller.msgMouseClickCodeBox
            ] (List.reverse svgWidgets)


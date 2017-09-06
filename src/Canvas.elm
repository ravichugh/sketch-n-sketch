module Canvas exposing (build, buildSvg, iconify)

-- Sketch-n-Sketch Libraries ---------------------------------------------------

import Config exposing (params)
import Utils
import Either exposing (Either(..))
import HtmlUtils exposing (handleEventAndStop)

import Lang exposing (..)
import LangSvg exposing (NodeId, ShapeKind, attr)
import ShapeWidgets exposing
  ( ZoneName, RealZone(..)
  , ShapeFeature, Feature(..), PointFeature(..), DistanceFeature(..), OtherFeature(..)
  , FeatureNum(..)
  )
import SleekLayout exposing (canvasPosition)
import Sync
import Draw
import InterfaceModel exposing (..)
import FastParser exposing (parseE)
import Eval

-- Elm Libraries ---------------------------------------------------------------

import String
import Dict
import Set
import Color
import Keys

import VirtualDom
import Json.Decode
import Svg exposing (Svg)
import Svg.Attributes as SAttr
import Svg.Events exposing (onMouseDown, onMouseUp, onMouseOver, onMouseOut)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing
  ( onClick, onInput, onMouseEnter, onMouseLeave
  , onWithOptions, defaultOptions
  )


--------------------------------------------------------------------------------

strOldInterfaceColor = "rgba(52,73,94,1.0)"
strOldButtonTopColor = "rgba(231,76,60,1.0)" -- from InterfaceButtons example

--------------------------------------------------------------------------------

pixels n = toString n ++ "px"

svgLine      = flip Svg.line []
svgRect      = flip Svg.rect []
svgCircle    = flip Svg.circle []
svgEllipse   = flip Svg.ellipse []
svgPolygon   = flip Svg.polygon []
svgPath      = flip Svg.path []


--------------------------------------------------------------------------------

msgClickZone zoneKey = Msg ("Click Zone" ++ toString zoneKey) <| \old ->
  case old.mode of
    Live info ->
      -- let _ = Debug.log ("Click Zone" ++ toString zoneKey) () in
      let (_, (mx, my)) = SleekLayout.clickToCanvasPoint old (mousePosition old) in
      let trigger = Sync.prepareLiveTrigger info old.inputExp zoneKey in
      let dragInfo = (trigger, (mx, my), False) in
      { old | mouseMode = MouseDragZone zoneKey (Just dragInfo) }
    _ ->
      old

msgMouseClickCanvas = Msg "MouseClickCanvas" <| \old ->
  case (old.tool, old.mouseMode) of
    (Cursor, MouseDragZone _ _) -> old
    (Cursor, MouseNothing) ->
      let dragMode = MouseDragSelect (mousePosition old) old.selectedShapes old.selectedFeatures old.selectedBlobs in
      if old.keysDown == Keys.shift
      then { old | mouseMode = dragMode }
      else { old | mouseMode = dragMode, selectedShapes = Set.empty, selectedFeatures = Set.empty, selectedBlobs = Dict.empty }

    (_ , MouseNothing) ->
      { old | mouseMode = MouseDrawNew NoPointsYet -- No points until drag begins, or (for paths/polys) mouse-up
            , selectedShapes = Set.empty, selectedBlobs = Dict.empty }

    _ -> old


--------------------------------------------------------------------------------


build wCanvas hCanvas model =
  let addZones = case (model.mode, model.preview) of
    (Live _, Nothing) -> model.tool == Cursor
    _                 -> False
  in
  let (widgets, slate) =
    case model.preview of
      Just (_, Ok (val, widgets, slate)) -> (widgets, slate)
      _                                  -> (model.widgets, model.slate)
  in
  let outputShapes = buildSvg (model, addZones) slate in
  let newShape = drawNewShape model in
  let svgWidgets =
    case (model.mode, model.showGhosts) of
      (Live _, True ) -> buildSvgWidgets wCanvas hCanvas widgets model
      _               -> []
  in
  let selectBox = drawSelectBox model in
  Svg.svg
     [ Attr.id "outputCanvas"
     , onMouseDown msgMouseClickCanvas
     , Attr.style
         [ ("width", pixels wCanvas)
         , ("height", pixels hCanvas)
         ]
     ]
     ([outputShapes] ++ newShape ++ svgWidgets ++ selectBox)


--------------------------------------------------------------------------------
-- Compiling to Svg

buildSvg : (Model, Bool) -> LangSvg.RootedIndexedTree -> (Svg Msg)
buildSvg (model, addZones) (i,d) = buildSvg_ (model, addZones) d i

-- START HERE
-- exploring why LangSvg.IndexedTree exists

buildSvg_ : (Model, Bool) -> LangSvg.IndexedTree -> LangSvg.NodeId -> (Svg Msg)
buildSvg_ (model, addZones) d i =
  case Utils.justGet_ ("buildSvg_ " ++ toString i) i d |> .interpreted of
   LangSvg.TextNode text -> VirtualDom.text text
   LangSvg.SvgNode shape attrs childIndices ->
    case (model.showGhosts, Utils.maybeRemoveFirst "HIDDEN" attrs) of
     (False, Just _) -> Svg.svg [] []
     _ ->
      -- TODO: figure out: (LangSvg.attr "draggable" "false")
      let (zones, attrs_) =
        case (addZones, Utils.maybeRemoveFirst "ZONES" attrs) of
          (False, Nothing)     -> ([], attrs)
          (False, Just (_, l)) -> ([], l)
          (True, Nothing) ->
            (makeZones model shape i attrs, attrs)
          (True, Just (aval, l)) -> case aval.interpreted of
            _ ->
              (makeZones model shape i attrs, l)
            -- TODO breaking these for now; see ZoneOptions comment.
{-
            LangSvg.AString "none" ->
              (makeZones model zoneOptions0 shape i attrs, l)
            LangSvg.AString "basic" ->
              let options' = { options | addRot = False, addColor = False } in
              (makeZones model options_ shape i attrs, l)
            _ -> Debug.crash "buildSvg_"
-}
      in
      let children = List.map (buildSvg_ (model, addZones) d) childIndices in
      -- let mainshape = (Svg.node shape) (LangSvg.compileAttrs attrs') children in
      let mainshape =
        let (rawKind, rawAttrs) = LangSvg.desugarShapeAttrs shape attrs_ in
        (Svg.node rawKind) (LangSvg.compileAttrs rawAttrs) children in
      if zones == []
        then mainshape
        else Svg.svg [] (mainshape :: zones)

-- for basic icons, env will be Eval.initEnv.
-- for LambdaTool icons, env will be from result of running main program.
--
iconify env code =
  let
    exp =
      Utils.fromOkay "Error parsing icon"
        <| parseE code
    ((val, _), _) =
      Utils.fromOkay "Error evaluating icon"
        <| Eval.doEval env exp
    tree =
      Utils.fromOkay "Error resolving index tree of icon"
        <| LangSvg.resolveToIndexedTree 1 1 0 val
    svgElements =
      buildSvg ({ initModel | showGhosts = False }, False) tree
    subPadding x =
      x - 10
  in
    Svg.svg
      [ SAttr.width <|
          (SleekLayout.px << subPadding << .width) SleekLayout.iconButton
      , SAttr.height <|
          (SleekLayout.px << subPadding << .height) SleekLayout.iconButton
      ]
      [ svgElements ]

--------------------------------------------------------------------------------

dragZoneEvents id shapeKind realZone =
  let zoneKey = (id, shapeKind, ShapeWidgets.unparseZone realZone) in
  [ onMouseDown (msgClickZone zoneKey)
  , onMouseOver (turnOnCaptionAndHighlights zoneKey)
  , onMouseOut turnOffCaptionAndHighlights
  ]

--------------------------------------------------------------------------------

drawNewShape model =
  case (model.tool, model.mouseMode) of
    (Line _,        MouseDrawNew (TwoPoints pt2 pt1))                             -> Draw.drawNewLine model pt2 pt1
    (Rect _,        MouseDrawNew (TwoPoints pt2 pt1))                             -> Draw.drawNewRect model.keysDown pt2 pt1
    (Oval _,        MouseDrawNew (TwoPoints pt2 pt1))                             -> Draw.drawNewEllipse model.keysDown pt2 pt1
    (Poly _,        MouseDrawNew (PolyPoints (ptLast::pts)))                      -> Draw.drawNewPolygon ptLast pts
    (Path _,        MouseDrawNew (PathPoints (ptLast::pts)))                      -> Draw.drawNewPath ptLast pts
    (PointOrOffset, MouseDrawNew (Offset1DFromExisting pt2 snap ((x1,_),(y1,_)))) -> drawNewPointAndOffset (snap /= NoSnap) pt2 (round x1, round y1)
    (PointOrOffset, MouseDrawNew (TwoPoints (_, pt2) (_, pt1)))                   -> drawNewPointAndOffset False pt2 pt1
    (HelperLine,    MouseDrawNew (TwoPoints pt2 pt1))                             -> Draw.drawNewLine model pt2 pt1
    (Lambda _,      MouseDrawNew (TwoPoints pt2 pt1))                             -> Draw.drawNewRect model.keysDown pt2 pt1
    (Text,          MouseDrawNew (TwoPoints pt2 pt1))                             -> Draw.drawNewRect model.keysDown pt2 pt1
    _                                                                             -> []


drawNewPointAndOffset shouldHighlight (x2, y2) (x1, y1) =
  let (axis, sign, amount) = Draw.horizontalVerticalSnap (x1, y1) (x2, y2) in
  let xyDot = svgXYDot (x1, y1) pointZoneStyles.fill.shown True [] in
  let (arrowParts, _) = svgOffsetWidget1DArrowPartsAndEndPoint ((toFloat x1, dummyTrace), (toFloat y1, dummyTrace)) axis sign (amount, dummyTrace) Nothing shouldHighlight [] in
  [xyDot] ++ arrowParts


--------------------------------------------------------------------------------
-- Widget Layer

svgOffsetWidget1DArrowPartsAndEndPoint ((baseX, baseXTr), (baseY, baseYTr)) axis sign (amount, amountTr) maybeCaptionText isSelected extraStyles =
  let (effectiveAmount, op) =
    case sign of
      Positive -> (amount, Plus)
      Negative -> (-amount, Minus)
  in
  let ((endX, endXTr), (endY, endYTr)) =
    case axis of
      X -> ((baseX + effectiveAmount, TrOp op [baseXTr, amountTr]), (baseY, baseYTr))
      Y -> ((baseX, baseXTr), (baseY + effectiveAmount, TrOp op [baseYTr, amountTr]))
  in
  let lineStyle =
    if isSelected then
      [ attr "stroke" colorPointSelected
      , attr "stroke-width" "5px"
      ]
    else
      [ attr "stroke" "black"
      , attr "stroke-width" "1px"
      , attr "stroke-dasharray" "1,1"
      ]
  in
  let line =
    flip Svg.line [] <|
      [ attrNum "x1" baseX, attrNum "y1" baseY
      , attrNum "x2" endX,  attrNum "y2" endY
      ] ++ lineStyle ++ extraStyles
  in
  let endArrow =
    let arrowOffset = 12 in
    let (opX1, opY1, opX2, opY2) =
      case (axis, Utils.sgn effectiveAmount) of
        (X, 1)  -> ((-), (-), (-), (+))
        (X, -1) -> ((+), (-), (+), (+))
        (Y, 1)  -> ((-), (-), (+), (-))
        _       -> ((-), (+), (+), (+))
    in
    flip Svg.polyline [] <|
      [ attr "fill" "rgba(0,0,0,0.0)"
      , attr "cursor" "pointer"
      , attr "points" <| toString (opX1 endX arrowOffset) ++ "," ++ toString (opY1 endY arrowOffset) ++ " " ++
                         toString endX                    ++ "," ++ toString endY ++ " " ++
                         toString (opX2 endX arrowOffset) ++ "," ++ toString (opY2 endY arrowOffset) ++ " "
      ] ++ lineStyle ++ extraStyles
  in
  let caption =
    let string =
      case maybeCaptionText of
        Just ident -> ident
        Nothing    -> toString amount
    in
    let (x, y, textAnchor) =
      case axis of
        X -> ((baseX + endX) / 2, baseY - 10, "middle")
        Y -> (baseX + 10, (baseY + endY) / 2, "start")
    in
    let maybeBold =
      if isSelected
      then [ attr "font-weight" "bold" ]
      else []
    in
    flip Svg.text_ [VirtualDom.text string] <|
      [ attr "font-family" params.mainSection.uiWidgets.font
      , attr "font-size" params.mainSection.uiWidgets.fontSize
      , attr "text-anchor" textAnchor
      , attr "x" (toString x)
      , attr "y" (toString y)
      ] ++ maybeBold ++ extraStyles
  in
  ([line, caption, endArrow], ((endX, endXTr), (endY, endYTr)))


buildSvgWidgets : Int -> Int -> Widgets -> InterfaceModel.Model -> List (Svg Msg)
buildSvgWidgets wCanvas hCanvas widgets model =
  let
    pad            = params.mainSection.uiWidgets.pad
    wSlider        = params.mainSection.uiWidgets.wSlider
    hSlider        = params.mainSection.uiWidgets.hSlider
    wCaption       = params.mainSection.uiWidgets.wCaption

    numWidgets    = List.length widgets
    wWidget       = wSlider + wCaption + 2*pad
    hWidget       = hSlider + 2*pad
    wToolBoxMax   = toFloat <| wCanvas - 2*pad
    numCols       = floor (wToolBoxMax / wWidget)
    numRows       = ceiling (toFloat numWidgets / toFloat numCols)
    wToolBox      = numCols * wWidget
    hToolBox      = numRows * hWidget
    xL            = pad
    yBL           = hCanvas - hWidget - pad
  in

  let drawNumWidget i_ widget locId cap_ minVal maxVal curVal =
    let i = i_ - 1 in
    let idAsShape = -2 - i_ in
    let
      (r,c) = (i % numRows, i // numRows)
      xi    = xL  + c*wWidget
      yi    = yBL - r*hWidget
      xi_   = xi + pad
      yi_   = yi + pad
    in
    let region =
      flip Svg.rect [] <|
        [ attr "fill" "lightgray"
        , attr "stroke" strOldInterfaceColor , attr "stroke-width" "3px"
        , attr "rx" "9px" , attr "ry" "9px"
        , attr "x" (toString (xL  + c*wWidget))
        , attr "y" (toString (yBL - r*hWidget))
        , attr "width" (toString wWidget) , attr "height" (toString hWidget)
        ]
    in
    let box =
      let nodeIdAndFeatureName =
        (idAsShape, "slider")
      in
      let color =
        case model.tool of
          Cursor ->
            if Set.member nodeIdAndFeatureName model.selectedFeatures
              then colorPointSelected
              else strOldInterfaceColor -- colorPointNotSelected
          _ -> strOldInterfaceColor
      in
      flip Svg.rect [] <|
        [ attr "fill" color
        , attr "stroke" "20px", attr "stroke-width" "20px"
        , attr "x" (toString (xL  + c*wWidget + pad))
        , attr "y" (toString (yBL - r*hWidget + pad))
        , attr "width" (toString wSlider) , attr "height" (toString hSlider)
        , onMouseDownAndStop (toggleSelected [nodeIdAndFeatureName])
        ]
    in
    let ball =
      let (range, diff) = (maxVal - minVal, curVal - minVal) in
      let pct = diff / range in
      let cx = xi + pad + round (pct*wSlider) in
      let cy = yi + pad + (hSlider//2) in
      flip Svg.circle [] <|
        [ attr "stroke" "black" , attr "stroke-width" "2px"
        , attr "fill" strOldButtonTopColor
        , attr "r" params.mainSection.uiWidgets.rBall
        , attr "cx" (toString cx) , attr "cy" (toString cy)
        , cursorOfZone ZSlider "default"
        ] ++ dragZoneEvents idAsShape "slider" ZSlider
    in
    let text =
      let cap = cap_ ++ strNumTrunc 5 curVal in
      flip Svg.text_ [VirtualDom.text cap] <|
        [ attr "fill" "black"
        , attr "font-family" params.mainSection.uiWidgets.font
        , attr "font-size" params.mainSection.uiWidgets.fontSize
        , attr "x" (toString (xi_ + wSlider + 10))
        , attr "y" (toString (yi_ + 18))
        ]
    in
    [region, box, text, ball]
  in
  let drawPointWidget i_ widget (cx, cxTr) xLazyVal (cy, cyTr) yLazyVal =
    let idAsShape = -2 - i_ in
    zoneSelectCrossDot model True (idAsShape, "point", LonePoint) (cx, cxTr) xLazyVal (cy, cyTr) yLazyVal
    ++ if model.tool /= Cursor then [] else zonePoint model True idAsShape "point" (ZPoint LonePoint) [] [attrNum "cx" cx, attrNum "cy" cy]
  in
  let drawOffsetWidget1D i_ baseXNumTr baseYNumTr axis sign (amount, amountTr) endXLazyVal endYLazyVal =
    let idAsShape = -2 - i_ in
    let isSelected = Set.member (idAsShape, "offset") model.selectedFeatures in
    let dragStyle =
      if model.tool == Cursor then
        [ attr "cursor" "pointer"
        , onMouseEnter (addHoveredShape idAsShape)
        ] ++ dragZoneEvents idAsShape "offset" ZOffset1D
      else
        [ attr "cursor" "default" ]
    in
    let maybeCaptionText = traceToMaybeIdent amountTr in
    let (arrowParts, (endXNumTr, endYNumTr)) =
      let shouldHighlight =
        isSelected || isTraceInModelHighlights model amountTr
      in
      svgOffsetWidget1DArrowPartsAndEndPoint (baseXNumTr, baseYNumTr) axis sign (amount, amountTr) maybeCaptionText shouldHighlight dragStyle
    in
    let endPt =
      zoneSelectCrossDot model False (idAsShape, "offset", EndPoint) endXNumTr endXLazyVal endYNumTr endYLazyVal
    in
    if amount /= 0 then
      [ Svg.g
          [onMouseLeave (removeHoveredShape idAsShape)]
          <| arrowParts ++ endPt
      ]
    else
      []
  in

  let draw (i_, widget) =
    case widget of

      WNumSlider _ _ _ _ _ True -> []
      WIntSlider _ _ _ _ _ True -> []

      WNumSlider minVal maxVal cap curVal (k,_,_) False ->
        drawNumWidget i_ widget k cap minVal maxVal curVal

      WIntSlider a b cap c (k,_,_) False ->
        let (minVal, maxVal, curVal) = (toFloat a, toFloat b, toFloat c) in
        drawNumWidget i_ widget k cap minVal maxVal curVal

      WPoint xNumTr xLazyVal yNumTr yLazyVal ->
        drawPointWidget i_ widget xNumTr xLazyVal yNumTr yLazyVal

      WOffset1D baseXNumTr baseYNumTr axis sign amountNumTr endXLazyVal endYLazyVal ->
        drawOffsetWidget1D i_ baseXNumTr baseYNumTr axis sign amountNumTr endXLazyVal endYLazyVal
  in

  List.concat <| Utils.mapi1 draw widgets

--------------------------------------------------------------------------------
-- Select Box

drawSelectBox : Model -> List (Svg Msg)
drawSelectBox model =
  case model.mouseMode of
    MouseDragSelect initialPosition _ _ _ ->
      let pos1 = canvasPosition model initialPosition in
      let pos2 = canvasPosition model (mousePosition model) in
      let top   = min pos1.y pos2.y in
      let left  = min pos1.x pos2.x in
      let bot   = max pos1.y pos2.y in
      let right = max pos1.x pos2.x in
      List.singleton <|
      flip Svg.rect [] <|
        [ attr "fill" "none"
        , attr "stroke" "black"
        , attr "stroke-width" "2px"
        , attr "stroke-dasharray" "5,5"
        , attr "x" (toString left)
        , attr "y" (toString top)
        , attr "width" (toString <| right - left)
        , attr "height" (toString <| bot - top)
        ]

    _ ->
      []

--------------------------------------------------------------------------------
-- Defining Zones

-- okay to use dummy VTraces/Traces here, b/c compileAttr throws them away
attrNum k n    = LangSvg.compileAttr k (LangSvg.aNum (n, dummyTrace))
attrNumTr k nt = LangSvg.compileAttr k (LangSvg.aNum nt)

onMouseDownAndStop = handleEventAndStop "mousedown"

-- TODO use RealZones rather than Zones more

removeHoveredShape id =
  Msg ("Remove Hovered Shape " ++ toString id) <| \m ->
    { m | hoveredShapes = Set.remove id m.hoveredShapes }

addHoveredShape id =
  Msg ("Add Hovered Shape " ++ toString id) <| \m ->
    if isMouseDown m
    then m
    else { m | hoveredShapes = Set.singleton id }
    -- { m | hoveredShapes = Set.insert id m.hoveredShapes }

addHoveredCrosshair tuple =
  Msg ("Add Hovered Crosshair " ++ toString tuple) <| \m ->
    if isMouseDown m
    then m
    else { m | hoveredCrosshairs = Set.insert tuple m.hoveredCrosshairs }

removeHoveredCrosshair tuple =
  Msg ("Remove Hovered Crosshair " ++ toString tuple) <| \m ->
    { m | hoveredCrosshairs = Set.remove tuple m.hoveredCrosshairs }

cursorStyle s = attr "cursor" s

-- TODO should take into account disabled zones in Live mode
cursorOfZone realZone default = case realZone of

  -- primary manipulation zones
  ZInterior        -> cursorStyle "move"
  ZPoint LonePoint -> cursorStyle "move"
  ZPoint LeftEdge  -> cursorStyle "ew-resize"
  ZPoint RightEdge -> cursorStyle "ew-resize"
  ZPoint TopLeft   -> cursorStyle "nwse-resize"
  ZPoint BotRight  -> cursorStyle "nwse-resize"
  ZPoint TopEdge   -> cursorStyle "ns-resize"
  ZPoint BotEdge   -> cursorStyle "ns-resize"
  ZPoint BotLeft   -> cursorStyle "nesw-resize"
  ZPoint TopRight  -> cursorStyle "nesw-resize"
  ZLineEdge        -> cursorStyle "pointer"
  ZPolyEdge _      -> cursorStyle "pointer"

  -- indirect manipulation zones
  ZOther _         -> cursorStyle "pointer"
  ZSlider          -> cursorStyle "pointer"

  _                -> cursorStyle default

isPrimaryZone realZone =
  case realZone of
    ZOther FillColor     -> False
    ZOther StrokeColor   -> False
    ZOther FillOpacity   -> False
    ZOther StrokeOpacity -> False
    ZOther StrokeWidth   -> False
    ZOther Rotation      -> False
    ZSlider              -> False
    _                    -> True

isFillStrokeZone realZone =
  case realZone of
    ZOther FillColor     -> True
    ZOther StrokeColor   -> True
    ZOther FillOpacity   -> True
    ZOther StrokeOpacity -> True
    ZOther StrokeWidth   -> True
    _                    -> False

isRotateZone realZone =
  case realZone of
    ZOther Rotation -> True
    _               -> False


-- Stuff for Basic Zones -------------------------------------------------------

draggableZone svgFunc addStroke model id shape realZone attrs =
  let showStroke = False in -- set to True for debugging
  flip svgFunc [] <|
    attrs ++
    dragZoneEvents id shape realZone ++
    [ cursorOfZone realZone "default"
    , attr "fill" "rgba(0,0,0,0.0)"
    , attr "stroke-width" <| if addStroke then "10" else "0"
    , attr "stroke" <| if showStroke
                               then "rgba(255,0,0,0.5)"
                               else "rgba(0,0,0,0.0)"
    ]

objectZoneIsCurrentlyBeingManipulated : Model -> NodeId -> (RealZone -> Bool) -> Bool
objectZoneIsCurrentlyBeingManipulated model nodeId zonePred =
  case model.mouseMode of
    MouseDragZone (id, _, zone) _ -> nodeId == id && zonePred (ShapeWidgets.parseZone zone)
    _                             -> False

objectIsCurrentlyBeingManipulated model nodeId =
  objectZoneIsCurrentlyBeingManipulated model nodeId (always True)

boundingBoxZones model id (left, top, right, bot) shapeWidgets =
  let pad = 10 in
  let maybeBackgroundBox =
    if objectIsCurrentlyBeingManipulated model id then []
    else if not (Set.member id model.hoveredShapes) then []
    else
      List.singleton <| svgRect <|
        [ attr "x" (toString (left - pad))
        , attr "y" (toString (top - pad))
        , attr "width" (toString (right - left + 2 * pad))
        , attr "height" (toString (bot - top + 2 * pad))
        , attr "fill" "rgba(100,100,100,0.0)"
        , attr "stroke" "lightgray"
        , attr "stroke-width" "1"
        ]
  in
  -- using group so that the onMouseLeave handler gets attached to
  -- all the nested widgets. probably not needed if pad >> 0, since
  -- will have to mouseLeave the backgroundBox after any other shapes.
  Svg.g
    [onMouseLeave (removeHoveredShape id) ]
    (maybeBackgroundBox ++ shapeWidgets)

minLengthForMiddleZones = 30

eightCardinalZones model id shape transform (left, top, right, bot) =
  let (width, height) = (right - left, bot - top) in
  let ifEnoughSpace len xs = if len < minLengthForMiddleZones then [] else xs in
  let mkPoint realZone cx cy =
    zonePoint model False id shape realZone transform [attrNum "cx" cx, attrNum "cy" cy]
  in
    mkPoint (ZPoint TopLeft) left top ++
    mkPoint (ZPoint TopRight) right top ++
    mkPoint (ZPoint BotLeft) left bot ++
    mkPoint (ZPoint BotRight) right bot ++
    ifEnoughSpace height (mkPoint (ZPoint LeftEdge) left (top + height / 2)) ++
    ifEnoughSpace height (mkPoint (ZPoint RightEdge) right (top + height / 2)) ++
    ifEnoughSpace width (mkPoint (ZPoint TopEdge) (left + width / 2) top) ++
    ifEnoughSpace width (mkPoint (ZPoint BotEdge) (left + width / 2) bot)

pointZoneStyles =
  { radius = "6"
  , stroke = "black"
  , strokeWidth = "2"
  , fill =
      { shown = "white" -- "silver" -- "rgba(255,0,0,0.5)"
      , selectedShape = "yellow"
      , selectedBlob = "aqua" -- "rgba(255,255,0,1.0)"
      , hidden = "rgba(0,0,0,0.0)"
      }
  }

pointZoneStylesFillSelected model nodeId =
  let d = Dict.filter (\_ nodeId_ -> nodeId == nodeId_) model.selectedBlobs in
  if Dict.isEmpty d
    then pointZoneStyles.fill.selectedShape
    else pointZoneStyles.fill.selectedBlob

zonePoint model alwaysShow id shapeKind realZone transform attrs =
  let maybeStyles =
    let maybeStyles_ () =
      if objectZoneIsCurrentlyBeingManipulated model id ((==) realZone) then
        Just (pointZoneStylesFillSelected model id)
      else if objectIsCurrentlyBeingManipulated model id then
        Nothing
      else if Set.member id model.selectedShapes then
        Just (pointZoneStylesFillSelected model id)
      else if Set.member id model.hoveredShapes || alwaysShow then
        Just pointZoneStyles.fill.shown
      else
        Nothing
    in
    case ShapeWidgets.zoneToCrosshair shapeKind realZone of
      Nothing -> maybeStyles_ ()
      Just (xFeature, yFeature) ->
        if Set.member (id, xFeature, yFeature) model.hoveredCrosshairs
        then Nothing
        else maybeStyles_ ()
  in
  case maybeStyles of
    Nothing -> []
    Just fill ->
      List.singleton <| svgCircle <|
        [ attr "r" pointZoneStyles.radius
        , attr "fill" fill
        , attr "stroke" pointZoneStyles.stroke
        , attr "stroke-width" pointZoneStyles.strokeWidth
        , cursorOfZone realZone "pointer"
        ] ++
        dragZoneEvents id shapeKind realZone ++
        transform ++
        attrs

zonePoints model id shape transform pts =
  List.concat <| flip Utils.mapi1 pts <| \(i, (x,y)) ->
    zonePoint model False id shape (ZPoint (Point i)) transform
      [ attrNumTr "cx" x, attrNumTr "cy" y ]

-- TODO rename this once original zonePoints is removed
zonePoints2 model id shape transform pts =
  List.concat <| flip Utils.mapi1 pts <| \(i, (x,y)) ->
    zonePoint model False id shape (ZPoint (Point i)) transform
      [ attrNum "cx" x, attrNum "cy" y ]

zoneLine model id shape realZone (x1,y1) (x2,y2) attrs =
  draggableZone Svg.line True model id shape realZone <|
    [ attrNumTr "x1" x1 , attrNumTr "y1" y1
    , attrNumTr "x2" x2 , attrNumTr "y2" y2
    , cursorStyle "pointer"
    ] ++ attrs

-- TODO rename this once original zoneLine is removed
zoneLine2 model id shape realZone (x1,y1) (x2,y2) attrs =
  draggableZone Svg.line True model id shape realZone <|
    [ attrNum "x1" x1 , attrNum "y1" y1
    , attrNum "x2" x2 , attrNum "y2" y2
    , cursorStyle "pointer"
    ] ++ attrs

-- Stuff for Rotate Zones ------------------------------------------------------

rotZoneDelta = 20

maybeTransformCmds : List LangSvg.Attr -> Maybe (List LangSvg.TransformCmd)
maybeTransformCmds l =
  case Utils.maybeFind "transform" l of
    Just aval -> case aval.interpreted of
      LangSvg.ATransform cmds -> Just cmds
      _                       -> Nothing
    _                         -> Nothing

transformAttr cmds =
  [LangSvg.compileAttr "transform" (LangSvg.aTransform cmds)]

maybeTransformAttr l =
  case maybeTransformCmds l of
    Just cmds -> transformAttr cmds
    Nothing   -> []

zoneRotate model id shape (cx,cy) r maybeCmds =
  let pred z = isPrimaryZone z || isFillStrokeZone z in
  case ( Set.member id model.selectedShapes
       , objectZoneIsCurrentlyBeingManipulated model id pred
       , maybeCmds ) of
    (True, False, Just cmds) -> zoneRotate_ model id shape cx cy r cmds
    _                        -> []

zoneRotate_ model id shape cx cy r cmds =
  let (a, stroke, strokeWidth, rBall) =
      (20, "rgba(192,192,192,0.5)", "5", "7") in
  let (fillBall, swBall) = ("silver", "2") in
  let transform = transformAttr cmds in
  let circle =
    flip Svg.circle [] <|
      [ attr "fill" "none"
      , attr "stroke" stroke , attr "stroke-width" strokeWidth
      , attr "cx" (toString cx) , attr "cy" (toString cy)
      , attr "r"  (toString r)
      ]
  in
  let ball =
    flip Svg.circle [] <|
      [ attr "stroke" "black" , attr "stroke-width" swBall
      , attr "fill" fillBall
      , attr "cx" (toString cx) , attr "cy" (toString (cy - r))
      , attr "r"  rBall
      , cursorOfZone (ZOther Rotation) "default"
      ] ++ transform
        ++ dragZoneEvents id shape (ZOther Rotation)
  in
  let line =
    let (strokeColor, maybeEventHandler) =
      case (cmds, model.tool) of
        ([LangSvg.Rot (_,trace) _ _], Cursor) ->
          let nodeIdAndFeatureName = (id, ShapeWidgets.shapeRotation) in
          let handler = [onMouseDownAndStop (toggleSelected [nodeIdAndFeatureName])] in
          if Set.member nodeIdAndFeatureName model.selectedFeatures
            then (colorPointSelected, handler)
            else (colorPointNotSelected, handler)
        _ ->
          (stroke, [])
    in
    flip Svg.line [] <|
      [ attr "stroke" strokeColor , attr "stroke-width" strokeWidth
      , attr "x1" (toString cx) , attr "y1" (toString cy)
      , attr "x2" (toString cx) , attr "y2" (toString (cy - r))
      ] ++ transform
        ++ maybeEventHandler
  in
  [circle, line, ball]

halfwayBetween (x1,y1) (x2,y2) = ((x1 + x2) / 2, (y1 + y2) / 2)
distance (x1,y1) (x2,y2)       = sqrt ((x2-x1)^2 + (y2-y1)^2)

projPt (x,y)                   = (Tuple.first x, Tuple.first y)
halfwayBetween_ pt1 pt2        = halfwayBetween (projPt pt1) (projPt pt2)
distance_ pt1 pt2              = distance (projPt pt1) (projPt pt2)

-- TODO redo callsite
zoneRotatePolyOrPath model id kind pts nodeAttrs =
  let (xMin, xMax, yMin, yMax) =
    Draw.boundingBoxOfPoints_ (List.map (\(x,y) -> (Tuple.first x, Tuple.first y)) pts) in
  let (w, h) = (xMax - xMin, yMax - yMin) in
  let (xMiddle, yMiddle) = (xMin + 0.5 * w, yMin + 0.5 * h) in
  let r = ((max w h) / 2) + rotZoneDelta in
  zoneRotate model id kind (xMiddle, yMiddle) r (maybeTransformCmds nodeAttrs)


--------------------------------------------------------------------------------

zonesStroke model id shape x y l =
  let (maybeColor, maybeOpacity) = maybeColorNumAttr "stroke" l in
  let maybeStrokeWidth = maybeStrokeWidthNumAttr l in
  zoneStrokeOpacity model id shape (x - wOpacityBox - 5) y maybeOpacity ++
  zoneStrokeColor model id shape x y maybeColor ++
  zoneStrokeWidth model id shape (x + wGradient + 5) y maybeStrokeWidth

zonesFill model id shape x y l =
  let (maybeColor, maybeOpacity) = maybeColorNumAttr "fill" l in
  zoneFillOpacity model id shape (x - wOpacityBox - 5) y maybeOpacity ++
  zoneFillColor model id shape x y maybeColor

zonesFillAndStroke model id shape x y l =
  zonesFill model id shape x y l ++
  zonesStroke model id shape x (y - hZoneColor - 5) l

zoneFillColor   = zoneColor (ZOther FillColor) ShapeWidgets.shapeFill
zoneStrokeColor = zoneColor (ZOther StrokeColor) ShapeWidgets.shapeStroke

zoneFillOpacity   = zoneOpacity (ZOther FillOpacity) ShapeWidgets.shapeFillOpacity
zoneStrokeOpacity = zoneOpacity (ZOther StrokeOpacity) ShapeWidgets.shapeStrokeOpacity


-- Stuff for Color Zones -------------------------------------------------------

wGradient = ShapeWidgets.wColorSlider
hZoneColor = 20

maybeColorNumAttr : String -> List LangSvg.Attr -> (Maybe NumTr, Maybe NumTr)
maybeColorNumAttr k l =
  case Utils.maybeFind k l of
    Just aval -> case aval.interpreted of
      LangSvg.AColorNum (nt, maybeOpacity) -> (Just nt, maybeOpacity)
      _                                    -> (Nothing, Nothing)
    _                                      -> (Nothing, Nothing)

zoneColor realZone shapeFeature model id shape x y maybeColor =
  let pred z = isPrimaryZone z || isRotateZone z in
  let shapeSelected = Set.member id model.selectedShapes in
  let featureSelected =
    Set.member (id, shapeFeature)
               model.selectedFeatures in
  case ( shapeSelected || featureSelected
       , objectZoneIsCurrentlyBeingManipulated model id pred
       , maybeColor ) of
    (True, False, Just nt) -> zoneColor_ realZone shapeFeature model id shape x y nt
    _                      -> []

zoneColor_ : RealZone -> ShapeFeature -> Model -> NodeId -> ShapeKind
          -> Num -> Num -> NumTr -> List (Svg Msg)
zoneColor_ realZone shapeFeature model id shape x y (n, trace) =
  let (w, h, a, stroke, strokeWidth, rBall) =
      (wGradient, hZoneColor, 20, "silver", "2", "7") in
  let yOff = a + rotZoneDelta in
  let nodeIdAndFeatureName = (id, shapeFeature) in
  let ball =
    let cx = x + (n / LangSvg.maxColorNum) * wGradient in
    let cy = y - yOff + (h/2) in
    flip Svg.circle [] <|
      [ attr "stroke" "black" , attr "stroke-width" strokeWidth
      , attr "fill" stroke
      , attr "cx" (toString cx) , attr "cy" (toString cy)
      , attr "r"  rBall
      , cursorOfZone realZone "default"
      ] ++ dragZoneEvents id shape realZone
  in
  let box =
    flip Svg.rect [] <|
      [ attr "fill" <|
          if Set.member nodeIdAndFeatureName model.selectedFeatures
            then colorPointSelected
            else "none" -- colorPointNotSelected
      , attr "stroke" stroke , attr "stroke-width" strokeWidth
      , attr "x" (toString x) , attr "y" (toString (y - yOff))
      , attr "width" (toString w) , attr "height" (toString h)
      ]
  in
  -- TODO would probably be faster with an image...
  let gradient () =
    List.map (\i ->
      let (r,g,b) = Utils.numToColor ShapeWidgets.wColorSlider i in

      let fill =
        "rgb" ++ Utils.parens (String.join "," (List.map toString [r,g,b]))
      in
      flip Svg.rect [] <|
        [ attr "fill" fill
        , attr "x" (toString (x+i)) , attr "y" (toString (y - yOff))
        , attr "width" "1" , attr "height" (toString h)
        ]) (List.map toFloat (List.range 0 w))
  in
  [ Svg.g
      [onMouseDownAndStop (toggleSelected [nodeIdAndFeatureName])]
      (gradient () ++ [box])
  , ball
  ]


-- Stuff for Color Opacity Zones -----------------------------------------------

wOpacityBox = ShapeWidgets.wOpacitySlider

-- TODO could abstract the zoneColor, zoneOpacity, and zoneStrokeWidth sliders

zoneOpacity realZone shapeFeature model id shape x y maybeOpacity =
  let pred z = isPrimaryZone z || isRotateZone z in
  let shapeSelected = Set.member id model.selectedShapes in
  let featureSelected =
    Set.member (id, shapeFeature)
               model.selectedFeatures in
  case ( shapeSelected || featureSelected
       , objectZoneIsCurrentlyBeingManipulated model id pred
       , maybeOpacity ) of
    (True, False, Just nt) -> zoneOpacity_ realZone shapeFeature model id shape x y nt
    _                      -> []

zoneOpacity_
   : RealZone -> ShapeFeature -> Model -> NodeId -> ShapeKind
  -> Num -> Num -> NumTr -> List (Svg Msg)
zoneOpacity_ realZone shapeFeature model id shape x y (n, trace) =
  let (w, h, a, stroke, strokeWidth, rBall) =
      (wOpacityBox, 20, 20, "silver", "2", "7") in
  let yOff = a + rotZoneDelta in
  let nodeIdAndFeatureName = (id, shapeFeature) in
  let ball =
    let cx = x + n * wOpacityBox in
    let cy = y - yOff + (h/2) in
    flip Svg.circle [] <|
      [ attr "stroke" "black" , attr "stroke-width" strokeWidth
      , attr "fill" stroke
      , attr "cx" (toString cx) , attr "cy" (toString cy)
      , attr "r"  rBall
      , cursorOfZone realZone "default"
      ] ++ dragZoneEvents id shape realZone
  in
  let box =
    flip Svg.rect [] <|
      [ attr "fill" <|
          if Set.member nodeIdAndFeatureName model.selectedFeatures
            then colorPointSelected
            else "white" -- colorPointNotSelected
      , attr "stroke" stroke , attr "stroke-width" strokeWidth
      , attr "x" (toString x) , attr "y" (toString (y - yOff))
      , attr "width" (toString w) , attr "height" (toString h)
      ]
  in
  [ Svg.g
      [onMouseDownAndStop (toggleSelected [nodeIdAndFeatureName])]
      ([box])
  , ball
  ]


-- Stuff for Stroke Width Zones ------------------------------------------------

wStrokeWidthBox = ShapeWidgets.wStrokeWidthSlider

maybeStrokeWidthNumAttr : List LangSvg.Attr -> Maybe NumTr
maybeStrokeWidthNumAttr l =
  case Utils.maybeFind "stroke-width" l of
    Just aval -> case aval.interpreted of
      LangSvg.ANum n -> Just n
      _              -> Nothing
    _                -> Nothing

zoneStrokeWidth model id shape x y maybeStrokeWidth =
  let pred z = isPrimaryZone z || isRotateZone z in
  let shapeSelected = Set.member id model.selectedShapes in
  let featureSelected =
    Set.member (id, ShapeWidgets.shapeStrokeWidth)
               model.selectedFeatures in
  case ( shapeSelected || featureSelected
       , objectZoneIsCurrentlyBeingManipulated model id pred
       , maybeStrokeWidth ) of
    (True, False, Just nt) -> zoneStrokeWidth_ model id shape x y nt
    _                      -> []

zoneStrokeWidth_ model id shape x y (n, trace) =
  let (w, h, a, stroke, strokeWidth, rBall) =
      (wStrokeWidthBox, LangSvg.maxStrokeWidthNum, 20, "silver", "2", "7") in
  let yOff = a + rotZoneDelta in
  let nodeIdAndFeatureName = (id, ShapeWidgets.shapeStrokeWidth) in
  let box =
    flip Svg.rect [] <|
      [ attr "fill" <|
          if Set.member nodeIdAndFeatureName model.selectedFeatures
            then colorPointSelected
            else "white" -- colorPointNotSelected
      , attr "stroke" stroke , attr "stroke-width" strokeWidth
      , attr "x" (toString x) , attr "y" (toString (y - yOff))
      , attr "width" (toString w) , attr "height" (toString h)
      ]
  in
  let ball =
    let cx = x + (n / LangSvg.maxStrokeWidthNum) * wStrokeWidthBox in
    let cy = y - yOff + (h/2) in
    flip Svg.circle [] <|
      [ attr "stroke" "black" , attr "stroke-width" strokeWidth
      , attr "fill" stroke
      , attr "cx" (toString cx) , attr "cy" (toString cy)
      , attr "r"  rBall
      , cursorOfZone (ZOther StrokeWidth) "default"
      ] ++ dragZoneEvents id shape (ZOther StrokeWidth)
  in
  let triangle =
    let (x0,y0) = (x                   , y - yOff + h/2 ) in
    let (x1,y1) = (x + wStrokeWidthBox , y - yOff       ) in
    let (x2,y2) = (x + wStrokeWidthBox , y - yOff + h   ) in
    svgPath <|
       [ attr "fill" "darkgray"
       , attr "d"
           ("M " ++ toString x0 ++ " " ++ toString y0 ++
           " L " ++ toString x1 ++ " " ++ toString y1 ++
           " L " ++ toString x2 ++ " " ++ toString y2 ++ " Z")
       ]
  in
  [ Svg.g
      [onMouseDownAndStop (toggleSelected [nodeIdAndFeatureName])]
      [box, triangle]
  , ball
  ]


-- Stuff for Delete Zones ------------------------------------------------------

zoneDelete id shape x y transform =
  let b = False in
  if b then zoneDelete_ id shape x y transform else []

zoneDelete_ id shape x y transform =
  let (w, h, stroke, strokeWidth) =
      (20, 20, "silver", "2") in
  let evt =
    let foo old =
      old
      -- { old | slate = Tuple.mapSnd (Dict.insert id LangSvg.dummySvgNode) old.slate }
    in
    onMouseDown (Msg "Delete..." foo) in
  let lines =
    let f x1 y1 x2 y2 =
      flip Svg.line [] <|
        [ attr "stroke" "darkred", attr "stroke-width" strokeWidth
        , attr "x1" (toString x1) , attr "y1" (toString y1)
        , attr "x2" (toString x2) , attr "y2" (toString y2)
        , evt
        ] ++ transform
      in
     [ f x y (x + w) (y + h) , f x (y + h) (x + w) y ] in
  let box =
    flip Svg.rect [] <|
      [ attr "fill" "white"
      , attr "stroke" stroke , attr "stroke-width" strokeWidth
      , attr "x" (toString x) , attr "y" (toString y)
      , attr "width" (toString w) , attr "height" (toString h)
      , evt
      ] ++ transform
  in
  [box] ++ lines


--------------------------------------------------------------------------------
-- Selection Zones

-- http://www.colorpicker.com/

colorPointSelected      = "#38F552" -- "rgba(0,128,0,1.0)"
colorPointNotSelected   = "#F5B038" -- "orange"
colorLineSelected       = "#B4FADB" -- "blue"
colorLineNotSelected    = "#FAB4D3" -- "red"

hairStrokeWidth         = "9"

type alias NodeIdAndAttrName     = (LangSvg.NodeId, String)
type alias NodeIdAndTwoAttrNames = (LangSvg.NodeId, String, String)

type alias NodeIdAndFeature      = (LangSvg.NodeId, ShapeWidgets.ShapeFeature)


toggleSelected nodeIdAndFeatures =
  Msg "Toggle Selected..." <| toggleSelectedLambda nodeIdAndFeatures

toggleSelectedLambda nodeIdAndFeatures =
  \model ->
    -- If only some of the features were selected, we want to select all of
    -- them, not toggle individually.
    let deselect = List.all (flip Set.member model.selectedFeatures) nodeIdAndFeatures in
    let updateSet nodeIdAndFeature acc =
      if deselect
        then Set.remove nodeIdAndFeature acc
        else Set.insert nodeIdAndFeature acc
    in
    { model | selectedFeatures = List.foldl updateSet model.selectedFeatures nodeIdAndFeatures }

svgXYDot (x, y) fill isVisible extraAttrs =
  svgCircle <|
    [ attr "cx" (toString x) , attr "cy" (toString y)
    , attr "fill" fill
    , attr "stroke" pointZoneStyles.stroke
    , attr "stroke-width" pointZoneStyles.strokeWidth
    , attr "r" <|
        if isVisible
        then pointZoneStyles.radius
        else "0"
    ] ++ extraAttrs


maybeZoneSelectCrossDot sideLength model thisCrosshair xNumTr xLazyVal yNumTr yLazyVal =
  if sideLength < minLengthForMiddleZones then []
  else zoneSelectCrossDot model False thisCrosshair xNumTr xLazyVal yNumTr yLazyVal

zoneSelectCrossDot : Model -> Bool -> (Int, ShapeKind, PointFeature)
                  -> NumTr -> LazyVal -> NumTr -> LazyVal -> List (Svg Msg)
zoneSelectCrossDot model alwaysShowDot (id, kind, pointFeature) xNumTr xLazyVal yNumTr yLazyVal =
  let ((xFloat, _), (yFloat, _)) = (xNumTr, yNumTr) in
  let (x, y) = (round xFloat, round yFloat) in
  let xFeatureName = ShapeWidgets.unparseFeatureNum (Just kind) (XFeat pointFeature) in
  let yFeatureName = ShapeWidgets.unparseFeatureNum (Just kind) (YFeat pointFeature) in
  let thisCrosshair = (id, xFeatureName, yFeatureName) in

  let len = 20 in
  let color nodeIdAndFeatureNames =
    if List.all (flip Set.member model.selectedFeatures) nodeIdAndFeatureNames
    then colorPointSelected
    else colorPointNotSelected
  in
  let
    xFeature = (id, xFeatureName)
    yFeature = (id, yFeatureName)
    (xColor, yColor) = (color [xFeature], color [yFeature])
  in
  let (backDisc, frontDisc) =
    let r =
      if Set.member thisCrosshair model.hoveredCrosshairs && model.tool == Cursor
        then toString len
        else "0"
    in
    let backDisc =
      svgCircle <|
        [ attr "r" r
        , attr "cx" (toString x) , attr "cy" (toString y)
        , attr "fill" "rgba(255,255,255,1.0)"
        ]
    in
    let frontDisc =
      svgCircle <|
        [ attr "r" r
        , attr "cx" (toString x) , attr "cy" (toString y)
        , attr "fill" "none"
        , attr "stroke" "black"
        , attr "stroke-width" pointZoneStyles.strokeWidth
        ]
    in
    (backDisc, frontDisc)
  in
  let xyDot =
    let dotFill =
      if Set.member id model.selectedShapes
        then pointZoneStylesFillSelected model id
        else pointZoneStyles.fill.shown
    in
    let isVisible =
      not (objectIsCurrentlyBeingManipulated model id)
         && (alwaysShowDot ||
             Set.member id model.selectedShapes ||
             Set.member id model.hoveredShapes ||
             Set.member thisCrosshair model.hoveredCrosshairs ||
             model.tool /= Cursor)
    in
    let extraAttrs =
      if model.tool == Cursor then
        [ onMouseDownAndStop <| Msg "Select Cross Dot..." <| \model ->
            if Set.member thisCrosshair model.hoveredCrosshairs
              then toggleSelectedLambda [xFeature, yFeature] model
              else { model | hoveredCrosshairs = Set.insert thisCrosshair model.hoveredCrosshairs }
        ]
      else if model.tool == PointOrOffset then
        [ onMouseDownAndStop <| Msg "Begin Offset From Point..." <| \model ->
            { model | mouseMode = MouseDrawNew (Offset1DFromExisting (x, y) NoSnap (xNumTr, yNumTr)) }
        ]
      else
        [ onMouseDownAndStop <| Msg "Mouse Down On Point..." <| \model ->
            { model | mouseState = (Just False, { x = x, y = y }, Just (PointWithProvenance xNumTr xLazyVal yNumTr yLazyVal)) } ]
    in
    svgXYDot (x, y) dotFill isVisible extraAttrs
  in
  let yLine =
    svgLine <|
      [ attr "stroke" yColor
      , attr "stroke-width" <|
          if (Set.member thisCrosshair model.hoveredCrosshairs && model.tool == Cursor) ||
             Set.member yFeature model.selectedFeatures
          then hairStrokeWidth
          else "0"
      , attr "x1" (toString (x-len)) , attr "y1" (toString y)
      , attr "x2" (toString (x+len)) , attr "y2" (toString y)
      ] ++ if model.tool /= Cursor then [] else
        [ onMouseDownAndStop (toggleSelected [yFeature]) ]
  in
  let xLine =
    svgLine <|
      [ attr "stroke" xColor
      , attr "stroke-width" <|
          if (Set.member thisCrosshair model.hoveredCrosshairs && model.tool == Cursor) ||
             Set.member xFeature model.selectedFeatures
          then hairStrokeWidth
          else "0"
      , attr "y1" (toString (y-len)) , attr "x1" (toString x)
      , attr "y2" (toString (y+len)) , attr "x2" (toString x)
      ] ++ if model.tool /= Cursor then [] else
        [ onMouseDownAndStop (toggleSelected [xFeature]) ]
  in
  -- using nested group for onMouseLeave handler
  List.singleton <| Svg.g
    [onMouseLeave (removeHoveredCrosshair thisCrosshair)]
    [backDisc, xLine, yLine, frontDisc, xyDot]

maybeZoneSelectLine sideLength model nodeId kind featureNum pt1 pt2 =
  if sideLength < minLengthForMiddleZones then []
  else zoneSelectLine model nodeId kind featureNum pt1 pt2

zoneSelectLine model nodeId kind featureNum pt1 pt2 =
  let nodeIdAndFeatureName =
    ( nodeId
    , ShapeWidgets.unparseFeatureNum (Just kind) featureNum ) in
  case model.mouseMode of
    MouseDragZone _ _ -> []
    _ ->
     if Set.member nodeId model.hoveredShapes ||
        Set.member nodeIdAndFeatureName model.selectedFeatures
     then zoneSelectLine_ model nodeIdAndFeatureName pt1 pt2
     else []

zoneSelectLine_ model nodeIdAndFeatureName (x1,y1) (x2,y2) =
  let color =
    if Set.member nodeIdAndFeatureName model.selectedFeatures
    then colorLineSelected
    else colorLineNotSelected
  in
  let line =
    svgLine [
        attr "stroke" color
      , attr "stroke-width" hairStrokeWidth
      , attr "x1" (toString x1) , attr "y1" (toString y1)
      , attr "x2" (toString x2) , attr "y2" (toString y2)
      , onMouseDownAndStop (toggleSelected [nodeIdAndFeatureName])
      ]
  in
  [line]

boxySelectZones model id kind boxyNums =

  let drawPoint maybeThreshold feature x y =
    case maybeThreshold of
      Just thresh -> maybeZoneSelectCrossDot thresh model (id, kind, feature) (x, dummyTrace) dummyLazyVal (y, dummyTrace) dummyLazyVal
      Nothing     -> zoneSelectCrossDot model False (id, kind, feature) (x, dummyTrace) dummyLazyVal (y, dummyTrace) dummyLazyVal in

  let drawLine threshold feature pt1 pt2 =
    maybeZoneSelectLine threshold model id kind feature pt1 pt2 in

  let {left, top, right, bot, cx, cy, width, height} = boxyNums in

  let distanceZone f =
    case f of
      DistanceFeature Width   -> drawLine height (DFeat Width) (left,cy) (right,cy)
      DistanceFeature Height  -> drawLine width (DFeat Height) (cx,top) (cx,bot)

      DistanceFeature Radius  -> drawLine width (DFeat Radius) (cx,cy) (right,cy)
      DistanceFeature RadiusX -> drawLine height (DFeat RadiusX) (cx,cy) (right,cy)
      DistanceFeature RadiusY -> drawLine width (DFeat RadiusY) (cx,top) (cx,cy)

      _ -> [] in

  let pointZone f =
    case f of
      PointFeature TopLeft  -> drawPoint Nothing TopLeft left top
      PointFeature TopRight -> drawPoint Nothing TopRight right top
      PointFeature BotLeft  -> drawPoint Nothing BotLeft left bot
      PointFeature BotRight -> drawPoint Nothing BotRight right bot

      PointFeature TopEdge   -> drawPoint (Just width) TopEdge cx top
      PointFeature BotEdge   -> drawPoint (Just width) BotEdge cx bot
      PointFeature LeftEdge  -> drawPoint (Just height) LeftEdge left cy
      PointFeature RightEdge -> drawPoint (Just height) RightEdge right cy
      PointFeature Center    -> drawPoint (Just (min width height)) Center cx cy

      _ -> [] in

  let features = Utils.find "boxySelectZones" ShapeWidgets.simpleKindFeatures kind in
  List.concatMap distanceZone features ++ List.concatMap pointZone features
    -- draw distance zones below point zones


--------------------------------------------------------------------------------

-- TODO significantly refactor point selection zones, by using
-- ShapeWidgets.featuresOfShape, BoxyFeatureEquations, eval FeatureEquation, etc.

makeZones : Model -> String -> LangSvg.NodeId -> List LangSvg.Attr -> List (Svg Msg)
makeZones model shape id l =
  case shape of
    "line"     -> makeZonesLine model id l
    "rect"     -> makeZonesRectOrBox model id shape l
    "BOX"      -> makeZonesRectOrBox model id shape l
    "circle"   -> makeZonesCircle model id l
    "ellipse"  -> makeZonesEllipseOrOval model id shape l
    "OVAL"     -> makeZonesEllipseOrOval model id shape l
    "polygon"  -> makeZonesPoly model shape id l
    "polyline" -> makeZonesPoly model shape id l
    "path"     -> makeZonesPath model shape id l
    -- "g"        -> makeZonesGroup model id l
    _          -> []

makeZonesLine model id l =
  let transform = maybeTransformAttr l in
  let (x1,y1,x2,y2,cx,cy) = ShapeWidgets.evaluateLineFeatures l in
  let (pt1,pt2) = ((x1,y1), (x2,y2)) in
  let bounds =
    let (xMin,xMax) = minMax x1 x2 in
    let (yMin,yMax) = minMax y1 y2 in
    (xMin, yMin, xMax, yMax) in
  let zLine =
    let enter = [ onMouseEnter (addHoveredShape id) ] in
    zoneLine2 model id "line" ZLineEdge pt1 pt2 (transform ++ enter)
  in
  let zonesSelect =
    List.concat
       [ maybeZoneSelectCrossDot (distance pt1 pt2) model (id, "line", Center) (cx, dummyTrace) dummyLazyVal (cy, dummyTrace) dummyLazyVal
       , zoneSelectCrossDot model False (id, "line", Point 1) (x1, dummyTrace) dummyLazyVal (y1, dummyTrace) dummyLazyVal
       , zoneSelectCrossDot model False (id, "line", Point 2) (x2, dummyTrace) dummyLazyVal (y2, dummyTrace) dummyLazyVal]
  in
  let primaryWidgets =
    boundingBoxZones model id bounds <|
      [zLine] ++
      zonesSelect ++
      zonePoints2 model id "line" transform [pt1, pt2]
  in
  let extraWidgets =
    let c = halfwayBetween pt1 pt2 in
    let r = (distance pt1 pt2 / 2) - rotZoneDelta in
    zoneRotate model id "line" (cx, cy) r (maybeTransformCmds l) ++
    zonesStroke model id "line" x2 y2 l
  in
  primaryWidgets :: extraWidgets

makeZonesRectOrBox model id shape l =
  let boxyNums = ShapeWidgets.evaluateBoxyNums shape l in
  let {left, top, right, bot, cx, cy, width, height} = boxyNums in
  let bounds = (left, top, right, bot) in
  let transform = maybeTransformAttr l in
  let zoneInterior =
    draggableZone Svg.rect False model id shape ZInterior <|
      [ attrNum "x" left , attrNum "y" top
      , attrNum "width" width , attrNum "height" height
      , onMouseEnter (addHoveredShape id)
      ] ++ transform
  in
  let zonesSelect = boxySelectZones model id shape boxyNums in
  let primaryWidgets =
    boundingBoxZones model id bounds <|
      [zoneInterior] ++
      zonesSelect ++
      eightCardinalZones model id shape transform bounds
  in
  let extraWidgets =
    let r = rotZoneDelta + (height/2) in
    zoneRotate model id shape (cx,cy) r (maybeTransformCmds l) ++
    zonesFillAndStroke model id shape left top l
  in
  primaryWidgets :: extraWidgets

makeZonesCircle model id l =
  let boxyNums = ShapeWidgets.evaluateBoxyNums "circle" l in
  let {left, top, right, bot, cx, cy, r} = boxyNums in
  let bounds = (left, top, right, bot) in
  let transform = maybeTransformAttr l in
  let zoneInterior =
    draggableZone Svg.circle False model id "circle" ZInterior <|
      [ attrNum "cx" cx, attrNum "cy" cy, attrNum "r" r
      , onMouseEnter (addHoveredShape id)
      ] ++ transform
  in
  let zonesSelect = boxySelectZones model id "circle" boxyNums in
  let primaryWidgets =
     boundingBoxZones model id bounds <|
       [zoneInterior] ++
       zonesSelect ++
       eightCardinalZones model id "circle" transform bounds
  in
  let extraWidgets =
    zoneRotate model id "circle" (cx,cy) (r + rotZoneDelta) (maybeTransformCmds l) ++
    zonesFillAndStroke model id "circle" (cx - r) (cy - r) l
  in
  primaryWidgets :: extraWidgets

makeZonesEllipseOrOval model id shape l =
  let boxyNums = ShapeWidgets.evaluateBoxyNums shape l in
  let {left, top, right, bot, width, height, cx, cy, rx, ry} = boxyNums in
  let bounds = (left, top, right, bot) in
  let transform = maybeTransformAttr l in
  let zoneInterior =
    draggableZone Svg.ellipse False model id shape ZInterior <|
      [ attrNum "cx" cx, attrNum "cy" cy, attrNum "rx" rx, attrNum "ry" ry
      , onMouseEnter (addHoveredShape id)
      ] ++ transform
  in
  let zonesSelect = boxySelectZones model id shape boxyNums in
  let primaryWidgets =
     boundingBoxZones model id bounds <|
       [zoneInterior] ++
       zonesSelect ++
       eightCardinalZones model id shape transform bounds
  in
  let extraWidgets =
    zoneRotate model id shape (cx,cy) (ry + rotZoneDelta) (maybeTransformCmds l) ++
    zonesFillAndStroke model id shape (cx - rx) (cy - ry) l
  in
  primaryWidgets :: extraWidgets

makeZonesPoly model shape id l =
  let _ = Utils.assert "makeZonesPoly" (shape == "polygon" || shape == "polyline") in
  let transform = maybeTransformAttr l in
  let pts = LangSvg.getPolyPoints l in
  let zPts = zonePoints model id shape transform pts in
  let zLines =
    let pairs = Utils.adjacentPairs (shape == "polygon") pts in
    let f (i,(pti,ptj)) = zoneLine model id shape (ZPolyEdge i) pti ptj transform in
    Utils.mapi1 f pairs in
  let zInterior =
    draggableZone Svg.polygon False model id shape ZInterior <|
      [ LangSvg.compileAttr "points" (LangSvg.aPoints pts)
      , onMouseEnter (addHoveredShape id)
      ] ++ transform
  in
{-
  let zInterior =
    if shape == "polygon" || (shape == "polyline" && firstEqLast pts)
    then [ zoneBorder Svg.polygon model id shape "Interior" False transform <|
             [ LangSvg.compileAttr "points" (LangSvg.aPoints pts)
             , onMouseEnter (addHoveredShape id)
             ]
         ]
    else []
  in
-}
  let zRot = zoneRotatePolyOrPath model id "polygon" pts l in
  let zFillAndStroke =
    case pts of
      (((x0,_),(y0,_))::_) ->
        zonesFillAndStroke model id shape x0 y0 l
      _ ->
        Debug.crash "makeZonesPoly" in
  let zSelect =
    let midptCrossDot ((i1, ((xi1, xTr1), (yi1, yTr1))), (i2, ((xi2, xTr2), (yi2, yTr2)))) =
      let (midX, midXTr) = ((xi1+xi2)/2, TrOp Plus [xTr1, xTr2]) in -- Can't divide by two until we unify traces which will allow introduction of constants. Here it only affects drawing new offests (for now).
      let (midY, midYTr) = ((yi1+yi2)/2, TrOp Plus [yTr1, yTr2]) in -- Can't divide by two until we unify traces which will allow introduction of constants. Here it only affects drawing new offests (for now).
      zoneSelectCrossDot model False (id, shape, Midpoint i1) (midX, midXTr) dummyLazyVal (midY, midYTr) dummyLazyVal
    in
    let ptCrossDot (i, (xNumTrI, yNumTrI)) =
      zoneSelectCrossDot model False (id, shape, Point i) xNumTrI dummyLazyVal yNumTrI dummyLazyVal
    in
    let midptCrossDots =
      let ptsI = Utils.mapi1 identity pts in
      let ptsIPairs = Utils.selfZipCircConsecPairs ptsI in
      List.concatMap midptCrossDot ptsIPairs
    in
    let crossDots = List.concat <| Utils.mapi1 ptCrossDot pts in
    midptCrossDots ++ crossDots
  in
  let primaryWidgets =
    let (x1,x2,y1,y2) = Draw.boundingBoxOfPoints_ (List.map (\(x,y) -> (Tuple.first x, Tuple.first y)) pts) in
    boundingBoxZones model id (x1,y1,x2,y2) <|
      [zInterior] ++ zLines ++ zSelect ++ zPts
  in
  primaryWidgets :: zRot ++ zFillAndStroke

firstEqLast xs = Utils.head_ xs == Utils.head_ (List.reverse xs)

makeZonesPath : Model -> String -> Int -> List LangSvg.Attr -> List (Svg Msg)
makeZonesPath model shape id nodeAttrs =
  let _ = Utils.assert "makeZonesPoly" (shape == "path") in
  let transform = maybeTransformAttr nodeAttrs in
  let cmds = Tuple.first <| LangSvg.toPath <| Utils.find_ nodeAttrs "d" in
  let add (mi,pt) acc = case mi of Nothing -> acc
                                   _       -> (mi,pt) :: acc in
  let listOfMaybeIndexWithPt =
    List.foldr (\c acc -> case c of
      LangSvg.CmdZ   s              -> acc
      LangSvg.CmdMLT s pt           -> add pt acc
      LangSvg.CmdHV  s n            -> acc
      LangSvg.CmdC   s pt1 pt2 pt3  -> add pt1 (add pt2 (add pt3 acc))
      LangSvg.CmdSQ  s pt1 pt2      -> add pt1 (add pt2 acc)
      LangSvg.CmdA   s a b c d e pt -> add pt acc) [] cmds
  in
  let pts = List.map Tuple.second listOfMaybeIndexWithPt in
  let dots = zonePoints model id shape transform pts in
  let zRot = zoneRotatePolyOrPath model id "path" pts nodeAttrs in
  let zFillAndStroke =
    case pts of
      (((x0,_),(y0,_))::_) ->
        zonesFillAndStroke model id shape x0 y0 nodeAttrs
      _ ->
        Debug.crash "makeZonesPath"
  in
  let zSelect =
    let ptCrossDot (maybeIndex, (xNumTr, yNumTr)) =
      let i = Utils.fromJust maybeIndex in
      zoneSelectCrossDot model False (id, shape, Point i) xNumTr dummyLazyVal yNumTr dummyLazyVal
    in
    let crossDots = List.concatMap ptCrossDot listOfMaybeIndexWithPt in
    crossDots
  in
  let zInterior =
    draggableZone Svg.path False model id shape ZInterior <|
      [ LangSvg.compileAttr "d" (Utils.find_ nodeAttrs "d")
      , onMouseEnter (addHoveredShape id)
      ] ++ transform
  in
  -- TODO add "Edge" zones
  let primaryWidgets =
    let (x1,x2,y1,y2) = Draw.boundingBoxOfPoints_ (List.map (\(x,y) -> (Tuple.first x, Tuple.first y)) pts) in
    boundingBoxZones model id (x1,y1,x2,y2) <|
      [zInterior] ++
      zSelect ++
      dots
  in
  primaryWidgets :: zRot ++ zFillAndStroke


--------------------------------------------------------------------------------
-- Zone Caption and Highlights

turnOnCaptionAndHighlights zoneKey =
  Msg ("Turn On Caption " ++ toString zoneKey) <| \m ->
    let codeBoxInfo = m.codeBoxInfo in
    let hi = liveInfoToHighlights zoneKey m in
    { m | caption = Just (Hovering zoneKey)
        , codeBoxInfo = { codeBoxInfo | highlights = hi } }

turnOffCaptionAndHighlights =
  Msg "Turn Off Caption" <| \m ->
    let codeBoxInfo = m.codeBoxInfo in
    { m | caption = Nothing
        , codeBoxInfo = { codeBoxInfo | highlights = [] }
        }
{-
        , codeBoxInfo = { codeBoxInfo | highlights = DeuceWidgets.expRangesToHighlights m Nothing ++
                                                     DeuceWidgets.expTargetsToHighlights m Nothing ++
                                                     DeuceWidgets.patRangesToHighlights m Nothing ++
                                                     DeuceWidgets.patTargetsToHighlights m Nothing} }
-}

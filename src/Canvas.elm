module Canvas exposing (build)

-- Sketch-n-Sketch Libraries ---------------------------------------------------

import Config exposing (params)
import Utils
import Either exposing (Either(..))
import HtmlUtils exposing (handleEventAndStop)

import Lang exposing (..)
import LangSvg exposing (NodeId, ShapeKind, attr)
import ShapeWidgets exposing
  ( Zone, RealZone(..)
  , ShapeFeature, Feature(..), PointFeature(..), DistanceFeature(..)
  , FeatureNum(..)
  )
import Layout
import Draw
import InterfaceModel exposing (..)
import InterfaceController as Controller

-- Elm Libraries ---------------------------------------------------------------

import String
import Dict
import Set
import Color

import VirtualDom
import Json.Decode
import Svg exposing (Svg)
import Svg.Events exposing (onMouseDown, onMouseUp, onMouseOver, onMouseOut)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing
  ( onClick, onInput, onMouseEnter, onMouseLeave
  , onWithOptions, defaultOptions
  )


--------------------------------------------------------------------------------

pixels n = toString n ++ "px"

svgLine      = flip Svg.line []
svgRect      = flip Svg.rect []
svgCircle    = flip Svg.circle []
svgEllipse   = flip Svg.ellipse []
svgPolygon   = flip Svg.polygon []
svgPath      = flip Svg.path []


--------------------------------------------------------------------------------

-- NOTE: removed SyncSelect animations from InterfaceView2
build wCanvas hCanvas model =
  let addZones = case model.mode of
    Live _ -> model.tool == Cursor
    _      -> False
  in
  let outputShapes = buildSvg (model, addZones) model.slate in
  let newShape = Draw.drawNewShape model in
  let widgets =
    case (model.mode, model.showGhosts) of
      (Live _, True ) -> buildSvgWidgets wCanvas hCanvas model
      _               -> []
  in
  Svg.svg
     [ Attr.id "outputCanvas"
     , onMouseDown Controller.msgMouseClickCanvas
     , Attr.style
         [ ("width", pixels wCanvas)
         , ("height", pixels hCanvas)
         ]
     ]
     ([outputShapes] ++ newShape ++ widgets)


--------------------------------------------------------------------------------
-- Compiling to Svg

buildSvg : (Model, Bool) -> LangSvg.RootedIndexedTree -> (Svg Msg)
buildSvg options (i,d) = buildSvg_ options d i

buildSvg_ : (Model, Bool) -> LangSvg.IndexedTree -> LangSvg.NodeId -> (Svg Msg)
buildSvg_ stuff d i =
  let (model, addZones) = stuff in
  case Utils.justGet_ ("buildSvg_ " ++ toString i) i d of
   LangSvg.TextNode text -> VirtualDom.text text
   LangSvg.SvgNode shape attrs js ->
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
          (True, Just (aval, l)) -> case aval.av_ of
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
      let children = List.map (buildSvg_ stuff d) js in
      -- let mainshape = (Svg.node shape) (LangSvg.compileAttrs attrs') children in
      let mainshape =
        let (rawKind, rawAttrs) = LangSvg.desugarShapeAttrs shape attrs_ in
        (Svg.node rawKind) (LangSvg.compileAttrs rawAttrs) children in
      if zones == []
        then mainshape
        else Svg.svg [] (mainshape :: zones)


--------------------------------------------------------------------------------

dragZoneEvents zoneKey =
  [ onMouseDown (Controller.msgClickZone zoneKey)
  , onMouseOver (turnOnCaptionAndHighlights zoneKey)
  , onMouseOut turnOffCaptionAndHighlights
  ]

zoneEvents id shape zone = dragZoneEvents (Left (id, shape, zone))
sliderZoneEvents i string = dragZoneEvents (Right (i, string))


--------------------------------------------------------------------------------
-- Widget Layer

buildSvgWidgets : Int -> Int -> Model -> List (Svg Msg)
buildSvgWidgets wCanvas hCanvas model =
  let
    widgets        = model.widgets

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

  let drawNumWidget i_ intOrNum widget locId cap_ minVal maxVal curVal =
    let i = i_ - 1 in
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
        , attr "stroke" Layout.strInterfaceColor , attr "stroke-width" "3px"
        , attr "rx" "9px" , attr "ry" "9px"
        , attr "x" (toString (xL  + c*wWidget))
        , attr "y" (toString (yBL - r*hWidget))
        , attr "width" (toString wWidget) , attr "height" (toString hWidget)
        ]
    in
    let box =
      let color =
        let feature =
          (ShapeWidgets.selectedTypeWidget, -1, "widget" ++ (toString locId))
        in
        case model.tool of
          Cursor ->
            if Set.member feature model.selectedFeatures
              then colorPointSelected
              else Layout.strInterfaceColor -- colorPointNotSelected
          _ -> Layout.strInterfaceColor
      in
      flip Svg.rect [] <|
        [ attr "fill" color
        , attr "stroke" "20px", attr "stroke-width" "20px"
        , attr "x" (toString (xL  + c*wWidget + pad))
        , attr "y" (toString (yBL - r*hWidget + pad))
        , attr "width" (toString wSlider) , attr "height" (toString hSlider)
        , onMouseDown (toggleSelectedWidget locId)
        ]
    in
    let ball =
      let (range, diff) = (maxVal - minVal, curVal - minVal) in
      let pct = diff / range in
      let cx = xi + pad + round (pct*wSlider) in
      let cy = yi + pad + (hSlider//2) in
      flip Svg.circle [] <|
        [ attr "stroke" "black" , attr "stroke-width" "2px"
        , attr "fill" Layout.strButtonTopColor
        , attr "r" params.mainSection.uiWidgets.rBall
        , attr "cx" (toString cx) , attr "cy" (toString cy)
        , cursorOfZone "SliderBall" "default"
        ] ++ sliderZoneEvents i_ intOrNum
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

  let drawPointWidget i_ widget cx cy =
    -- copied from ball above
    let ball =
      flip Svg.circle [] <|
        [ attr "stroke" "black" , attr "stroke-width" "2px"
        , attr "fill" Layout.strButtonTopColor
        , attr "r" params.mainSection.uiWidgets.rBall
        , attr "cx" (toString cx) , attr "cy" (toString cy)
        , cursorOfZone "SliderBall" "default"
        ] ++ sliderZoneEvents i_ "Point"
    in
    [ball]
  in

  let draw (i_, widget) =
    case widget of

      WNumSlider minVal maxVal cap curVal (k,_,_) ->
        drawNumWidget i_ "Num" widget k cap minVal maxVal curVal

      WIntSlider a b cap c (k,_,_) ->
        let (minVal, maxVal, curVal) = (toFloat a, toFloat b, toFloat c) in
        drawNumWidget i_ "Int" widget k cap minVal maxVal curVal

      WPointSlider (xVal, _) (yVal, _) ->
        drawPointWidget i_ widget xVal yVal
  in

  List.concat <| Utils.mapi draw widgets

-- abstract the following with toggleSelected and toggleSelectedBlob
toggleSelectedWidget locId =
  let feature =
    (ShapeWidgets.selectedTypeWidget, -1, "widget" ++ (toString locId))
  in
  Msg ("Toggle Selected Widget " ++ toString locId) <| \model ->
    let update =
      if Set.member feature model.selectedFeatures
        then Set.remove
        else Set.insert
    in
    { model | selectedFeatures = update feature model.selectedFeatures }


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
    { m | hoveredShapes = Set.singleton id }
    -- { m | hoveredShapes = Set.insert id m.hoveredShapes }

addHoveredCrosshair tuple =
  Msg ("Add Hovered Crosshair " ++ toString tuple) <| \m ->
    { m | hoveredCrosshairs = Set.insert tuple m.hoveredCrosshairs }

removeHoveredCrosshair tuple =
  Msg ("Remove Hovered Crosshair " ++ toString tuple) <| \m ->
    { m | hoveredCrosshairs = Set.remove tuple m.hoveredCrosshairs }

cursorStyle s = LangSvg.attr "cursor" s

-- TODO should take into account disabled zones in Live mode
cursorOfZone zone default = case ShapeWidgets.parseZone zone of

  -- primary manipulation zones
  ZInterior        -> cursorStyle "move"
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

isPrimaryZone zone =
  case zone of
    "FillBall"        -> False
    "StrokeBall"      -> False
    "FillOpacityBall"   -> False
    "StrokeOpacityBall" -> False
    "StrokeWidthBall" -> False
    "RotateBall"      -> False
    "SliderBall"      -> False
    _                 -> True

isFillStrokeZone zone =
  case zone of
    "FillBall"        -> True
    "StrokeBall"      -> True
    "FillOpacityBall"   -> True
    "StrokeOpacityBall" -> True
    "StrokeWidthBall" -> True
    _                 -> False

isRotateZone zone =
  case zone of
    "RotateBall"      -> True
    _                 -> False


-- Stuff for Basic Zones -------------------------------------------------------

draggableZone svgFunc addStroke model id shape zone attrs =
  let showStroke = False in -- set to True for debugging
  flip svgFunc [] <|
    attrs ++
    zoneEvents id shape zone ++
    [ cursorOfZone zone "default"
    , LangSvg.attr "fill" "rgba(0,0,0,0.0)"
    , LangSvg.attr "stroke-width" <| if addStroke then "10" else "0"
    , LangSvg.attr "stroke" <| if showStroke
                               then "rgba(255,0,0,0.5)"
                               else "rgba(0,0,0,0.0)"
    ]

objectZoneIsCurrentlyBeingManipulated model nodeId zonePred =
  case model.mouseMode of
    MouseDragZone (Left (id, _, zone)) _ -> nodeId == id && zonePred zone
    _                                    -> False

objectIsCurrentlyBeingManipulated model nodeId =
  objectZoneIsCurrentlyBeingManipulated model nodeId (always True)

boundingBoxZones model id (left, top, right, bot) shapeWidgets =
  let pad = 10 in
  let maybeBackgroundBox =
    if objectIsCurrentlyBeingManipulated model id then []
    else if not (Set.member id model.hoveredShapes) then []
    else
      Utils.singleton <| svgRect <|
        [ LangSvg.attr "x" (toString (left - pad))
        , LangSvg.attr "y" (toString (top - pad))
        , LangSvg.attr "width" (toString (right - left + 2 * pad))
        , LangSvg.attr "height" (toString (bot - top + 2 * pad))
        , LangSvg.attr "fill" "rgba(100,100,100,0.0)"
        , LangSvg.attr "stroke" "lightgray"
        , LangSvg.attr "stroke-width" "1"
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
  let mkPoint zone cx cy =
    zonePoint model id shape zone transform [attrNum "cx" cx, attrNum "cy" cy]
  in
    mkPoint "TopLeft" left top ++
    mkPoint "TopRight" right top ++
    mkPoint "BotLeft" left bot ++
    mkPoint "BotRight" right bot ++
    ifEnoughSpace height (mkPoint "LeftEdge" left (top + height / 2)) ++
    ifEnoughSpace height (mkPoint "RightEdge" right (top + height / 2)) ++
    ifEnoughSpace width (mkPoint "TopEdge" (left + width / 2) top) ++
    ifEnoughSpace width (mkPoint "BotEdge" (left + width / 2) bot)

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

zonePoint model id shape zone transform attrs =
  let maybeStyles =
    let maybeStyles_ () =
      if objectZoneIsCurrentlyBeingManipulated model id ((==) zone) then
        Just (pointZoneStylesFillSelected model id)
      else if objectIsCurrentlyBeingManipulated model id then
        Nothing
      else if Set.member id model.selectedShapes then
        Just (pointZoneStylesFillSelected model id)
      else if Set.member id model.hoveredShapes then
        Just pointZoneStyles.fill.shown
      else
        Nothing
    in
    case ShapeWidgets.zoneToCrosshair shape zone of
      Nothing -> maybeStyles_ ()
      Just (xFeature, yFeature) ->
        if Set.member (id, xFeature, yFeature) model.hoveredCrosshairs
        then Nothing
        else maybeStyles_ ()
  in
  case maybeStyles of
    Nothing -> []
    Just fill ->
      Utils.singleton <| svgCircle <|
        [ LangSvg.attr "r" pointZoneStyles.radius
        , LangSvg.attr "fill" fill
        , LangSvg.attr "stroke" pointZoneStyles.stroke
        , LangSvg.attr "stroke-width" pointZoneStyles.strokeWidth
        , cursorOfZone zone "pointer"
        ] ++
        zoneEvents id shape zone ++
        transform ++
        attrs

zonePoints model id shape transform pts =
  List.concat <| flip Utils.mapi pts <| \(i, (x,y)) ->
    zonePoint model id shape ("Point" ++ toString i) transform
      [ attrNumTr "cx" x, attrNumTr "cy" y ]

-- TODO rename this once original zonePoints is removed
zonePoints2 model id shape transform pts =
  List.concat <| flip Utils.mapi pts <| \(i, (x,y)) ->
    zonePoint model id shape ("Point" ++ toString i) transform
      [ attrNum "cx" x, attrNum "cy" y ]

zoneLine model id shape zone (x1,y1) (x2,y2) attrs =
  draggableZone Svg.line True model id shape zone <|
    [ attrNumTr "x1" x1 , attrNumTr "y1" y1
    , attrNumTr "x2" x2 , attrNumTr "y2" y2
    , cursorStyle "pointer"
    ] ++ attrs

-- TODO rename this once original zoneLine is removed
zoneLine2 model id shape zone (x1,y1) (x2,y2) attrs =
  draggableZone Svg.line True model id shape zone <|
    [ attrNum "x1" x1 , attrNum "y1" y1
    , attrNum "x2" x2 , attrNum "y2" y2
    , cursorStyle "pointer"
    ] ++ attrs

-- Stuff for Rotate Zones ------------------------------------------------------

rotZoneDelta = 20

maybeTransformCmds : List LangSvg.Attr -> Maybe (List LangSvg.TransformCmd)
maybeTransformCmds l =
  case Utils.maybeFind "transform" l of
    Just aval -> case aval.av_ of
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
      [ LangSvg.attr "fill" "none"
      , LangSvg.attr "stroke" stroke , LangSvg.attr "stroke-width" strokeWidth
      , LangSvg.attr "cx" (toString cx) , LangSvg.attr "cy" (toString cy)
      , LangSvg.attr "r"  (toString r)
      ]
  in
  let ball =
    flip Svg.circle [] <|
      [ LangSvg.attr "stroke" "black" , LangSvg.attr "stroke-width" swBall
      , LangSvg.attr "fill" fillBall
      , LangSvg.attr "cx" (toString cx) , LangSvg.attr "cy" (toString (cy - r))
      , LangSvg.attr "r"  rBall
      , cursorOfZone "RotateBall" "default"
      ] ++ transform
        ++ zoneEvents id shape "RotateBall"
  in
  let line =
    let (strokeColor, maybeEventHandler) =
      case (cmds, model.tool) of
        ([LangSvg.Rot (_,trace) _ _], Cursor) ->
          let typeAndNodeIdAndFeature = (ShapeWidgets.selectedTypeShapeFeature, id, ShapeWidgets.shapeRotation) in
          let handler = [onMouseDown (toggleSelected [typeAndNodeIdAndFeature])] in
          if Set.member typeAndNodeIdAndFeature model.selectedFeatures
            then (colorPointSelected, handler)
            else (colorPointNotSelected, handler)
        _ ->
          (stroke, [])
    in
    flip Svg.line [] <|
      [ LangSvg.attr "stroke" strokeColor , LangSvg.attr "stroke-width" strokeWidth
      , LangSvg.attr "x1" (toString cx) , LangSvg.attr "y1" (toString cy)
      , LangSvg.attr "x2" (toString cx) , LangSvg.attr "y2" (toString (cy - r))
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

zoneFillColor   = zoneColor "FillBall" ShapeWidgets.shapeFill
zoneStrokeColor = zoneColor "StrokeBall" ShapeWidgets.shapeStroke

zoneFillOpacity   = zoneOpacity "FillOpacityBall" ShapeWidgets.shapeFillOpacity
zoneStrokeOpacity = zoneOpacity "StrokeOpacityBall" ShapeWidgets.shapeStrokeOpacity


-- Stuff for Color Zones -------------------------------------------------------

wGradient = ShapeWidgets.wColorSlider
hZoneColor = 20

maybeColorNumAttr : String -> List LangSvg.Attr -> (Maybe NumTr, Maybe NumTr)
maybeColorNumAttr k l =
  case Utils.maybeFind k l of
    Just aval -> case aval.av_ of
      LangSvg.AColorNum (nt, maybeOpacity) -> (Just nt, maybeOpacity)
      _                                    -> (Nothing, Nothing)
    _                                      -> (Nothing, Nothing)

zoneColor zoneName shapeFeature model id shape x y maybeColor =
  let pred z = isPrimaryZone z || isRotateZone z in
  let shapeSelected = Set.member id model.selectedShapes in
  let featureSelected =
    Set.member (ShapeWidgets.selectedTypeShapeFeature, id, shapeFeature)
               model.selectedFeatures in
  case ( shapeSelected || featureSelected
       , objectZoneIsCurrentlyBeingManipulated model id pred
       , maybeColor ) of
    (True, False, Just nt) -> zoneColor_ zoneName shapeFeature model id shape x y nt
    _                      -> []

zoneColor_ : Zone -> ShapeFeature -> Model -> NodeId -> ShapeKind
          -> Num -> Num -> NumTr -> List (Svg Msg)
zoneColor_ zoneName shapeFeature model id shape x y (n, trace) =
  let (w, h, a, stroke, strokeWidth, rBall) =
      (wGradient, hZoneColor, 20, "silver", "2", "7") in
  let yOff = a + rotZoneDelta in
  let typeAndNodeIdAndFeature = (ShapeWidgets.selectedTypeShapeFeature, id, shapeFeature) in
  let ball =
    let cx = x + (n / LangSvg.maxColorNum) * wGradient in
    let cy = y - yOff + (h/2) in
    flip Svg.circle [] <|
      [ LangSvg.attr "stroke" "black" , LangSvg.attr "stroke-width" strokeWidth
      , LangSvg.attr "fill" stroke
      , LangSvg.attr "cx" (toString cx) , LangSvg.attr "cy" (toString cy)
      , LangSvg.attr "r"  rBall
      , cursorOfZone zoneName "default"
      ] ++ zoneEvents id shape zoneName
  in
  let box =
    flip Svg.rect [] <|
      [ LangSvg.attr "fill" <|
          if Set.member typeAndNodeIdAndFeature model.selectedFeatures
            then colorPointSelected
            else "none" -- colorPointNotSelected
      , LangSvg.attr "stroke" stroke , LangSvg.attr "stroke-width" strokeWidth
      , LangSvg.attr "x" (toString x) , LangSvg.attr "y" (toString (y - yOff))
      , LangSvg.attr "width" (toString w) , LangSvg.attr "height" (toString h)
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
        [ LangSvg.attr "fill" fill
        , LangSvg.attr "x" (toString (x+i)) , LangSvg.attr "y" (toString (y - yOff))
        , LangSvg.attr "width" "1" , LangSvg.attr "height" (toString h)
        ]) (List.map toFloat (List.range 0 w))
  in
  [ Svg.g
      [onMouseDownAndStop (toggleSelected [typeAndNodeIdAndFeature])]
      (gradient () ++ [box])
  , ball
  ]


-- Stuff for Color Opacity Zones -----------------------------------------------

wOpacityBox = ShapeWidgets.wOpacitySlider

-- TODO could abstract the zoneColor, zoneOpacity, and zoneStrokeWidth sliders

zoneOpacity zoneName shapeFeature model id shape x y maybeOpacity =
  let pred z = isPrimaryZone z || isRotateZone z in
  let shapeSelected = Set.member id model.selectedShapes in
  let featureSelected =
    Set.member (ShapeWidgets.selectedTypeShapeFeature, id, shapeFeature)
               model.selectedFeatures in
  case ( shapeSelected || featureSelected
       , objectZoneIsCurrentlyBeingManipulated model id pred
       , maybeOpacity ) of
    (True, False, Just nt) -> zoneOpacity_ zoneName shapeFeature model id shape x y nt
    _                      -> []

zoneOpacity_
   : Zone -> ShapeFeature -> Model -> NodeId -> ShapeKind
  -> Num -> Num -> NumTr -> List (Svg Msg)
zoneOpacity_ zoneName shapeFeature model id shape x y (n, trace) =
  let (w, h, a, stroke, strokeWidth, rBall) =
      (wOpacityBox, 20, 20, "silver", "2", "7") in
  let yOff = a + rotZoneDelta in
  let typeAndNodeIdAndFeature = (ShapeWidgets.selectedTypeShapeFeature, id, shapeFeature) in
  let ball =
    let cx = x + n * wOpacityBox in
    let cy = y - yOff + (h/2) in
    flip Svg.circle [] <|
      [ LangSvg.attr "stroke" "black" , LangSvg.attr "stroke-width" strokeWidth
      , LangSvg.attr "fill" stroke
      , LangSvg.attr "cx" (toString cx) , LangSvg.attr "cy" (toString cy)
      , LangSvg.attr "r"  rBall
      , cursorOfZone zoneName "default"
      ] ++ zoneEvents id shape zoneName
  in
  let box =
    flip Svg.rect [] <|
      [ LangSvg.attr "fill" <|
          if Set.member typeAndNodeIdAndFeature model.selectedFeatures
            then colorPointSelected
            else "white" -- colorPointNotSelected
      , LangSvg.attr "stroke" stroke , LangSvg.attr "stroke-width" strokeWidth
      , LangSvg.attr "x" (toString x) , LangSvg.attr "y" (toString (y - yOff))
      , LangSvg.attr "width" (toString w) , LangSvg.attr "height" (toString h)
      ]
  in
  [ Svg.g
      [onMouseDownAndStop (toggleSelected [typeAndNodeIdAndFeature])]
      ([box])
  , ball
  ]


-- Stuff for Stroke Width Zones ------------------------------------------------

wStrokeWidthBox = ShapeWidgets.wStrokeWidthSlider

maybeStrokeWidthNumAttr : List LangSvg.Attr -> Maybe NumTr
maybeStrokeWidthNumAttr l =
  case Utils.maybeFind "stroke-width" l of
    Just aval -> case aval.av_ of
      LangSvg.ANum n -> Just n
      _              -> Nothing
    _                -> Nothing

zoneStrokeWidth model id shape x y maybeStrokeWidth =
  let pred z = isPrimaryZone z || isRotateZone z in
  let shapeSelected = Set.member id model.selectedShapes in
  let featureSelected =
    Set.member (ShapeWidgets.selectedTypeShapeFeature, id, ShapeWidgets.shapeStrokeWidth)
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
  let typeAndNodeIdAndFeature =
    (ShapeWidgets.selectedTypeShapeFeature, id, ShapeWidgets.shapeStrokeWidth) in
  let box =
    flip Svg.rect [] <|
      [ LangSvg.attr "fill" <|
          if Set.member typeAndNodeIdAndFeature model.selectedFeatures
            then colorPointSelected
            else "white" -- colorPointNotSelected
      , LangSvg.attr "stroke" stroke , LangSvg.attr "stroke-width" strokeWidth
      , LangSvg.attr "x" (toString x) , LangSvg.attr "y" (toString (y - yOff))
      , LangSvg.attr "width" (toString w) , LangSvg.attr "height" (toString h)
      ]
  in
  let ball =
    let cx = x + (n / LangSvg.maxStrokeWidthNum) * wStrokeWidthBox in
    let cy = y - yOff + (h/2) in
    flip Svg.circle [] <|
      [ LangSvg.attr "stroke" "black" , LangSvg.attr "stroke-width" strokeWidth
      , LangSvg.attr "fill" stroke
      , LangSvg.attr "cx" (toString cx) , LangSvg.attr "cy" (toString cy)
      , LangSvg.attr "r"  rBall
      , cursorOfZone "StrokeWidthBall" "default"
      ] ++ zoneEvents id shape "StrokeWidthBall"
  in
  let triangle =
    let (x0,y0) = (x                   , y - yOff + h/2 ) in
    let (x1,y1) = (x + wStrokeWidthBox , y - yOff       ) in
    let (x2,y2) = (x + wStrokeWidthBox , y - yOff + h   ) in
    svgPath <|
       [ LangSvg.attr "fill" "darkgray"
       , LangSvg.attr "d"
           ("M " ++ toString x0 ++ " " ++ toString y0 ++
           " L " ++ toString x1 ++ " " ++ toString y1 ++
           " L " ++ toString x2 ++ " " ++ toString y2 ++ " Z")
       ]
  in
  [ Svg.g
      [onMouseDownAndStop (toggleSelected [typeAndNodeIdAndFeature])]
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
      { old | slate = Utils.mapSnd (Dict.insert id LangSvg.dummySvgNode) old.slate }
    in
    onMouseDown (Msg "Delete..." foo) in
  let lines =
    let f x1 y1 x2 y2 =
      flip Svg.line [] <|
        [ LangSvg.attr "stroke" "darkred", LangSvg.attr "strokeWidth" strokeWidth
        , LangSvg.attr "x1" (toString x1) , LangSvg.attr "y1" (toString y1)
        , LangSvg.attr "x2" (toString x2) , LangSvg.attr "y2" (toString y2)
        , evt
        ] ++ transform
      in
     [ f x y (x + w) (y + h) , f x (y + h) (x + w) y ] in
  let box =
    flip Svg.rect [] <|
      [ LangSvg.attr "fill" "white"
      , LangSvg.attr "stroke" stroke , LangSvg.attr "strokeWidth" strokeWidth
      , LangSvg.attr "x" (toString x) , LangSvg.attr "y" (toString y)
      , LangSvg.attr "width" (toString w) , LangSvg.attr "height" (toString h)
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

hairStrokeWidth         = "5" -- pointZoneStyles.radius - 1

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

maybeZoneSelectCrossDot sideLength model thisCrosshair x y =
  if sideLength < minLengthForMiddleZones then []
  else zoneSelectCrossDot model thisCrosshair x y

zoneSelectCrossDot : Model -> (Int, ShapeKind, PointFeature)
                  -> number -> number2 -> List (Svg Msg)
zoneSelectCrossDot model (id, kind, pointFeature) x y =
  let xFeatureName = ShapeWidgets.unparseFeatureNum (Just kind) (X pointFeature) in
  let yFeatureName = ShapeWidgets.unparseFeatureNum (Just kind) (Y pointFeature) in
  let thisCrosshair = (id, xFeatureName, yFeatureName) in

  let len = 20 in
  let color typeAndNodeIdAndFeatures =
    if List.all (flip Set.member model.selectedFeatures) typeAndNodeIdAndFeatures
    then colorPointSelected
    else colorPointNotSelected
  in
  let
    xFeature = (ShapeWidgets.selectedTypeShapeFeature, id, xFeatureName)
    yFeature = (ShapeWidgets.selectedTypeShapeFeature, id, yFeatureName)
    (xColor, yColor) = (color [xFeature], color [yFeature])
  in
  let (backDisc, frontDisc) =
    let r =
      if Set.member thisCrosshair model.hoveredCrosshairs
        then toString len
        else "0"
    in
    let backDisc =
      svgCircle <|
        [ LangSvg.attr "r" r
        , LangSvg.attr "cx" (toString x) , LangSvg.attr "cy" (toString y)
        , LangSvg.attr "fill" "rgba(255,255,255,1.0)"
        ]
    in
    let frontDisc =
      svgCircle <|
        [ LangSvg.attr "r" r
        , LangSvg.attr "cx" (toString x) , LangSvg.attr "cy" (toString y)
        , LangSvg.attr "fill" "none"
        , LangSvg.attr "stroke" "black"
        , LangSvg.attr "stroke-width" pointZoneStyles.strokeWidth
        ]
    in
    (backDisc, frontDisc)
  in
  let xyDot =
    svgCircle <|
      [ LangSvg.attr "cx" (toString x) , LangSvg.attr "cy" (toString y)
      , LangSvg.attr "fill" <| -- "darkgray"
          if Set.member id model.selectedShapes
            then pointZoneStylesFillSelected model id
            else pointZoneStyles.fill.shown
      , LangSvg.attr "stroke" pointZoneStyles.stroke
      , LangSvg.attr "stroke-width" pointZoneStyles.strokeWidth
      , LangSvg.attr "r" <|
          if not (objectIsCurrentlyBeingManipulated model id)
             && (Set.member id model.selectedShapes ||
                 Set.member id model.hoveredShapes ||
                 Set.member thisCrosshair model.hoveredCrosshairs)
          then pointZoneStyles.radius
          else "0"
      , onMouseDown <| Msg "Select Cross Dot..." <| \model ->
          if Set.member thisCrosshair model.hoveredCrosshairs
            then toggleSelectedLambda [xFeature, yFeature] model
            else { model | hoveredCrosshairs = Set.insert thisCrosshair model.hoveredCrosshairs }
      ]
  in
  let yLine =
    svgLine [
        LangSvg.attr "stroke" yColor
      , LangSvg.attr "stroke-width" <|
          if Set.member thisCrosshair model.hoveredCrosshairs ||
             Set.member yFeature model.selectedFeatures
          then hairStrokeWidth
          else "0"
      , LangSvg.attr "x1" (toString (x-len)) , LangSvg.attr "y1" (toString y)
      , LangSvg.attr "x2" (toString (x+len)) , LangSvg.attr "y2" (toString y)
      , onMouseDown (toggleSelected [yFeature])
      ]
  in
  let xLine =
    svgLine [
        LangSvg.attr "stroke" xColor
      , LangSvg.attr "stroke-width" <|
          if Set.member thisCrosshair model.hoveredCrosshairs ||
             Set.member xFeature model.selectedFeatures
          then hairStrokeWidth
          else "0"
      , LangSvg.attr "y1" (toString (y-len)) , LangSvg.attr "x1" (toString x)
      , LangSvg.attr "y2" (toString (y+len)) , LangSvg.attr "x2" (toString x)
      , onMouseDown (toggleSelected [xFeature])
      ]
  in
  -- using nested group for onMouseLeave handler
  Utils.singleton <| Svg.g
    [onMouseLeave (removeHoveredCrosshair thisCrosshair)]
    [backDisc, xLine, yLine, frontDisc, xyDot]

maybeZoneSelectLine sideLength model nodeId kind featureNum pt1 pt2 =
  if sideLength < minLengthForMiddleZones then []
  else zoneSelectLine model nodeId kind featureNum pt1 pt2

zoneSelectLine model nodeId kind featureNum pt1 pt2 =
  let typeAndNodeIdAndFeature =
    ( ShapeWidgets.selectedTypeShapeFeature
    , nodeId
    , ShapeWidgets.unparseFeatureNum (Just kind) featureNum ) in
  case model.mouseMode of
    MouseDragZone (Left _) _ -> []
    _ ->
     if Set.member nodeId model.hoveredShapes ||
        Set.member typeAndNodeIdAndFeature model.selectedFeatures
     then zoneSelectLine_ model typeAndNodeIdAndFeature pt1 pt2
     else []

zoneSelectLine_ model typeAndNodeIdAndFeature (x1,y1) (x2,y2) =
  let color =
    if Set.member typeAndNodeIdAndFeature model.selectedFeatures
    then colorLineSelected
    else colorLineNotSelected
  in
  let line =
    svgLine [
        LangSvg.attr "stroke" color
      , LangSvg.attr "stroke-width" hairStrokeWidth
      , LangSvg.attr "x1" (toString x1) , LangSvg.attr "y1" (toString y1)
      , LangSvg.attr "x2" (toString x2) , LangSvg.attr "y2" (toString y2)
      , onMouseDown (toggleSelected [typeAndNodeIdAndFeature])
      ]
  in
  [line]

boxySelectZones model id kind boxyNums =

  let drawPoint maybeThreshold feature x y =
    case maybeThreshold of
      Just thresh -> maybeZoneSelectCrossDot thresh model (id, kind, feature) x y
      Nothing     -> zoneSelectCrossDot model (id, kind, feature) x y in

  let drawLine threshold feature pt1 pt2 =
    maybeZoneSelectLine threshold model id kind feature pt1 pt2 in

  let {left, top, right, bot, cx, cy, width, height} = boxyNums in

  let distanceZone f =
    case f of
      DistanceFeature Width   -> drawLine height (D Width) (left,cy) (right,cy)
      DistanceFeature Height  -> drawLine width (D Height) (cx,top) (cx,bot)

      DistanceFeature Radius  -> drawLine width (D Radius) (cx,cy) (right,cy)
      DistanceFeature RadiusX -> drawLine height (D RadiusX) (cx,cy) (right,cy)
      DistanceFeature RadiusY -> drawLine width (D RadiusY) (cx,top) (cx,cy)

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
  let (x1,y1,x2,y2,cx,cy) = ShapeWidgets.evaluateLineFeatures id l in
  let (pt1,pt2) = ((x1,y1), (x2,y2)) in
  let bounds =
    let (xMin,xMax) = minMax x1 x2 in
    let (yMin,yMax) = minMax y1 y2 in
    (xMin, yMin, xMax, yMax) in
  let zLine =
    let enter = [ onMouseEnter (addHoveredShape id) ] in
    zoneLine2 model id "line" "Edge" pt1 pt2 (transform ++ enter)
  in
  let zonesSelect =
    List.concat
       [ maybeZoneSelectCrossDot (distance pt1 pt2) model (id, "line", Center) cx cy
       , zoneSelectCrossDot model (id, "line", Point 1) x1 y1
       , zoneSelectCrossDot model (id, "line", Point 2) x2 y2 ]
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
  let boxyNums = ShapeWidgets.evaluateBoxyNums id shape l in
  let {left, top, right, bot, cx, cy, width, height} = boxyNums in
  let bounds = (left, top, right, bot) in
  let transform = maybeTransformAttr l in
  let zoneInterior =
    draggableZone Svg.rect False model id shape "Interior" <|
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
  let boxyNums = ShapeWidgets.evaluateBoxyNums id "circle" l in
  let {left, top, right, bot, cx, cy, r} = boxyNums in
  let bounds = (left, top, right, bot) in
  let transform = maybeTransformAttr l in
  let zoneInterior =
    draggableZone Svg.circle False model id "circle" "Interior" <|
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
  let boxyNums = ShapeWidgets.evaluateBoxyNums id shape l in
  let {left, top, right, bot, width, height, cx, cy, rx, ry} = boxyNums in
  let bounds = (left, top, right, bot) in
  let transform = maybeTransformAttr l in
  let zoneInterior =
    draggableZone Svg.ellipse False model id shape "Interior" <|
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
    let f (i,(pti,ptj)) = zoneLine model id shape ("Edge" ++ toString i) pti ptj transform in
    Utils.mapi f pairs in
  let zInterior =
    draggableZone Svg.polygon False model id shape "Interior" <|
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
    let midptCrossDot ((i1, ((xi1,_),(yi1,_))), (i2, ((xi2,_),(yi2,_)))) =
      let (xAttr1, yAttr1) = ("x" ++ toString i1, "y" ++ toString i1) in
      let (xAttr2, yAttr2) = ("x" ++ toString i2, "y" ++ toString i2) in
      zoneSelectCrossDot model (id, shape, Midpoint i1) (xi1/2+xi2/2) (yi1/2+yi2/2)
    in
    let ptCrossDot (i, ((xi,_),(yi,_))) =
      let (xAttr, yAttr) = ("x" ++ toString i, "y" ++ toString i) in
      zoneSelectCrossDot model (id, shape, Point i) xi yi
    in
    let midptCrossDots =
      let ptsI = Utils.mapi identity pts in
      let ptsIPairs = Utils.selfZipCircConsecPairs ptsI in
      List.concatMap midptCrossDot ptsIPairs
    in
    let crossDots = List.concat <| Utils.mapi ptCrossDot pts in
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
    let ptCrossDot (maybeIndex, ((xi,_),(yi,_))) =
      let i = Utils.fromJust maybeIndex in
      let (xAttr, yAttr) = ("x" ++ toString i, "y" ++ toString i) in
      zoneSelectCrossDot model (id, shape, Point i) xi yi
    in
    let crossDots = List.concatMap ptCrossDot listOfMaybeIndexWithPt in
    crossDots
  in
  let zInterior =
    draggableZone Svg.path False model id shape "Interior" <|
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
        , codeBoxInfo = { codeBoxInfo | highlights = expRangesToHighlights m } }


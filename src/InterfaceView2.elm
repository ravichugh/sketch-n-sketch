module InterfaceView2 (view, scaleColorBall , drawNewPolygonDotSize
                      , boundingBox , squareBoundingBox , snapLine
                      ) where

--Import the little language and its parsing utilities
import Lang exposing (..) --For access to what makes up the Vals
import LangParser2 as Parser exposing (parseE)
import Sync
import Eval
import Utils
import Keys
import InterfaceModel exposing (..)
import LangSvg exposing (toNum, toNumTr, addi, attr)
import ExamplesGenerated as Examples
import Config exposing (params)
import OurParser2 as P

import VirtualDom

--Core Libraries
import List
import Dict
import Set
import String
import Graphics.Element as GE
import Graphics.Collage as GC
import Graphics.Input as GI
import Graphics.Input.Field as GIF
import Text as T exposing (defaultStyle)
import Color

--Signaling Libraries
import Mouse
import Window
import Task exposing (Task, andThen)

--Storage Libraries
import InterfaceStorage exposing (taskMailbox, saveStateLocally, loadLocalState,
                                  checkAndSave, getLocalSaves, clearLocalSaves,
                                  deleteLocalSave)

--Library for Ace
import CodeBox exposing (saveRequestInfo, runRequestInfo)

--Html Libraries
import Html
import Html.Attributes as Attr
import Html.Events as Events
import Html.Lazy

--Svg Libraries
import Svg
import Svg.Attributes
import Svg.Events
import Svg.Lazy

--Error Checking Libraries
import Debug

--------------------------------------------------------------------------------

svgLine      = flip Svg.line []
svgRect      = flip Svg.rect []
svgCircle    = flip Svg.circle []
svgEllipse   = flip Svg.ellipse []
svgPolygon   = flip Svg.polygon []

-- TODO use these more below

--------------------------------------------------------------------------------

dimToPix d = String.append (toString d) "px"

interfaceColor = Color.rgba 52 73 94 1.0
strInterfaceColor = "rgba(52,73,94,1.0)"
strButtonTopColor = "rgba(231,76,60,1.0)" -- from InterfaceButtons example
textColor = "white"

titleStyle =
  { defaultStyle | typeface = ["Courier", "monospace"]
                 , height = Just 18
                 , bold = False
                 , color = Color.white}

imgPath s = "img/" ++ s

-- Creates an Html button with the text properly offset
type ButtonStatus = Raised | Highlighted | Depressed | Disabled
type ButtonKind   = Regular | Selected | Unselected
type alias ButtonState = (ButtonKind, ButtonStatus)

-- Currently assumes:
--  font-size is 16px
--  the top of the button occupies 90% of the height of the button
--  the depressed button should move the text down 3/50 of the total height of the
--   button
makeButton : ButtonState -> Int -> Int -> String -> GE.Element
makeButton (kind, status) w h text =
  let fontsize = 16
      topprop = 0.9
      depdip = 0.06
      raisedoffset = round <| 0.5 * topprop * toFloat h - 0.5 * fontsize
      depressedoffset = round <| toFloat raisedoffset + depdip * toFloat h
      prefix = case kind of
        Regular    -> ""
        Selected   -> ""
        Unselected -> "unselected_"
      (img, dip) = case status of
        Raised      -> (imgPath <| prefix ++ "button_raised.svg", raisedoffset)
        Highlighted -> (imgPath <| prefix ++ "button_highlighted.svg", raisedoffset)
        Depressed   -> (imgPath <| prefix ++ "button_depressed.svg", depressedoffset)
        Disabled    -> (imgPath <| prefix ++ "button_disabled.svg", raisedoffset)
  in
  GE.flow GE.outward
    [ GE.image w h img
    , Html.toElement w h <|
        Html.div
          [ Attr.style
              [ ("color", textColor)
              , ("font-family", "sans-serif")
              , ("text-align", "center")
              , ("width", dimToPix w)
              , ("height", dimToPix h)
              , ("transform", "translate(0px," ++ dimToPix dip ++ ")")
              ]
          ] [ Html.text text ]
    ]


--------------------------------------------------------------------------------
-- Zone Options (per shape)

type alias ZoneOptions =
  { showBasic : Bool , addBasic : Bool , addRot : Bool , addColor : Bool
  , addDelete : Bool , addSelect : Bool }

zoneOptions0 =
  { showBasic = False , addBasic = False , addRot = False , addColor = False
  , addDelete = False , addSelect = False }

optionsOf : ShowZones -> ZoneOptions
optionsOf x =
  if x == showZonesNone       then { zoneOptions0 | addBasic = True }
  else if x == showZonesBasic then { zoneOptions0 | addBasic = True, showBasic = True }
  else if x == showZonesExtra then { zoneOptions0 | addRot = True, addColor = True }
  else if x == showZonesDel   then { zoneOptions0 | addDelete = True }
  else if x == showZonesSelect then { zoneOptions0 | addSelect = True }
  else
    Debug.crash "optionsOf"


--------------------------------------------------------------------------------
-- Compiling to Svg

buildSvg : (Model, Bool) -> LangSvg.RootedIndexedTree -> Svg.Svg
buildSvg options (i,d) = buildSvg_ options d i

buildSvg_ : (Model, Bool) -> LangSvg.IndexedTree -> LangSvg.NodeId -> Svg.Svg
buildSvg_ stuff d i =
  let (model, addZones) = stuff in
  case Utils.justGet_ ("buildSvg_ " ++ toString i) i d of
   LangSvg.TextNode text -> VirtualDom.text text
   LangSvg.SvgNode shape attrs js ->
    case (model.showWidgets, Utils.maybeRemoveFirst "HIDDEN" attrs) of
     (False, Just _) -> Svg.svg [] []
     _ ->
      -- TODO: figure out: (LangSvg.attr "draggable" "false")
      let (zones, attrs') =
        let options = optionsOf model.showZones in
        case (addZones, Utils.maybeRemoveFirst "ZONES" attrs) of
          (False, Nothing)     -> ([], attrs)
          (False, Just (_, l)) -> ([], l)
          (True, Nothing) ->
            (makeZones model options shape i attrs, attrs)
          (True, Just (aval, l)) -> case aval.av_ of
            LangSvg.AString "none" ->
              (makeZones model zoneOptions0 shape i attrs, l)
            LangSvg.AString "basic" ->
              let options' = { options | addRot = False, addColor = False } in
              (makeZones model options' shape i attrs, l)
            _ -> Debug.crash "buildSvg_"
      in
      let children = List.map (buildSvg_ stuff d) js in
      -- let mainshape = (Svg.node shape) (LangSvg.compileAttrs attrs') children in
      let mainshape =
        let (rawKind, rawAttrs) = LangSvg.desugarShapeAttrs shape attrs' in
        (Svg.node rawKind) (LangSvg.compileAttrs rawAttrs) children in
      if zones == []
        then mainshape
        else Svg.svg [] (mainshape :: zones)


--------------------------------------------------------------------------------
-- Widget Layer

buildSvgWidgets : Int -> Int -> Widgets -> Svg.Svg
buildSvgWidgets wCanvas hCanvas widgets =
  let
    pad            = params.mainSection.uiWidgets.pad
    wSlider        = params.mainSection.uiWidgets.wSlider
    hSlider        = params.mainSection.uiWidgets.hSlider
    wCaption       = params.mainSection.uiWidgets.wCaption
    dedupedWidgets = Utils.dedup widgets

    numWidgets    = List.length dedupedWidgets
    wWidget       = wSlider + wCaption + 2*pad
    hWidget       = hSlider + 2*pad
    wToolBoxMax   = wCanvas - 2*pad
    numCols       = floor (wToolBoxMax / wWidget)
    numRows       = ceiling (toFloat numWidgets / toFloat numCols)
    wToolBox      = numCols * wWidget
    hToolBox      = numRows * hWidget
    xL            = pad
    yBL           = hCanvas - hWidget - pad
  in
  let draw (i_, widget) =
    let i = i_ - 1 in
    let
      (r,c) = (i % numRows, i // numRows)
      xi    = xL  + c*wWidget
      yi    = yBL - r*hWidget
      xi'   = xi + pad
      yi'   = yi + pad
    in
    let region =
      flip Svg.rect [] <|
        [ attr "fill" "lightgray"
        , attr "stroke" strInterfaceColor , attr "stroke-width" "3px"
        , attr "rx" "9px" , attr "ry" "9px"
        , attr "x" (toString (xL  + c*wWidget))
        , attr "y" (toString (yBL - r*hWidget))
        , attr "width" (toString wWidget) , attr "height" (toString hWidget)
        ]
    in
    let box =
      flip Svg.rect [] <|
        [ attr "fill" strInterfaceColor
        , attr "stroke" "20px", attr "stroke-width" "20px"
        , attr "x" (toString (xL  + c*wWidget + pad))
        , attr "y" (toString (yBL - r*hWidget + pad))
        , attr "width" (toString wSlider) , attr "height" (toString hSlider)
        ]
    in
    let ball =
      let (minVal,maxVal,curVal) = case widget of
        WIntSlider a b _ c _ -> (toFloat a, toFloat b, toFloat c)
        WNumSlider a b _ c _ -> (a, b, c)
      in
      let (range, diff) = (maxVal - minVal, curVal - minVal) in
      let pct = diff / range in
      let cx = xi + pad + round (pct*wSlider) in
      let cy = yi + pad + (hSlider//2) in
      flip Svg.circle [] <|
        [ attr "stroke" "black" , attr "stroke-width" "2px"
        , attr "fill" strButtonTopColor
        , attr "r" params.mainSection.uiWidgets.rBall
        , attr "cx" (toString cx) , attr "cy" (toString cy)
        , cursorOfZone "SliderBall" "default"
        ] ++ sliderZoneEvents widget
    in
    let text =
      let cap = case widget of
        WIntSlider _ _ s targetVal _ -> s ++ strNumTrunc 5 targetVal
        WNumSlider _ _ s targetVal _ -> s ++ strNumTrunc 5 targetVal
      in
      flip Svg.text' [VirtualDom.text cap] <|
        [ attr "fill" "black" , attr "font-family" "Tahoma, sans-serif"
        , attr "font-size" params.mainSection.uiWidgets.fontSize
        , attr "x" (toString (xi' + wSlider + 10))
        , attr "y" (toString (yi' + 18))
        ]
    in
    [region, box, text, ball]
  in
  Svg.svg [] (List.concat (Utils.mapi draw dedupedWidgets))

sliderZoneEvents widgetState =
  let foo old = case old.mode of
    Live _ -> { old | mouseMode = MouseSlider widgetState Nothing }
    _      -> old
  in
  [ onMouseDown (UpdateModel foo) , onMouseUp MouseUp ]


--------------------------------------------------------------------------------
-- Defining Zones

-- okay to use dummy VTraces/Traces here, b/c compileAttr throws them away
attrNum k n    = LangSvg.compileAttr k (LangSvg.aNum (n, dummyTrace))
attrNumTr k nt = LangSvg.compileAttr k (LangSvg.aNum nt)

onMouseDown = Svg.Events.onMouseDown << Signal.message events.address
onMouseUp   = Svg.Events.onMouseUp   << Signal.message events.address
onMouseOver = Svg.Events.onMouseOver << Signal.message events.address
onMouseOut  = Svg.Events.onMouseOut  << Signal.message events.address

zoneEvents id shape zone =
  [ onMouseDown (SelectObject id shape zone)
  , onMouseUp MouseUp
  , onMouseOver (turnOnCaptionAndHighlights id shape zone)
  , onMouseOut turnOffCaptionAndHighlights
  ]

zone svgFunc id shape zone l =
  svgFunc (zoneEvents id shape zone ++ l) []

cursorStyle s = LangSvg.attr "cursor" s

-- TODO should take into account disabled zones in Live mode
cursorOfZone zone default = case zone of
  -- rect zones
  "Interior"       -> cursorStyle "move"
  "RightEdge"      -> cursorStyle "ew-resize"
  "BotRightCorner" -> cursorStyle "nwse-resize"
  "BotEdge"        -> cursorStyle "ns-resize"
  "BotLeftCorner"  -> cursorStyle "nesw-resize"
  "LeftEdge"       -> cursorStyle "ew-resize"
  "TopLeftCorner"  -> cursorStyle "nwse-resize"
  "TopEdge"        -> cursorStyle "ns-resize"
  "TopRightCorner" -> cursorStyle "nesw-resize"
  -- circle/ellipse zones
  "Edge"           -> cursorStyle "pointer"
  -- indirect manipulation zones
  "FillBall"       -> cursorStyle "pointer"
  "RotateBall"     -> cursorStyle "pointer"
  "SliderBall"     -> cursorStyle "pointer"
  -- default
  _                -> cursorStyle default

-- Stuff for Basic Zones -------------------------------------------------------

-- TODO use zone
-- TODO basic addBasic through these funcs, and honor them

zoneBorder svgFunc id shape zone flag show transform =
  flip svgFunc [] <<
  (++) (zoneEvents id shape zone) <<
  (++) transform <<
  (++) [ if flag && show
         then LangSvg.attr "stroke" "rgba(255,0,0,0.5)"
         else LangSvg.attr "stroke" "rgba(0,0,0,0.0)"
       , LangSvg.attr "stroke-width" (if flag then "5" else "0")
       , LangSvg.attr "fill" "rgba(0,0,0,0)"
       , cursorOfZone zone "default"
       ]

zonePoint id shape zone show transform =
  flip Svg.circle [] <<
  (++) (zoneEvents id shape zone) <<
  (++) transform <<
  (++) [ LangSvg.attr "r" "6"
       , if show
         then LangSvg.attr "fill" "rgba(255,0,0,0.5)"
         else LangSvg.attr "fill" "rgba(0,0,0,0.0)"
       , cursorOfZone zone "pointer"
       ]

zonePoints id shape show transform pts =
  flip Utils.mapi pts <| \(i, (x,y)) ->
    zonePoint id shape (addi "Point" i) show transform
      [ attrNumTr "cx" x, attrNumTr "cy" y ]

zoneLine id shape zone show transform (x1,y1) (x2,y2) =
  zoneBorder Svg.line id shape zone True show transform [
      attrNumTr "x1" x1 , attrNumTr "y1" y1
    , attrNumTr "x2" x2 , attrNumTr "y2" y2
    , cursorStyle "pointer"
    ]

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

zoneRotate b id shape (cx,cy) r m =
  case (b, m) of
    (True, Just cmds) -> zoneRotate_ id shape cx cy r cmds
    _                 -> []

zoneRotate_ id shape cx cy r cmds =
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
    flip Svg.line [] <|
      [ LangSvg.attr "stroke" stroke , LangSvg.attr "stroke-width" strokeWidth
      , LangSvg.attr "x1" (toString cx) , LangSvg.attr "y1" (toString cy)
      , LangSvg.attr "x2" (toString cx) , LangSvg.attr "y2" (toString (cy - r))
      ] ++ transform
  in
  [circle, line, ball]

halfwayBetween (x1,y1) (x2,y2) = ((x1 + x2) / 2, (y1 + y2) / 2)
distance (x1,y1) (x2,y2)       = sqrt ((x2-x1)^2 + (y2-y1)^2)

projPt (x,y)                   = (fst x, fst y)
halfwayBetween_ pt1 pt2        = halfwayBetween (projPt pt1) (projPt pt2)
distance_ pt1 pt2              = distance (projPt pt1) (projPt pt2)

-- Stuff for Color Zones -------------------------------------------------------

wGradient = 250
scaleColorBall = 1 / (wGradient / LangSvg.maxColorNum)

numToColor = Utils.numToColor wGradient

maybeColorNumAttr : String -> List LangSvg.Attr -> Maybe NumTr
maybeColorNumAttr k l =
  case Utils.maybeFind k l of
    Just aval -> case aval.av_ of
      LangSvg.AColorNum n -> Just n
      _                   -> Nothing
    _                     -> Nothing

zoneColor b id shape x y rgba =
  case (b, rgba) of
    (True, Just n) -> zoneColor_ id shape x y n
    _              -> []

zoneColor_ id shape x y n =
  let rgba = [LangSvg.compileAttr "fill" (LangSvg.aColorNum n)] in
  let (w, h, a, stroke, strokeWidth, rBall) =
      (wGradient, 20, 20, "silver", "2", "7") in
  let yOff = a + rotZoneDelta in
  let ball =
    let cx = x + (fst n / LangSvg.maxColorNum) * wGradient in
    let cy = y - yOff + (h/2) in
    flip Svg.circle [] <|
      [ LangSvg.attr "stroke" "black" , LangSvg.attr "stroke-width" strokeWidth
      , LangSvg.attr "fill" stroke
      , LangSvg.attr "cx" (toString cx) , LangSvg.attr "cy" (toString cy)
      , LangSvg.attr "r"  rBall
      , cursorOfZone "FillBall" "default"
      ] ++ zoneEvents id shape "FillBall"
  in
  let box =
    flip Svg.rect [] <|
      [ LangSvg.attr "fill" "none"
      , LangSvg.attr "stroke" stroke , LangSvg.attr "stroke-width" strokeWidth
      , LangSvg.attr "x" (toString x) , LangSvg.attr "y" (toString (y - yOff))
      , LangSvg.attr "width" (toString w) , LangSvg.attr "height" (toString h)
      ]
  in
  -- TODO would probably be faster with an image...
  let gradient =
    List.map (\i ->
      let (r,g,b) = numToColor i in
      let fill =
        "rgb" ++ Utils.parens (String.join "," (List.map toString [r,g,b]))
      in
      flip Svg.rect [] <|
        [ LangSvg.attr "fill" fill
        , LangSvg.attr "x" (toString (x+i)) , LangSvg.attr "y" (toString (y - yOff))
        , LangSvg.attr "width" "1" , LangSvg.attr "height" (toString h)
        ]) [0 .. w]
  in
  gradient ++ [box, ball]

-- Stuff for Delete Zones ------------------------------------------------------

zoneDelete b id shape x y transform =
  if b then zoneDelete_ id shape x y transform else []

zoneDelete_ id shape x y transform =
  let (w, h, stroke, strokeWidth) =
      (20, 20, "silver", "2") in
  let evt =
    let foo old =
      { old | slate = Utils.mapSnd (Dict.insert id LangSvg.dummySvgNode) old.slate }
    in
    onMouseDown (UpdateModel foo) in
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

colorPointSelected      = "rgba(0,128,0,1.0)"
colorPointNotSelected   = "orange"
colorLineSelected       = "blue"
colorLineNotSelected    = "red"

strokeWidth             = LangSvg.attr "stroke-width" "10"

type alias NodeIdAndAttrName     = (LangSvg.NodeId, String)
type alias NodeIdAndTwoAttrNames = (LangSvg.NodeId, String, String)

type alias NodeIdAndFeature      = (LangSvg.NodeId, LangSvg.ShapeFeature)


toggleSelected model nodeIdAndFeatures =
  UpdateModel <| \model ->
    -- If only some of the features were selected, we want to select all of
    -- them, not toggle individually.
    let deselect = List.all (flip Set.member model.selectedFeatures) nodeIdAndFeatures in
    let updateSet nodeIdAndFeature acc =
      if deselect
        then Set.remove nodeIdAndFeature acc
        else Set.insert nodeIdAndFeature acc
    in
    { model | selectedFeatures = List.foldl updateSet model.selectedFeatures nodeIdAndFeatures }

zoneSelectCrossDot : Model -> Bool -> (Int, List LangSvg.ShapeFeature, List LangSvg.ShapeFeature) -> number -> number' -> List Svg.Svg
zoneSelectCrossDot model addSelect tuple x y =
  if addSelect then zoneSelectCrossDot_ model tuple x y else []

zoneSelectCrossDot_ model (id, xFeatures, yFeatures) x y =
  let len = 20 in
  let color nodeIdAndFeatures =
    if List.all (flip Set.member model.selectedFeatures) nodeIdAndFeatures
    then colorPointSelected
    else colorPointNotSelected
  in
  let xNodeIdAndFeatures = List.map ((,) id) xFeatures in
  let yNodeIdAndFeatures = List.map ((,) id) yFeatures in
  -- let (xNodeIdAndAttrName, yNodeIdAndAttrName) = ((id, xAttr), (id, yAttr)) in
  let (xColor, yColor) = (color xNodeIdAndFeatures, color yNodeIdAndFeatures) in
  let yLine =
    svgLine [
        LangSvg.attr "stroke" yColor , strokeWidth
      , LangSvg.attr "x1" (toString (x-len)) , LangSvg.attr "y1" (toString y)
      , LangSvg.attr "x2" (toString (x+len)) , LangSvg.attr "y2" (toString y)
      , onMouseDown (toggleSelected model yNodeIdAndFeatures)
      ]
  in
  let xLine =
    svgLine [
        LangSvg.attr "stroke" xColor , strokeWidth
      , LangSvg.attr "y1" (toString (y-len)) , LangSvg.attr "x1" (toString x)
      , LangSvg.attr "y2" (toString (y+len)) , LangSvg.attr "x2" (toString x)
      , onMouseDown (toggleSelected model xNodeIdAndFeatures)
      ]
  in
  let xyDot = zoneSelectDot_ model (id, xFeatures ++ yFeatures) x y in
  [xLine, yLine] ++ xyDot

zoneSelectDot model addSelect (id, attrNames) x y =
  if addSelect then zoneSelectDot_ model (id, attrNames) x y else []

zoneSelectDot_ model (id, features) x y =
  let nodeIdAndFeatures = List.map ((,) id) features in
  [ svgCircle [
      LangSvg.attr "fill" "darkgray" , LangSvg.attr "r" "6"
    , LangSvg.attr "cx" (toString x) , LangSvg.attr "cy" (toString y)
    , onMouseDown (toggleSelected model nodeIdAndFeatures)
    ] ]

-- TODO given need for model, remove addSelect
zoneSelectLine model addSelect nodeIdAndFeature pt1 pt2 =
  if addSelect then zoneSelectLine_ model nodeIdAndFeature pt1 pt2 else []

zoneSelectLine_ model nodeIdAndFeature (x1,y1) (x2,y2) =
  let color =
    if Set.member nodeIdAndFeature model.selectedFeatures
    then colorLineSelected
    else colorLineNotSelected
  in
  let line =
    svgLine [
        LangSvg.attr "stroke" color , strokeWidth
      , LangSvg.attr "x1" (toString x1) , LangSvg.attr "y1" (toString y1)
      , LangSvg.attr "x2" (toString x2) , LangSvg.attr "y2" (toString y2)
      , onMouseDown (toggleSelected model [nodeIdAndFeature])
      ]
  in
  [line]


--------------------------------------------------------------------------------

-- TODO given need for Model, rethink ZoneOptions...
{-
makeZones : ZoneOptions -> String -> LangSvg.NodeId -> List LangSvg.Attr -> List Svg.Svg
makeZones options shape id l =
-}
makeZones : Model -> ZoneOptions -> String -> LangSvg.NodeId -> List LangSvg.Attr -> List Svg.Svg
makeZones model options shape id l =
  case shape of

    "rect"    -> makeZonesRect model options shape id l
    "BOX"     -> makeZonesBox model options id l
    "circle"  -> makeZonesCircle  model options id l
    "ellipse" -> makeZonesEllipse model options id l

    "line" ->
        let transform = maybeTransformAttr l in
        let (x1,y1,x2,y2) =
          Utils.unwrap4 <| List.map (toNumTr << Utils.find_ l) ["x1","y1","x2","y2"] in
        let (pt1,pt2) = ((x1,y1), (x2,y2)) in
        let zLine = zoneLine id shape "Edge" options.showBasic transform pt1 pt2 in
        let zPts = zonePoints id shape options.showBasic transform [pt1,pt2] in
        let zRot =
          let c = halfwayBetween_ pt1 pt2 in
          let r = (distance_ pt1 pt2 / 2) - rotZoneDelta in
          zoneRotate options.addRot id shape c r (maybeTransformCmds l) in
        let zSelect =
          List.concat
             [ zoneSelectCrossDot model options.addSelect (id, [LangSvg.lineCX], [LangSvg.lineCY]) ((fst x1)/2+(fst x2)/2) ((fst y1)/2+(fst y2)/2)
             , zoneSelectCrossDot model options.addSelect (id, [LangSvg.lineX1], [LangSvg.lineY1]) (fst x1) (fst y1)
             , zoneSelectCrossDot model options.addSelect (id, [LangSvg.lineX2], [LangSvg.lineY2]) (fst x2) (fst y2) ] in
        zLine :: zPts ++ zRot ++ zSelect

    "polygon"  -> makeZonesPoly model options shape id l
    "polyline" -> makeZonesPoly model options shape id l

    "path" -> makeZonesPath options.showBasic shape id l

    _ -> []

findNums l attrs = List.map (toNum << Utils.find_ l) attrs

-- TODO remove shape and shape == "BOX" path
makeZonesRect model options shape id l =
  let transform = maybeTransformAttr l in
  let mk zone x_ y_ w_ h_ =
    zoneBorder Svg.rect id shape zone True options.showBasic transform <|
      [ attrNum "x" x_ , attrNum "y" y_
      , attrNum "width" w_ , attrNum "height" h_
      ]
  in
  let
    (x,y,w,h)     = Utils.unwrap4 <| findNums l ["x","y","width","height"]
    gut           = 0.125
    (x0,x1,x2)    = (x, x + gut*w, x + (1-gut)*w)
    (y0,y1,y2)    = (y, y + gut*h, y + (1-gut)*h)
    (wSlim,wWide) = (gut*w, (1-2*gut)*w)
    (hSlim,hWide) = (gut*h, (1-2*gut)*h)
  in
  let zRot =
    let c = (x + (w/2), y + (h/2)) in
    let r = rotZoneDelta + (h/2) in
    zoneRotate options.addRot id shape c r (maybeTransformCmds l)
  in
  let zColor =
    zoneColor options.addColor id shape x y (maybeColorNumAttr "fill" l)
  in
  let zonesSelect =
       zoneSelectLine model options.addSelect (id, LangSvg.rectWidth) (x,y+h/2) (x+w,y+h/2)
    ++ zoneSelectLine model options.addSelect (id, LangSvg.rectHeight) (x+w/2,y) (x+w/2,y+h)
    ++ zoneSelectCrossDot model options.addSelect (id, [LangSvg.rectTLX], [LangSvg.rectTLY]) x y
    ++ zoneSelectCrossDot model options.addSelect (id, [LangSvg.rectTRX], [LangSvg.rectTRY]) (x+w) y
    ++ zoneSelectCrossDot model options.addSelect (id, [LangSvg.rectBLX], [LangSvg.rectBLY]) x (y+h)
    ++ zoneSelectCrossDot model options.addSelect (id, [LangSvg.rectBRX], [LangSvg.rectBRY]) (x+w) (y+h)
    ++ zoneSelectCrossDot model options.addSelect (id, [LangSvg.rectCX], [LangSvg.rectCY]) (x+w/2) (y+h/2)
  in
    [ mk "Interior"       x1 y1 wWide hWide
    , mk "RightEdge"      x2 y1 wSlim hWide
    , mk "BotRightCorner" x2 y2 wSlim hSlim
    , mk "BotEdge"        x1 y2 wWide hSlim
    , mk "BotLeftCorner"  x0 y2 wSlim hSlim
    , mk "LeftEdge"       x0 y1 wSlim hWide
    , mk "TopLeftCorner"  x0 y0 wSlim hSlim
    , mk "TopEdge"        x1 y0 wWide hSlim
    , mk "TopRightCorner" x2 y0 wSlim hSlim
    ] ++ zRot
      ++ zColor
      ++ zoneDelete options.addDelete id shape x y (maybeTransformAttr l)
      ++ zonesSelect

makeZonesBox model options id l =
  let transform = maybeTransformAttr l in
  let mkInterior zone x_ y_ w_ h_ =
    zoneBorder Svg.rect id "BOX" zone True options.showBasic transform <|
      [ attrNum "x" x_ , attrNum "y" y_
      , attrNum "width" w_ , attrNum "height" h_
      ]
  in
  let mkPoint zone cx cy =
    zonePoint id "BOX" zone options.showBasic transform [attrNum "cx" cx, attrNum "cy" cy]
  in
  let
    (left, top, right, bot) = Utils.unwrap4 <| findNums l ["LEFT","TOP","RIGHT","BOT"]
    (width, height) = (right - left, bot - top)
  in
  let (cx, cy) = (left + width/2, top + height/2) in
  let zonesSelect =
       zoneSelectLine model options.addSelect (id, LangSvg.boxWidth) (left, cy) (right, cy)
    ++ zoneSelectLine model options.addSelect (id, LangSvg.boxHeight) (cx, top) (cx, bot)
    ++ zoneSelectCrossDot model options.addSelect (id, [LangSvg.boxTLX], [LangSvg.boxTLY]) left top
    ++ zoneSelectCrossDot model options.addSelect (id, [LangSvg.boxTRX], [LangSvg.boxTRY]) right top
    ++ zoneSelectCrossDot model options.addSelect (id, [LangSvg.boxBLX], [LangSvg.boxBLY]) left bot
    ++ zoneSelectCrossDot model options.addSelect (id, [LangSvg.boxBRX], [LangSvg.boxBRY]) right bot
    ++ zoneSelectCrossDot model options.addSelect (id, [LangSvg.boxCX], [LangSvg.boxCY]) cx cy
    --    zoneSelectCrossDot model options.addSelect (id, [LangSvg.boxLeft], [LangSvg.boxTop]) left top
    -- ++ zoneSelectCrossDot model options.addSelect (id, [LangSvg.boxRight], [LangSvg.boxBottom]) right bot
  in
    [ mkInterior "Interior" left top width height
    , mkPoint "TopLeftCorner" left top
    , mkPoint "TopRightCorner" right top
    , mkPoint "BotLeftCorner" left bot
    , mkPoint "BotRightCorner" right bot
    , mkPoint "LeftEdge" left (top + height / 2)
    , mkPoint "RightEdge" right (top + height / 2)
    , mkPoint "TopEdge" (left + width / 2) top
    , mkPoint "BotEdge" (left + width / 2) bot
    ] ++ zonesSelect
    -- TODO rot, color zones, styles of interior and point zones

-- makeZonesCircle options id l =
makeZonesCircle model options id l =
  let transform = maybeTransformAttr l in
  let (cx,cy,r) = Utils.unwrap3 <| findNums l ["cx","cy","r"] in
  let attrs = [ attrNum "cx" cx, attrNum "cy" cy, attrNum "r" r ] in
     [zoneBorder Svg.circle id "circle" "Edge" True options.showBasic attrs transform]
  ++ [zoneBorder Svg.circle id "circle" "Interior" False options.showBasic attrs transform]
  ++ (zoneRotate options.addRot id "circle" (cx,cy) (r + rotZoneDelta) (maybeTransformCmds l))
  ++ (zoneColor options.addColor id "circle" (cx - r) (cy - r) (maybeColorNumAttr "fill" l))
  ++ (zoneSelectLine model options.addSelect (id, LangSvg.circleR) (cx,cy) (cx+r,cy))
  ++ (zoneSelectCrossDot model options.addSelect (id, [LangSvg.circleCX], [LangSvg.circleCY]) cx cy)

-- makeZonesEllipse options id l =
makeZonesEllipse model options id l =
  let transform = maybeTransformAttr l in
  let (cx,cy,rx,ry) = Utils.unwrap4 <| findNums l ["cx","cy","rx","ry"] in
  let attrs = [ attrNum "cx" cx, attrNum "cy" cy, attrNum "rx" rx, attrNum "ry" ry ] in
     [zoneBorder Svg.ellipse id "ellipse" "Edge" True options.showBasic attrs transform]
  ++ [zoneBorder Svg.ellipse id "ellipse" "Interior" False options.showBasic attrs transform]
  ++ (zoneRotate options.addRot id "circle" (cx,cy) (ry + rotZoneDelta) (maybeTransformCmds l))
  ++ (zoneColor options.addColor id "ellipse" (cx - rx) (cy - ry) (maybeColorNumAttr "fill" l))
  ++ (zoneSelectLine model options.addSelect (id, LangSvg.ellipseRX) (cx,cy) (cx+rx,cy))
  ++ (zoneSelectLine model options.addSelect (id, LangSvg.ellipseRY) (cx,cy) (cx,cy+ry))
  ++ (zoneSelectCrossDot model options.addSelect (id, [LangSvg.ellipseCX], [LangSvg.ellipseCY]) cx cy)

-- makeZonesPoly options shape id l =
makeZonesPoly model options shape id l =
  let _ = Utils.assert "makeZonesPoly" (shape == "polygon" || shape == "polyline") in
  let transform = maybeTransformAttr l in
  let pts = LangSvg.toPoints <| Utils.find_ l "points" in
  let zPts = zonePoints id shape options.showBasic transform pts in
  let zLines =
    let pairs = Utils.adjacentPairs (shape == "polygon") pts in
    let f (i,(pti,ptj)) = zoneLine id shape (addi "Edge" i) options.showBasic transform pti ptj in
    Utils.mapi f pairs in
  let zInterior =
    zoneBorder Svg.polygon id shape "Interior" False options.showBasic transform [
        LangSvg.compileAttr "points" (LangSvg.aPoints pts)
      ] in
  let zRot =
    case pts of
      (((x0,_),(y0,_))::_) ->
        zoneColor options.addColor id shape x0 y0 (maybeColorNumAttr "fill" l)
      _ ->
        Debug.crash "makeZonesPoly" in
  let zSelect =
    let midptCrossDot ((i1, ((xi1,_),(yi1,_))), (i2, ((xi2,_),(yi2,_)))) =
      let (xAttr1, yAttr1) = ("x" ++ toString i1, "y" ++ toString i1) in
      let (xAttr2, yAttr2) = ("x" ++ toString i2, "y" ++ toString i2) in
      zoneSelectCrossDot model options.addSelect (id, [LangSvg.polyPathMidptX ++ toString i1], [LangSvg.polyPathMidptY ++ toString i1]) (xi1/2+xi2/2) (yi1/2+yi2/2)
    in
    let ptCrossDot (i, ((xi,_),(yi,_))) =
      let (xAttr, yAttr) = ("x" ++ toString i, "y" ++ toString i) in
      zoneSelectCrossDot model options.addSelect (id, [LangSvg.polyPathPtX ++ toString i], [LangSvg.polyPathPtY ++ toString i]) xi yi
    in
    let dots =
      let ptsI = Utils.mapi identity pts in
      let ptsIPairs = Utils.selfZipCircConsecPairs ptsI in
      List.concatMap midptCrossDot ptsIPairs
    in
    let crossDots = List.concat <| Utils.mapi ptCrossDot pts in
    dots ++ crossDots
  in
  let firstEqLast xs = Utils.head_ xs == Utils.head_ (List.reverse xs) in
  if shape == "polygon"   then zInterior :: (zLines ++ zPts ++ zRot ++ zSelect)
  else if firstEqLast pts then zInterior :: (zLines ++ zPts ++ zRot ++ zSelect)
  else                         zLines ++ zPts ++ zRot ++ zSelect

makeZonesPath : Bool -> String -> Int -> List LangSvg.Attr -> List Svg.Svg
makeZonesPath showZones shape id l =
  let _ = Utils.assert "makeZonesPoly" (shape == "path") in
  let transform = maybeTransformAttr l in
  let cmds = fst <| LangSvg.toPath <| Utils.find_ l "d" in
  let (+++) (mi,pt) acc = case mi of Nothing -> acc
                                     _       -> pt :: acc in
  let pts =
    List.foldr (\c acc -> case c of
      LangSvg.CmdZ   s              -> acc
      LangSvg.CmdMLT s pt           -> pt +++ acc
      LangSvg.CmdHV  s n            -> acc
      LangSvg.CmdC   s pt1 pt2 pt3  -> pt1 +++ (pt2 +++ (pt3 +++ acc))
      LangSvg.CmdSQ  s pt1 pt2      -> pt1 +++ (pt2 +++ acc)
      LangSvg.CmdA   s a b c d e pt -> pt +++ acc) [] cmds
  in
  zonePoints id shape showZones transform pts


--------------------------------------------------------------------------------
-- Drawing Tools

drawNewShape model =
  case model.mouseMode of
    MouseDrawNew "line"    [pt2, pt1]    -> drawNewLine model pt2 pt1
    MouseDrawNew "rect"    [pt2, pt1]    -> drawNewRect model.keysDown pt2 pt1
    MouseDrawNew "ellipse" [pt2, pt1]    -> drawNewEllipse model.keysDown pt2 pt1
    MouseDrawNew "polygon" (ptLast::pts) -> drawNewPolygon ptLast pts
    MouseDrawNew "DOT" [pt]              -> drawNewHelperDot pt
    MouseDrawNew "LAMBDA"  [pt2, pt1]    -> drawNewRect model.keysDown pt2 pt1
    _                                    -> []

defaultOpacity        = Attr.style [("opacity", "0.5")]
defaultStroke         = LangSvg.attr "stroke" "gray"
defaultStrokeWidth    = LangSvg.attr "stroke-width" "5"
defaultFill           = LangSvg.attr "fill" "gray"
dotFill               = LangSvg.attr "fill" "red"
dotSize               = LangSvg.attr "r" (toString drawNewPolygonDotSize)

drawNewPolygonDotSize = 10

guideStroke           = LangSvg.attr "stroke" "aqua"

boundingBox : (Int, Int) -> (Int, Int) -> (Int, Int, Int, Int)
boundingBox (x2,y2) (x1,y1) =
  (min x1 x2, max x1 x2, min y1 y2, max y1 y2)

squareBoundingBox : (Int, Int) -> (Int, Int) -> (Int, Int, Int, Int)
squareBoundingBox (x2,y2) (x1,y1) =
  let (xDiff, yDiff) = (abs (x2 - x1), abs (y2 - y1)) in
  case (yDiff > xDiff, x1 < x2, y1 < y2) of
    (True,  True , _    ) -> (x1, x1 + yDiff, min y1 y2, max y1 y2)
    (True,  False, _    ) -> (x1 - yDiff, x1, min y1 y2, max y1 y2)
    (False, _    , True ) -> (min x1 x2, max x1 x2, y1, y1 + xDiff)
    (False, _    , False) -> (min x1 x2, max x1 x2, y1 - xDiff, y1)

slicesPerQuadrant = 2 -- can toggle this parameter
radiansPerSlice   = pi / (2 * slicesPerQuadrant)

snapLine keysDown (x2,y2) (x1,y1) =
  if keysDown == Keys.shift then
    let (dx, dy) = (x2 - x1, y2 - y1) in
    let angle = atan2 (toFloat (-dy)) (toFloat dx) in
    let slice = round (angle / radiansPerSlice) in
    let r = Utils.distanceInt (x2,y2) (x1,y1) in
    let xb = toFloat x1 + r * cos (toFloat slice * radiansPerSlice) in
    let yb = toFloat y1 - r * sin (toFloat slice * radiansPerSlice) in
    (round xb, round yb)
  else
    (x2, y2)

drawNewLine model (x2,y2) (x1,y1) =
  let stroke = if model.toolType == HelperLine then guideStroke else defaultStroke in
  let (xb, yb) = snapLine model.keysDown (x2,y2) (x1,y1) in
  let line =
    svgLine [
        stroke , defaultStrokeWidth , defaultOpacity
      , LangSvg.attr "x1" (toString x1) , LangSvg.attr "y1" (toString y1)
      , LangSvg.attr "x2" (toString xb) , LangSvg.attr "y2" (toString yb)
      ]
  in
  [ line ]

drawNewRect keysDown pt2 pt1 =
  let (xa, xb, ya, yb) =
    if keysDown == Keys.shift
    then squareBoundingBox pt2 pt1
    else boundingBox pt2 pt1
  in
  let rect =
    svgRect [
        defaultFill , defaultOpacity
      , LangSvg.attr "x" (toString xa) , LangSvg.attr "width" (toString (xb-xa))
      , LangSvg.attr "y" (toString ya) , LangSvg.attr "height" (toString (yb-ya))
      ]
  in
  [ rect ]

drawNewEllipse keysDown pt2 pt1 =
  let (xa, xb, ya, yb) =
    if keysDown == Keys.shift
    then squareBoundingBox pt2 pt1
    else boundingBox pt2 pt1
  in
  let (rx, ry) = ((xb-xa)//2, (yb-ya)//2) in
  let ellipse =
    svgEllipse [
        defaultFill , defaultOpacity
      , LangSvg.attr "cx" (toString (xa + rx))
      , LangSvg.attr "cy" (toString (ya + ry))
      , LangSvg.attr "rx" (toString rx)
      , LangSvg.attr "ry" (toString ry)
      ]
  in
  [ ellipse ]

drawNewPolygon ptLast points =
  let (xInit,yInit) = Utils.last_ (ptLast::points) in
  let dot =
    svgCircle [
        dotSize , dotFill , defaultOpacity
      , LangSvg.attr "cx" (toString xInit)
      , LangSvg.attr "cy" (toString yInit)
      ] in
  let maybeShape =
    case (ptLast::points) of
      [_] -> []
      _ ->
        -- don't need to reverse, but keeping it same as resulting shape
        let polyPoints = List.reverse (ptLast::points) in
        let sPoints =
          Utils.spaces <|
            List.map (\(x,y) -> String.join "," (List.map toString [x,y]))
                     polyPoints
        in
        [ svgPolygon [
            defaultStroke , defaultStrokeWidth , defaultFill , defaultOpacity
          , LangSvg.attr "points" sPoints
          ] ]
   in
   dot :: maybeShape

-- TODO this doesn't appear right away
-- (dor does initial poly, which appears only on MouseUp...)
drawNewHelperDot (x,y) =
  let r = drawNewPolygonDotSize in
  let dot =
    svgCircle [
        defaultFill , defaultOpacity
      , LangSvg.attr "cx" (toString 200)
      , LangSvg.attr "cy" (toString 200)
      , LangSvg.attr "r" (toString 10)
      ]
  in
  [ dot ]


--------------------------------------------------------------------------------
-- User Interface

strTitle = " sketch-n-sketch " ++ params.strVersion

colorDebug_ c1 c2 =
  if params.debugLayout
    then GE.color c1
    else GE.color c2

colorDebug c1 = colorDebug_ c1 interfaceColor

codebox : Int -> Int -> Model -> GE.Element
codebox w h model =
  let
    event = case model.mode of
              SyncSelect _ -> []
              _ -> [Events.on "input" Events.targetValue
                      (Signal.message events.address << CodeUpdate)]
    code = codeToShow model
  in
    codebox_ w h event code (not (editingMode model))

highlightThisIf b =
  if b
  then ("box-shadow", "inset 0 0 10px 4px rgba(231, 76, 60,0.5)")
  else ("box-shadow", "inset 0 0 10px 4px darkgray")

codebox_ w h event s readOnly =
  let innerPadding = 4
  in
    Html.toElement w h <|
      Html.textarea
        ([ Attr.id "editor"
         , Attr.spellcheck False
         , Attr.readonly readOnly
         , Attr.style
             [ ("font-family", params.mainSection.codebox.font)
             , ("font-size", params.mainSection.codebox.fontSize)
             , ("border", params.mainSection.codebox.border)
             , ("whiteSpace", "pre")
             , ("height", "100%")
             , ("width", "100%")
             , ("resize", "none")
             , ("overflow", "auto")
             -- Horizontal Scrollbars in Chrome
             , ("word-wrap", "normal")
             , ("background-color", "whitesmoke")
             , ("padding", toString innerPadding ++ "px")
             -- Makes the 100% for width/height work as intended
             , ("box-sizing", "border-box")
             , highlightThisIf (not readOnly)
             ]
         , Attr.value s
         , Events.onMouseUp events.address MouseUp
         -- doesn't work here, need to handle this in Ace
         -- , Events.onMouseDown events.address Edit
         ] ++ event)
        []

-- Replaces the canvas if we are displaying an error
-- Is mostly a copy of the basic code box in the not manipulable mode
errorBox : Int -> Int -> String -> GE.Element
errorBox w h errormsg =
  Html.toElement w h <|
    Html.textarea
      [ Attr.spellcheck False
      , Attr.readonly True
      , Attr.style
        [ ("font-family", params.mainSection.codebox.font)
        , ("font-size", params.mainSection.codebox.fontSize)
        , ("border", params.mainSection.codebox.border)
        , ("whiteSpace", "pre")
        , ("height", "100%")
        , ("width", "100%")
        , ("resize", "none")
        , ("overflow", "auto")
        -- Horizontal Scrollbars in Chrome
        , ("word-wrap", "normal")
        , ("background-color", "whitesmoke")
        , ("padding", "4px")
        -- Makes the 100% for width/height work as intended
        , ("box-sizing", "border-box")
        , highlightThisIf False
        ]
      , Attr.value errormsg
      , Events.onMouseUp events.address MouseUp
      ]
      []

canvas : Int -> Int -> Model -> GE.Element
canvas w h model =
  case model.mode of
    Print s -> codebox_ w h [] s True
    _       -> canvas_ w h model

canvas_ w h model =
  let addZones = case (editingMode model, model.mode) of
    (False, AdHoc)  -> True
    (False, Live _) -> case model.toolType of
                         Cursor       -> True
                         SelectAttrs  -> True
                         SelectShapes -> True
                         _            -> False
    _               -> False
  in
  let mainCanvas_ = buildSvg (model, addZones) model.slate in
  let mainCanvas =
    case drawNewShape model of
      []       -> mkSvg addZones mainCanvas_
      drawings -> mkSvg addZones (Svg.g [] (mainCanvas_ :: drawings))
  in
  case (model.mode, model.showWidgets) of
    (Live _, True) ->
      let widgets = buildSvgWidgets w h model.widgets in
      let svg = mkSvg addZones (Svg.g [] [mainCanvas, widgets]) in
      Html.toElement w h svg
    (SyncSelect possibleChanges, _) ->
      let possibleChangeStyle = [ ("width",  toString (w//3 - 32))
                                , ("height", toString (h//3 - 32))
                                , ("margin", "10px")
                                , ("background", "white")
                                , ("border", "solid 2px black")
                                , ("cursor", "pointer")
                                , ("text-align", "center")
                                ]
      in
      let
        animatePossibleChange (exp, val, slate, code) =
          let decimalPart a = a - (toFloat <| truncate a) in
          let nToRand n = -- Semi-random function mapping integers to [0.0, 1.0)
            let f = toFloat n in
            decimalPart ((1.0 + f*f*f*f) * e)
          in
          let animateNumber i x time =
            let baseSpeed = 0.4 in
            let frequency = baseSpeed * (0.25 + nToRand i) in
            let theta = time * frequency * 2.0 * pi in
            x * (1 + 0.2 * sin(theta))
          in
          let locIdsAndNumbers = unfrozenLocIdsAndNumbers exp in
          let subst = Dict.fromList (Utils.mapi (\(i, (locId, x)) -> (locId, animateNumber i x model.syncSelectTime)) locIdsAndNumbers) in
          -- let _ = Debug.log (toString subst) subst in
          -- let _ = Debug.log (toString model.runAnimation) model.runAnimation in
          let newExp = applyLocSubst subst exp in
          let (newVal,_) = Eval.run newExp in
          let slateToDraw = LangSvg.resolveToIndexedTree model.slideNumber model.movieNumber model.movieTime newVal in
          (slateToDraw, (exp, val, slate, code))
        possibleChangeToSvg (slateToDraw, (exp, val, slate, code)) =
          let model' = { model | showZones = 0, showWidgets = False} in
          Svg.svg [ Svg.Attributes.viewBox (String.join " " (List.map toString [0, 0, w, h]))
                  , Attr.style possibleChangeStyle
                  , Events.onClick events.address (SelectOption (exp, val, slate, code))
                  , Events.onMouseOver events.address (PreviewCode (Just code))
                  , Events.onMouseOut events.address (PreviewCode Nothing)
                  ]
                  [ buildSvg (model', False) slateToDraw ]
        cancelButton = Html.button [ Attr.style (possibleChangeStyle ++ [("font-size", "25px")])
                                   , Events.onClick events.address CancelSync
                                   ]
                                   [Html.text "Cancel"]
      in
      GE.color (Color.grayscale 0.1)
        <| Html.toElement w h
        <| Html.div [ Attr.style [("overflow", "auto"), ("width", toString w), ("height", toString h)]
                    ]
        <| (List.map possibleChangeToSvg (List.map animatePossibleChange possibleChanges)) ++ [cancelButton]
    _ ->
      Html.toElement w h (mkSvg addZones mainCanvas)

mkSvg hilite svg =
  Svg.svg
     [ onMouseUp MouseUp
     , onMouseDown MouseClickCanvas
     , Attr.style [ ("width", "100%") , ("height", "100%")
                  , ("border", params.mainSection.canvas.border)
                  , highlightThisIf hilite
                  ] ]
     [ svg ]

twoButtons w h b1 b2 =
  let delta = 3 in
  let wHalf = (w//2 - delta) in
  GE.flow GE.right [ b1 wHalf h, GE.spacer (2 * delta) h, b2 wHalf h ]

threeButtons w h b1 b2 b3 =
  let delta = 3 in
  let sep   = GE.spacer (2 * delta) h in
  let w_    = (w//3 - delta) in
  GE.flow GE.right [ b1 w_ h, sep, b2 w_ h, sep, b3 w_ h ]

widgetsExampleNavigation w h model =
  [ twoButtons w h (codeButton model) (canvasButton model)
  , dropdownExamples model w h
  , editRunButton model w h
  , twoButtons w h (saveButton model) (saveAsButton model)
  , loadButton model w h
  ]

widgetsUndoRedo w h model =
  [ twoButtons w h (undoButton model) (redoButton model)
  , cleanButton model w h
  ]

widgetsSlideNavigation w h model =
  [ gapWidget w h
  , twoButtons w h (previousSlideButton model) (nextSlideButton model)
  , twoButtons w h (previousMovieButton model) (nextMovieButton model)
  , slideNumber model w h
  ]

widgetsTools w h model =
{-
  [ threeButtons w h
      (toolButton model Cursor)
      (toolButton model SelectAttrs)
      (toolButton model SelectShapes)
  , threeButtons w h
      (toolButton model Line)
      (toolButton model Rect)
      (toolButton model Oval)
  , threeButtons w h
      (toolButton model Poly)
      (toolButton model Path)
      (toolButton model Text)
  ]
-}
  [ toolButton model Cursor w h
  , twoButtons w h
      (toolButton model Line)
      (toolButton model Rect)
  , twoButtons w h
      (toolButton model Oval)
      (toolButton model Poly)
  , twoButtons w h
      (toolButton model HelperLine)
      (toolButton model HelperDot)
  , toolButton model (Lambda "star") w h
  ]

widgetsToolExtras w h model =
  let gap = gapWidget w h in
  case model.toolType of
{-
    -- TODO get rid of showZonesSelect
    Cursor       -> [ gap , zoneButton model w h  ]
    SelectAttrs  -> [ gap , relateAttrsButton w h ]
-}
    Cursor       -> if model.showZones == showZonesSelect
                    then gap :: (zoneButtons model w h)
                             ++ [ gap, twoButtons w h relateAttrsButton digHoleButton ]
                             -- ++ [ relateAttrsButton w h, digHoleButton w h]
                    else gap :: (zoneButtons model w h)
    SelectShapes -> [ gap , relateShapesButton w h ]
    _            -> []

middleWidgets row1 row2 w h wWrap hWrap model =

  let exampleNavigation = widgetsExampleNavigation w h model in
  let undoRedo = widgetsUndoRedo w h model in
  let tools = widgetsTools w h model in
  let extras = widgetsToolExtras w h model in
  let slideNavigation = widgetsSlideNavigation w h model in

  let l1  = if row1 then exampleNavigation ++ undoRedo else [] in
  let l2_ = if row2 then tools ++ extras else [] in

  let l2 =
    if row1 && row2
      then gapWidget w h :: l2_  -- vertical   (row1 == row2 == True)
      else l2_ in                -- horizontal (row1 XOR row2)

  List.map (GE.container wWrap hWrap GE.middle) <|
    case (editingMode model, model.mode, unwrapVList model.inputVal) of
      (False, SyncSelect _, _) -> []
      (False, Print _, _) -> l1
      (False, _, Just [VConst (slideCount, _), _]) ->
        l1 ++
        (if row1 then slideNavigation else []) ++
        l2
      (False, _, _) -> l1 ++ l2
      (True, _, _) -> l1

      -- modeButton and syncButton...

gapWidget w h = GE.spacer w h

{-
syncButton_ w h model =
  case (model.mode, model.showZones == showZonesSelect) of
    (AdHoc, False) -> [syncButton w h]
    (Live _, True) -> [relateButton w h]
    _              -> []
-}
{-
  case model.mode of
    AdHoc -> [syncButton w h]
    _     -> []
-}

wBtn = params.mainSection.widgets.wBtn
hBtn = params.mainSection.widgets.hBtn

wBtnWide = params.mainSection.widgets.wBtnWide

buttonAttrs w h =
  Attr.style
    [ ("width", dimToPix w)
    , ("height", dimToPix h)
    , ("font-family", params.mainSection.widgets.font)
    , ("font-size", params.mainSection.widgets.fontSize)
    ]

gutterForResizing orient w h =
  let s = if orient == Vertical then "ew-resize" else "ns-resize" in
  colorDebug Color.darkBlue <|
    Html.toElement w h <|
      Html.div
          [ Events.onMouseDown events.address StartResizingMid
          , Events.onMouseUp events.address MouseUp
          , Attr.style
              [ ("width", dimToPix w) , ("height", dimToPix h)
              , ("cursor", s) ]
          ]
          [ ]

-- Makes a div appropriate for the Ace code editor to be inserted into
-- Flashing of the code editor is caused because of the 'Element' abstraction
-- torching the interior of the codeBox portion of the screen and necessitates a
-- re-embedding of the editor on the Ace side of things, the delay of which
-- (needs to be sent through a port and such) makes it flash.
codeBox : Int -> Int -> GE.Element
codeBox w h = Html.toElement w h <|
    Html.Lazy.lazy (\a -> Html.div [ Attr.id "editor"
             , Attr.style
                 [ ("width", "100%") -- The toElement makes a wrapping Div that
                                     -- has the appropriate w/h
                 , ("height", "100%")
                 , ("pointer-events", "auto")
                 , ("z-index", "1")
                 ]
             ] []) True -- No need to rerender on size changes

mainSectionVertical : Int -> Int -> Model -> GE.Element
mainSectionVertical w h model =
  let
    wGut    = params.mainSection.vertical.wGut
    wMiddle = wBtn
    wCode_  = (w - wMiddle - wGut - wGut) // 2
{-
    wCode   = wCode_ + model.midOffsetX
    wCanvas = wCode_ - model.midOffsetX
-}
    wCode   = if model.hideCode then 0
              else if model.hideCanvas then (w - wMiddle - wGut - wGut)
              else wCode_ + model.midOffsetX
    wCanvas = w - wMiddle - wGut - wGut - wCode
    hCanvas = h - hZInfo
    hZInfo  = params.mainSection.canvas.hZoneInfo
    hWidget = params.mainSection.widgets.hBtn
                + params.mainSection.vertical.hExtra
    wExtra  = params.mainSection.horizontal.wExtra
  in

  let codeSection = if model.basicCodeBox
                       then codebox wCode h model
                       else codeBox wCode h in

  let canvasSection = case model.errorBox of
    Nothing ->
      GE.size wCanvas h <|
        GE.flow GE.down
          [ canvas wCanvas hCanvas model
          , GE.flow GE.left
              [ colorDebug Color.red <|
                  GE.container wBtn (hZInfo+1) GE.middle <|
                  outputButton model wBtn hBtn
              , colorDebug Color.orange <| GE.spacer wExtra (hZInfo+1)
              , colorDebug Color.red <|
                  GE.container wBtnWide (hZInfo+1) GE.middle <|
                  ghostsButton model wBtnWide hBtn
              , caption model (wCanvas+1-(wBtn+wExtra+wBtnWide)) (hZInfo+1) -- NOTE: +1 is a band-aid
              ]
          -- , caption model (wCanvas+1) hZInfo -- NOTE: +1 is a band-aid
          ]
    Just errormsg -> errorBox wCanvas h errormsg
  in

  let gutter = gutterForResizing model.orient wGut h in

  let middleSection =
    colorDebug Color.lightBlue <|
      GE.size wMiddle h <|
        GE.flow GE.down <|
          middleWidgets True True wBtn hBtn wMiddle hWidget model in
  GE.flow GE.right <|
    [ codeSection, gutter, middleSection, gutter, canvasSection ]

mainSectionHorizontal : Int -> Int -> Model -> GE.Element
mainSectionHorizontal w h model =
  let
    hGut    = params.mainSection.horizontal.hGut
    hMiddle = hBtn
    hCode_  = (h - 2*hMiddle - 3*hGut) // 2
    hCode   = hCode_ + model.midOffsetY
    hCanvas = hCode_ - model.midOffsetY - hZInfo
    hZInfo  = params.mainSection.canvas.hZoneInfo
    wWidget = params.mainSection.widgets.wBtn + wExtra
    wExtra  = params.mainSection.horizontal.wExtra
  in

  let codeSection = if model.basicCodeBox
                       then codebox w hCode model
                       else codeBox w hCode in

  let canvasSection = case model.errorBox of
    Nothing ->
        GE.size w (hCanvas + hZInfo) <|
          GE.flow GE.down
            [ canvas w hCanvas model
            , GE.flow GE.left
                [ colorDebug Color.red <|
                    GE.container wBtn (hZInfo+1) GE.middle <|
                    outputButton model wBtn hBtn
                , colorDebug Color.orange <| GE.spacer wExtra (hZInfo+1)
                , colorDebug Color.red <|
                    GE.container wBtnWide (hZInfo+1) GE.middle <|
                    ghostsButton model wBtnWide hBtn
                , caption model (w-(wBtn+wExtra+wBtnWide)) (hZInfo+1) -- NOTE: +1 is a band-aid
                ]
            -- , caption model w (hZInfo+1) -- NOTE: +1 is a band-aid
            ]
    Just errormsg -> errorBox w (hCanvas + hZInfo) errormsg
  in

  let gutter = gutterForResizing model.orient w hGut in

  let (middleSection1, middleSection2) =
    let foo row1 row2 =
      colorDebug Color.lightBlue <|
        GE.size w hMiddle <|
          GE.flow GE.right <|
            middleWidgets row1 row2 wBtn hBtn wWidget hMiddle model
    in
    (foo True False, foo False True) in

  GE.flow GE.down <|
    [ codeSection
    , gutter , middleSection1 , gutter , middleSection2 , gutter
    , canvasSection
    ]

simpleButton_
   : Signal.Address a -> ButtonKind -> a -> Bool -> a -> String -> String -> String
  -> Int -> Int -> GE.Element
simpleButton_ addy btnKind defaultMsg disabled msg value name text w h =
  if disabled then
      GI.customButton (Signal.message addy defaultMsg)
        (makeButton (btnKind, Disabled) w h text)
        (makeButton (btnKind, Disabled) w h text)
        (makeButton (btnKind, Disabled) w h text)
  else
      GI.customButton (Signal.message addy msg)
        (makeButton (btnKind, Raised) w h text)
        (makeButton (btnKind, Highlighted) w h text)
        (makeButton (btnKind, Depressed) w h text)

simpleEventButton_ = simpleButton_ events.address Regular Noop
simpleTaskButton_  = simpleButton_ taskMailbox.address Regular (Task.succeed ())

simpleButton = simpleEventButton_ False
simpleTaskButton = simpleTaskButton_ False

-- displayKey s = " " ++ Utils.parens s
displayKey s = " " ++ s

editRunButton model w h =
  let disabled = model.mode == AdHoc in
  case editingMode model of
    True -> simpleEventButton_ disabled WaitRun
              "Run" "Run" "Run Code" w h
    False -> simpleEventButton_ disabled Edit "Edit" "Edit" "Edit Code" w h

outputButton model w h =
  let disabled = model.mode == AdHoc in
  let cap =
     case model.mode of
       Print _ -> "[Out] SVG"
       _       -> "[Out] Canvas"
  in
  simpleEventButton_ disabled ToggleOutput "Toggle Output" "Toggle Output" cap w h

ghostsButton model w h =
  let cap =
     case model.showWidgets of
       True  -> "[Widgets] Shown"
       False -> "[Widgets] Hidden"
  in
  let foo old =
    let showWidgets' = not old.showWidgets in
    let mode' =
      case old.mode of
        Print _ -> Print (LangSvg.printSvg showWidgets' old.slate)
        _       -> old.mode
    in
    { old | showWidgets = showWidgets', mode = mode' }
  in
  simpleEventButton_ False (UpdateModel foo) "Toggle Output" "Toggle Output" cap w h

syncButton =
  simpleButton Sync "Sync" "Sync the code to the canvas" "Sync"

relateAttrsButton =
  simpleButton RelateAttrs "Relate" "Relate" "Relate" -- "Relate Attrs"

digHoleButton =
  simpleButton DigHole "unused?" "unused?" "Dig" -- "Dig Hole"

relateShapesButton =
  simpleButton RelateShapes "Relate" "Relate" "Relate Shapes"

zoneButtons model w h =
  let caption mode =
    if mode == showZonesNone        then "Hide" -- "[Zones] Hidden"
    else if mode == showZonesBasic  then "Show" -- "[Zones] Basic"
    else if mode == showZonesSelect then "Attrs" -- "[Zones] Attrs"
    else if mode == showZonesExtra  then "Extra" -- "[Zones] Extra"
    else if mode == showZonesDel    then Debug.crash "[Zones] Delete"
    else
      Debug.crash "zoneButton caption"
  in
  let zoneButton mode w h =
    let selected = (model.showZones == mode) in
    let btnKind = if selected then Selected else Unselected in
      simpleButton_ events.address btnKind Noop False (SelectZonesMode mode)
        "Still dunno what this does" "Dunno what this is for" (caption mode) w h
  in
    -- Delete turned off for now
    -- List.map zoneButton showZonesModes
    -- List.map zoneButton [ 0 .. (showZonesModeCount - 1 - 1) ]
    [ twoButtons w h (zoneButton showZonesNone) (zoneButton showZonesBasic)
    , twoButtons w h (zoneButton showZonesExtra) (zoneButton showZonesSelect)
    ]

{-
shapeButton model =
  let (cap_, next) = case model.newShapeKind of
    -- Nothing          -> ("None", Just "line")
    Nothing          -> ("Cursor", Just "line")
    Just "line"      -> ("Line", Just "rect")
    Just "rect"      -> ("Rect", Just "ellipse")
    Just "ellipse"   -> ("Ellipse", Just "polygon")
    Just "polygon"   -> ("Polygon", Nothing)
    _                -> Debug.crash "shapeButton"
  in
  let foo m = { m | newShapeKind = next } in
  -- let cap = "[Draw] " ++ cap_ in
  let cap = "[Tool] " ++ cap_ in
  simpleButton (UpdateModel foo) cap cap cap
-}

luckyButton model =
  let foo old =
    let so = old.syncOptions in
    let so' = { so | feelingLucky = Sync.toggleHeuristicMode so.feelingLucky } in
    let m' =
      case old.mode of
        Live _ -> mkLive_ so' old.slideNumber old.movieNumber old.movieTime old.inputExp
        _      -> old.mode
    in
    { old | syncOptions = so', mode = m' }
  in
  -- let yesno = if model.syncOptions.feelingLucky then "Yes" else "No" in
  -- simpleButton (UpdateModel foo) "Lucky" "Lucky" ("[Lucky?] " ++ yesno)
  let yesno =
    let hm = model.syncOptions.feelingLucky in
    if hm == Sync.heuristicsNone then "None"
    else if hm == Sync.heuristicsFair then "Fair"
    else "Biased"
  in
  simpleButton (UpdateModel foo) "Heur" "Heur" ("[Heuristics] " ++ yesno)

{-
frozenButton model =
  let cap = if model.syncOptions.thawedByDefault then "[Default] n?" else "[Default] n!" in
  simpleButton ToggleThawed "ToggleThawed " "Toggle ?/!" cap
-}

toolButton : Model -> ToolType -> Int -> Int -> GE.Element
toolButton model tt w h =
  let cap = case tt of
    -- Cursor -> "1"
    Cursor -> "Cursor"
    SelectAttrs -> "2"
    SelectShapes -> "3"
    Line -> "Line"
    Rect -> "Rect"
    Oval -> "Oval"
    Poly -> "Poly"
    -- Line -> "L"
    -- Rect -> "R"
    -- Oval -> "E"
    -- Poly -> "P"
    Path -> "-"
    Text -> "-"
    HelperLine -> "(Rule)"
    HelperDot -> "(Dot)"
    Lambda f -> Utils.bracks Utils.uniLambda ++ " " ++ f
  in
  let btnKind = if model.toolType == tt then Selected else Unselected in
  simpleButton_ events.address btnKind Noop False
    (UpdateModel (\m -> { m | toolType = tt })) cap cap cap w h

saveButton : Model -> Int -> Int -> GE.Element
saveButton model w h =
    let cap = "Save" in
    let disabled = List.any ((==) model.exName << Utils.fst3) Examples.list in
    simpleEventButton_
      disabled (InterfaceModel.WaitSave model.exName)
      cap cap cap w h
      -- dn dn Utils.uniSave w h

saveAsButton : Model -> Int -> Int -> GE.Element
saveAsButton model w h =
    let cap = "Clone" in
    simpleTaskButton
      (saveStateLocally model.exName True model)
      cap cap cap w h
      -- dn dn (Utils.uniCamera) w h

loadButton : Model -> Int -> Int -> GE.Element
loadButton model w h =
  let cap = "Revert" in
  simpleTaskButton
    (loadLocalState model.exName)
    cap cap cap w h
    -- "Reload" "Reload" Utils.uniReload w h

undoButton : Model -> Int -> Int -> GE.Element
undoButton model =
  let past = fst model.history in
  simpleEventButton_ (List.length past == 0) Undo "Undo" "Undo" "Undo" -- Utils.uniUndo

redoButton : Model -> Int -> Int -> GE.Element
redoButton model =
  let future = snd model.history in
  simpleEventButton_ (List.length future == 0) Redo "Redo" "Redo" "Redo" -- Utils.uniRedo

previousSlideButton : Model -> Int -> Int -> GE.Element
previousSlideButton model =
  simpleEventButton_ (model.slideNumber == 1 && model.movieNumber == 1) PreviousSlide "what is this for" "is this redundant" "◀◀"

nextSlideButton : Model -> Int -> Int -> GE.Element
nextSlideButton model =
  simpleEventButton_ (model.slideNumber == model.slideCount && model.movieNumber == model.movieCount) NextSlide "WHAT IS THIS FOR" "IS THIS REDUNDANT" "▶▶"

previousMovieButton : Model -> Int -> Int -> GE.Element
previousMovieButton model =
  simpleEventButton_ (model.slideNumber == 1 && model.movieNumber == 1) PreviousMovie "what is this for" "is this redundant" "◀"

nextMovieButton : Model -> Int -> Int -> GE.Element
nextMovieButton model =
  simpleEventButton_ (model.slideNumber == model.slideCount && model.movieNumber == model.movieCount) NextMovie "WHAT IS THIS FOR" "IS THIS REDUNDANT" "▶"

slideNumber : Model -> Int -> Int -> GE.Element
slideNumber model w h =
  let slideNumberElement = GE.centered << T.color Color.white << (T.typeface ["sans-serif"]) << T.fromString in
  GE.container w h GE.middle <|
    slideNumberElement ("Slide " ++ toString model.slideNumber ++ "/" ++ toString model.slideCount)

dropdownExamples : Model -> Int -> Int -> GE.Element
dropdownExamples model w h =
  let
    choices = case model.mode of
      AdHoc -> [(model.exName, Signal.send events.address Noop)]
      _ ->
        let foo (name,_,thunk) = (name, Signal.send events.address (SelectExample name thunk))
            bar saveName = (saveName, loadLocalState saveName)
            blank = ("", Task.succeed ())
            localsaves = case model.localSaves of
                [] -> []
                l  ->
                  List.concat
                    [ [ ("Local Saves:", Task.succeed ())
                      , blank
                      ]
                    , List.map bar l
                    , [ blank ]
                    ]
        in List.concat
            [ localsaves
            , [ ("Builtin Examples:", Task.succeed ())
              , blank
              ]
            , (List.map foo Examples.list)
            , [ blank
              , ("*Clear Local Saves*", clearLocalSaves)
              ]
            ]
    options = List.map (\(name,task) ->
        if name == model.exName then
              Html.option
                [ Attr.value name
                , Attr.selected True
                ]
                [ Html.text name ]
        else
              Html.option
                [ Attr.value name
                ]
                [ Html.text name ]) choices
    findTask name choices = case choices of
        (n,t) :: rest -> if n == name
                           then t
                           else findTask name rest
        [] -> Debug.crash "Dropdown example does not have associated task"
  in Html.toElement 120 24 <| Html.select
        [ Attr.style
          [ ("pointer-events", "auto")
          , ("border", "0 solid")
          , ("display", "block")
          , ("width", "120px")
          , ("height", "24px")
          , ("font-family", "sans-serif")
          , ("font-size", "1em")
          ]
        , Events.on "change" Events.targetValue
                (\selected -> Signal.message taskMailbox.address <|
                                findTask selected choices)
        ] options

{-
modeButton model =
  if model.mode == AdHoc
  then simpleEventButton_ True Noop "SwitchMode" "SwitchMode" "[Mode] Ad Hoc"
  else simpleEventButton_ (model.newShapeKind /= Nothing)
         (SwitchMode AdHoc) "SwitchMode" "SwitchMode" "[Mode] Live"
-}

cleanButton model =
  let disabled = case model.mode of Live _ -> False
                                    _      -> True in
  simpleEventButton_ disabled WaitClean "Clean" "Clean" "Clean Up"

orientationButton w h model =
    let text = "[Orientation] " ++ toString model.orient
    in
      simpleButton SwitchOrient text text text w h

basicBoxButton w h model =
    let (text, evt) = case model.basicCodeBox of
          True  -> ("[Code Box] Basic", ToggleBasicCodeBox)
          False -> case model.editingMode of
              Nothing -> ("[Code Box] Fancy", ToggleBasicCodeBox)
              Just _  -> ("[Code Box] Fancy", WaitCodeBox)
    in
       simpleButton
         evt
         text text text w h

codeButton model w h =
  let (cap, btnKind) = case model.hideCode of
    True  -> ("Code", Unselected)
    False -> ("Code", Selected)
  in
  let foo model = { model | hideCode = not model.hideCode } in
  simpleButton_ events.address btnKind Noop
    model.hideCanvas (UpdateModel foo) cap cap cap w h

canvasButton model w h =
  let (cap, btnKind) = case model.hideCanvas of
    True  -> ("Canvas", Unselected)
    False -> ("Canvas", Selected)
  in
  let foo model = { model | hideCanvas = not model.hideCanvas } in
  simpleButton_ events.address btnKind Noop
    model.hideCode (UpdateModel foo) cap cap cap w h


--------------------------------------------------------------------------------
-- Zone Caption and Highlights

caption : Model -> Int -> Int -> GE.Element
caption model w h =
  let eStr = GE.leftAligned << T.color Color.white << T.monospace << T.fromString in
  let tStr col = T.height 16 << T.color col << T.monospace << T.fromString in
  let tSpace = T.height 5 << T.color Color.white << T.monospace << T.fromString <| "\n" in
  colorDebug Color.orange <|
    GE.container w h GE.topLeft <|
      case (model.caption, model.mode, model.mouseMode) of
        (Just (Hovering (i,k,z)), Live info, MouseNothing) ->
          case hoverInfo info (i,k,z) of
            Nothing -> GE.empty
            Just l ->
              let numLocs = List.map (\(s,n) -> toString n.val ++ Utils.braces s) l in
              let line1 = (k ++ toString i) ++ " " ++ z in
              let line2 = Utils.spaces numLocs in
              -- eStr (" " ++ line1 ++ "\n " ++ line2)
              let cap =
                if line2 == ""
                then T.bold <| tStr Color.red " (INACTIVE)"
                else T.bold <| tStr Color.green " (ACTIVE)"
              in
              GE.leftAligned <| T.concat
                 [ tSpace -- slop
                 , tStr Color.white (" " ++ line1)
                 , cap
                 , tStr Color.white ("\n " ++ line2)
                 ]
        (Just (LangError err), _, _) ->
          eStr err
        _ ->
          GE.empty

-- this is a bit redundant with Model.liveInfoToHighlights...
hoverInfo info (i,k,z) =
  let err y = "hoverInfo: " ++ toString y in
  flip Utils.bindMaybe (Dict.get i info.assignments) <| \d ->
  flip Utils.bindMaybe (Dict.get z d)                <| \(locset,_) ->
    let locs = Set.toList locset in
    Just <|
      List.map (\(lid,_,x) ->
        let n = Utils.justGet_ (err (i,z,lid)) lid info.initSubst in
        if x == ""
          then ("loc_" ++ toString lid, n)
          else (x, n)
       ) locs

turnOnCaptionAndHighlights id shape zone =
  UpdateModel <| \m ->
    let codeBoxInfo = m.codeBoxInfo in
    let hi = liveInfoToHighlights id zone m in
    { m | caption = Just (Hovering (id, shape, zone))
        , codeBoxInfo = { codeBoxInfo | highlights = hi } }

turnOffCaptionAndHighlights =
  UpdateModel <| \m ->
    let codeBoxInfo = m.codeBoxInfo in
    { m | caption = Nothing
        , codeBoxInfo = { codeBoxInfo | highlights = [] } }

--------------------------------------------------------------------------------

-- The pop-up save dialog box
-- TODO clean this up, is needlessly bulky
saveElement : Model -> Int -> Int -> GE.Element
saveElement model w h = case model.mode of
  SaveDialog x ->
      -- Note that dimBox must not be a parent of the pickBox, as
      -- opacity of a parent clobbers that of all its children
      let dimBox = GE.color Color.black
                      <| GE.opacity 0.5
                      <| GE.spacer w h
          pickBox = GE.container w h GE.middle
                      <| GE.color interfaceColor
                      <| GE.container 400 200 GE.middle
                      <| GE.flow GE.down
                           [ GE.flow GE.right
                              [ GE.spacer 42 18
                              , GE.centered <|
                                  T.style titleStyle
                                  (T.fromString "Save Work to Browser")
                              ]
                           , GE.spacer 160 10
                           , GE.flow GE.right
                              [ Html.toElement 200 40
                                  <| Html.input
                                      [ Attr.type' "text"
                                      , Attr.style
                                          [ ("height", "32px")
                                          , ("width", "192px")
                                          , ("padding", "4px")
                                          , ("border-width", "0px")
                                          , ("pointer-events", "auto")
                                          , ("box-shadow", "inset 0 0 10px 3px lightgray")
                                          ]
                                      , Attr.value model.fieldContents.value
                                      , Attr.placeholder
                                            model.fieldContents.hint
                                      , Attr.autofocus True
                                      , Events.on "input" Events.targetValue
                                          (\cont -> Signal.message events.address
                                            <| UpdateFieldContents
                                                { value = cont
                                                , hint =
                                                    model.fieldContents.hint
                                                }
                                          )
                                      ]
                                      []
                              , GE.spacer 10 40
                              , simpleTaskButton
                                  ( checkAndSave model.fieldContents.value
                                                 model
                                  )
                                  "Create Save" "Create Save" "Create Save"
                                  100 40
                              ]
                           , GE.spacer 160 10
                           , GE.flow GE.right
                              [ GE.spacer 47 50
                              , GE.centered <|
                                  T.height 12 <|
                                  T.color Color.white <|
                                  (T.fromString <|
                                  "Note: This will overwrite saves with\n"
                                  ++ "the same name. You must choose a\n"
                                  ++ "name different than a built-in example.")
                              ]
                           , GE.spacer 160 10
                           , GE.flow GE.right
                               [ GE.spacer 112 30
                               , simpleButton
                                  (RemoveDialog False "")
                                  "Cancel" "Cancel" "Cancel"
                                  75 30
                               ]
                           ]
      in GE.flow GE.outward [ dimBox, pickBox ]
  _ -> GE.empty


view : (Int, Int) -> Model -> GE.Element
view (w,h) model =
  let
    wAll = w - (2 * wGut) - 1
    wGut = params.wGut
    hTop = params.topSection.h
    hBot = params.botSection.h
    hMid = h - hTop - hBot - 1
    hTot = hTop + hMid + hBot
  in

  let topSection =
    let
      title = (\e -> GE.container (GE.widthOf e) hTop GE.middle e) <|
                GE.leftAligned <| T.style titleStyle (T.fromString strTitle)

      wLogo = params.topSection.wLogo
      logo  = GE.image wLogo wLogo (imgPath "light_logo.svg")

      wBtnO = params.topSection.wBtnO
      hBtnO = params.topSection.hBtnO
      wJunk = params.topSection.wJunk
      wSpcB = params.mainSection.horizontal.wExtra

      -- wSep  = GE.spacer (wAll - (wLogo + 2 * wBtnO + wJunk + wSpcB)) 1
      wSep  = GE.spacer (wAll - (wLogo + 2 * wBtnO + wJunk + wSpcB)) 1
      btnO  = (\e -> GE.container (GE.widthOf e) hTop GE.middle e) <|
                orientationButton wBtnO hBtnO model

      {- not displaying Codebox button for now
      spcB  = GE.spacer wSpcB hTop
      btnB  = (\e -> GE.container (GE.widthOf e) hTop GE.middle e) <|
                basicBoxButton wBtnO hBtnO model
      -}

      spcH  = GE.spacer wSpcB hTop
      btnH  = (\e -> GE.container (GE.widthOf e) hTop GE.middle e) <|
                luckyButton model wBtnO hBtnO
    in
      GE.size wAll hTop <|
        GE.flow GE.right
          [ GE.container wLogo hTop GE.middle logo
          , GE.container (wAll - wLogo) hTop GE.middle <|
              -- GE.flow GE.right [ title, wSep, btnB, spcB, btnO ]
              GE.flow GE.right [ title, wSep, btnH, spcH, btnO ]
          ]
  in

  let midSection =
    GE.size wAll hMid <|
      case model.orient of
        Vertical   -> mainSectionVertical wAll hMid model
        Horizontal -> mainSectionHorizontal wAll hMid model in

  let botSection = GE.spacer wAll hBot in
  let sideGutter = colorDebug Color.black <| GE.spacer wGut hTot in

  let basicUI =
    GE.flow GE.right
       [ sideGutter
       , GE.flow GE.down
           [ colorDebug Color.lightYellow <| topSection
           , midSection
           , colorDebug Color.lightYellow <| botSection
           ]
       , sideGutter
       ]
  in

  -- Runs a task at startup by making the whole window hoverable briefly, which
  -- fires the task to the taskMailbox basically right away (the user's mouse is
  -- presumably over the window). Note that it is important to add the event
  -- handler to a dummy object that is removed, as adding it to the whole body
  -- results in nothing being clickable after the load is successful.
  case (model.startup, model.mode) of
    (True, _) ->
      let foo _ =
        Signal.message taskMailbox.address <|
          -- Insert more tasks to run at startup here
          getLocalSaves `andThen` \_ ->

          ---
          Signal.send
            events.address
            (UpdateModel (\m -> { m | startup = False}))
      in
      GE.flow GE.inward
        [ GI.hoverable foo <| GE.spacer w h
        , basicUI
        ]
    (False, SaveDialog m) ->
      GE.flow GE.inward
        [ saveElement model w h
        , basicUI
        ]
    _ ->
      basicUI

-- TODO: add onMouseUp DeselectObject event to all GE.Elements...

------------------------------------------------------------------------------

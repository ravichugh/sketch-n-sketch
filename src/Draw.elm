module Draw exposing
  ( drawNewShape
  , drawDotSize
  , boundingBoxOfPoints_
  , addLine
  , addRawSquare , addRawRect , addStretchySquare , addStretchyRect
  , addRawCircle , addRawOval , addStretchyCircle , addStretchyOval
  , addPath , addPolygon
  , addLambda , addHelperDot
  , addTextBox
  , lambdaToolOptionsOf
  , makeTwiddleTools
  )

import Lang exposing (..)
import LangSvg
import Blobs exposing (..)
import LangUnparser exposing (unparse)
import InterfaceModel exposing (..)
import LangTools
import Utils
import Either exposing (..)
import Keys

import String
import Regex
import Html.Attributes as Attr
import Svg


--------------------------------------------------------------------------------

svgLine      = flip Svg.line []
svgRect      = flip Svg.rect []
svgCircle    = flip Svg.circle []
svgEllipse   = flip Svg.ellipse []
svgPolygon   = flip Svg.polygon []
svgPath      = flip Svg.path []


--------------------------------------------------------------------------------
-- Bounding Boxes

-- TODO change order of return values
boundingBoxOfPoints_ : List (Float, Float) -> (Float, Float, Float, Float)
boundingBoxOfPoints_ pts =
  let (xs, ys) = List.unzip pts in
  let xMax = Utils.fromJust <| List.maximum xs in
  let xMin = Utils.fromJust <| List.minimum xs in
  let yMax = Utils.fromJust <| List.maximum ys in
  let yMin = Utils.fromJust <| List.minimum ys in
  (xMin, xMax, yMin, yMax)

boundingBoxOfPoints : List (Int, Int) -> (Int, Int, Int, Int)
boundingBoxOfPoints pts =
  let pts_ = List.map (\(x,y) -> (toFloat x, toFloat y)) pts in
  let (a,b,c,d) = boundingBoxOfPoints_ pts_ in
  (round a, round b, round c, round d)


--------------------------------------------------------------------------------
-- Drawing Tools (previously in View)

drawNewShape model =
  case (model.tool, model.mouseMode) of
    (Line _,     MouseDrawNew [pt2, pt1])    -> drawNewLine model pt2 pt1
    (Rect _,     MouseDrawNew [pt2, pt1])    -> drawNewRect model.keysDown pt2 pt1
    (Oval _,     MouseDrawNew [pt2, pt1])    -> drawNewEllipse model.keysDown pt2 pt1
    (Poly _,     MouseDrawNew (ptLast::pts)) -> drawNewPolygon ptLast pts
    (Path _,     MouseDrawNew (ptLast::pts)) -> drawNewPath ptLast pts
    (HelperDot,  MouseDrawNew [pt])          -> drawNewHelperDot pt
    (HelperLine, MouseDrawNew [pt2, pt1])    -> drawNewLine model pt2 pt1
    (Lambda _,   MouseDrawNew [pt2, pt1])    -> drawNewRect model.keysDown pt2 pt1
    (Text,       MouseDrawNew [pt2, pt1])    -> drawNewRect model.keysDown pt2 pt1
    _                                        -> []

defaultOpacity        = Attr.style [("opacity", "0.5")]
defaultStroke         = LangSvg.attr "stroke" "gray"
defaultStrokeWidth    = LangSvg.attr "stroke-width" "5"
defaultFill           = LangSvg.attr "fill" "gray"
dotFill               = LangSvg.attr "fill" "red"
dotFill2              = LangSvg.attr "fill" "orange"
dotFillControlPt      = LangSvg.attr "fill" "green"
dotFillCursor         = LangSvg.attr "fill" "none"
drawDotSize           = 10
dotSize               = LangSvg.attr "r" (toString drawDotSize)
dotStroke             = LangSvg.attr "stroke" "black"
dotStrokeWidth        = LangSvg.attr "stroke-width" "2"
guideStroke           = LangSvg.attr "stroke" "aqua"

drawDot fill (cx,cy) =
  svgCircle [
      dotSize , fill , defaultOpacity , dotStroke, dotStrokeWidth
    , LangSvg.attr "cx" (toString cx)
    , LangSvg.attr "cy" (toString cy)
    ]

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

snapLine keysDown (_,(x2,y2)) (_,(x1,y1)) =
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

drawNewLine model click2 click1 =
  let ((_,(x2,y2)),(_,(x1,y1))) = (click2, click1) in
  let stroke = if model.tool == HelperLine then guideStroke else defaultStroke in
  let (xb, yb) = snapLine model.keysDown click2 click1 in
  let line =
    svgLine [
        stroke , defaultStrokeWidth , defaultOpacity
      , LangSvg.attr "x1" (toString x1) , LangSvg.attr "y1" (toString y1)
      , LangSvg.attr "x2" (toString xb) , LangSvg.attr "y2" (toString yb)
      ]
  in
  let clearDots = List.map (drawDot dotFillCursor) [(xb,yb),(x2,y2),(x1,y1)] in
  clearDots ++ [ line ]

drawNewRect keysDown (_,pt2) (_,pt1) =
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
  let clearDots = List.map (drawDot dotFillCursor) [(xb,yb),(xa,ya),pt2,pt1] in
  clearDots ++ [ rect ]

drawNewEllipse keysDown (_,pt2) (_,pt1) =
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
  let clearDots = List.map (drawDot dotFillCursor) [(xb,yb),(xa,ya),pt2,pt1] in
  clearDots ++ [ ellipse ]

drawNewPolygon (_,ptLast) keysAndPoints =
  let points = List.map Tuple.second keysAndPoints in
  let (xInit,yInit) = Utils.last_ (ptLast::points) in
  let redDot = drawDot dotFill (xInit,yInit) in
  let clearDots = List.map (drawDot dotFillCursor) (ptLast::points) in
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
   redDot :: clearDots ++ maybeShape

strPt (x,y) = Utils.spaces [toString x, toString y]

drawNewPath (keysLast,ptLast) keysAndPoints =
  let points = List.map Tuple.second keysAndPoints in
  let redDot = [drawDot dotFill (Utils.last_ (ptLast::points))] in
  let yellowDot =
    case points of
      [] -> []
      _  -> [drawDot dotFill2 ptLast]
  in
  let pathAndPoints =
    let plus (s1,l1) (s2,l2) = (s1 ++ s2, l1 ++ l2) in
    let foo list0 = case list0 of
      [] -> ("", [])
      (modifiers1,click1) :: list1 ->
        if modifiers1 == Keys.q then
          case list1 of
            [] -> ("", [click1])
            (_,click2) :: list2 ->
              let cmd = Utils.spaces [" Q", strPt click1, strPt click2] in
              plus (cmd, [click1]) (foo list2)
        else if modifiers1 == Keys.c then
          case list1 of
            [] -> ("", [click1])
            (_,click2) :: [] -> ("", [click1, click2])
            (_,click2) :: (_,click3) :: list3 ->
              let cmd = Utils.spaces [" C", strPt click1, strPt click2, strPt click3] in
              plus (cmd, [click1, click2]) (foo list3)
        else
          plus (Utils.spaces [" L", strPt click1], []) (foo list1)
    in
    case List.reverse ((keysLast,ptLast)::keysAndPoints) of
      []  -> []
      [_] -> []
      (_,firstClick) :: rest ->
        let (sPath, controlPoints) =
          plus (Utils.spaces ["M", strPt firstClick], []) (foo rest) in
        let path =
          svgPath [
              defaultStroke , defaultStrokeWidth , defaultFill , defaultOpacity
            , LangSvg.attr "d" sPath
            ] in
        let points = List.map (drawDot dotFillControlPt) controlPoints in
        path :: points
  in
  let clearDots = List.map (drawDot dotFillCursor) (ptLast::points) in
  redDot ++ yellowDot ++ clearDots ++ pathAndPoints

-- TODO this doesn't appear right away
-- (dor does initial poly, which appears only on MouseUp...)
drawNewHelperDot (x,y) =
  let dot =
    svgCircle [
        defaultFill , defaultOpacity
      , LangSvg.attr "cx" (toString 200)
      , LangSvg.attr "cy" (toString 200)
      , LangSvg.attr "r" (toString drawDotSize)
      ]
  in
  [ dot ]


--------------------------------------------------------------------------------
-- New Shapes (previously in Controller)

randomColor model = eConst0 (toFloat model.randomColor) dummyLoc

randomColor1 model = eConst (toFloat model.randomColor) dummyLoc

{-
randomColorWithSlider model =
  withDummyPos (EConst "" (toFloat model.randomColor) dummyLoc colorNumberSlider)

randomColor1WithSlider model =
  withDummyPos (EConst " " (toFloat model.randomColor) dummyLoc colorNumberSlider)
-}

--------------------------------------------------------------------------------

-- when line is snapped, not enforcing the angle in code
addLine old click2 click1 =
  let ((_,(x2,y2)),(_,(x1,y1))) = (click2, click1) in
  let (xb, yb) = snapLine old.keysDown click2 click1 in
  let color =
    if old.tool == HelperLine
      then eStr "aqua"
      else randomColor old
  in
  let (f, args) =
    maybeGhost (old.tool == HelperLine)
       (eVar0 "line")
       (List.map eVar ["color","width","x1","y1","x2","y2"])
  in
  add "line" old
    [ makeLet ["x1","y1","x2","y2"] (makeInts [x1,y1,xb,yb])
    , makeLet ["color", "width"]
              [ color , eConst 5 dummyLoc ]
    ] f args

{- using variables x1/x2/y1/y2 instead of left/top/right/bot:

  let (f, args) =
    maybeGhost (old.toolType == HelperLine)
       (eVar0 "line")
       (eStr color :: eStr "5" :: List.map eVar ["x1","y1","x2","y2"])
  in
  add "line" old
    [ makeLet ["x1","x2"] (makeInts [x1,xb])
    , makeLet ["y1","y2"] (makeInts [y1,yb])
    ] f args
-}

--------------------------------------------------------------------------------

addRawRect old (_,pt2) (_,pt1) =
  let (xa, xb, ya, yb) = boundingBox pt2 pt1 in
  let (x, y, w, h) = (xa, ya, xb - xa, yb - ya) in
  let (fill, stroke, strokeWidth) = (old.randomColor, old.randomColor, 0) in
  let (rot) = 0 in
  addShape old "rect"
    (stencilRawRect x y w h fill stroke strokeWidth rot)

stencilRawRect x y w h fill stroke strokeWidth rot =
  makeCallWithLocals False
    [ makeLet ["x","y","w","h"] (makeInts [x,y,w,h])
    , makeLet ["fill", "stroke","strokeWidth"] (makeInts [fill, stroke, strokeWidth])
    , makeLet ["rot"] [eConst (toFloat rot) dummyLoc] ]
    (eVar0 "rawRect")
    [ eVar "fill", eVar "stroke", eVar "strokeWidth"
    , eVar "x", eVar "y", eVar "w", eVar "h", eVar "rot" ]

reRawRect = -- using . instead of escaping ( ) [ ]
 """^
  .let .x y w h. .(\\d+) (\\d+) (\\d+) (\\d+).
  .let .fill stroke strokeWidth. .(\\d+) (\\d+) (\\d+).
  .let rot (\\d+)
    . .rawRect fill stroke strokeWidth x y w h rot. ....$"""

--------------------------------------------------------------------------------

addRawSquare old (_,pt2) (_,pt1) =
  let (xa, xb, ya, yb) = squareBoundingBox pt2 pt1 in
  let (x, y, side) = (xa, ya, xb - xa) in
  add "square" old
    [ makeLet ["x","y","side"] (makeInts [x,y,side])
    , makeLet ["color","rot"] [randomColor old, eConst 0 dummyLoc] ]
    (eVar0 "rawRect")
    [ eVar "color", eConst 360 dummyLoc, eConst 0 dummyLoc
    , eVar "x", eVar "y", eVar "side", eVar "side", eVar "rot" ]

--------------------------------------------------------------------------------

addStretchyRect old (_,pt2) (_,pt1) =
  let (xMin, xMax, yMin, yMax) = boundingBox pt2 pt1 in
  let (fill) = (old.randomColor) in
  let (stroke, strokeWidth, rot) = (360, 0, 0) in
  addShape old "rect"
    (stencilStretchyRect xMin yMin xMax yMax fill stroke strokeWidth rot)

stencilStretchyRect left top right bot fill stroke strokeWidth rot =
  makeCallWithLocals False
    [ makeLetAs "bounds" ["left","top","right","bot"] (makeInts [left,top,right,bot])
    , makeLet ["color"] [eConst (toFloat fill) dummyLoc] ]
    (eVar0 "rectangle")
    [eVar "color", eConst (toFloat stroke) dummyLoc, eConst (toFloat strokeWidth) dummyLoc
    , eConst (toFloat rot) dummyLoc, eVar "bounds"]

reStretchyRect =
 """^
  .let bounds @ .left top right bot. .(\\d+)[ ]+(\\d+)[ ]+(\\d+)[ ]+(\\d+).
  .let color (\\d+)
    . .rectangle color (\\d+)[ ]+(\\d+)[ ]+(\\d+)[ ]+bounds. ...$"""

--------------------------------------------------------------------------------

addStretchySquare old (_,pt2) (_,pt1) =
  let (xMin, xMax, yMin, _) = squareBoundingBox pt2 pt1 in
  let side = (xMax - xMin) in
  add "square" old
    [ makeLet ["left","top","side"] (makeInts [xMin,yMin,side])
    , makeLet ["bounds"] [eList (listOfRaw ["left","top","(+ left side)","(+ top side)"]) Nothing]
    , makeLet ["rot"] [eConst 0 dummyLoc]
    , makeLet ["color","strokeColor","strokeWidth"]
              [randomColor old, eConst 360 dummyLoc, eConst 0 dummyLoc] ]
    (eVar0 "rectangle")
    (List.map eVar ["color","strokeColor","strokeWidth","rot","bounds"])

--------------------------------------------------------------------------------

addRawOval old (_,pt2) (_,pt1) =
  let (xa, xb, ya, yb) = boundingBox pt2 pt1 in
  let (rx, ry) = ((xb-xa)//2, (yb-ya)//2) in
  let (cx, cy) = (xa + rx, ya + ry) in
  add "ellipse" old
    [ makeLet ["cx","cy","rx","ry"] (makeInts [cx,cy,rx,ry])
    , makeLet ["color","rot"] [randomColor old, eConst 0 dummyLoc] ]
    (eVar0 "rawEllipse")
    [ eVar "color", eConst 360 dummyLoc, eConst 0 dummyLoc
    , eVar "cx", eVar "cy", eVar "rx", eVar "ry", eVar "rot" ]

--------------------------------------------------------------------------------

addRawCircle old (_,pt2) (_,pt1) =
  let (xa, xb, ya, yb) = squareBoundingBox pt2 pt1 in
  let r = (xb-xa)//2 in
  let (cx, cy) = (xa + r, ya + r) in
  add "circle" old
    [ makeLet ["cx","cy","r"] (makeInts [cx,cy,r])
    , makeLet ["color"] [randomColor1 old] ]
    (eVar0 "rawCircle")
    [ eVar "color", eConst 360 dummyLoc, eConst 0 dummyLoc
    , eVar "cx", eVar "cy", eVar "r" ]

--------------------------------------------------------------------------------

addStretchyOval old (_,pt2) (_,pt1) =
  let (xa, xb, ya, yb) = boundingBox pt2 pt1 in
  add "ellipse" old
    [ makeLetAs "bounds" ["left","top","right","bot"] (makeInts [xa,ya,xb,yb])
    , makeLet ["color","strokeColor","strokeWidth"]
              [randomColor old, eConst 360 dummyLoc, eConst 0 dummyLoc] ]
    (eVar0 "oval")
    (List.map eVar ["color","strokeColor","strokeWidth","bounds"])

--------------------------------------------------------------------------------

addStretchyCircle old (_,pt2) (_,pt1) =
  let (left, right, top, _) = squareBoundingBox pt2 pt1 in
  add "circle" old
    [ makeLet ["left", "top", "r"] (makeInts [left, top, (right-left)//2])
    , makeLet ["bounds"]
        [eList [eVar0 "left", eVar "top", eRaw "(+ left (* 2! r))", eRaw "(+ top (* 2! r))"] Nothing]
    , makeLet ["color","strokeColor","strokeWidth"]
              [randomColor old, eConst 360 dummyLoc, eConst 0 dummyLoc] ]
    (eVar0 "oval")
    (List.map eVar ["color","strokeColor","strokeWidth","bounds"])

--------------------------------------------------------------------------------

addHelperDot old (_,(cx,cy)) =
  -- style matches center of attr crosshairs (View.zoneSelectPoint_)
  let r = 6 in
  let (f, args) =
    ghost (eVar0 "circle")
          (eStr "aqua" :: List.map eVar ["cx","cy","r"])
  in
  add "helperDot" old
    [ makeLet ["cx","cy","r"] (makeInts [cx,cy,r]) ]
    f args

--------------------------------------------------------------------------------

{-
maybeFreeze n =
    then toString n ++ "!"
    else toString n
-}

maybeThaw n =
  if n == 0 || n == 1
    then toString n
    else toString n ++ "?"

maybeThaw0 s = if s == "0" then s else s ++ "?"

{-
qMark n  = toString n ++ "?"
qMarkLoc = dummyLoc_ thawed
-}

addPolygon stk old points =
  case stk of
    Raw      -> addRawPolygon old points
    Stretchy -> addStretchablePolygon old points
    Sticky   -> addStickyPolygon old points

addRawPolygon old keysAndPoints =
  let points = List.map Tuple.second keysAndPoints in
  let sPts =
    Utils.bracks <| Utils.spaces <|
      flip List.map (List.reverse points) <| \(x,y) ->
        let xStr = toString x in
        let yStr = toString y in
        Utils.bracks (Utils.spaces [xStr,yStr])
  in
  add "polygon" old
    [ makeLet ["pts"] [eRaw sPts]
    , makeLet ["color","strokeColor","strokeWidth"]
              [randomColor old, eConst 360 dummyLoc, eConst 2 dummyLoc]
    ]
    (eVar0 "rawPolygon")
    [ eVar "color", eVar "strokeColor", eVar "strokeWidth"
    , eVar "pts", eConst 0 dummyLoc ]

addStretchablePolygon old keysAndPoints =
  let points = List.map Tuple.second keysAndPoints in
  let (xMin, xMax, yMin, yMax) = boundingBoxOfPoints points in
  let (width, height) = (xMax - xMin, yMax - yMin) in
  let sPcts =
    Utils.bracks <| Utils.spaces <|
      flip List.map (List.reverse points) <| \(x,y) ->
        let xPct = (toFloat x - toFloat xMin) / toFloat width in
        let yPct = (toFloat y - toFloat yMin) / toFloat height in
        let xStr = maybeThaw xPct in
        let yStr = maybeThaw yPct in
        Utils.bracks (Utils.spaces [xStr,yStr])
  in
  add "polygon" old
    [ makeLetAs "bounds" ["left","top","right","bot"] (makeInts [xMin,yMin,xMax,yMax])
    , makeLet ["color","strokeColor","strokeWidth"]
              [randomColor old, eConst 360 dummyLoc, eConst 2 dummyLoc]
    , makeLet ["pcts"] [eRaw sPcts] ]
    (eVar0 "stretchyPolygon")
    (List.map eVar ["bounds","color","strokeColor","strokeWidth","pcts"])

addStickyPolygon old keysAndPoints =
  let points = List.map Tuple.second keysAndPoints in
  let (xMin, xMax, yMin, yMax) = boundingBoxOfPoints points in
  let (width, height) = (xMax - xMin, yMax - yMin) in
  let sOffsets =
    Utils.bracks <| Utils.spaces <|
      flip List.map (List.reverse points) <| \(x,y) ->
        let
          (dxLeft, dxRight) = (x - xMin, x - xMax)
          (dyTop , dyBot  ) = (y - yMin, y - yMax)
          xOff = if dxLeft <= abs dxRight
                   then Utils.bracks (Utils.spaces ["left", maybeThaw0 (toString dxLeft)])
                   else Utils.bracks (Utils.spaces ["right", maybeThaw0 (toString dxRight)])
          yOff = if dyTop <= abs dyBot
                   then Utils.bracks (Utils.spaces ["top", maybeThaw0 (toString dyTop)])
                   else Utils.bracks (Utils.spaces ["bot", maybeThaw0 (toString dyBot)])
        in
        Utils.bracks (Utils.spaces [xOff,yOff])
  in
  add "polygon" old
    [ makeLetAs "bounds" ["left","top","right","bot"] (makeInts [xMin,yMin,xMax,yMax])
    , makeLet ["color","strokeColor","strokeWidth"]
              [randomColor old, eConst 360 dummyLoc, eConst 2 dummyLoc]
    , makeLet ["offsets"] [eRaw sOffsets] ]
    (eVar0 "stickyPolygon")
    (List.map eVar ["bounds","color","strokeColor","strokeWidth","offsets"])

strPoint strX strY (x,y) = Utils.spaces [strX x, strY y]

addPath : ShapeToolKind -> Model -> List (KeysDown, (Int, Int)) -> Model
addPath stk old keysAndPoints =
  case stk of
    Raw      -> addAbsolutePath old keysAndPoints
    Stretchy -> addStretchyPath old keysAndPoints
    Sticky   -> addStretchyPath old keysAndPoints -- TODO

pathCommands strX strY keysAndPoints =
  let strPt = strPoint strX strY in
  let keysAndPoints_ = List.reverse keysAndPoints in
  let (_,firstClick) = Utils.head_ keysAndPoints_ in
  let (_,lastClick) = Utils.last_ keysAndPoints_ in
  let (extraLets, firstCmd, lastPoint) =
    if firstClick /= lastClick
    then ([], "'M' " ++ strPt firstClick, strPt firstClick)
    else
      let extraLets =
        [ makeLet
           ["x0", "y0"]
           [ eVar0 (strX (Tuple.first firstClick)), eVar (strY (Tuple.second firstClick)) ] ]
      in
      (extraLets, "'M' x0 y0", "x0 y0")
  in
  let remainingCmds =
    let foo list0 = case list0 of
      [] -> []

      (modifiers1,click1) :: list1 ->

        if modifiers1 == Keys.q then
          case list1 of
            (modifiers2,click2) :: list2 ->
              case (click2 == firstClick, list2) of
                (True, []) -> [Utils.spaces ["'Q'", strPt click1, lastPoint]]
                (False, _) -> (Utils.spaces ["'Q'", strPt click1, strPt click2]) :: foo list2
                (True, _)  -> Debug.crash "addPath Q1"
            _ -> Debug.crash "addPath Q2"

        else if modifiers1 == Keys.c then
          case list1 of
            (modifiers2,click2) :: (modifiers3,click3) :: list3 ->
              case (click3 == firstClick, list3) of
                (True, []) -> [Utils.spaces ["'C'", strPt click1, strPt click2, lastPoint]]
                (False, _) -> (Utils.spaces ["'C'", strPt click1, strPt click2, strPt click3]) :: foo list3
                (True, _)  -> Debug.crash "addPath C1"
            _ -> Debug.crash "addPath C2"

        else
          case (click1 == firstClick, list1) of
            (True, []) -> ["'Z'"]
            (False, _) -> (Utils.spaces ["'L'", strPt click1]) :: foo list1
            (True, _)  -> Debug.crash "addPath ZL"
    in
    foo (Utils.tail_ keysAndPoints_)
  in
  let sD = Utils.bracks (Utils.spaces (firstCmd :: remainingCmds)) in
  (extraLets, sD)

addAbsolutePath old keysAndPoints =
  let (extraLets, sD) = pathCommands toString toString keysAndPoints in
  add "path" old
    ([ makeLet ["strokeColor","strokeWidth","color"]
               [randomColor old, eConst 5 dummyLoc, randomColor1 old] ]
    ++ extraLets
    ++ [makeLet ["d"] [eVar sD] ])
    (eVar0 "rawPath")
    [ eVar "color", eVar "strokeColor", eVar "strokeWidth"
    , eVar "d", eConst 0 dummyLoc ]

addStretchyPath old keysAndPoints =
  let points = List.map Tuple.second keysAndPoints in
  let (xMin, xMax, yMin, yMax) = boundingBoxOfPoints points in
  let (width, height) = (toFloat (xMax - xMin), toFloat (yMax - yMin)) in
  let strX x = maybeThaw (toFloat (x - xMin) / width) in
  let strY y = maybeThaw (toFloat (y - yMin) / height) in
  let (extraLets, sD) = pathCommands strX strY keysAndPoints in
  add "path" old
    ([ makeLetAs "bounds" ["left","top","right","bot"] (makeInts [xMin,yMin,xMax,yMax])
     , makeLet ["strokeColor","strokeWidth","color"]
               [ randomColor old, eConst 5 dummyLoc, randomColor1 old ] ]
     ++ extraLets
     ++ [ makeLet ["dPcts"] [eVar sD] ])
    (eVar0 "stretchyPath")
    (List.map eVar ["bounds","color","strokeColor","strokeWidth","dPcts"])

addStickyPath old keysAndPoints =
  Debug.crash "TODO: addStickyPath"

-- copied from ExpressionBasedTransform
eAsPoint e =
  let e_ = replacePrecedingWhitespace "" e in
  withDummyPos <|
    EColonType " " e_ " " (withDummyRange <| TNamed " " "Point") ""

{-
addLambda old (_,pt2) (_,pt1) =
  let func =
    let (selectedIdx, exps) = old.lambdaTools in
    Utils.geti selectedIdx exps
  in
-}
addLambda selectedIdx old pt2 pt1 =
  let exps = old.lambdaTools in
  case Utils.geti selectedIdx exps of
    LambdaBounds func -> addLambdaBounds old pt2 pt1 func
    LambdaAnchor func -> addLambdaAnchor old pt2 pt1 func

addLambdaBounds old (_,pt2) (_,pt1) func =
  let (xa, xb, ya, yb) =
    if old.keysDown == Keys.shift
      then squareBoundingBox pt2 pt1
      else boundingBox pt2 pt1
  in

  {- this version adds a call to main: -}

  let bounds = eList (makeInts [xa,ya,xb,yb]) Nothing in
  let args = [] in
  let eNew =
    withDummyPos (EApp "\n  " (eVar0 "withBounds") [ bounds, func ] "") in
  -- TODO refactor Program to keep (f,args) in sync with exp
  let newBlob = withBoundsBlob eNew (bounds, "XXXXX", args) in
  let (defs, mainExp) = splitExp old.inputExp in
  let mainExp_ = addToMainExp newBlob mainExp in
  let code = unparse (fuseExp (defs, mainExp_)) in

  -- upstate Run
    { old | code = code
          , mouseMode = MouseNothing }

  {- this version adds the call inside a new top-level definition:

  add funcName old
    [ makeLet ["left","top","right","bot"] (makeInts [xa,ya,xb,yb])
    , makeLet ["bounds"] [eList (listOfVars ["left","top","right","bot"]) Nothing]
    ]
    (eVar0 "with") [ eVar "bounds" , eVar funcName ]

  -}

-- For simplicity, keeping this as a click-and-drag drawing tool instead
-- of a single-click tool. Therefore, ignoring pt2 argument.
--
addLambdaAnchor old _ (_,(x,y)) func =
  let anchor = eAsPoint (eList (makeInts [x,y]) Nothing) in
  let args = [] in
  let eNew =
    withDummyPos (EApp "\n  " (eVar0 "withAnchor") [ anchor, func ] "") in
  -- TODO refactor Program to keep (f,args) in sync with exp
  let newBlob = withAnchorBlob eNew (anchor , "XXXXX", args) in
  let (defs, mainExp) = splitExp old.inputExp in
  let mainExp_ = addToMainExp newBlob mainExp in
  let code = unparse (fuseExp (defs, mainExp_)) in
  { old | code = code
        , mouseMode = MouseNothing }

addTextBox old click2 click1 =
  let (xa, xb, ya, yb) = boundingBox (Tuple.second click2) (Tuple.second click1) in
  let fontSize =
    eConst0 (toFloat (yb - ya)) dummyLoc
    -- withDummyPos (EConst "" (toFloat (yb - ya)) dummyLoc (intSlider 0 128))
  in
  add "text" old
    [ makeLet ["fontSize","textVal"] [fontSize, eStr "Text"] ]
    (eVar0 "simpleText")
    [ eStr "Tahoma, sans-serif", eStr "black", eVar "fontSize"
    , eConst (toFloat xa) dummyLoc, eConst (toFloat xb) dummyLoc
    , eConst (toFloat yb) dummyLoc
    , eConst 1.5 dummyLoc, eVar "textVal" ]

--------------------------------------------------------------------------------

addShape old newShapeKind newShapeExp =
  let shapeVarName =
    LangTools.nonCollidingName newShapeKind 1 <|
      LangTools.identifiersVisibleAtProgramEnd old.inputExp in
  let newShapeName = withDummyRange (PVar " " shapeVarName noWidgetDecl) in
  let newDef = ("\n\n", newShapeName, newShapeExp, "") in
  let (defs, mainExp) = splitExp old.inputExp in
  let defs_ = defs ++ [newDef] in
  let eNew = withDummyPos (EVar "\n  " shapeVarName) in
  let mainExp_ = addToMainExp (varBlob eNew shapeVarName) mainExp in
  let code = unparse (fuseExp (defs_, mainExp_)) in
  { old | code = code
        , genSymCount = old.genSymCount + 1
        , mouseMode = MouseNothing }

-- TODO: replace all calls to "add" to "addShapeToProgram"; remove "add"
-- TODO: remove randomColor/1 when they are no longer needed
add newShapeKind old newShapeLocals newShapeFunc newShapeArgs =
  let shapeVarName =
    LangTools.nonCollidingName newShapeKind 1 (LangTools.identifiersVisibleAtProgramEnd old.inputExp)
  in
  let newDef =
    let multi = -- check if the stencil for current tool returns List SVG or SVG
      case old.tool of
        Lambda _ -> True
        Text     -> True
        _        -> False
    in
    let newShapeName = withDummyRange (PVar " " shapeVarName noWidgetDecl) in
    let newShapeExp = makeCallWithLocals multi newShapeLocals newShapeFunc newShapeArgs in
    ("\n\n", newShapeName, newShapeExp, "")
  in
  let (defs, mainExp) = splitExp old.inputExp in
  let defs_ = defs ++ [newDef] in
  let eNew = withDummyPos (EVar "\n  " shapeVarName) in
  let mainExp_ = addToMainExp (varBlob eNew shapeVarName) mainExp in
  let code = unparse (fuseExp (defs_, mainExp_)) in

  -- upstate Run
    { old | code = code
          , genSymCount = old.genSymCount + 1
          , mouseMode = MouseNothing }

makeCallWithLocals multi locals func args =
  let recurse locals =
    case locals of
      [] ->
        if multi then
          withDummyPos (EApp "\n    " func args "")
        else
          let app = withDummyPos (EApp " " func args "") in
          withDummyPos (EList "\n    " [app] "" Nothing " ")
      (p,e)::locals_ -> withDummyPos (ELet "\n  " Let False p e (recurse locals_) "")
  in
  recurse locals

makeLet : List Ident -> List Exp -> (Pat, Exp)
makeLet vars exps =
  case (vars, exps) of
    ([x],[e])     -> (pVar x, e)
    (x::xs,e::es) -> let ps = List.map pVar xs in
                     let p = pVar0 x in
                     (pList (p::ps), eList (e::es) Nothing)
    _             -> Debug.crash "makeLet"

makeLetAs : Ident -> List Ident -> List Exp -> (Pat, Exp)
makeLetAs x vars exps =
  let (p, e) = makeLet vars exps in
  (pAs x p, e)

makeInts : List Int -> List Exp
makeInts nums =
  case nums of
    []    -> Debug.crash "makeInts"
    [n]   -> [eConst0 (toFloat n) dummyLoc]
    n::ns -> let e = eConst0 (toFloat n) dummyLoc in
             let es = List.map (\n_ -> eConst (toFloat n_) dummyLoc) ns in
             e::es

addToMainExp : BlobExp -> MainExp -> MainExp
addToMainExp newBlob mainExp =
  case mainExp of
    SvgConcat shapes f -> SvgConcat (shapes ++ [fromBlobExp newBlob]) f
    Blobs shapes f     -> Blobs (shapes ++ [newBlob]) f
    OtherExp main ->
      let ws = "\n" in -- TODO take main into account
      OtherExp <| withDummyPos <|
        EApp ws (eVar0 "addBlob") [fromBlobExp newBlob, main] ""

maybeGhost b f args =
  if b
    then (eVar0 "ghost", [ withDummyPos (EApp " " f args "") ])
    else (f, args)

ghost = maybeGhost True

{-
switchToCursorTool old =
  { old | mouseMode = MouseNothing , tool = Cursor }
-}


--------------------------------------------------------------------------------
-- Lambda Tool

lambdaToolOptionsOf : LittleProgram -> List LambdaTool
lambdaToolOptionsOf (defs, mainExp) =
  case mainExp of

    Blobs blobs _ ->
      let lambdaPreFuncs =
        -- will be easier with better TopDefs
        List.concatMap (\(_,p,e,_) ->
          case (p.val, e.val.e__) of
            (PVar _ f _, EFun _ params _ _) ->
              case List.reverse params of
                lastParam :: _ ->
                  case varsOfPat lastParam of
                    ["bounds"]                            -> [Left f]
                    ["left","top","right","bot"]          -> [Left f]
                    ["bounds","left","top","right","bot"] -> [Left f]
                    ["anchor"]                            -> [Right f]
                    ["xAnchor","yAnchor"]                 -> [Right f]
                    ["anchor","xAnchor","yAnchor"]        -> [Right f]
                    _                                     -> []
                [] -> []
            _ -> []
          ) defs
      in
      let withBlobs =
        List.reverse <| -- reverse so that most recent call wins
          List.concatMap (\blob ->
            case blob of
              NiceBlob _ (WithBoundsBlob (_, f, args)) -> [Left (f,args)]
              NiceBlob _ (WithAnchorBlob (_, f, args)) -> [Right (f,args)]
              _                                        -> []
            ) blobs
      in
      let lambdaCalls =
        List.concatMap (\preFunc ->
          let pred withBlob =
            case (preFunc, withBlob) of
              (Left f, Left (g, _))   -> f == g
              (Right f, Right (g, _)) -> f == g
              _                       -> False
          in
          case Utils.findFirst pred withBlobs of
            Nothing               -> []
            Just (Left (f,args))  -> [LambdaBounds <| withDummyPos (EApp " " (eVar0 f) args "")]
            Just (Right (f,args)) -> [LambdaAnchor <| withDummyPos (EApp " " (eVar0 f) args "")]
          ) lambdaPreFuncs
      in
      lambdaCalls

    _ -> []


--------------------------------------------------------------------------------
-- Syntactic Twiddling

matchOne str (strRegex, f) =
  case Regex.find (Regex.AtMost 1) (Regex.regex strRegex) str of
    [match] ->
      case Utils.projJusts match.submatches of
        Nothing -> []
        Just xs -> f xs
    _ ->
      []

makeTwiddleTools m eId eShape =

  let strShape = unparse eShape in

  let evaluateRulesUntilMatch rules =
    case rules of
      [] -> []
      rule :: rest ->
        case matchOne strShape rule of
          []    -> evaluateRulesUntilMatch rest
          tools -> tools
  in

  let rewriteAndReturn newShape =
    let newExp =
      replaceExpNodePreservingPreceedingWhitespace eId newShape m.inputExp
    in
    let result =
      { description = "XXX", exp = newExp, sortKey = [], children = Nothing }
    in
    ([SynthesisResult result], [])
  in

  let rewriteRectRawToStretchy args =
    if List.length args /= 8 then []
    else
      let (x, y, w, h, fill, stroke, strokeWidth, rot) =
        Utils.unwrap8 (List.map Utils.parseInt args)
      in
      [ ("Rewrite Stretchy", \() ->
          let newShape =
            stencilStretchyRect x y (x + w) (y + h) fill stroke strokeWidth rot in
          rewriteAndReturn newShape
        ) ]
  in

  let rewriteRectStretchyToRaw args =
    if List.length args /= 8 then []
    else
      let (left, top, right, bot, fill, stroke, strokeWidth, rot) =
        Utils.unwrap8 (List.map Utils.parseInt args)
      in
      [ ("Rewrite Raw", \() ->
          let newShape =
            stencilRawRect left top (right - left) (bot - top) fill stroke strokeWidth rot in
          rewriteAndReturn newShape
        ) ]
  in

  evaluateRulesUntilMatch <|
    [ (reStretchyRect, rewriteRectStretchyToRaw)
    , (reRawRect, rewriteRectRawToStretchy)
    ]

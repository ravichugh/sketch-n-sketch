module Draw exposing
  ( pointZoneStyles
  , colorPointSelected, colorPointNotSelected, colorLineSelected, colorLineNotSelected, colorInput, colorOutput, colorInputAndOutput
  , drawDotSize
  -- , drawNewLine
  -- , drawNewRect
  -- , drawNewEllipse
  , drawNewPolygon
  -- , drawNewPath
  , drawNewFunction
  , svgXYDot
  -- , newFunctionCallExp
  , boundingBoxOfPoints_
  -- , addLine
  -- , addRawSquare , addRawRect , addStretchySquare , addStretchyRect
  -- , addRawCircle , addRawOval , addStretchyCircle , addStretchyOval
  , addPath , addPolygon
  , addRawPolygonList
  -- , addLambda
  , addFunction
  , addPoint , addOffsetAndMaybePoint , horizontalVerticalSnap
  -- , addTextBox
  , getDrawableFunctions
  )

import CodeMotion
import DrawAddShape
import Lang exposing (..)
import LangSvg
import Types
import Blobs exposing (..)
import InterfaceModel exposing (..)
import FastParser
import LangUnparser
import LangTools
import TypeDirectedFunctionUtils
import StaticAnalysis
import Provenance
import FocusedEditingContext
import Utils
import Either exposing (..)
import Keys
import Eval -- used to determine bounding box of LambdaAnchor tools
            -- for the purposes of rendering icons in drawing toolbox
            -- Also for drawing function tools
import Config
import Syntax exposing (Syntax)

import String
import Regex
import Html.Attributes as Attr
import Dict
import Set
import Svg


--------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugController

--------------------------------------------------------------------------------

svgLine      = flip Svg.line []
svgRect      = flip Svg.rect []
svgCircle    = flip Svg.circle []
svgEllipse   = flip Svg.ellipse []
svgPolygon   = flip Svg.polygon []
svgPath      = flip Svg.path []

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

-- http://www.colorpicker.com/

colorPointSelected      = "#38F552" -- bright green
colorPointNotSelected   = "#F5B038" -- "orange"
colorLineSelected       = "#B4FADB" -- "blue"
colorLineNotSelected    = "#FAB4D3" -- "red"

colorInput              = "#FFA340"
colorOutput             = "#91C5FF"
colorInputAndOutput     = "#C8B4A0" -- https://meyerweb.com/eric/tools/color-blend/#FFA340:91C5FF:1:hex

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
boundingBox (x1,y1) (x2,y2) =
  (min x1 x2, max x1 x2, min y1 y2, max y1 y2)

squareBoundingBox : (Int, Int) -> (Int, Int) -> (Int, Int, Int, Int)
squareBoundingBox (x1,y1) (x2,y2) =
  let (xDiff, yDiff) = (abs (x2 - x1), abs (y2 - y1)) in
  case (yDiff > xDiff, x1 < x2, y1 < y2) of
    (True,  True , _    ) -> (x1, x1 + yDiff, min y1 y2, max y1 y2)
    (True,  False, _    ) -> (x1 - yDiff, x1, min y1 y2, max y1 y2)
    (False, _    , True ) -> (min x1 x2, max x1 x2, y1, y1 + xDiff)
    (False, _    , False) -> (min x1 x2, max x1 x2, y1 - xDiff, y1)

slicesPerQuadrant = 2 -- can toggle this parameter
radiansPerSlice   = pi / (2 * slicesPerQuadrant)

snapLine keysDown ((x1, _), (y1, _)) ((x2, _), (y2, _)) =
  if keysDown == [Keys.keyShift] then
    let (dx, dy) = (x2 - x1, y2 - y1) in
    let angle = atan2 (toFloat (-dy)) (toFloat dx) in
    let slice = round (angle / radiansPerSlice) in
    let r = Utils.distanceInt (x2,y2) (x1,y1) in
    let xb = toFloat x1 + r * cos (toFloat slice * radiansPerSlice) in
    let yb = toFloat y1 - r * sin (toFloat slice * radiansPerSlice) in
    (round xb, round yb)
  else
    (x2, y2)

-- drawNewLine model click2 click1 =
--   let ((_,(x2,y2)),(_,(x1,y1))) = (click2, click1) in
--   let stroke = if model.tool == HelperLine then guideStroke else defaultStroke in
--   let (xb, yb) = snapLine model.keysDown click2 click1 in
--   let line =
--     svgLine [
--         stroke , defaultStrokeWidth , defaultOpacity
--       , LangSvg.attr "x1" (toString x1) , LangSvg.attr "y1" (toString y1)
--       , LangSvg.attr "x2" (toString xb) , LangSvg.attr "y2" (toString yb)
--       ]
--   in
--   let clearDots = List.map (drawDot dotFillCursor) [(xb,yb),(x2,y2),(x1,y1)] in
--   clearDots ++ [ line ]
--
-- drawNewRect keysDown (_,pt2) (_,pt1) =
--   let (xa, xb, ya, yb) =
--     if keysDown == [Keys.keyShift]
--     then squareBoundingBox pt1 pt2
--     else boundingBox pt1 pt2
--   in
--   let rect =
--     svgRect [
--         defaultFill , defaultOpacity
--       , LangSvg.attr "x" (toString xa) , LangSvg.attr "width" (toString (xb-xa))
--       , LangSvg.attr "y" (toString ya) , LangSvg.attr "height" (toString (yb-ya))
--       ]
--   in
--   let clearDots = List.map (drawDot dotFillCursor) [(xb,yb),(xa,ya),pt2,pt1] in
--   clearDots ++ [ rect ]
--
-- drawNewEllipse keysDown (_,pt2) (_,pt1) =
--   let (xa, xb, ya, yb) =
--     if keysDown == [Keys.keyShift]
--     then squareBoundingBox pt1 pt2
--     else boundingBox pt1 pt2
--   in
--   let (rx, ry) = ((xb-xa)//2, (yb-ya)//2) in
--   let ellipse =
--     svgEllipse [
--         defaultFill , defaultOpacity
--       , LangSvg.attr "cx" (toString (xa + rx))
--       , LangSvg.attr "cy" (toString (ya + ry))
--       , LangSvg.attr "rx" (toString rx)
--       , LangSvg.attr "ry" (toString ry)
--       ]
--   in
--   let clearDots = List.map (drawDot dotFillCursor) [(xb,yb),(xa,ya),pt2,pt1] in
--   clearDots ++ [ ellipse ]

drawNewPolygon : PointWithSnap -> List PointWithSnap -> List (Svg.Svg Msg)
drawNewPolygon ptLast points =
  let allRawPoints =
    ptLast::points |> List.map (\((x, _), (y, _)) -> (x, y))
  in
  let (xInit,yInit) = Utils.last_ allRawPoints in
  let redDot = drawDot dotFill (xInit,yInit) in
  let clearDots = List.map (drawDot dotFillCursor) allRawPoints in
  let maybeShape =
    case allRawPoints of
      [_] -> []
      _ ->
        -- don't need to reverse, but keeping it same as resulting shape
        let polyPoints = List.reverse allRawPoints in
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

-- drawNewPath (keysLast,ptLast) keysAndPoints =
--   let points = List.map Tuple.second keysAndPoints in
--   let redDot = [drawDot dotFill (Utils.last_ (ptLast::points))] in
--   let yellowDot =
--     case points of
--       [] -> []
--       _  -> [drawDot dotFill2 ptLast]
--   in
--   let pathAndPoints =
--     let plus (s1,l1) (s2,l2) = (s1 ++ s2, l1 ++ l2) in
--     let foo list0 = case list0 of
--       [] -> ("", [])
--       (modifiers1,click1) :: list1 ->
--         if modifiers1 == Keys.q then
--           case list1 of
--             [] -> ("", [click1])
--             (_,click2) :: list2 ->
--               let cmd = Utils.spaces [" Q", strPt click1, strPt click2] in
--               plus (cmd, [click1]) (foo list2)
--         else if modifiers1 == Keys.c then
--           case list1 of
--             [] -> ("", [click1])
--             (_,click2) :: [] -> ("", [click1, click2])
--             (_,click2) :: (_,click3) :: list3 ->
--               let cmd = Utils.spaces [" C", strPt click1, strPt click2, strPt click3] in
--               plus (cmd, [click1, click2]) (foo list3)
--         else
--           plus (Utils.spaces [" L", strPt click1], []) (foo list1)
--     in
--     case List.reverse ((keysLast,ptLast)::keysAndPoints) of
--       []  -> []
--       [_] -> []
--       (_,firstClick) :: rest ->
--         let (sPath, controlPoints) =
--           plus (Utils.spaces ["M", strPt firstClick], []) (foo rest) in
--         let path =
--           svgPath [
--               defaultStroke , defaultStrokeWidth , defaultFill , defaultOpacity
--             , LangSvg.attr "d" sPath
--             ] in
--         let points = List.map (drawDot dotFillControlPt) controlPoints in
--         path :: points
--   in
--   let clearDots = List.map (drawDot dotFillCursor) (ptLast::points) in
--   redDot ++ yellowDot ++ clearDots ++ pathAndPoints


drawNewFunction fName model pt1 pt2 =
  let inputPtDots =
    let ((x1, x1Snap), (y1, y1Snap)) = pt1 in
    let ((x2, x2Snap), (y2, y2Snap)) = pt2 in
    let dot1 = if x1Snap == NoSnap || y1Snap == NoSnap then [svgXYDot (x1, y1) pointZoneStyles.fill.shown True [ LangSvg.attr "opacity" "0.4" ]] else [] in
    let dot2 = if x2Snap == NoSnap || y2Snap == NoSnap then [svgXYDot (x2, y2) pointZoneStyles.fill.shown True [ LangSvg.attr "opacity" "0.4" ]] else [] in
    dot1 ++ dot2
  in
  newFunctionCallExp fName model pt1 pt2
  |> Maybe.andThen
    (\(callExp, returnType) ->
      let pseudoProgram =
        model.inputExp
        |> replaceExpNode
            (LangTools.lastSameLevelExp model.inputExp).val.eid
            callExp
            -- (eApp funcExp (LangTools.expToAppArgs (expEffectiveExp callExp)))
        -- |> (\program -> let _ = Utils.log <| Syntax.unparser Syntax.Elm program in program)
      in
      if Types.isPointType returnType || Types.isPointListType returnType then
        let maybePoints =
          Eval.doEval Syntax.Elm Eval.initEnv pseudoProgram
          |> Utils.perhapsLogError "drawNewFunction error"
          |> Result.toMaybe
          -- Handle single point or list of points
          |> Maybe.andThen (\((val, _), _, _) -> (valToMaybePoint val |> Maybe.map List.singleton) |> Utils.orMaybe (vListToMaybeVals val |> Maybe.andThen (List.map valToMaybePoint >> Utils.projJusts)) )
        in
        maybePoints
        |> Maybe.map (List.map (\(x,y) -> svgXYDot (x, y) pointZoneStyles.fill.shown True []))
      else
        LangSvg.evalToSvg Syntax.Elm Eval.initEnv pseudoProgram
        |> Utils.perhapsLogError "drawNewFunction error"
        |> Result.toMaybe
        |> Maybe.map List.singleton
    )
  |> Maybe.withDefault []
  |> (flip (++) inputPtDots)


svgXYDot (x, y) fill isVisible extraAttrs =
  -- let
  --   x = toFloat x_ - model.outputCanvasInfo.scrollLeft
  --   y = toFloat y_ - model.outputCanvasInfo.scrollTop
  -- in
  svgCircle <|
    [ LangSvg.attr "cx" (toString x) , LangSvg.attr "cy" (toString y)
    , LangSvg.attr "fill" fill
    , LangSvg.attr "stroke" pointZoneStyles.stroke
    , LangSvg.attr "stroke-width" pointZoneStyles.strokeWidth
    , LangSvg.attr "r" <|
        if isVisible
        then pointZoneStyles.radius
        else "0"
    ] ++ extraAttrs


--------------------------------------------------------------------------------
-- New Shapes (previously in Controller)

randomColor model = eConst0 (toFloat model.randomColor) dummyLoc

randomColor1 model = eConst (toFloat model.randomColor) dummyLoc

{-
randomColorWithSlider model =
  withDummyExpInfo (EConst "" (toFloat model.randomColor) dummyLoc colorNumberSlider)

randomColor1WithSlider model =
  withDummyExpInfo (EConst " " (toFloat model.randomColor) dummyLoc colorNumberSlider)
-}

--------------------------------------------------------------------------------

-- -- when line is snapped, not enforcing the angle in code
-- addLine : Model -> PointWithSnap -> PointWithSnap -> Model
-- addLine old pt1 pt2 =
--   let ((x1Exp, y1Exp), (x2Exp, y2Exp)) =
--     case pt2 of
--       ((_, NoSnap), (_, NoSnap)) -> (makeIntPairOrSnap pt1, makeIntPair (snapLine old.keysDown pt1 pt2))
--       _                          -> (makeIntPairOrSnap pt1, makeIntPairOrSnap pt2)
--   in
--   let color =
--     if old.tool == HelperLine
--       then eStr "aqua"
--       else randomColor old
--   in
--   let (f, args) =
--     maybeGhost (old.tool == HelperLine)
--        (eVar0 "line")
--        (List.map eVar ["color","width","x1","y1","x2","y2"])
--   in
--   let lineExp =
--     makeCallWithLocals
--         [ makeLet ["x1","y1","x2","y2"] [removePrecedingWhitespace x1Exp, y1Exp, x2Exp, y2Exp]
--         , makeLet ["color", "width"]
--                   [ color , eConst 5 dummyLoc ]
--         ]
--         f
--         args
--   in
--   addShapeToModel old "line" lineExp


{- using variables x1/x2/y1/y2 instead of left/top/right/bot:

  let (f, args) =
    maybeGhost (old.toolType == HelperLine)
       (eVar0 "line")
       (eStr color :: eStr "5" :: List.map eVar ["x1","y1","x2","y2"])
  in
  add old "line" old
    [ makeLet ["x1","x2"] (makeInts [x1,xb])
    , makeLet ["y1","y2"] (makeInts [y1,yb])
    ] f args
-}

--------------------------------------------------------------------------------

-- addRawRect : Model -> PointWithSnap -> PointWithSnap -> Model
-- addRawRect old (_,pt2) (_,pt1) =
--   let (xa, xb, ya, yb) = boundingBox pt1 pt2 in
--   let (x, y, w, h) = (xa, ya, xb - xa, yb - ya) in
--   let (fill, stroke, strokeWidth) = (old.randomColor, old.randomColor, 0) in
--   let (rot) = 0 in
--   addShapeToModel old "rect"
--     (stencilRawRect x y w h fill stroke strokeWidth rot)
--
-- stencilRawRect : Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp
-- stencilRawRect x y w h fill stroke strokeWidth rot =
--   makeCallWithLocals
--     [ makeLet ["x","y","w","h"] (makeInts [x,y,w,h])
--     , makeLet ["fill", "stroke","strokeWidth"] (makeInts [fill, stroke, strokeWidth])
--     , makeLet ["rot"] [eConst (toFloat rot) dummyLoc] ]
--     (eVar0 "rawRect")
--     [ eVar "fill", eVar "stroke", eVar "strokeWidth"
--     , eVar "x", eVar "y", eVar "w", eVar "h", eVar "rot" ]
--
-- --------------------------------------------------------------------------------
--
-- addRawSquare : Model -> PointWithSnap -> PointWithSnap -> Model
-- addRawSquare old (_,pt2) (_,pt1) =
--   let (xa, xb, ya, yb) = squareBoundingBox pt1 pt2 in
--   let (x, y, side) = (xa, ya, xb - xa) in
--   let squareExp =
--     makeCallWithLocals
--         [ makeLet ["x","y","side"] (makeInts [x,y,side])
--         , makeLet ["color","rot"] [randomColor old, eConst 0 dummyLoc] ]
--         (eVar0 "rawRect")
--         [ eVar "color", eConst 360 dummyLoc, eConst 0 dummyLoc
--         , eVar "x", eVar "y", eVar "side", eVar "side", eVar "rot" ]
--   in
--   addShapeToModel old "square" squareExp
--
-- --------------------------------------------------------------------------------
--
-- addStretchyRect : Model -> PointWithSnap -> PointWithSnap -> Model
-- addStretchyRect old (_,pt2) (_,pt1) =
--   let (xMin, xMax, yMin, yMax) = boundingBox pt1 pt2 in
--   let (fill) = (old.randomColor) in
--   let (stroke, strokeWidth, rot) = (360, 0, 0) in
--   addShapeToModel old "rect"
--     (stencilStretchyRect xMin yMin xMax yMax fill stroke strokeWidth rot)
--
-- stencilStretchyRect : Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp -> Exp
-- stencilStretchyRect left top right bot fill stroke strokeWidth rot =
--   makeCallWithLocals
--     [ makeLetAs "bounds" ["left","top","right","bot"] (makeInts [left,top,right,bot])
--     , makeLet ["color"] [eConst (toFloat fill) dummyLoc] ]
--     (eVar0 "rectangle")
--     [eVar "color", eConst (toFloat stroke) dummyLoc, eConst (toFloat strokeWidth) dummyLoc
--     , eConst (toFloat rot) dummyLoc, eVar "bounds"]
--
-- --------------------------------------------------------------------------------
--
-- addStretchySquare : Model -> PointWithSnap -> PointWithSnap -> Model
-- addStretchySquare old (_,pt2) (_,pt1) =
--   let (xMin, xMax, yMin, _) = squareBoundingBox pt1 pt2 in
--   let side = (xMax - xMin) in
--   let squareExp =
--     makeCallWithLocals
--         [ makeLet ["left","top","side"] (makeInts [xMin,yMin,side])
--         , makeLet ["bounds"] [eList (listOfRaw ["left","top","(+ left side)","(+ top side)"]) Nothing]
--         , makeLet ["rot"] [eConst 0 dummyLoc]
--         , makeLet ["color","strokeColor","strokeWidth"]
--                   [randomColor old, eConst 360 dummyLoc, eConst 0 dummyLoc] ]
--         (eVar0 "rectangle")
--         (List.map eVar ["color","strokeColor","strokeWidth","rot","bounds"])
--   in
--   addShapeToModel old "square" squareExp
--
-- --------------------------------------------------------------------------------
--
-- addRawOval : Model -> PointWithSnap -> PointWithSnap -> Model
-- addRawOval old (_,pt2) (_,pt1) =
--   let (xa, xb, ya, yb) = boundingBox pt1 pt2 in
--   let (rx, ry) = ((xb-xa)//2, (yb-ya)//2) in
--   let (cx, cy) = (xa + rx, ya + ry) in
--   let ellipseExp =
--     makeCallWithLocals
--         [ makeLet ["cx","cy","rx","ry"] (makeInts [cx,cy,rx,ry])
--         , makeLet ["color","rot"] [randomColor old, eConst 0 dummyLoc] ]
--         (eVar0 "rawEllipse")
--         [ eVar "color", eConst 360 dummyLoc, eConst 0 dummyLoc
--         , eVar "cx", eVar "cy", eVar "rx", eVar "ry", eVar "rot" ]
--   in
--   addShapeToModel old "ellipse" ellipseExp
--
-- --------------------------------------------------------------------------------
--
-- addRawCircle : Model -> PointWithSnap -> PointWithSnap -> Model
-- addRawCircle old (_,pt2) (_,pt1) =
--   let (xa, xb, ya, yb) = squareBoundingBox pt1 pt2 in
--   let r = (xb-xa)//2 in
--   let (cx, cy) = (xa + r, ya + r) in
--   let circleExp =
--     makeCallWithLocals
--         [ makeLet ["cx","cy","r"] (makeInts [cx,cy,r])
--         , makeLet ["color"] [randomColor1 old] ]
--         (eVar0 "rawCircle")
--         [ eVar "color", eConst 360 dummyLoc, eConst 0 dummyLoc
--         , eVar "cx", eVar "cy", eVar "r" ]
--   in
--   addShapeToModel old "circle" circleExp
--
-- --------------------------------------------------------------------------------
--
-- addStretchyOval : Model -> PointWithSnap -> PointWithSnap -> Model
-- addStretchyOval old (_,pt2) (_,pt1) =
--   let (xa, xb, ya, yb) = boundingBox pt1 pt2 in
--   let ellipseExp =
--     makeCallWithLocals
--         [ makeLetAs "bounds" ["left","top","right","bot"] (makeInts [xa,ya,xb,yb])
--         , makeLet ["color","strokeColor","strokeWidth"]
--                   [randomColor old, eConst 360 dummyLoc, eConst 0 dummyLoc] ]
--         (eVar0 "oval")
--         (List.map eVar ["color","strokeColor","strokeWidth","bounds"])
--   in
--   addShapeToModel old "ellipse" ellipseExp
--
-- --------------------------------------------------------------------------------
--
-- addStretchyCircle : Model -> PointWithSnap -> PointWithSnap -> Model
-- addStretchyCircle old (_,pt2) (_,pt1) =
--   let (left, right, top, _) = squareBoundingBox pt1 pt2 in
--   let circleExp =
--     makeCallWithLocals
--         [ makeLet ["left", "top", "r"] (makeInts [left, top, (right-left)//2])
--         , makeLet ["bounds"]
--             [eList [eVar0 "left", eVar "top", eRaw "(+ left (* 2! r))", eRaw "(+ top (* 2! r))"] Nothing]
--         , makeLet ["color","strokeColor","strokeWidth"]
--                   [randomColor old, eConst 360 dummyLoc, eConst 0 dummyLoc] ]
--         (eVar0 "oval")
--         (List.map eVar ["color","strokeColor","strokeWidth","bounds"])
--   in
--   addShapeToModel old "circle" circleExp

--------------------------------------------------------------------------------

addPoint : Model -> (Int, Int) -> Model
addPoint old (x, y) =
  -- style matches center of attr crosshairs (View.zoneSelectPoint_)
  let originalProgram = old.inputExp in
  let contextExp = FocusedEditingContext.drawingContextExp old.editingContext originalProgram in
  case LangTools.nonCollidingNames ["point", "x", "y"] 2 <| LangTools.visibleIdentifiersAtEIds originalProgram (Set.singleton (LangTools.lastExp contextExp).val.eid) of
    [pointName, xName, yName] ->
      let
        programWithPoint =
          LangTools.newLetAfterComments contextExp.val.eid (pAs pointName (pList [pVar0 xName, pVar yName])) (identity (eTuple [eInt0 x, eInt y])) originalProgram -- eAsPoint
      in
      { old | code = Syntax.unparser old.syntax programWithPoint }

    _ -> Debug.crash "unsatisfied list length invariant in LangTools.nonCollidingNames or bug in Draw.addPoint"


horizontalVerticalSnap : (Int, Int) -> (Int, Int) -> (Axis, Sign, Int)
horizontalVerticalSnap (x1, y1) (x2, y2) =
  if abs (x2 - x1) >= abs (y2 - y1) then
    if x2 - x1 > 0
    then (X, Positive, x2 - x1)
    else (X, Negative, x1 - x2)
  else
    if y2 - y1 > 0
    then (Y, Positive, y2 - y1)
    else (Y, Negative, y1 - y2)


-- Call this after drawing into a function to perhaps set up a recursive skeleton.
perhapsPrepareRecursiveFunction : EId -> Exp -> Exp
perhapsPrepareRecursiveFunction someEIdAtTopLevelOfFunction program =
  let
    funcBody     = LangTools.outerSameValueExp program (LangTools.justFindExpByEId program someEIdAtTopLevelOfFunction)
    maybeFuncEId = parentByEId program funcBody.val.eid |> Maybe.withDefault Nothing |> Maybe.map (.val >> .eid)
  in
  case maybeFuncEId of
    Just funcEId ->
      case LangTools.findLetAndIdentBindingExpLoose funcEId program of
        Just (letExp, funcName) ->
          if LangTools.expToLetRec letExp then
            case (expEffectiveExp funcBody).val.e__ of
              EIf _ _ _ _ _ _ _ -> program
              ECase _ _ _ _     -> program
              ETypeCase _ _ _ _ -> program
              _                 ->
                if Set.member funcName (LangTools.freeIdentifiers funcBody) then
                  let
                    freshenedProgram = FastParser.freshen program -- Ensure everything has an EId. EId of func should not change b/c it shouldn't have been duplicated.
                    dependencies  = StaticAnalysis.grossDependencies freshenedProgram
                    freshFuncBody = LangTools.justFindExpByEId freshenedProgram funcBody.val.eid
                    recursiveVars = LangTools.freeVars freshFuncBody |> List.filter (LangTools.expToIdent >> (==) funcName)

                    -- From after a certain cutoff, put all remaining lets into the recursive case.
                    findExpToWrap e =
                      case (LangTools.expToMaybeLetBoundExp e, LangTools.maybeSameLevelChild e) of
                        (Just boundExp, Just body) ->
                          if [] /= Utils.intersectAsSet (flattenExpTree boundExp) recursiveVars
                          then e
                          else findExpToWrap body
                        (_, Just body) -> findExpToWrap body
                        (_, Nothing)   -> e

                    expToWrap = findExpToWrap freshFuncBody

                    -- If original return expression not dependent on anything in only the recursive case, put it in a variable outside the recursive case.
                    originalRetExp = freshFuncBody |> LangTools.lastSameLevelExp
                    retExpDependsOnAnythingInRecursiveCase =
                      Utils.anyOverlapListSet
                        (Utils.diffAsSet (allEIds expToWrap) (allEIds originalRetExp))
                        (StaticAnalysis.eidDependencies dependencies originalRetExp.val.eid)


                    indentationAtLet = indentationAt letExp.val.eid program
                    newFuncBody =
                      unindent freshFuncBody
                      |> mapExpNode
                          expToWrap.val.eid
                          (\expToWrap ->
                            if retExpDependsOnAnythingInRecursiveCase then
                              -- newLetFancyWhitespace insertedLetEId isRec newPat newBoundExp expToWrap programToModify
                              eIf
                                  (eHoleNamed "terminationCondition")
                                  (eTuple [] |> replacePrecedingWhitespace "\n  ")
                                  (expToWrap |> replaceIndentation "  " |> replacePrecedingWhitespace "\n  ")
                              |> replacePrecedingWhitespace "\n"
                            else
                              let
                                suggestedName = LangTools.expNameForEId freshFuncBody originalRetExp.val.eid
                                nameForRetExp = LangTools.nonCollidingName suggestedName 2 <| LangTools.visibleIdentifiersAtEIds program (Set.singleton (LangTools.lastExp funcBody).val.eid)
                              in
                              eLet [(nameForRetExp, LangTools.reflowBoundExpWhitespace originalRetExp)]
                                (
                                  eIf
                                      (eHoleNamed "terminationCondition")
                                      (eVar nameForRetExp |> replacePrecedingWhitespace "\n  ")
                                      (expToWrap |> replaceIndentation "  " |> replaceExpNodePreservingPrecedingWhitespace originalRetExp.val.eid (eVar nameForRetExp) |> replacePrecedingWhitespace "\n  ")
                                  |> replacePrecedingWhitespace "\n"
                                )
                          )
                      |> indent (indentationAtLet ++ "  ")

                  in
                  program
                  |> replaceExpNode funcBody.val.eid newFuncBody
                else
                  program
          else
            program

        Nothing ->
          program

    Nothing ->
      program


-- Bug not worth fixing right now:
-- If the function is recursive, only ever adds to the recursive branch (even if the base case is focused).
addToEndOfDrawingContext : Model -> Ident -> Exp -> Model
addToEndOfDrawingContext old varSuggestedName exp =
  let originalProgram = old.inputExp in
  let (contextExp, endOfDrawingContextExp) = FocusedEditingContext.contextExpAndEndOfDrawingContextExp old.editingContext originalProgram in
  let varName = LangTools.nonCollidingName varSuggestedName 2 <| LangTools.visibleIdentifiersAtEIds originalProgram (Set.singleton (LangTools.lastExp contextExp).val.eid) in
  let newProgram =
    originalProgram
    |> LangTools.newLetAfterComments endOfDrawingContextExp.val.eid (pVar varName) exp
    |> perhapsPrepareRecursiveFunction endOfDrawingContextExp.val.eid
    |> CodeMotion.resolveValueAndLocHoles old.solutionsCache old.syncOptions old.maybeEnv
    |> List.head
    |> Maybe.withDefault originalProgram
  in
  { old | code = Syntax.unparser old.syntax newProgram }


addOffsetAndMaybePoint : Model -> PointWithSnap -> Snap -> (Int, Int) -> Model
addOffsetAndMaybePoint old pt1 amountSnap (x2Int, y2Int) =
  -- style matches center of attr crosshairs (View.zoneSelectPoint_)
  let originalProgram = old.inputExp in
  let ((x1Int, _), (y1Int, _)) = pt1 in
  let (axis, sign, offsetAmount) = horizontalVerticalSnap (x1Int, y1Int) (x2Int, y2Int) in
  if offsetAmount <= 1 && amountSnap == NoSnap then
    addPoint old (x1Int, y1Int)
  else
    let plusOrMinus = if sign == Positive then Plus else Minus in
    let offsetAmountExp =
      case amountSnap of
        NoSnap          -> eInt offsetAmount
        SnapVal snapVal -> eHoleVal snapVal
    in
    let offsetFromExisting baseVal =
      let offsetSuggestedName = Provenance.nameForVal originalProgram baseVal ++ "Offset" in
      let offsetExp = eOp plusOrMinus [eHoleVal baseVal, offsetAmountExp] in
      addToEndOfDrawingContext old offsetSuggestedName offsetExp
    in
    case (axis, pt1) of
      (X, ((_, SnapVal x1Val), _)) -> offsetFromExisting x1Val
      (Y, (_, (_, SnapVal y1Val))) -> offsetFromExisting y1Val
      _                            ->
        let (contextExp, endOfDrawingContextExp) = FocusedEditingContext.contextExpAndEndOfDrawingContextExp old.editingContext originalProgram in
        case LangTools.nonCollidingNames ["point", "x", "y", "x{n}Offset", "y{n}Offset"] 1 <| LangTools.visibleIdentifiersAtEIds originalProgram (Set.singleton (LangTools.lastExp endOfDrawingContextExp).val.eid) of
          [pointName, xName, yName, offsetXName, offsetYName] ->
            let
              (offsetName, offsetFromName) = if axis == X then (offsetXName, xName) else (offsetYName, yName)
              -- Put point at beginning of context...
              programWithPoint =
                LangTools.newLetAfterComments contextExp.val.eid (pAs pointName (pList [pVar0 xName, pVar yName])) (identity (eTuple [eInt0 x1Int, eInt y1Int])) originalProgram |> FastParser.freshen -- eAsPoint
              -- ...and offset at the end.
              programWithOffsetAndPoint =
                LangTools.newLetAfterComments endOfDrawingContextExp.val.eid (pVar offsetName) (eOp plusOrMinus [eVar offsetFromName, offsetAmountExp]) programWithPoint
                |> CodeMotion.resolveValueAndLocHoles old.solutionsCache old.syncOptions old.maybeEnv
                |> List.head
                |> Maybe.withDefault originalProgram
            in
            { old | code = Syntax.unparser old.syntax programWithOffsetAndPoint }

          _ -> Debug.crash "unsatisfied list length invariant in LangTools.nonCollidingNames or bug in Draw.addOffsetAndMaybePoint"


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

pointWithSnapToXYExps ((x, xSnap), (y, ySnap)) =
  let xExp =
    case xSnap of
      NoSnap          -> eConstDummyLoc0 (toFloat x)
      SnapVal snapVal -> eHoleVal0 snapVal
  in
  let yExp =
    case ySnap of
      NoSnap          -> eConstDummyLoc (toFloat y)
      SnapVal snapVal -> eHoleVal snapVal
  in
  (xExp, yExp)

pointWithSnapToPairExp pointWithSnap =
  let (xExp, yExp) = pointWithSnapToXYExps pointWithSnap in
  eTuple [xExp, yExp]

addPolygon old pointsWithSnap =
  let points = pointsWithSnap |> List.map (\((x, _), (y, _)) -> (x, y)) in
  addRawPolygon old pointsWithSnap

addRawPolygon old pointsWithSnap =
  let ePts =
    List.reverse pointsWithSnap
    |> List.map pointWithSnapToPairExp
  in
  let polygonExp =
    makeCallWithLocals
        [ makeLet ["pts"] [eTuple ePts]
        , makeLet ["color","strokeColor","strokeWidth"]
                  [randomColor old, eConst 360 dummyLoc, eConst 2 dummyLoc]
        ]
        (eVar0 "rawPolygon")
        [ eVar "color", eVar "strokeColor", eVar "strokeWidth"
        , eVar "pts", eConst 0 dummyLoc ]
  in
  addShapeToModel old "polygon" polygonExp

addRawPolygonList old listExp =
  let polygonExp =
    makeCallWithLocals
        [ makeLet ["pts"] [listExp]
        , makeLet ["color","strokeColor","strokeWidth"]
                  [randomColor old, eConst 360 dummyLoc, eConst 2 dummyLoc]
        ]
        (eVar0 "rawPolygon")
        [ eVar "color", eVar "strokeColor", eVar "strokeWidth"
        , eVar "pts", eConst 0 dummyLoc ]
  in
  addShapeToModel old "polygon" polygonExp

strPoint strX strY (x,y) = Utils.spaces [strX x, strY y]

addPath : Model -> List (KeysDown, (Int, Int)) -> Model
addPath old keysAndPoints =
  addAbsolutePath old keysAndPoints

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
                (True, []) -> [Utils.spaces ["'Q'", strPt click1, lastPoint, "'Z'"]]
                (False, _) -> (Utils.spaces ["'Q'", strPt click1, strPt click2]) :: foo list2
                (True, _)  -> Debug.crash "addPath Q1"
            _ -> Debug.crash "addPath Q2"

        else if modifiers1 == Keys.c then
          case list1 of
            (modifiers2,click2) :: (modifiers3,click3) :: list3 ->
              case (click3 == firstClick, list3) of
                (True, []) -> [Utils.spaces ["'C'", strPt click1, strPt click2, lastPoint, "'Z'"]]
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
  let pathExp =
    makeCallWithLocals
        ([ makeLet ["strokeColor","strokeWidth","color"]
                   [randomColor old, eConst 5 dummyLoc, randomColor1 old] ]
        ++ extraLets
        ++ [makeLet ["d"] [eVar sD] ])
        (eVar0 "rawPath")
        [ eVar "color", eVar "strokeColor", eVar "strokeWidth"
        , eVar "d", eConst 0 dummyLoc ]
  in
  addShapeToModel old "path" pathExp

-- copied from ExpressionBasedTransform
eAsPoint e = eColonTypeAlias e "Point"


addFunction : Ident -> Model -> PointWithSnap -> PointWithSnap -> Model
addFunction fName old pt1 pt2 =
  case newFunctionCallExp fName old pt1 pt2 of
    Just (callExp, returnType) ->
      if TypeDirectedFunctionUtils.clearlyNotShapeOrListOfShapesType returnType then
        addToEndOfDrawingContext old fName callExp
      else
        addShapeToModel old fName callExp
    _ ->
      let _ = Utils.log <| "Could not draw function " ++ fName ++ "!" in old

-- Returns (funcCall, returnType)
newFunctionCallExp : Ident -> Model -> PointWithSnap -> PointWithSnap -> Maybe (Exp, Type)
newFunctionCallExp fName model pt1 pt2 =
  case model.drawableFunctions |> Utils.maybeFind fName of
    Just funcType ->
      case Types.typeToMaybeArgTypesAndReturnType funcType of
        Just (argTypes, returnType) ->
          -- let _ = Utils.log fName in
          -- See if func can take 2 Points
          let (ptsUnused, argMaybeExpsTwoPoints) =
            argTypes
            |> Utils.foldl
                ([pt1, pt2], [])
                (\argType (ptsRemaining, argMaybeExps) ->
                  case (ptsRemaining, Types.isPointType argType) of
                    (pt::otherPts, True) -> (otherPts,     argMaybeExps ++ [Just (makePointExpFromPointWithSnap pt)])
                    _                    -> (ptsRemaining, argMaybeExps ++ [TypeDirectedFunctionUtils.maybeFillInArgPrimitive argType])
                )
            -- |> Debug.log "(ptsUnused, argMaybeExpsTwoPoints)"
          in
          -- See if func can take 1 Point + some distance(s)
          let (ptUsed, widthUsed, heightUsed, argMaybeExpsPointWidthHeight) =
            let maxId = FastParser.maxId model.inputExp in
            let x1y1Exp = makePointExpFromPointWithSnap pt1 |> FastParser.freshenFrom (maxId + 1) in
            let maybeX1Y1Vals = Eval.simpleEvalToMaybeVal x1y1Exp |> Maybe.andThen valToMaybeXYVals in
            let ((x1Int, _), (y1Int, _)) = pt1 in
            let ((x2Int, _), (y2Int, _)) = pt2 in
            let majorAxis = if abs (x2Int - x1Int) >= abs (y2Int - y1Int) then X else Y in -- For circle drawing which is the "Radius"?
            argTypes
            |> Utils.foldl
                (False, False, False, [])
                (\argType (pointUsed, widthUsed, heightUsed, argMaybeExps) ->
                  case ( pointUsed               , Types.isPointType argType
                       , widthUsed               , Types.typeToRoles argType |> Utils.anyOverlapListSet ["Width", "HalfWidth", "HorizontalDistance"]
                       , heightUsed              , Types.typeToRoles argType |> Utils.anyOverlapListSet ["Height", "HalfHeight", "VerticalDistance"]
                       , heightUsed || widthUsed , Types.typeToRoles argType |> Set.member "Radius"
                       ) of
                    (False, True, _, _, _, _, _, _) -> (True,      widthUsed, heightUsed, argMaybeExps ++ [Just x1y1Exp])
                    (_, _, False, True, _, _, _, _) -> (pointUsed, True,      heightUsed, argMaybeExps ++ [Just (makeAxisDifferenceExpFromPointsWithSnap maybeX1Y1Vals X pt2 pt1)])
                    (_, _, _, _, False, True, _, _) -> (pointUsed, widthUsed, True,       argMaybeExps ++ [Just (makeAxisDifferenceExpFromPointsWithSnap maybeX1Y1Vals Y pt2 pt1)])
                    (_, _, _, _, _, _, False, True) -> (pointUsed, True,      True,       argMaybeExps ++ [Just (makeAxisDifferenceExpFromPointsWithSnap maybeX1Y1Vals majorAxis pt2 pt1)])
                    _                               -> (pointUsed, widthUsed, heightUsed, argMaybeExps ++ [TypeDirectedFunctionUtils.maybeFillInArgPrimitive argType])
                )
            -- |> Debug.log "(ptUsed, widthUsed, heightUsed, argMaybeExpsPointWidthHeight)"
          in
          let perhapsPointAnnotation = if Types.isPointType returnType then identity else identity in -- eAsPoint
          Utils.orMaybe
              (Utils.projJusts argMaybeExpsTwoPoints        |> Utils.filterMaybe (always (ptsUnused == [])))
              (Utils.projJusts argMaybeExpsPointWidthHeight |> Utils.filterMaybe (always (ptUsed && (widthUsed || heightUsed))))
          |> Maybe.map (\argExps -> (perhapsPointAnnotation (eCall fName argExps), returnType))

        Nothing -> Debug.crash <| "Draw.newFunctionCallExp bad function type: " ++ Syntax.typeUnparser Syntax.Elm funcType

    Nothing -> let _ = Utils.log <| "Could not find function " ++ fName ++ " to draw!" in Nothing


-- addTextBox old click2 click1 =
--   let (xa, xb, ya, yb) = boundingBox (Tuple.second click2) (Tuple.second click1) in
--   let fontSize =
--     eConst0 (toFloat (yb - ya)) dummyLoc
--     -- withDummyExpInfo (EConst "" (toFloat (yb - ya)) dummyLoc (intSlider 0 128))
--   in
--   let textExp =
--     makeCallWithLocals
--         [ makeLet ["fontSize","textVal"] [fontSize, eStr "Text"] ]
--         (eVar0 "simpleText")
--         [ eStr "Tahoma, sans-serif", eStr "black", eVar "fontSize"
--         , eConst (toFloat xa) dummyLoc, eConst (toFloat xb) dummyLoc
--         , eConst (toFloat yb) dummyLoc
--         , eConst 1.5 dummyLoc, eVar "textVal" ]
--   in
--   addShapeToModel old "text" textExp

--------------------------------------------------------------------------------


-- TODO: remove randomColor/1 when they are no longer needed


addShapeToModel : Model -> String -> Exp -> Model
addShapeToModel model newShapeName newShapeExp =
  let newProgram = DrawAddShape.addShape model (always True) (Just newShapeName) newShapeExp (Just 1) Nothing Nothing Nothing False model.inputExp in
  { model | code = Syntax.unparser model.syntax newProgram }


makeCallWithLocals locals func args =
  let recurse locals =
    case locals of
      [] ->
        -- if multi then
        withDummyExpInfo (EApp (ws "\n    ") func args SpaceApp space0)
        -- else
        --   let app = withDummyExpInfo (EApp space1 func args SpaceApp space0) in
        --   withDummyExpInfo (EList (ws "\n    ") [app] space0 Nothing space1)
      (p,e)::locals_ -> withDummyExpInfo (ELet (ws "\n  ") Let False p space1 e space1 (recurse locals_) space0)
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
    [n]   -> [eInt0 n]
    n::ns -> eInt0 n :: List.map eInt ns

-- Don't always throw into a pair, so both have preceding whitespace.
makeIntPair : (Int, Int) -> (Exp, Exp)
makeIntPair (x, y) = (eInt x, eInt y)

-- Don't always throw into a pair, so both have preceding whitespace.
makeIntPairOrSnap : PointWithSnap -> (Exp, Exp)
makeIntPairOrSnap ((x, xSnap), (y, ySnap)) =
  case (xSnap, ySnap) of
    (NoSnap,       NoSnap)       -> (eInt x,        eInt y)
    (SnapVal xVal, NoSnap)       -> (eHoleVal xVal, eInt y)
    (NoSnap,       SnapVal yVal) -> (eInt x,        eHoleVal yVal)
    (SnapVal xVal, SnapVal yVal) -> (eHoleVal xVal, eHoleVal yVal)

makePointExpFromPointWithSnap : PointWithSnap -> Exp
makePointExpFromPointWithSnap pt =
  let (xExp, yExp) = makeIntPairOrSnap pt in
  identity <| ePair (removePrecedingWhitespace xExp) yExp -- eAsPoint

-- For making height/width from mouse end/start positions
-- If the end is snapped but the start is not, then we want to introduce new variables for the x1/y1 pair so we can
-- use it in both the x1/y1 point and in the calculation of width/height. That's what maybeX1Y1Vals is for.
-- (I tried making an HoleEId hole type specifically for this case but that (a) sometimes fails and (b) breaks the drawing preview b/c Eval cannot eval through EId holes.
-- So we will use val holes.)
makeAxisDifferenceExpFromPointsWithSnap : Maybe (Val, Val) -> Axis -> PointWithSnap -> PointWithSnap -> Exp
makeAxisDifferenceExpFromPointsWithSnap maybeX1Y1Vals axis ((x2, x2Snap), (y2, y2Snap)) ((x1, x1Snap), (y1, y1Snap)) =
  let ((endCoord, endSnap), (startCoord, startSnap)) =
    case axis of
      X -> ((x2, x2Snap), (x1, x1Snap))
      Y -> ((y2, y2Snap), (y1, y1Snap))
  in
  let maybeStartVal =
    case (axis, maybeX1Y1Vals) of
      (X, Just (x1Val, _)) -> Just x1Val
      (Y, Just (_, y1Val)) -> Just y1Val
      _                    -> Nothing
  in
  case (endSnap, startSnap, maybeStartVal) of
    (NoSnap,         _,      _)             -> eInt (endCoord - startCoord)
    (SnapVal endVal, NoSnap, Just startVal) -> eMinus (eHoleVal endVal) (eHoleVal startVal)
    (SnapVal endVal, NoSnap, Nothing)       -> eMinus (eHoleVal endVal) (eInt startCoord)
    (SnapVal endVal, SnapVal startVal, _)   -> eMinus (eHoleVal endVal) (eHoleVal startVal)

addToMainExp : BlobExp -> MainExp -> MainExp
addToMainExp newBlob mainExp =
  case mainExp of
    SvgConcat shapes f -> SvgConcat (shapes ++ [fromBlobExp newBlob]) f
    Blobs shapes f     -> Blobs (shapes ++ [newBlob]) f
    OtherExp main ->
      let wsN = ws "\n" in -- TODO take main into account
      OtherExp <| withDummyExpInfo <|
        EApp (wsN) (eVar0 "addBlob") [fromBlobExp newBlob, main] SpaceApp space0

maybeGhost b f args =
  if b
    then (eVar0 "ghost", [ withDummyExpInfo (EApp space1 f args SpaceApp space0) ])
    else (f, args)

ghost = maybeGhost True


--------------------------------------------------------------------------------
-- Function Tool (generalized lambda tool)


-- Returns list of (fName, typeSig), fExp is an EFun
getDrawableFunctions : Model -> List (Ident, Type)
getDrawableFunctions model =
  TypeDirectedFunctionUtils.getFunctionsByPredicateOnType
      isDrawableType
      model.idToTypeAndContextThunk
      model.inputExp
      model.editingContext


-- Supported inputs:
--  - 2+ Points
--  - 1+ Point + w + h
--
-- Supported outputs:
-- Anything
--
-- Dual is in newFunctionCallExp where the args are actually filled in.
isDrawableType : Type -> Bool
isDrawableType tipe =
  case Types.typeToMaybeArgTypesAndReturnType tipe of
    Just (inputTypes, _) ->
      Utils.count Types.isPointType inputTypes >= 2 ||
      (
        Utils.count Types.isPointType inputTypes >= 1 &&
        (
          Utils.count (Types.typeToRoles >> Utils.anyOverlapListSet ["Width",  "HalfWidth", "HorizontalDistance"])  inputTypes >= 1 ||
          Utils.count (Types.typeToRoles >> Utils.anyOverlapListSet ["Height", "HalfHeight", "VerticalDistance"]) inputTypes >= 1 ||
          Utils.count (Types.typeToRoles >> Set.member "Radius") inputTypes >= 1
        )
      )

    _ ->
      False


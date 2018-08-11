module Draw exposing
  ( drawDotSize
  , drawNewLine
  , drawNewRect
  , drawNewEllipse
  , drawNewPolygon
  , drawNewPath
  , boundingBoxOfPoints_
  , addShape
  , addLine
  , addRawSquare , addRawRect , addStretchySquare , addStretchyRect
  , addRawCircle , addRawOval , addStretchyCircle , addStretchyOval
  , addPath , addPolygon
  , addLambda , addFunction
  , addPoint , addOffset , addOffsetAndPoint , horizontalVerticalSnap
  , addTextBox
  , lambdaToolOptionsOf
  , getDrawableFunctions
  )

import CodeMotion
import Lang exposing (..)
import LangSvg
import Types
import Blobs exposing (..)
import InterfaceModel exposing (..)
import ElmParser as Parser
import LangTools
import LangUnparser
import StaticAnalysis
import ColorNum
import Provenance
import Utils
import Either exposing (..)
import Keys
import Eval -- used to determine bounding box of LambdaAnchor tools
            -- for the purposes of rendering icons in drawing toolbox
import EvalUpdate
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


--------------------------------------------------------------------------------
-- Bounding Boxes

-- TODO change order of return values
boundingBoxOfPoints_ : List (Float, Float) -> Maybe (Float, Float, Float, Float)
boundingBoxOfPoints_ pts =
  let (xs, ys) = List.unzip pts in
  case List.maximum xs of
    Nothing -> Nothing
    Just xMax ->
  case List.minimum xs of
    Nothing -> Nothing
    Just xMin ->
  case List.maximum ys of
    Nothing -> Nothing
    Just yMax ->
  case List.minimum ys of
    Nothing -> Nothing
    Just yMin ->
  Just (xMin, xMax, yMin, yMax)

boundingBoxOfPoints : List (Int, Int) -> Maybe (Int, Int, Int, Int)
boundingBoxOfPoints pts =
  let pts_ = List.map (\(x,y) -> (toFloat x, toFloat y)) pts in
  boundingBoxOfPoints_ pts_ |> Maybe.map (\(a,b,c,d) ->
  (round a, round b, round c, round d))


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
    if keysDown == [Keys.keyShift]
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
    if keysDown == [Keys.keyShift]
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


--------------------------------------------------------------------------------
-- New Shapes (previously in Controller)

useColorNums = False

-- Uses of the following functions are a bit messier now because they
-- return different types of Little expressions.

randomColor model =
  if useColorNums then
    eConst0 (toFloat model.randomColor) dummyLoc
  else
    eStr0 (ColorNum.randomHtmlColorName model.randomColor)

randomColor1 model =
  if useColorNums then
    eConst (toFloat model.randomColor) dummyLoc
  else
    eStr (ColorNum.randomHtmlColorName model.randomColor)

eDefaultStroke =
  if useColorNums then
    eConst 360 dummyLoc
  else
    eStr "black"

{-
randomColorWithSlider model =
  withDummyExpInfo (EConst "" (toFloat model.randomColor) dummyLoc colorNumberSlider)

randomColor1WithSlider model =
  withDummyExpInfo (EConst " " (toFloat model.randomColor) dummyLoc colorNumberSlider)
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
  let lineExp =
    makeCallWithLocals
        [ makeLet ["x1","y1","x2","y2"] (makeInts [x1,y1,xb,yb])
        , makeLet ["color", "width"]
                  [ color , eConst 5 dummyLoc ]
        ]
        f
        args
  in
  addShapeToModel old "line" lineExp


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

addRawRect old (_,pt2) (_,pt1) =
  let (xa, xb, ya, yb) = boundingBox pt2 pt1 in
  let (x, y, w, h) = (xa, ya, xb - xa, yb - ya) in
  let (fill, stroke, strokeWidth) = (randomColor1 old, eDefaultStroke, 0) in
  let (rot) = 0 in
  addShapeToModel old "rect"
    (stencilRawRect x y w h fill stroke strokeWidth rot)

stencilRawRect x y w h fill _ _ _ =
  makeCallWithLocals
    [ makeLet ["x","y","w","h"] (makeInts [x,y,w,h]) ]
    (eVar0 "rect")
    [ fill , eVar "x", eVar "y", eVar "w", eVar "h" ]
    {-
    [ makeLet ["x","y","w","h"] (makeInts [x,y,w,h])
    , makeLet ["fill", "stroke","strokeWidth"] [fill, stroke, eConstDummyLoc strokeWidth]
    , makeLet ["rot"] [eConst (toFloat rot) dummyLoc] ]
    (eVar0 "rawRect")
    [ eVar "fill", eVar "stroke", eVar "strokeWidth"
    , eVar "x", eVar "y", eVar "w", eVar "h", eVar "rot" ]
    -}

--------------------------------------------------------------------------------

addRawSquare old (_,pt2) (_,pt1) =
  let (xa, xb, ya, yb) = squareBoundingBox pt2 pt1 in
  let (x, y, side) = (xa, ya, xb - xa) in
  let squareExp =
    makeCallWithLocals
        [ makeLet ["x","y","side"] (makeInts [x,y,side])
        , makeLet ["color","rot"] [randomColor old, eConst 0 dummyLoc] ]
        (eVar0 "rawRect")
        [ eVar "color", eDefaultStroke, eConst 0 dummyLoc
        , eVar "x", eVar "y", eVar "side", eVar "side", eVar "rot" ]
  in
  addShapeToModel old "square" squareExp

--------------------------------------------------------------------------------

addStretchyRect old (_,pt2) (_,pt1) =
  let (xMin, xMax, yMin, yMax) = boundingBox pt2 pt1 in
  let (fill) = (randomColor old) in
  let (stroke, strokeWidth, rot) = (eDefaultStroke, 0, 0) in
  addShapeToModel old "rect"
    (stencilStretchyRect xMin yMin xMax yMax fill stroke strokeWidth rot)

stencilStretchyRect left top right bot fill stroke strokeWidth rot =
  makeCallWithLocals
    [ makeLetAs "bounds" ["left","top","right","bot"] (makeInts [left,top,right,bot])
    , makeLet ["color"] [fill] ]
    (eVar0 "rectangle")
    [eVar "color", stroke, eConst (toFloat strokeWidth) dummyLoc
    , eConst (toFloat rot) dummyLoc, eVar "bounds"]

--------------------------------------------------------------------------------

addStretchySquare old (_,pt2) (_,pt1) =
  let (xMin, xMax, yMin, _) = squareBoundingBox pt2 pt1 in
  let side = (xMax - xMin) in
  let squareExp =
    makeCallWithLocals
        [ makeLet ["left","top","side"] (makeInts [xMin,yMin,side])
        , makeLet ["bounds"] [eList (listOfRaw ["left","top","(+ left side)","(+ top side)"]) Nothing]
        , makeLet ["rot"] [eConst 0 dummyLoc]
        , makeLet ["color","strokeColor","strokeWidth"]
                  [randomColor old, eDefaultStroke, eConst 0 dummyLoc] ]
        (eVar0 "rectangle")
        (List.map eVar ["color","strokeColor","strokeWidth","rot","bounds"])
  in
  addShapeToModel old "square" squareExp

--------------------------------------------------------------------------------

addRawOval old (_,pt2) (_,pt1) =
  let (xa, xb, ya, yb) = boundingBox pt2 pt1 in
  let (rx, ry) = ((xb-xa)//2, (yb-ya)//2) in
  let (cx, cy) = (xa + rx, ya + ry) in
  let ellipseExp =
    makeCallWithLocals
        [ makeLet ["cx","cy","rx","ry"] (makeInts [cx,cy,rx,ry]) ]
        (eVar0 "ellipse")
        [ randomColor1 old, eVar "cx", eVar "cy", eVar "rx", eVar "ry" ]
{-
        [ makeLet ["cx","cy","rx","ry"] (makeInts [cx,cy,rx,ry])
        , makeLet ["color","rot"] [randomColor old, eConst 0 dummyLoc] ]
        (eVar0 "rawEllipse")
        [ eVar "color", eDefaultStroke, eConst 0 dummyLoc
        , eVar "cx", eVar "cy", eVar "rx", eVar "ry", eVar "rot" ]
-}
  in
  addShapeToModel old "ellipse" ellipseExp

--------------------------------------------------------------------------------

addRawCircle old (_,pt2) (_,pt1) =
  let (xa, xb, ya, yb) = squareBoundingBox pt2 pt1 in
  let r = (xb-xa)//2 in
  let (cx, cy) = (xa + r, ya + r) in
  let circleExp =
    makeCallWithLocals
        [ makeLet ["cx","cy","r"] (makeInts [cx,cy,r])
        , makeLet ["color"] [randomColor1 old] ]
        (eVar0 "rawCircle")
        [ eVar "color", eDefaultStroke, eConst 0 dummyLoc
        , eVar "cx", eVar "cy", eVar "r" ]
  in
  addShapeToModel old "circle" circleExp

--------------------------------------------------------------------------------

addStretchyOval old (_,pt2) (_,pt1) =
  let (xa, xb, ya, yb) = boundingBox pt2 pt1 in
  let ellipseExp =
    makeCallWithLocals
        [ makeLetAs "bounds" ["left","top","right","bot"] (makeInts [xa,ya,xb,yb])
        , makeLet ["color","strokeColor","strokeWidth"]
                  [randomColor old, eDefaultStroke, eConst 0 dummyLoc] ]
        (eVar0 "oval")
        (List.map eVar ["color","strokeColor","strokeWidth","bounds"])
  in
  addShapeToModel old "ellipse" ellipseExp

--------------------------------------------------------------------------------

addStretchyCircle old (_,pt2) (_,pt1) =
  let (left, right, top, _) = squareBoundingBox pt2 pt1 in
  let circleExp =
    makeCallWithLocals
        [ makeLet ["left", "top", "r"] (makeInts [left, top, (right-left)//2])
        , makeLet ["bounds"]
            [eList [eVar0 "left", eVar "top", eRaw "(+ left (* 2! r))", eRaw "(+ top (* 2! r))"] Nothing]
        , makeLet ["color","strokeColor","strokeWidth"]
                  [randomColor old, eDefaultStroke, eConst 0 dummyLoc] ]
        (eVar0 "oval")
        (List.map eVar ["color","strokeColor","strokeWidth","bounds"])
  in
  addShapeToModel old "circle" circleExp

--------------------------------------------------------------------------------

addPoint : Model -> (Int, Int) -> Model
addPoint old (x, y) =
  -- style matches center of attr crosshairs (View.zoneSelectPoint_)
  let originalProgram = old.inputExp in
  case LangTools.nonCollidingNames ["point", "x", "y"] 2 <| EvalUpdate.identifiersVisibleAtProgramEnd originalProgram of
    [pointName, xName, yName] ->
      let
        programWithPoint =
          LangTools.addFirstDef originalProgram (pAs pointName (pList [pVar0 xName, pVar yName])) <| Expr (eColonType (eTuple0 [eConstDummyLoc0 (toFloat x), eConstDummyLoc (toFloat y)]) (tApp space1 (withDummyRange <| TVar space0 "Point") [] SpaceApp))
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


addOffsetAndPoint : Model -> Snap -> (Num, Num) -> (Int, Int) -> Model
addOffsetAndPoint old snap (x1, y1) (x2, y2) =
  addOffsetAndMaybePoint old snap (x1, y1) Nothing (x2, y2)


addOffset : Model -> Snap -> (Val, Val) -> (Int, Int) -> Model
addOffset old snap (x1Val, y1Val) (x2, y2) =
  addOffsetAndMaybePoint old snap (valToNum x1Val, valToNum y1Val) (Just (x1Val, y1Val)) (x2, y2)


addToEndOfProgram : Model -> Ident -> Exp -> Model
addToEndOfProgram old varSuggestedName exp =
  let originalProgram = old.inputExp in
  let insertBeforeEId = expEId <| LangTools.lastTopLevelExp originalProgram in
  let varName = LangTools.nonCollidingName varSuggestedName 2 <| EvalUpdate.visibleIdentifiersAtEIds originalProgram (Set.singleton insertBeforeEId) in
  let newProgram =
    originalProgram
    |> mapExpNode insertBeforeEId (\lastTopLevelExp -> LangTools.newLetFancyWhitespace -1 False (pVar varName) exp lastTopLevelExp originalProgram)
    |> CodeMotion.resolveValueHoles old.syncOptions
    |> List.head
    |> Maybe.withDefault originalProgram
  in
  { old | code = Syntax.unparser old.syntax newProgram }


addOffsetAndMaybePoint : Model -> Snap -> (Num, Num) -> Maybe (Val, Val) -> (Int, Int) -> Model
addOffsetAndMaybePoint old snap (x1, y1) maybeExistingPoint (x2, y2) =
  -- style matches center of attr crosshairs (View.zoneSelectPoint_)
  let originalProgram = old.inputExp in
  let (axis, sign, offsetAmount) = horizontalVerticalSnap (round x1, round y1) (x2, y2) in
  let plusOrMinus = if sign == Positive then Plus else Minus in
  let offsetFromExisting baseVal =
    let offsetSuggestedName = Provenance.nameForVal originalProgram baseVal ++ "Offset" in
    let offsetExp =
      let offsetAmountExp =
        case snap of
          NoSnap          -> eConstDummyLoc (toFloat offsetAmount)
          SnapVal snapVal -> eSnapHoleVal snapVal
      in
      eOp plusOrMinus [eSnapHoleVal baseVal, offsetAmountExp]
    in
    addToEndOfProgram old offsetSuggestedName offsetExp
  in
  case (axis, maybeExistingPoint) of
    (X, Just (x1Val, _)) -> offsetFromExisting x1Val
    (Y, Just (_, y1Val)) -> offsetFromExisting y1Val
    _                    ->
      case LangTools.nonCollidingNames ["point", "x", "y", "x{n}Offset", "y{n}Offset"] 1 <| EvalUpdate.identifiersVisibleAtProgramEnd originalProgram of
        [pointName, xName, yName, offsetXName, offsetYName] ->
          let
            (offsetName, offsetFromName) = if axis == X then (offsetXName, xName) else (offsetYName, yName)
            programWithOffset =
              LangTools.addFirstDef originalProgram (pVar offsetName) (eOp plusOrMinus [eVar offsetFromName, eConstDummyLoc (toFloat offsetAmount)]) |> Parser.freshen
            programWithOffsetAndPoint =
              LangTools.addFirstDef programWithOffset (pAs pointName (pList [pVar0 xName, pVar yName])) <| Expr (eColonType (eTuple0 [eConstDummyLoc0 x1, eConstDummyLoc y1]) (tApp space1 (withDummyRange <| TVar space0 "Point") [] SpaceApp))
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
      SnapVal snapVal -> eSnapHoleVal0 snapVal
  in
  let yExp =
    case ySnap of
      NoSnap          -> eConstDummyLoc (toFloat y)
      SnapVal snapVal -> eSnapHoleVal snapVal
  in
  (xExp, yExp)

pointWithSnapToPairExp pointWithSnap =
  let (xExp, yExp) = pointWithSnapToXYExps pointWithSnap in
  eTuple [xExp, yExp]

addPolygon stk old pointsWithSnap =
  let points = pointsWithSnap |> List.map (\((x, _), (y, _)) -> (x, y)) in
  case stk of
    Raw      -> addRawPolygon old pointsWithSnap
    Stretchy -> addStretchablePolygon old points
    Sticky   -> addStickyPolygon old points

addRawPolygon old pointsWithSnap =
  let ePts =
    List.reverse pointsWithSnap
    |> List.map pointWithSnapToPairExp
  in
  let polygonExp =
    makeCallWithLocals
        [ makeLet ["pts"] [eTuple ePts]
        , makeLet ["color","strokeColor","strokeWidth"]
                  [randomColor old, eDefaultStroke, eConst 2 dummyLoc]
                  -- [randomColor old, eDefaultStroke, eConst 2 dummyLoc]
        ]
        -- (eVar0 "rawPolygon")
        (eVar0 "polygon")
        [ eVar "color", eVar "strokeColor", eVar "strokeWidth"
        , eVar "pts" ]
        -- , eVar "pts", eConst 0 dummyLoc ]
  in
  addShapeToModel old "polygon" polygonExp

addStretchablePolygon old points =
  case boundingBoxOfPoints points of
    Nothing -> old
    Just (xMin, xMax, yMin, yMax) ->
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
  let polygonExp =
    makeCallWithLocals
        [ makeLetAs "bounds" ["left","top","right","bot"] (makeInts [xMin,yMin,xMax,yMax])
        , makeLet ["color","strokeColor","strokeWidth"]
                  [randomColor old, eDefaultStroke, eConst 2 dummyLoc]
        , makeLet ["pcts"] [eRaw sPcts] ]
        (eVar0 "stretchyPolygon")
        (List.map eVar ["bounds","color","strokeColor","strokeWidth","pcts"])
  in
  addShapeToModel old "polygon" polygonExp

addStickyPolygon old points =
  case boundingBoxOfPoints points of
    Nothing -> old
    Just (xMin, xMax, yMin, yMax) ->
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
  let polygonExp =
    makeCallWithLocals
        [ makeLetAs "bounds" ["left","top","right","bot"] (makeInts [xMin,yMin,xMax,yMax])
        , makeLet ["color","strokeColor","strokeWidth"]
                  [randomColor old, eDefaultStroke, eConst 2 dummyLoc]
        , makeLet ["offsets"] [eRaw sOffsets] ]
        (eVar0 "stickyPolygon")
        (List.map eVar ["bounds","color","strokeColor","strokeWidth","offsets"])
  in
  addShapeToModel old "polygon" polygonExp

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

addStretchyPath old keysAndPoints =
  let points = List.map Tuple.second keysAndPoints in
  case boundingBoxOfPoints points of
    Nothing -> old
    Just (xMin, xMax, yMin, yMax) ->
  let (width, height) = (toFloat (xMax - xMin), toFloat (yMax - yMin)) in
  let strX x = maybeThaw (toFloat (x - xMin) / width) in
  let strY y = maybeThaw (toFloat (y - yMin) / height) in
  let (extraLets, sD) = pathCommands strX strY keysAndPoints in
  let pathExp =
    makeCallWithLocals
        ([ makeLetAs "bounds" ["left","top","right","bot"] (makeInts [xMin,yMin,xMax,yMax])
         , makeLet ["strokeColor","strokeWidth","color"]
                   [ randomColor old, eConst 5 dummyLoc, randomColor1 old ] ]
         ++ extraLets
         ++ [ makeLet ["dPcts"] [eVar sD] ])
        (eVar0 "stretchyPath")
        (List.map eVar ["bounds","color","strokeColor","strokeWidth","dPcts"])
  in
  addShapeToModel old "path" pathExp

addStickyPath old keysAndPoints =
  Debug.crash "TODO: addStickyPath"

-- copied from ExpressionBasedTransform
eAsPoint e =
  let e_ = replacePrecedingWhitespace "" e in
  withDummyExpInfo <|
    EColonType space1 e_ space1 (withDummyRange <| TApp space1 (withDummyRange <| TVar space0 "Point") [] SpaceApp) space1

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
    LambdaAnchor func _ -> addLambdaAnchor old pt2 pt1 func

addLambdaBounds old (_,pt2) (_,pt1) func =
  let (xa, xb, ya, yb) =
    if old.keysDown == [Keys.keyShift]
       then squareBoundingBox pt2 pt1
       else boundingBox pt2 pt1
  in

  {- this version adds a call to main: -}

  let bounds = eList (makeInts [xa,ya,xb,yb]) Nothing in
  let args = [] in
  let eNew =
    withDummyExpInfo (EApp (ws "\n  ") (eVar0 "withBounds") [ bounds, func ] SpaceApp space0) in
  -- TODO refactor Program to keep (f,args) in sync with exp
  let newBlob = withBoundsBlob eNew (bounds, "XXXXX", args) in
  let (defs, mainExp) = splitExp old.inputExp in
  let mainExp_ = addToMainExp newBlob mainExp in
  let code = Syntax.unparser old.syntax (fuseExp (defs, mainExp_)) in
  { old | code = code }

  {- this version adds the call inside a new top-level definition:

  add funcName old
    [ makeLet ["left","top","right","bot"] (makeInts [xa,ya,xb,yb])
    , makeLet ["bounds"] [eList (listOfVars ["left","top","right","bot"]) Nothing]
    ]
    (eVar0 "with") [ eVar "bounds" , eVar funcName ]

  -}

-- For simplicity, keeping this as a click-and-drag drawing tool instead
-- of a single-click tool, and using center of bounding box as anchor.
--
addLambdaAnchor old click2 click1 func =
  let (xa, xb, ya, yb) = boundingBox (Tuple.second click2) (Tuple.second click1) in
  let (x, y) = ((xa + xb) // 2, (ya + yb) // 2) in
  let anchor = eAsPoint (eList (makeInts [x,y]) Nothing) in
  let args = [] in
  let eNew =
    withDummyExpInfo (EApp (ws "\n  ") (eVar0 "withAnchor") [ anchor, func ] SpaceApp space0) in
  -- TODO refactor Program to keep (f,args) in sync with exp
  let newBlob = withAnchorBlob eNew (anchor , "XXXXX", args) in
  let (defs, mainExp) = splitExp old.inputExp in
  let mainExp_ = addToMainExp newBlob mainExp in
  let code = Syntax.unparser old.syntax (fuseExp (defs, mainExp_)) in
  { old | code = code }


addFunction : Ident -> Model -> (KeysDown, (Int, Int)) -> (KeysDown, (Int, Int)) -> Model
addFunction fName old (_, (x2, y2)) (_, (x1, y1)) =
  let fillInArgPrimitive argType =
    case argType.val of
       TNum _                         -> Just <| eConstDummyLoc 0
       TBool _                        -> Just <| eFalse
       TString _                      -> Just <| eStr "string"
       TNull _                        -> Just <| eNull
       TList _ _ _                    -> Just <| eTuple []
       TDict _ _ _ _                  -> Just <| eOp DictEmpty []
       TTuple _ headTypes _ Nothing _ -> List.map fillInArgPrimitive headTypes |> Utils.projJusts |> Maybe.map eTuple
       TUnion _ (firstType::_) _      -> fillInArgPrimitive firstType
       TVar _ _                       -> Just <| eTuple []
       TWildcard _                    -> Just <| eTuple []
       TApp _ tFunArg _ _             -> case tFunArg.val of
          TVar _ "Color" -> Just <| eConstDummyLoc 0
          TVar _ "Point" -> Just <| eTuple (makeInts [0,0])
          _ -> Nothing
       _                              -> Nothing
  in
  case getDrawableFunctions old |> Utils.maybeFind fName |> Maybe.andThen Types.typeToMaybeArgTypesAndReturnType of
    Just (argTypes, returnType) ->
      let (_, argMaybeExps) =
        argTypes
        |> List.foldl
            (\argType (ptsRemaining, argMaybeExps) ->
              case (ptsRemaining, argType.val) of
                ((x,y)::otherPts, TApp _ tFunArg _ _) -> case tFunArg.val of
                  TVar _ "Point" -> (otherPts,     argMaybeExps ++ [Just (eTuple (makeInts [x,y]))])
                  _ -> (ptsRemaining, argMaybeExps ++ [fillInArgPrimitive argType])
                _                                   -> (ptsRemaining, argMaybeExps ++ [fillInArgPrimitive argType])
            )
            ([(x1, y1), (x2, y2)], [])
      in
      case (Utils.projJusts argMaybeExps, returnType.val) of
        (Just argExps, TApp _ funType _ _) -> case funType.val of
           TVar _ "Point" -> addToEndOfProgram old fName (eCall fName argExps)
           TVar _ "Shape" -> addShapeToModel   old fName (eCall fName argExps)
           _ -> let _ = Utils.log <| "Could not draw function " ++ fName ++ "!" in old
        _                                ->
          let _ = Utils.log <| "Could not draw function " ++ fName ++ "!" in old

    Nothing -> let _ = Utils.log <| "Could not find function " ++ fName ++ " to draw!" in old


addTextBox old click2 click1 =
  let (xa, xb, ya, yb) = boundingBox (Tuple.second click2) (Tuple.second click1) in
  let fontSize =
    eConst0 (toFloat (yb - ya)) dummyLoc
    -- withDummyExpInfo (EConst "" (toFloat (yb - ya)) dummyLoc (intSlider 0 128))
  in
  let textExp =
    makeCallWithLocals
        [ makeLet ["fontSize","textVal"] [fontSize, eStr "Text"] ]
        (eVar0 "simpleText")
        [ eStr "Tahoma, sans-serif", eStr "black", eVar "fontSize"
        , eConst (toFloat xa) dummyLoc, eConst (toFloat xb) dummyLoc
        , eConst (toFloat yb) dummyLoc
        , eConst 1.5 dummyLoc, eVar "textVal" ]
  in
  addShapeToModel old "text" textExp

--------------------------------------------------------------------------------


-- TODO: remove randomColor/1 when they are no longer needed


addShapeToModel : Model -> String -> Exp -> Model
addShapeToModel model newShapeName newShapeExp =
  let newProgram = addShape model newShapeName newShapeExp 1 in
  { model | code = Syntax.unparser model.syntax newProgram }


-- 1. Find all list literals.
-- 2. Make candidate programs by adding both `shape` and `[shape]` to the end of each list.
-- 3. Resolve value holes.
-- 4. Keep those programs that do not crash.
-- 5. Keep those programs that result in one more shape in the output.
-- 6. Finally, use list the others do not depend on.
addShape : Model -> String -> Exp -> Int -> Exp
addShape model newShapeName newShapeExp numberOfNewShapesExpected =
  let program = model.inputExp in
  let oldShapeTree =
    case runAndResolve model program of
      Ok (_, _, _, (root, shapeTree), _) -> shapeTree
      _                               -> Dict.empty
  in
  -- 1. Find all list literals.
  let lists = flattenExpTree program |> List.filter isList in -- Possible optimization: exclude lists with numeric element
  -- 2. Make candidate programs by adding both `shape` and `[shape]` to the end of each list.
  let listEIdWithPossiblePrograms =
    lists
    |> List.concatMap
        (\listExp ->
          let (varName, programWithNewDef) = EvalUpdate.newVariableVisibleTo -1 newShapeName 1 newShapeExp [(expEId listExp)] program in
          let (ws1, heads, ws2, maybeTail, ws3) = LangTools.expToListParts listExp in
          let newListFlat      = replaceE__ listExp <| EList ws1 (List.map ((,) space0) (imitateExpListWhitespace_ heads ws3.val (heads ++ [eVar varName])))           ws2 maybeTail ws3 in
          let newListSingleton = replaceE__ listExp <| EList ws1 (List.map ((,) space0) (imitateExpListWhitespace_ heads ws3.val (heads ++ [eTuple [eVar0 varName]]))) ws2 maybeTail ws3 in
          let newProgramFlat      = programWithNewDef |> replaceExpNode (expEId listExp) newListFlat in
          let newProgramSingleton = programWithNewDef |> replaceExpNode (expEId listExp) newListSingleton in
          [ ((expEId listExp), newProgramFlat)
          , ((expEId listExp), newProgramSingleton)
          ]
        )
    -- 3. Resolve value holes.
    |> List.concatMap
        (\(listEId, newProgramWithHoles) -> CodeMotion.resolveValueHoles model.syncOptions newProgramWithHoles |> List.map ((,) listEId))
    -- 4. Keep those programs that do not crash.
    -- 5. Keep those programs that result in one more shape in the output.
    |> List.filter
        (\(listEId, newProgram) ->
          case runAndResolve model newProgram of
            Ok (_, _, _, (root, shapeTree), _) -> Dict.size oldShapeTree + numberOfNewShapesExpected == Dict.size shapeTree
            _                               -> False
        )
  in
  -- 6. Finally, use list the others do not depend on.
  let (listEIds, _) = List.unzip listEIdWithPossiblePrograms in
  let grossDependencies = StaticAnalysis.grossDependencies program in
  let (_, bestProgram) =
    listEIdWithPossiblePrograms
    |> Utils.findFirst
        (\(listEId, _) -> listEIds |> List.all (\otherListEId -> not <| StaticAnalysis.isDependentOn grossDependencies otherListEId listEId))
    |> Maybe.withDefault (-1, program)
  in
  bestProgram


makeCallWithLocals locals func args =
  let recurse locals =
    case locals of
       [] ->
         -- if multi then
         withDummyExpInfo (EApp (ws "\n    ") func args SpaceApp space0)
         -- else
         --   let app = withDummyExpInfo (EApp space1 func args SpaceApp space0) in
         --   withDummyExpInfo (EList (ws "\n    ") [app] space0 Nothing space1)
       (p,e)::locals_ -> withDummyExpInfo (eLet__ (ws "\n  ") Let False p space1 e space1 (recurse locals_) space0)
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
      let wsN = ws "\n" in -- TODO take main into account
      OtherExp <| withDummyExpInfo <|
        EApp (wsN) (eVar0 "addBlob") [fromBlobExp newBlob, main] SpaceApp space0

maybeGhost b f args =
  if b
    then (eVar0 "ghost", [ withDummyExpInfo (EApp space1 f args SpaceApp space0) ])
    else (f, args)

ghost = maybeGhost True

{-
switchToCursorTool old =
  { old | mouseMode = MouseNothing , tool = Cursor }
-}


--------------------------------------------------------------------------------
-- Lambda Tool

lambdaToolOptionsOf : Syntax -> SplitProgram -> Env -> List LambdaTool
lambdaToolOptionsOf syntax (defs, mainExp) finalEnv =
  let _ = Debug.log "Draw.lambdaToolOptionsOf uses a soon obsolete version of Blobs. Returning nothing" () in
  []
  {-
  case mainExp of

    Blobs blobs _ ->
      let lambdaPreFuncs =
        -- will be easier with better TopDefs
        List.concatMap (\(_,p,e,_) ->
          case (p.val.p__, (unwrapExp e)) of
            (PVar _ fName _, EFun _ params _ _) ->
              case List.reverse params of
                lastParam :: _ ->
                  case varsOfPat lastParam of
                    ["bounds"]                            -> [Left fName]
                    ["left","top","right","bot"]          -> [Left fName]
                    ["bounds","left","top","right","bot"] -> [Left fName]
                    ["anchor"]                            -> [Right fName]
                    ["xAnchor","yAnchor"]                 -> [Right fName]
                    ["anchor","xAnchor","yAnchor"]        -> [Right fName]
                    _                                     -> []
                [] -> []
            _ -> []
          ) defs
      in
      let withBlobs =
        List.reverse <| -- reverse so that most recent call wins
          List.concatMap (\blob ->
            case blob of
              NiceBlob _ (WithBoundsBlob (_, fName, argExps)) -> [Left (fName, argExps)]
              NiceBlob _ (WithAnchorBlob (_, fName, argExps)) -> [Right (fName, argExps)]
              _                                        -> []
            ) blobs
      in
      let lambdaCalls =
        lambdaPreFuncs
        |> List.concatMap (\preFunc ->
          let pred withBlob =
            case (preFunc, withBlob) of
              (Left f, Left (g, _))   -> f == g
              (Right f, Right (g, _)) -> f == g
              _                       -> False
          in
          case Utils.findFirst pred withBlobs of
            Nothing               -> []
            Just (Left (f,args))  -> [LambdaBounds <| withDummyExpInfo (EApp space1 (eVar0 f) args SpaceApp space0)]
            Just (Right (f,args)) ->
              let maybeViewBoxAndAnchor =
                --
                -- 1. Evaluate the function at anchor (100,100),
                -- 2. (Try to) Determine its bounding box (via 'BOUNDS'), and
                -- 3. Translate bounding box and anchor relative to (0,0).
                --
                -- Step 2 depends heavily on the structure of blob values.
                --
                let
                  (xAnchor, yAnchor) =
                    (100, 100)
                  argsAndAnchor =
                    args ++ [eTuple [eConstDummyLoc xAnchor, eConstDummyLoc yAnchor]]
                  exp =
                    withDummyExpInfo (EApp space1 (eVar0 f) argsAndAnchor SpaceApp space0)
                  ((val, _), _) =
                    Eval.doEval syntax finalEnv exp
                      |> Utils.fromOkay "lambdaToolOptionsOf LambdaAnchor"
                in
                case val.v_ of
                  VList [v1] ->
                    case LangSvg.valToIndexedTree v1 of
                      Ok (i, it) ->
                        case Utils.justGet i it |> .interpreted of
                          LangSvg.SvgNode "g" nodeAttrs _ ->
                            case LangSvg.maybeFindBounds nodeAttrs of
                              Just bounds ->
                                let ((left,_),(top,_),(right,_),(bot,_)) = bounds in
                                debugLog "LambdaAnchor bounds found" <|
                                  Just <|
                                    { width   = round <| right - left
                                    , height  = round <| bot - top
                                    , xAnchor = round <| xAnchor - left
                                    , yAnchor = round <| yAnchor - top
                                    }
                              Nothing ->
                                debugLog "LambdaAnchor bounds not found (4)" Nothing
                          _ ->
                            debugLog "LambdaAnchor bounds not found (3)" Nothing
                      _ ->
                        debugLog "LambdaAnchor bounds not found (2)" Nothing
                  _ ->
                    debugLog "LambdaAnchor bounds not found (1)" Nothing
              in
              [LambdaAnchor
                 (withDummyExpInfo (EApp space1 (eVar0 f) args SpaceApp space0))
                 maybeViewBoxAndAnchor]
          )
      in
      lambdaCalls

    _ -> []
  -}

--------------------------------------------------------------------------------
-- Function Tool (generalized lambda tool)


preludeDrawableFunctions : List (Ident, Type)
preludeDrawableFunctions =
  getDrawableFunctions_ Parser.prelude (expEId <| LangTools.lastTopLevelExp Parser.prelude)


getDrawableFunctions : Model -> List (Ident, Type)
getDrawableFunctions model =
  getDrawableFunctions_
      model.inputExp
      (expEId <| LangTools.lastTopLevelExp model.inputExp) ++
  preludeDrawableFunctions
  |> Utils.dedupBy Tuple.first -- Remove shadowed prelude functions.


-- Supported inputs:
-- 2 Points
--
-- Supported outputs:
-- Shape
-- Point
isDrawableType : Type -> Bool
isDrawableType tipe =
  case tipe.val of
    TArrow _ argTypes _ ->
      case (Utils.maybeLast argTypes |> Maybe.map .val, Utils.dropLast 1 argTypes) of
        (Just (TApp _ retAliasType _ _), otherArgs) ->
          case retAliasType.val of
            TVar _ retAliasName ->
              if retAliasName == "Shape" || retAliasName == "Point" then
                let aliasArgIdents = List.filterMap Types.typeToMaybeAliasIdent otherArgs in
                Utils.count ((==) "Point") aliasArgIdents == 2
              else
                False
            _ -> False
        _ ->
          False
    _ ->
      False


getDrawableFunctions_ : Exp -> EId -> List (Ident, Type)
getDrawableFunctions_ program viewerEId =
  let boundExpsInScope =
    LangTools.expEnvAt_ program viewerEId
    |> Utils.fromJust_ "getDrawableFunctions_ expEnvAt_"
    |> Dict.toList
    |> List.filterMap
        (\(ident, boundExp) ->
          case boundExp of
            LangTools.Bound exp    -> Just (expEffectiveExp exp)
            LangTools.BoundUnknown -> Nothing
        )
  in
  findWithAncestorsByEId program viewerEId
  |> Utils.fromJust_ "getDrawableFunctions_ findWithAncestorsByEId"
  |> List.filterMap
      (\exp ->
        let _ = Debug.log "TODO: Draw.getDrawableFunctions_ is expected ETyp but they are now moved as ELet's Declarationss' ELetAnnotation" () in
        case (unwrapExp exp) of
          {-ETyp _ typePat tipe body _ -> -- Only single types at a time for now.
            if isDrawableType tipe then
              case LangTools.expToMaybeLetPatAndBoundExp body of
                Just (letPat, boundExp) ->
                  case (typePat.val.p__, letPat.val.p__) of
                    (PVar _ typeIdent _, PVar _ letIdent _) ->
                      if typeIdent == letIdent && List.member (expEffectiveExp boundExp) boundExpsInScope
                      then Just (typeIdent, tipe)
                      else Nothing
                    _ -> Nothing
                _ -> Nothing
            else
              Nothing-}
          _ -> Nothing
      )


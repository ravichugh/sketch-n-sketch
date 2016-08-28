module ShapeWidgets where 

import Lang exposing (..)
import LangSvg exposing (RootedIndexedTree, NodeId, ShapeKind, Attr)
import Utils

import String
import Regex


------------------------------------------------------------------------------
-- Shape Features

type Feature
  = PointFeature PointFeature
  | DistanceFeature DistanceFeature
  | OtherFeature OtherFeature

type PointFeature
  = TopLeft | TopRight  | BotLeft | BotRight
  | TopEdge | RightEdge | BotEdge | LeftEdge
  | Center
  | Point Int
  | Midpoint Int

type DistanceFeature
  = Width | Height
  | Radius
  | RadiusX | RadiusY

type OtherFeature
  = FillColor | FillOpacity
  | StrokeColor | StrokeOpacity | StrokeWidth
  | Rotation

eightPointFeatures =
  List.map PointFeature
     [ TopLeft , TopRight  , BotLeft , BotRight
     , TopEdge , RightEdge , BotEdge , LeftEdge
     ]

ninePointFeatures =
  PointFeature Center :: eightPointFeatures

simpleKindFeatures : List (ShapeKind, List Feature)
simpleKindFeatures =
  [ ( "rect", ninePointFeatures ++ List.map DistanceFeature [Width, Height])
  , ( "BOX", ninePointFeatures ++ List.map DistanceFeature [Width, Height])
  , ( "circle", ninePointFeatures ++ List.map DistanceFeature [Radius])
  , ( "OVAL", ninePointFeatures ++ List.map DistanceFeature [RadiusX, RadiusY])
  , ( "ellipse", ninePointFeatures ++ List.map DistanceFeature [RadiusX, RadiusY])
  , ( "line", List.map PointFeature [Point 1, Point 2, Center])
  ]

polyKindFeatures : ShapeKind -> List Attr -> List Feature
polyKindFeatures kind attrs =
  let cap = "polyKindFeatures" in
  let err s = Debug.crash <| Utils.spaces [cap, kind, ": ", s] in
  if kind == "polygon" then
    case (Utils.find cap attrs "points").av_ of
      LangSvg.APoints pts ->
        List.concatMap
          (\i -> [PointFeature (Point i), PointFeature (Midpoint i)])
          [1 .. List.length pts]
      _ ->
        err "bad points"
  else if kind == "path" then
    err "TODO"
  else
    err "bad shape kind"

featuresOfShape : ShapeKind -> List Attr -> List Feature
featuresOfShape kind attrs =
  case Utils.maybeFind kind simpleKindFeatures of
    Just features -> features
    Nothing       -> polyKindFeatures kind attrs

pointFeaturesOfShape : ShapeKind -> List Attr -> List PointFeature
pointFeaturesOfShape kind attrs =
  featuresOfShape kind attrs |> List.concatMap (\feature ->
    case feature of
      PointFeature pf -> [pf]
      _               -> []
  )


------------------------------------------------------------------------------
-- FeatureNum (for selecting/relating individual values)

type FeatureNum
  = X PointFeature
  | Y PointFeature
  | D DistanceFeature
  | O OtherFeature


------------------------------------------------------------------------------
-- ShapeFeature ~= a comparable version of ShapeKind + FeatureNum

-- Must be a comparable to be put in a Set
-- Otherwise, this shouldn't be a string
-- For now, these are unnecessarily entangled with ShapeKinds.
-- See sanityChecks below for the output of unparseFeatureNum.
--
type alias ShapeFeature = String

unparseFeatureNum : Maybe ShapeKind -> FeatureNum -> ShapeFeature
unparseFeatureNum mKind featureNum =
  case mKind of
    Just kind -> String.toLower kind ++ strFeatureNum kind featureNum
    Nothing   -> strFeatureNum "XXX" featureNum

strFeatureNum : ShapeKind -> FeatureNum -> ShapeFeature
strFeatureNum kind featureNum =
  case (kind, featureNum) of
    ("line", X (Point 1)) -> "X1"
    ("line", X (Point 2)) -> "X2"
    ("line", Y (Point 1)) -> "Y1"
    ("line", Y (Point 2)) -> "Y2"
    (_,      X pf)        -> strPointFeature pf "X"
    (_,      Y pf)        -> strPointFeature pf "Y"
    (_,      D df)        -> strDistanceFeature df
    (_,      O f)         -> strOtherFeature f

strPointFeature pointFeature xy =
  case pointFeature of
    TopLeft    -> "TL" ++ xy
    TopRight   -> "TR" ++ xy
    BotLeft    -> "BL" ++ xy
    BotRight   -> "BR" ++ xy
    TopEdge    -> "TC" ++ xy
    RightEdge  -> "CR" ++ xy
    BotEdge    -> "BC" ++ xy
    LeftEdge   -> "CL" ++ xy
    Center     -> "C" ++ xy
    Point i    -> "Pt" ++ xy ++ toString i
    Midpoint i -> "Midpt" ++ xy ++ toString i

strDistanceFeature distanceFeature =
  case distanceFeature of
    Width         -> "Width"
    Height        -> "Height"
    Radius        -> "R"
    RadiusX       -> "RX"
    RadiusY       -> "RY"

strOtherFeature otherFeature =
  case otherFeature of
    FillColor     -> "fill"
    StrokeColor   -> "stroke"
    FillOpacity   -> "fillOpacity"
    StrokeOpacity -> "strokeOpacity"
    StrokeWidth   -> "strokeWidth"
    Rotation      -> "rotation"

shapeKindRegexStr  = "line|rect|circle|ellipse|polygon|path|box|oval"
xShapeFeatureRegex = Regex.regex <| "^(" ++ shapeKindRegexStr ++ ")(.*)X(\\d*)$"
yShapeFeatureRegex = Regex.regex <| "^(" ++ shapeKindRegexStr ++ ")(.*)Y(\\d*)$"

parseFeatureNum : ShapeFeature -> FeatureNum
parseFeatureNum shapeFeature =
  if Regex.contains xShapeFeatureRegex shapeFeature then
    Regex.find (Regex.AtMost 1) xShapeFeatureRegex shapeFeature
      |> Utils.head_
      |> (.submatches)
      |> parseShapeFeaturePoint
      |> X
  else if Regex.contains yShapeFeatureRegex shapeFeature then
    Regex.find (Regex.AtMost 1) yShapeFeatureRegex shapeFeature
      |> Utils.head_
      |> (.submatches)
      |> parseShapeFeaturePoint
      |> Y
  else
    case shapeFeature of

      "Width"  -> D Width
      "Height" -> D Height
      "R"      -> D Radius
      "RX"     -> D RadiusX
      "RY"     -> D RadiusY

      "fill"          -> O FillColor
      "stroke"        -> O StrokeColor
      "fillOpacity"   -> O FillOpacity
      "strokeOpacity" -> O StrokeOpacity
      "strokeWidth"   -> O StrokeWidth
      "rotation"      -> O Rotation

      _ -> Debug.crash <| "parseFeatureNum: " ++ shapeFeature

parseShapeFeaturePoint matches =
  case matches of

    [Just kind, Just "TL", Just ""] -> TopLeft
    [Just kind, Just "TR", Just ""] -> TopRight
    [Just kind, Just "BL", Just ""] -> BotLeft
    [Just kind, Just "BR", Just ""] -> BotRight
    [Just kind, Just "TC", Just ""] -> TopEdge
    [Just kind, Just "CR", Just ""] -> RightEdge
    [Just kind, Just "BC", Just ""] -> BotEdge
    [Just kind, Just "CL", Just ""] -> LeftEdge
    [Just kind, Just "C" , Just ""] -> Center

    [Just kind, Just "", Just "1"] -> Point 1
    [Just kind, Just "", Just "2"] -> Point 2

    [Just kind, Just "Pt", Just s] -> Point (Utils.parseInt s)

    [Just kind, Just "MidPt", Just s] -> Midpoint (Utils.parseInt s)

    _ -> Debug.crash <| "parsePoint: " ++ toString matches


------------------------------------------------------------------------------
-- Selected Shape Features

-- String instead of ADT so that it's comparable
--
type alias SelectedType  = String
selectedTypeShapeFeature = "shapeFeature"
selectedTypeWidget       = "widget"

type alias SelectedShapeFeature = (SelectedType, NodeId, ShapeFeature)

selectedPointFeatureOf : SelectedShapeFeature -> SelectedShapeFeature
                      -> Maybe (NodeId, PointFeature)
selectedPointFeatureOf selected1 selected2 =
  let (_, id1, feature1) = selected1 in
  let (_, id2, feature2) = selected2 in
  if id1 /= id2 then Nothing
  else
    case pointFeatureOf (feature1, feature2) of
      Just pt -> Just (id1, pt)
      Nothing -> selectedPointFeatureOf selected2 selected1

pointFeatureOf : (ShapeFeature, ShapeFeature) -> Maybe PointFeature
pointFeatureOf (feature1, feature2) =
  case (parseFeatureNum feature1, parseFeatureNum feature2) of
    (X pointFeature1, Y pointFeature2) ->
      if pointFeature1 == pointFeature2
      then Just pointFeature1
      else Nothing
    _ ->
      Nothing


------------------------------------------------------------------------------

-- Keeping these Strings around to avoid pervasive changes to
-- ValueBasedTransform. Can remove them in favor of FeatureNums instead.

assertString string result =
  if result == string then string
  else Debug.crash <| Utils.spaces ["assertString:", result, "/= ", string]

sanityCheck string kind featureNum =
  assertString string (unparseFeatureNum (Just kind) featureNum)

sanityCheckOther string featureNum =
  assertString string (unparseFeatureNum Nothing featureNum)

shapeFill          = sanityCheckOther "fill" (O FillColor)
shapeStroke        = sanityCheckOther "stroke" (O StrokeColor)
shapeFillOpacity   = sanityCheckOther "fillOpacity" (O FillOpacity)
shapeStrokeOpacity = sanityCheckOther "strokeOpacity" (O StrokeOpacity)
shapeStrokeWidth   = sanityCheckOther "strokeWidth" (O StrokeWidth)
shapeRotation      = sanityCheckOther "rotation" (O Rotation)

rectTLX = sanityCheck "rectTLX" "rect" (X TopLeft)
rectTLY = sanityCheck "rectTLY" "rect" (Y TopLeft)
rectTRX = sanityCheck "rectTRX" "rect" (X TopRight)
rectTRY = sanityCheck "rectTRY" "rect" (Y TopRight)
rectBLX = sanityCheck "rectBLX" "rect" (X BotLeft)
rectBLY = sanityCheck "rectBLY" "rect" (Y BotLeft)
rectBRX = sanityCheck "rectBRX" "rect" (X BotRight)
rectBRY = sanityCheck "rectBRY" "rect" (Y BotRight)
rectTCX = sanityCheck "rectTCX" "rect" (X TopEdge)
rectTCY = sanityCheck "rectTCY" "rect" (Y TopEdge)
rectCRX = sanityCheck "rectCRX" "rect" (X RightEdge)
rectCRY = sanityCheck "rectCRY" "rect" (Y RightEdge)
rectBCX = sanityCheck "rectBCX" "rect" (X BotEdge)
rectBCY = sanityCheck "rectBCY" "rect" (Y BotEdge)
rectCLX = sanityCheck "rectCLX" "rect" (X LeftEdge)
rectCLY = sanityCheck "rectCLY" "rect" (Y LeftEdge)
rectCX  = sanityCheck "rectCX"  "rect" (X Center)
rectCY  = sanityCheck "rectCY"  "rect" (Y Center)

rectWidth  = sanityCheck "rectWidth"  "rect" (D Width)
rectHeight = sanityCheck "rectHeight" "rect" (D Height)

boxTLX = sanityCheck "boxTLX" "BOX" (X TopLeft)
boxTLY = sanityCheck "boxTLY" "BOX" (Y TopLeft)
boxTRX = sanityCheck "boxTRX" "BOX" (X TopRight)
boxTRY = sanityCheck "boxTRY" "BOX" (Y TopRight)
boxBLX = sanityCheck "boxBLX" "BOX" (X BotLeft)
boxBLY = sanityCheck "boxBLY" "BOX" (Y BotLeft)
boxBRX = sanityCheck "boxBRX" "BOX" (X BotRight)
boxBRY = sanityCheck "boxBRY" "BOX" (Y BotRight)
boxTCX = sanityCheck "boxTCX" "BOX" (X TopEdge)
boxTCY = sanityCheck "boxTCY" "BOX" (Y TopEdge)
boxCRX = sanityCheck "boxCRX" "BOX" (X RightEdge)
boxCRY = sanityCheck "boxCRY" "BOX" (Y RightEdge)
boxBCX = sanityCheck "boxBCX" "BOX" (X BotEdge)
boxBCY = sanityCheck "boxBCY" "BOX" (Y BotEdge)
boxCLX = sanityCheck "boxCLX" "BOX" (X LeftEdge)
boxCLY = sanityCheck "boxCLY" "BOX" (Y LeftEdge)
boxCX  = sanityCheck "boxCX"  "BOX" (X Center)
boxCY  = sanityCheck "boxCY"  "BOX" (Y Center)

boxWidth  = sanityCheck "boxWidth"  "BOX" (D Width)
boxHeight = sanityCheck "boxHeight" "BOX" (D Height)

ovalTLX = sanityCheck "ovalTLX" "OVAL" (X TopLeft)
ovalTLY = sanityCheck "ovalTLY" "OVAL" (Y TopLeft)
ovalTRX = sanityCheck "ovalTRX" "OVAL" (X TopRight)
ovalTRY = sanityCheck "ovalTRY" "OVAL" (Y TopRight)
ovalBLX = sanityCheck "ovalBLX" "OVAL" (X BotLeft)
ovalBLY = sanityCheck "ovalBLY" "OVAL" (Y BotLeft)
ovalBRX = sanityCheck "ovalBRX" "OVAL" (X BotRight)
ovalBRY = sanityCheck "ovalBRY" "OVAL" (Y BotRight)
ovalTCX = sanityCheck "ovalTCX" "OVAL" (X TopEdge)
ovalTCY = sanityCheck "ovalTCY" "OVAL" (Y TopEdge)
ovalCRX = sanityCheck "ovalCRX" "OVAL" (X RightEdge)
ovalCRY = sanityCheck "ovalCRY" "OVAL" (Y RightEdge)
ovalBCX = sanityCheck "ovalBCX" "OVAL" (X BotEdge)
ovalBCY = sanityCheck "ovalBCY" "OVAL" (Y BotEdge)
ovalCLX = sanityCheck "ovalCLX" "OVAL" (X LeftEdge)
ovalCLY = sanityCheck "ovalCLY" "OVAL" (Y LeftEdge)
ovalCX  = sanityCheck "ovalCX"  "OVAL" (X Center)
ovalCY  = sanityCheck "ovalCY"  "OVAL" (Y Center)

ovalRX = sanityCheck "ovalRX" "OVAL" (D RadiusX)
ovalRY = sanityCheck "ovalRY" "OVAL" (D RadiusY)

circleTCX = sanityCheck "circleTCX" "circle" (X TopEdge)
circleTCY = sanityCheck "circleTCY" "circle" (Y TopEdge)
circleCRX = sanityCheck "circleCRX" "circle" (X RightEdge)
circleCRY = sanityCheck "circleCRY" "circle" (Y RightEdge)
circleBCX = sanityCheck "circleBCX" "circle" (X BotEdge)
circleBCY = sanityCheck "circleBCY" "circle" (Y BotEdge)
circleCLX = sanityCheck "circleCLX" "circle" (X LeftEdge)
circleCLY = sanityCheck "circleCLY" "circle" (Y LeftEdge)
circleCX  = sanityCheck "circleCX"  "circle" (X Center)
circleCY  = sanityCheck "circleCY"  "circle" (Y Center)

circleR = sanityCheck "circleR" "circle" (D Radius)

ellipseTCX = sanityCheck "ellipseTCX" "ellipse" (X TopEdge)
ellipseTCY = sanityCheck "ellipseTCY" "ellipse" (Y TopEdge)
ellipseCRX = sanityCheck "ellipseCRX" "ellipse" (X RightEdge)
ellipseCRY = sanityCheck "ellipseCRY" "ellipse" (Y RightEdge)
ellipseBCX = sanityCheck "ellipseBCX" "ellipse" (X BotEdge)
ellipseBCY = sanityCheck "ellipseBCY" "ellipse" (Y BotEdge)
ellipseCLX = sanityCheck "ellipseCLX" "ellipse" (X LeftEdge)
ellipseCLY = sanityCheck "ellipseCLY" "ellipse" (Y LeftEdge)
ellipseCX  = sanityCheck "ellipseCX"  "ellipse" (X Center)
ellipseCY  = sanityCheck "ellipseCY"  "ellipse" (Y Center)

ellipseRX = sanityCheck "ellipseRX" "ellipse" (D RadiusX)
ellipseRY = sanityCheck "ellipseRY" "ellipse" (D RadiusY)

lineX1 = sanityCheck "lineX1" "line" (X (Point 1))
lineY1 = sanityCheck "lineY1" "line" (Y (Point 1))
lineX2 = sanityCheck "lineX2" "line" (X (Point 2))
lineY2 = sanityCheck "lineY2" "line" (Y (Point 2))
lineCX = sanityCheck "lineCX" "line" (X Center)
lineCY = sanityCheck "lineCY" "line" (Y Center)

pathPtX i    = sanityCheck (pathPtXPrefix ++ toString i) "path" (X (Point i))
pathPtY i    = sanityCheck (pathPtYPrefix ++ toString i) "path" (Y (Point i))
polyPtX i    = sanityCheck (polyPtXPrefix ++ toString i) "polygon" (X (Point i))
polyPtY i    = sanityCheck (polyPtYPrefix ++ toString i) "polygon" (Y (Point i))
polyMidptX i = sanityCheck (polyMidptXPrefix ++ toString i) "polygon" (X (Midpoint i))
polyMidptY i = sanityCheck (polyMidptYPrefix ++ toString i) "polygon" (Y (Midpoint i))

pathPtXPrefix = "pathPtX"
pathPtYPrefix = "pathPtY"
polyPtXPrefix = "polygonPtX"
polyPtYPrefix = "polygonPtY"
polyMidptXPrefix = "polygonMidptX"
polyMidptYPrefix = "polygonMidptY"


------------------------------------------------------------------------------
-- Feature Equations

-- Can't just use Trace because we need to introduce
-- constants not found in the program's Subst
type FeatureEquation
  = EqnVal Val
  | EqnOp Op_ (List FeatureEquation)


featureEquation : NodeId -> ShapeKind -> ShapeFeature -> List Attr -> FeatureEquation
featureEquation nodeId kind feature nodeAttrs =
  let featureNum = parseFeatureNum feature in
  featureEquationOf nodeId kind nodeAttrs featureNum


featureEquationOf : NodeId -> ShapeKind -> List Attr -> FeatureNum -> FeatureEquation
featureEquationOf id kind attrs featureNum =

  let eqnVal attr = EqnVal <| LangSvg.maybeFindAttr id kind attr attrs in
  let eqnVal2     = EqnVal <| vConst (2, dummyTrace) in
  let crash () =
    let s = unparseFeatureNum (Just kind) featureNum in
    Debug.crash <| Utils.spaces [ "featureEquationOf:", kind, s ] in

  let handleLine () =
    case featureNum of
      X (Point 1) -> eqnVal "x1"
      X (Point 2) -> eqnVal "x2"
      Y (Point 1) -> eqnVal "x1"
      Y (Point 2) -> eqnVal "x2"
      X Center    -> EqnOp Div [EqnOp Plus [eqnVal "x1", eqnVal "x2"], eqnVal2]
      Y Center    -> EqnOp Div [EqnOp Plus [eqnVal "y1", eqnVal "y2"], eqnVal2]
      _           -> crash () in

  let handleBoxyShape () =
    let equations =
      case kind of

        "rect" ->
          { left = eqnVal "x"
          , top = eqnVal "y"
          , right = EqnOp Plus [eqnVal "x", eqnVal "width"]
          , bottom = EqnOp Plus [eqnVal "y", eqnVal "height"]
          , cx = EqnOp Plus [eqnVal "x", EqnOp Div [eqnVal "width",  eqnVal2]]
          , cy = EqnOp Plus [eqnVal "y", EqnOp Div [eqnVal "height", eqnVal2]]
          , mWidth = Just <| eqnVal "width"
          , mHeight = Just <| eqnVal "height"
          , mRadius = Nothing
          , mRadiusX = Nothing
          , mRadiusY = Nothing
          }

        "BOX" ->
          { left = eqnVal "LEFT"
          , top = eqnVal "TOP"
          , right = eqnVal "RIGHT"
          , bottom = eqnVal "BOT"
          , cx = EqnOp Div [EqnOp Plus [eqnVal "LEFT", eqnVal "RIGHT"], eqnVal2]
          , cy = EqnOp Div [EqnOp Plus [eqnVal "TOP", eqnVal "BOT"], eqnVal2]
          , mWidth = Just <| EqnOp Minus [eqnVal "RIGHT", eqnVal "LEFT"]
          , mHeight = Just <| EqnOp Minus [eqnVal "BOT", eqnVal "TOP"]
          , mRadius = Nothing
          , mRadiusX = Nothing
          , mRadiusY = Nothing
          }

        "OVAL" ->
          { left = eqnVal "LEFT"
          , top = eqnVal "TOP"
          , right = eqnVal "RIGHT"
          , bottom = eqnVal "BOT"
          , cx = EqnOp Div [EqnOp Plus [eqnVal "LEFT", eqnVal "RIGHT"], eqnVal2]
          , cy = EqnOp Div [EqnOp Plus [eqnVal "TOP", eqnVal "BOT"], eqnVal2]
          , mWidth = Just <| EqnOp Minus [eqnVal "RIGHT", eqnVal "LEFT"]
          , mHeight = Just <| EqnOp Minus [eqnVal "BOT", eqnVal "TOP"]
          , mRadius = Nothing
          , mRadiusX = Nothing
          , mRadiusY = Nothing
          }

        "circle" ->
          { left = EqnOp Minus [eqnVal "cx", eqnVal "r"]
          , top = EqnOp Minus [eqnVal "cy", eqnVal "r"]
          , right = EqnOp Plus  [eqnVal "cx", eqnVal "r"]
          , bottom = EqnOp Plus  [eqnVal "cy", eqnVal "r"]
          , cx = eqnVal "cx"
          , cy = eqnVal "cy"
          , mWidth = Nothing
          , mHeight = Nothing
          , mRadius = Just <| eqnVal "r"
          , mRadiusX = Nothing
          , mRadiusY = Nothing
          }

        "ellipse" ->
          { left = EqnOp Minus [eqnVal "cx", eqnVal "rx"]
          , top = EqnOp Minus [eqnVal "cy", eqnVal "ry"]
          , right = EqnOp Plus  [eqnVal "cx", eqnVal "rx"]
          , bottom = EqnOp Plus  [eqnVal "cy", eqnVal "ry"]
          , cx = eqnVal "cx"
          , cy = eqnVal "cy"
          , mWidth = Nothing
          , mHeight = Nothing
          , mRadius = Nothing
          , mRadiusX = Just <| eqnVal "rx"
          , mRadiusY = Just <| eqnVal "ry"
          }

        _ -> crash () in

    case featureNum of

      X TopLeft   -> equations.left
      Y TopLeft   -> equations.top
      X TopRight  -> equations.right
      Y TopRight  -> equations.bottom
      X BotLeft   -> equations.left
      Y BotLeft   -> equations.bottom
      X BotRight  -> equations.right
      Y BotRight  -> equations.bottom
      X TopEdge   -> equations.cx
      Y TopEdge   -> equations.top
      X BotEdge   -> equations.cx
      Y BotEdge   -> equations.bottom
      X LeftEdge  -> equations.left
      Y LeftEdge  -> equations.cy
      X RightEdge -> equations.right
      Y RightEdge -> equations.cy
      X Center    -> equations.cx
      Y Center    -> equations.cy

      D distanceFeature ->
        let s = strDistanceFeature distanceFeature in
        let cap = Utils.spaces ["shapeFeatureEquationOf:", kind, s] in
        case distanceFeature of
          Width     -> Utils.fromJust_ cap equations.mWidth
          Height    -> Utils.fromJust_ cap equations.mHeight
          Radius    -> Utils.fromJust_ cap equations.mRadius
          RadiusX   -> Utils.fromJust_ cap equations.mRadiusX
          RadiusY   -> Utils.fromJust_ cap equations.mRadiusY

      _ -> crash () in

  let handlePath () =
    let x i = eqnVal ("x" ++ toString i) in
    let y i = eqnVal ("y" ++ toString i) in
    case featureNum of
      X (Point i) -> x i
      Y (Point i) -> y i
      _           -> crash () in

  let handlePoly () =
    let ptCount = LangSvg.getPtCount attrs in
    let x i = eqnVal ("x" ++ toString i) in
    let y i = eqnVal ("y" ++ toString i) in
    case featureNum of

      X (Point i) -> x i
      Y (Point i) -> y i

      X (Midpoint i1) ->
        let i2 = if i1 == ptCount then 1 else i1 + 1 in
        EqnOp Div [EqnOp Plus [(x i1), (x i2)], eqnVal2]
      Y (Midpoint i1) ->
        let i2 = if i1 == ptCount then 1 else i1 + 1 in
        EqnOp Div [EqnOp Plus [(y i1), (y i2)], eqnVal2]

      _  -> crash () in

  case featureNum of

    O FillColor   -> eqnVal "fill"
    O StrokeColor -> eqnVal "stroke"
    O StrokeWidth -> eqnVal "stroke-width"

    O FillOpacity ->
      case (Utils.find_ attrs "fill").av_ of
        LangSvg.AColorNum (_, Just opacity) -> EqnVal (vConst opacity)
        _                                   -> Debug.crash "featureEquationOf: fillOpacity"
    O StrokeOpacity ->
      case (Utils.find_ attrs "stroke").av_ of
        LangSvg.AColorNum (_, Just opacity) -> EqnVal (vConst opacity)
        _                                   -> Debug.crash "featureEquationOf: strokeOpacity"
    O Rotation ->
      let (rot,cx,cy) = LangSvg.toTransformRot <| Utils.find_ attrs "transform" in
      EqnVal (vConst rot)

    _ ->
      case kind of
        "line"     -> handleLine ()
        "polygon"  -> handlePoly ()
        "polyline" -> handlePoly ()
        "path"     -> handlePath ()
        _          -> handleBoxyShape ()


------------------------------------------------------------------------------
-- Point Feature Equations

type alias PointEquations = (FeatureEquation, FeatureEquation)

getPointEquations : NodeId -> ShapeKind -> List Attr -> PointFeature -> PointEquations
getPointEquations nodeId kind attrs pointFeature =
  ( featureEquationOf nodeId kind attrs (X pointFeature)
  , featureEquationOf nodeId kind attrs (Y pointFeature) )

getPrimitivePointEquations : RootedIndexedTree -> NodeId -> List (Val, Val)
getPrimitivePointEquations (_, tree) nodeId =
  case Utils.justGet_ "LangSvg.getPrimitivePoints" nodeId tree of
    LangSvg.SvgNode kind attrs _ ->
      List.concatMap (\pointFeature ->
        case getPointEquations nodeId kind attrs pointFeature of
          (EqnVal v1, EqnVal v2) -> [(v1,v2)]
          _                      -> []
      ) (pointFeaturesOfShape kind attrs)
    _ ->
      Debug.crash "LangSvg.getPrimitivePoints"

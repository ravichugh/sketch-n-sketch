module ShapeWidgets exposing (..)

import Lang exposing (..)
import FastParser
import LangUnparser
import LangSvg exposing (RootedIndexedTree, IndexedTree, NodeId, ShapeKind, Attr, AVal)
import Eval
import Provenance
import Utils
import ValUnparser exposing (strVal)
import ValWidgets

import Dict exposing (Dict)
import Regex
import Set exposing (Set)
import String


-- In most code, the plain word "feature" refers to a SelectableFeature

type PointFeature
  = TopLeft | TopRight  | BotLeft | BotRight
  | TopEdge | RightEdge | BotEdge | LeftEdge
  | Center
  | LonePoint
  | Point Int
  | Midpoint Int
  | EndPoint

type DistanceFeature
  = Width | Height
  | Radius
  | RadiusX | RadiusY
  | Offset

type OtherFeature
  = FillColor | FillOpacity
  | StrokeColor | StrokeOpacity | StrokeWidth
  | Rotation
  | Quantity


type ShapeFeature
  = XFeat PointFeature
  | YFeat PointFeature
  | DFeat DistanceFeature
  | OFeat OtherFeature


type alias SelectablePoint = (NodeId, PointFeature)

type SelectableFeature
  = ShapeFeature NodeId ShapeFeature
  | DistanceBetweenFeatures (Set SelectablePoint) -- Invariant: Set size == 2


------------------------------------------------------------------------------
-- GenericFeatures (what kind of things does each shape have?)

type GenericFeature
  = PointFeature PointFeature
  | DistanceFeature DistanceFeature
  -- | OtherFeature OtherFeature

eightGenericPointFeatures =
  List.map PointFeature
     [ TopLeft , TopRight  , BotLeft , BotRight
     , TopEdge , RightEdge , BotEdge , LeftEdge
     ]

nineGenericPointFeatures =
  eightGenericPointFeatures ++ [PointFeature Center]

simpleKindGenericFeatures : List (ShapeKind, List GenericFeature)
simpleKindGenericFeatures =
  [ ( "rect",    nineGenericPointFeatures ++ List.map DistanceFeature [Width, Height])
  , ( "BOX",     nineGenericPointFeatures ++ List.map DistanceFeature [Width, Height])
  , ( "circle",  nineGenericPointFeatures ++ List.map DistanceFeature [Radius])
  , ( "OVAL",    nineGenericPointFeatures ++ List.map DistanceFeature [RadiusX, RadiusY])
  , ( "ellipse", nineGenericPointFeatures ++ List.map DistanceFeature [RadiusX, RadiusY])
  , ( "line", List.map PointFeature [Point 1, Point 2, Center])
  ]

polyKindGenericFeatures : ShapeKind -> List Attr -> List GenericFeature
polyKindGenericFeatures kind attrs =
  let cap = "polyKindGenericFeatures" in
  let err s = Debug.crash <| Utils.spaces [cap, kind, ": ", s] in
  if kind == "polygon" then
    case (Utils.find cap attrs "points").interpreted of
      LangSvg.APoints pts ->
        List.concatMap
          (\i -> [PointFeature (Point i), PointFeature (Midpoint i)])
          (List.range 1 (List.length pts))
      _ ->
        err "polyKindGenericFeatures: points not found"
  else if kind == "path" then
    case (Utils.find cap attrs "d").interpreted of
      LangSvg.APath2 (_, pathCounts) ->
        List.concatMap
          (\i -> [PointFeature (Point i)])
          (List.range 1 (pathCounts.numPoints))
      _ ->
        err "polyKindGenericFeatures: d not found"
  else
    err <| "polyKindGenericFeatures: " ++ kind

genericFeaturesOfShape : ShapeKind -> List Attr -> List GenericFeature
genericFeaturesOfShape kind attrs =
  case (Utils.maybeFind kind simpleKindGenericFeatures, kind) of
    (Just features, _)   -> features
    (Nothing, "polygon") -> polyKindGenericFeatures kind attrs
    (Nothing, "path")    -> polyKindGenericFeatures kind attrs
    _                    -> []

pointFeaturesOfShape : ShapeKind -> List Attr -> List PointFeature
pointFeaturesOfShape kind attrs =
  genericFeaturesOfShape kind attrs |> List.concatMap (\feature ->
    case feature of
      PointFeature pf -> [pf]
      _               -> []
  )

pointFeaturesOfWidget : Widget -> List PointFeature
pointFeaturesOfWidget widget =
  case widget of
    WIntSlider _ _ _ _ _ _ _  -> []
    WNumSlider _ _ _ _ _ _ _  -> []
    WPoint _ _ _ _ _          -> [LonePoint]
    WOffset1D _ _ _ _ _ _ _ _ -> [EndPoint]
    WCall _ _ _ _ _           -> []
    WList _                   -> []


------------------------------------------------------------------------------
-- ShapeFeature (features of shapes)

-- Used only in one place for point selection, may be able to remove.
shapeFeaturesOfGenericFeature : GenericFeature -> List ShapeFeature
shapeFeaturesOfGenericFeature genericFeature =
  case genericFeature of
    PointFeature pf    -> [XFeat pf, YFeat pf]
    DistanceFeature df -> [DFeat df]
    -- OtherFeature feat  -> [OFeat feat]


shapeFeaturesOfShape : ShapeKind -> List Attr -> List ShapeFeature
shapeFeaturesOfShape kind attrs =
  genericFeaturesOfShape kind attrs
  |> List.concatMap shapeFeaturesOfGenericFeature


------------------------------------------------------------------------------
-- SelectableFeature (for selecting/relating individual values)

featuresOfShape : Int -> ShapeKind -> List Attr -> List SelectableFeature
featuresOfShape nodeId kind attrs =
  shapeFeaturesOfShape kind attrs
  |> List.map (\shapeFeature -> ShapeFeature nodeId shapeFeature)


simpleDesc : a -> String
simpleDesc adt =
  toString adt
  |> Utils.stringReplace " " ""
  |> Utils.stringReplace "(" ""
  |> Utils.stringReplace ")" ""


extractSelectablePoints : Set SelectablePoint -> (SelectablePoint, SelectablePoint)
extractSelectablePoints selectablePointsPair =
  case Set.toList selectablePointsPair of
    [pt1, pt2] -> (pt1, pt2)
    _          -> Debug.crash "extractSelectablePoints: expected distancePair set to have two elements"


selectableDistanceBetweenFeaturesDesc : Set SelectablePoint -> String -- Set size should have exactly two elements
selectableDistanceBetweenFeaturesDesc selectablePointsPair =
  let ((_{- nodeId1 -}, pointFeature1), (_{- nodeId2 -}, pointFeature2)) = extractSelectablePoints selectablePointsPair in
  simpleDesc pointFeature1 ++ "To" ++ simpleDesc pointFeature2


featureDesc : SelectableFeature -> String
featureDesc feature =
  case feature of
    ShapeFeature nodeId shapeFeature             -> simpleDesc shapeFeature -- Slightly nicer if we had shape kind here, but not worth the complexity.
    DistanceBetweenFeatures selectablePointsPair -> selectableDistanceBetweenFeaturesDesc selectablePointsPair


featureIsX : SelectableFeature -> Bool
featureIsX feature =
  case feature of
    ShapeFeature _ (XFeat _) -> True
    _                        -> False


featureIsY : SelectableFeature -> Bool
featureIsY feature =
  case feature of
    ShapeFeature _ (YFeat _) -> True
    _                        -> False


featureIsXOrY : SelectableFeature -> Bool
featureIsXOrY feature =
  featureIsX feature || featureIsY feature


featuresToMaybeSelectablePoint : SelectableFeature -> SelectableFeature -> Maybe SelectablePoint
featuresToMaybeSelectablePoint feature1 feature2 =
  case (feature1, feature2) of
    (ShapeFeature nodeId1 (XFeat pf1), ShapeFeature nodeId2 (YFeat pf2)) -> if nodeId1 == nodeId2 && pf1 == pf2 then Just (nodeId1, pf1) else Nothing
    (ShapeFeature nodeId1 (YFeat pf1), ShapeFeature nodeId2 (XFeat pf2)) -> if nodeId1 == nodeId2 && pf1 == pf2 then Just (nodeId1, pf1) else Nothing
    _                                                                      -> Nothing


featuresAreXYPairs : SelectableFeature -> SelectableFeature -> Bool
featuresAreXYPairs feature1 feature2 =
  featuresToMaybeSelectablePoint feature1 feature2
  |> Utils.maybeToBool


-- Find all XY pairs in a list of features.
featuresToSelectablePoints : List SelectableFeature -> List SelectablePoint
featuresToSelectablePoints features =
  case features of
    feature1::otherFeatures ->
      otherFeatures
      |> List.filterMap (\otherFeature -> featuresToMaybeSelectablePoint feature1 otherFeature)
      |> (++) (featuresToSelectablePoints otherFeatures)

    [] ->
      []


selectablePointToSelectableFeatures : SelectablePoint -> (SelectableFeature, SelectableFeature)
selectablePointToSelectableFeatures (nodeId, pointFeature) =
  ( ShapeFeature nodeId (XFeat pointFeature)
  , ShapeFeature nodeId (YFeat pointFeature)
  )


maybeEvaluateShapePointFeature : ShapeKind -> List Attr -> PointFeature -> Maybe (Num, Num)
maybeEvaluateShapePointFeature shapeKind shapeAttrs pointFeature =
  let (xEqn, yEqn) = getPointEquations shapeKind shapeAttrs pointFeature in
  case (evaluateFeatureEquation xEqn, evaluateFeatureEquation yEqn) of
    (Just xVal, Just yVal) -> Just (valToNum xVal, valToNum yVal)
    _                      -> Nothing


maybeEvaluateWidgetPointFeature : Widget -> PointFeature -> Maybe (Num, Num)
maybeEvaluateWidgetPointFeature widget pointFeature =
  case (widget, pointFeature) of
    (WIntSlider _ _ _ _ _ _ _, _)               -> Nothing
    (WNumSlider _ _ _ _ _ _ _, _)               -> Nothing
    (WPoint (x, xTr) _ (y, yTr) _ _, LonePoint) -> Just (x, y)
    (WOffset1D (baseX, baseXTr) (baseY, baseYTr) axis sign (amount, amountTr) _ _ _, EndPoint) ->
      let (_{- effectiveAmount -}, ((endX, _{- endXTr -}), (endY, _{- endYTr -}))) =
        offsetWidget1DEffectiveAmountAndEndPoint ((baseX, baseXTr), (baseY, baseYTr)) axis sign (amount, amountTr)
      in
      Just (endX, endY)
    (WCall _ _ _ _ _, _) -> Nothing
    (WList _, _)         -> Nothing
    _                    -> Debug.crash <| "bad feature for widget: " ++ toString pointFeature


selectablePointToMaybeXY : SelectablePoint -> LangSvg.RootedIndexedTree -> Widgets -> Maybe (Num, Num)
selectablePointToMaybeXY (nodeId, pointFeature) slate widgets =
  if nodeId < -2 then
    let idAsShape = -2 - nodeId in
    Utils.maybeGeti1 idAsShape widgets
    |> Maybe.andThen (\widget -> maybeEvaluateWidgetPointFeature widget pointFeature)
  else
    LangSvg.maybeGetSvgNode nodeId slate
    |> Maybe.andThen (\(shapeKind, shapeAttrs, _) -> maybeEvaluateShapePointFeature shapeKind shapeAttrs pointFeature)



------------------------------------------------------------------------------
-- Feature Equations

-- Can't just use Trace because we need to introduce
-- constants not found in the program's Subst
-- If need more structured values in the future,
-- add EqnVal AVal (rather than EqnVal Val).
--
-- See LocEqn.elm for a discussion of all the equation types in SnS.


type FeatureEquation
  = EqnNum Val
  | EqnOp Op_ (List FeatureEquation)


featureToEquation : SelectableFeature -> IndexedTree -> Widgets -> Maybe FeatureEquation
featureToEquation selectableFeature tree widgets =
  let vConst n = { v_ = VConst Nothing (n, dummyTrace), provenance = dummyProvenance, parents = Parents [] } in
  -- featureToEquation_ shapeFeatureEquation widgetFeatureEquation vConst selectableFeature tree widgets
  case selectableFeature of
    ShapeFeature nodeId shapeFeature ->
      if not <| nodeId < -2 then
        -- shape feature
        case Dict.get nodeId tree |> Maybe.map .interpreted of
          Just (LangSvg.SvgNode kind nodeAttrs _) ->
            Just (shapeFeatureEquation shapeFeature kind nodeAttrs)

          Just (LangSvg.TextNode _) ->
            Nothing

          Nothing ->
            Debug.crash <| "ShapeWidgets.selectableShapeFeatureToEquation " ++ toString nodeId ++ " " ++ toString tree
      else
        -- widget feature
        -- change to index widgets by position in widget list; then pull feature from widget type
        let widgetId = -nodeId - 2 in -- widget nodeId's are encoded at -2 and count down. (And they are 1-indexed, so actually they start at -3)
        case Utils.maybeGeti1 widgetId widgets of
          Just widget -> Just (widgetFeatureEquation shapeFeature widget)
          Nothing     -> Debug.crash <| "ShapeWidgets.selectableShapeFeatureToEquation can't find widget " ++ toString widgetId ++ " in " ++ toString widgets

    DistanceBetweenFeatures pointPairSet ->
      let (selectablePoint1, selectablePoint2) = extractSelectablePoints pointPairSet in
      let (x1Feature, y1Feature) = selectablePointToSelectableFeatures selectablePoint1 in
      let (x2Feature, y2Feature) = selectablePointToSelectableFeatures selectablePoint2 in
      let getEqn selectableFeature = featureToEquation selectableFeature tree widgets in
      case (getEqn x1Feature, getEqn y1Feature, getEqn x2Feature, getEqn y2Feature) of
        (Just x1Eqn, Just y1Eqn, Just x2Eqn, Just y2Eqn) ->
          let deltaXEqn = EqnOp Minus [x2Eqn, x1Eqn] in -- x2 - x1
          let deltaYEqn = EqnOp Minus [y2Eqn, y1Eqn] in -- y2 - y1
          let deltaXSquaredEqn = EqnOp Pow [deltaXEqn, eqnNumTwo] in -- (x2 - x1)^2
          let deltaYSquaredEqn = EqnOp Pow [deltaYEqn, eqnNumTwo] in -- (y2 - y1)^2
          let distanceEqn = EqnOp Sqrt [EqnOp Plus [deltaXSquaredEqn, deltaYSquaredEqn]] in -- sqrt( (x2-x1)^2 + (y2-y1)^2 )
          Just distanceEqn

        _ ->
          Nothing


shapeIdToMaybeVal : NodeId -> IndexedTree -> Widgets -> Maybe Val
shapeIdToMaybeVal nodeId shapeTree widgets =
  if -2 - nodeId > 0 then
    let widgetId = -2 - nodeId in
    case Utils.maybeGeti1 widgetId widgets of
      Just (WNumSlider _ _ _ _ val _ _)        -> Just val
      Just (WIntSlider _ _ _ _ val _ _)        -> Just val
      Just (WPoint _ _ _ _ pairVal)            -> Just pairVal
      Just (WOffset1D _ _ _ _ _ amountVal _ _) -> Just amountVal
      Just (WCall _ _ _ retVal _)              -> Just retVal
      Just (WList val)                         -> Just val
      Nothing                                  -> Nothing
  else
    Dict.get nodeId shapeTree
    |> Maybe.map .val


selectedShapeToEquation : NodeId -> IndexedTree -> Widgets -> Maybe FeatureEquation
selectedShapeToEquation nodeId shapeTree widgets =
  shapeIdToMaybeVal nodeId shapeTree widgets
  |> Maybe.map EqnNum


equationNumTrs : FeatureEquation -> List NumTr
equationNumTrs featureEqn =
  case featureEqn of
    EqnNum val   -> [valToNumTr val]
    EqnOp _ eqns -> List.concatMap equationNumTrs eqns



type alias BoxyFeatureEquations =
  { left : FeatureEquation
  , top : FeatureEquation
  , right : FeatureEquation
  , bottom : FeatureEquation
  , cx : FeatureEquation
  , cy : FeatureEquation
  , mWidth : Maybe FeatureEquation
  , mHeight : Maybe FeatureEquation
  , mRadius : Maybe FeatureEquation
  , mRadiusX : Maybe FeatureEquation
  , mRadiusY : Maybe FeatureEquation
  }



eqnNumTwo = EqnNum (Val (VConst Nothing (2, dummyTrace)) (Provenance [] (eConst0 2 dummyLoc) [] []) (Parents []))
plus a b  = EqnOp Plus [a, b]
minus a b = EqnOp Minus [a, b]
div a b   = EqnOp Div [a, b]


getAttrVal : String -> List Attr -> Val
getAttrVal attrName attrList =
  Utils.find ("featureEquation: getAttr " ++ attrName) attrList attrName |> .val


shapeFeatureEquation : ShapeFeature -> ShapeKind -> List Attr -> FeatureEquation
shapeFeatureEquation shapeFeature kind nodeAttrs =
  let getPathPoint attrList i =
    let toPointValPairs vListElems =
      let commandIsAnyOf cmd options = String.contains (String.toUpper cmd) options in
      case vListElems of
        []     -> []
        v::rest -> case v.v_ of
          VBase (VString cmd) ->
            if commandIsAnyOf cmd "Z" then
              toPointValPairs rest
            else if commandIsAnyOf cmd "MLT" then
              case Utils.split 2 rest of
                ([xVal, yVal], rest) -> (xVal, yVal) :: toPointValPairs rest
                _                    -> let _ = Utils.log "toPointValPairs MLT parse fail" in []
            else if commandIsAnyOf cmd "HV" then
              toPointValPairs (List.drop 1 rest)
            else if commandIsAnyOf cmd "C" then
              case Utils.split 6 rest of
                ([x1,y1,x2,y2,x,y], rest) -> (x1,y1) :: (x2,y2) :: (x,y) :: toPointValPairs rest
                _                         -> let _ = Utils.log "toPointValPairs C parse fail" in []
            else if commandIsAnyOf cmd "SQ" then
              case Utils.split 4 rest of
                ([x1,y1,x,y], rest) -> (x1,y1) :: (x,y) :: toPointValPairs rest
                _                   -> let _ = Utils.log "toPointValPairs SQ parse fail" in []
            else if commandIsAnyOf cmd "A" then
              case Utils.split 7 rest of
                ([rx,ry,axis,flag,sweep,x,y], rest) -> (x,y) :: toPointValPairs rest
                _                                   -> let _ = Utils.log "toPointValPairs A parse fail" in []
            else
              let _ = Utils.log ("toPointValPairs bad command " ++ cmd) in
              []

          _ ->
            let _ = Utils.log ("toPointValPairs expected command string, got " ++ strVal v) in
            []
    in
    case (Utils.find "featureEquation: getPathPoint d" attrList "d").val.v_ of
      VList cmds -> toPointValPairs cmds |> Utils.geti i
      _          -> Debug.crash "featureEquation: getPathPoint2"
  in
  let getPolyPoint attrList i =
    case (Utils.find "featureEquation: getPolyPoint" attrList "points").val.v_ of
      VList points ->
        case (Utils.geti i points).v_ of
          VList [xVal, yVal] -> (xVal, yVal)
          _                  -> Debug.crash "featureEquation: getPolyPoint2"
      _            -> Debug.crash "featureEquation: getPolyPoint3"
  in
  let toOpacity attrVal =
    case attrVal.val.v_ of
      VList [_, opacityVal] -> opacityVal
      _                     -> Debug.crash "featureEquation: toOpacity"
  in
  let toTransformRot attrVal =
    case attrVal.val.v_ of
      VList [cmd, rot, cx, cy] -> if cmd.v_ == VBase (VString "rotate") then (rot, cx, cy) else Debug.crash "featureEquation: bad rotate command"
      _                        -> Debug.crash "featureEquation: toTransformRot"
  in
  let get attr  = EqnNum <| getAttrVal attr nodeAttrs in
  let crash _ = -- Elm compiler crashes if this is instead written as "let crash () ="
    Debug.crash <| Utils.spaces [ "shapeFeatureEquationOf:", kind, toString shapeFeature ] in

  let handleLine () =
    case shapeFeature of
      XFeat (Point 1) -> get "x1"
      XFeat (Point 2) -> get "x2"
      YFeat (Point 1) -> get "y1"
      YFeat (Point 2) -> get "y2"
      XFeat Center    -> div (plus (get "x1") (get "x2")) eqnNumTwo
      YFeat Center    -> div (plus (get "y1") (get "y2")) eqnNumTwo
      _           -> crash () in

  let handleBoxyShape () =
    let equations = boxyFeatureEquations kind nodeAttrs in
    case shapeFeature of

      XFeat TopLeft   -> equations.left
      YFeat TopLeft   -> equations.top
      XFeat TopRight  -> equations.right
      YFeat TopRight  -> equations.top
      XFeat BotLeft   -> equations.left
      YFeat BotLeft   -> equations.bottom
      XFeat BotRight  -> equations.right
      YFeat BotRight  -> equations.bottom
      XFeat TopEdge   -> equations.cx
      YFeat TopEdge   -> equations.top
      XFeat BotEdge   -> equations.cx
      YFeat BotEdge   -> equations.bottom
      XFeat LeftEdge  -> equations.left
      YFeat LeftEdge  -> equations.cy
      XFeat RightEdge -> equations.right
      YFeat RightEdge -> equations.cy
      XFeat Center    -> equations.cx
      YFeat Center    -> equations.cy

      DFeat distanceFeature ->
        let cap = Utils.spaces ["shapeFeatureEquationOf:", kind, toString shapeFeature] in
        case distanceFeature of
          Width     -> Utils.fromJust_ cap equations.mWidth
          Height    -> Utils.fromJust_ cap equations.mHeight
          Radius    -> Utils.fromJust_ cap equations.mRadius
          RadiusX   -> Utils.fromJust_ cap equations.mRadiusX
          RadiusY   -> Utils.fromJust_ cap equations.mRadiusY
          _         -> crash ()

      _ -> crash () in

  let handlePath () =
    let x i = EqnNum <| Tuple.first <| getPathPoint nodeAttrs i in
    let y i = EqnNum <| Tuple.second <| getPathPoint nodeAttrs i in
    case shapeFeature of
      XFeat (Point i) -> x i
      YFeat (Point i) -> y i
      _           -> crash () in

  let handlePoly () =
    let ptCount = LangSvg.getPtCount nodeAttrs in
    let x i = EqnNum <| Tuple.first <| getPolyPoint nodeAttrs i in
    let y i = EqnNum <| Tuple.second <| getPolyPoint nodeAttrs i in
    case shapeFeature of

      XFeat (Point i) -> x i
      YFeat (Point i) -> y i

      XFeat (Midpoint i1) ->
        let i2 = if i1 == ptCount then 1 else i1 + 1 in
        div (plus (x i1) (x i2)) eqnNumTwo
      YFeat (Midpoint i1) ->
        let i2 = if i1 == ptCount then 1 else i1 + 1 in
        div (plus (y i1) (y i2)) eqnNumTwo

      _  -> crash () in

  case shapeFeature of

    OFeat FillColor   -> get "fill"
    OFeat StrokeColor -> get "stroke"
    OFeat StrokeWidth -> get "stroke-width"

    OFeat FillOpacity   -> EqnNum <| toOpacity <| Utils.find_ nodeAttrs "fill"
    OFeat StrokeOpacity -> EqnNum <| toOpacity <| Utils.find_ nodeAttrs "stroke"
    OFeat Rotation ->
      let (rot,_{- cx -},_{- cy -}) = toTransformRot <| Utils.find_ nodeAttrs "transform" in
      EqnNum rot

    _ ->
      case kind of
        "line"     -> handleLine ()
        "polygon"  -> handlePoly ()
        "polyline" -> handlePoly ()
        "path"     -> handlePath ()
        "rect"     -> handleBoxyShape ()
        "BOX"      -> handleBoxyShape ()
        "circle"   -> handleBoxyShape ()
        "ellipse"  -> handleBoxyShape ()
        "OVAL"     -> handleBoxyShape ()
        _          -> crash ()


widgetFeatureEquation : ShapeFeature -> Widget -> FeatureEquation
widgetFeatureEquation shapeFeature widget =
  case widget of
    WIntSlider low high caption curVal valVal (locId,_,_) _ -> EqnNum valVal
    WNumSlider low high caption curVal valVal (locId,_,_) _ -> EqnNum valVal
    WPoint (x, xTr) xVal (y, yTr) yVal pairVal ->
      case shapeFeature of
        XFeat LonePoint -> EqnNum xVal
        YFeat LonePoint -> EqnNum yVal
        _               -> Debug.crash <| "widgetFeatureEquation WPoint only supports XFeat LonePoint and YFeat LonePoint; but asked for " ++ toString shapeFeature
    WOffset1D (baseX, baseXTr) (baseY, baseYTr) axis sign (amount, amountTr) amountVal endXVal endYVal ->
      case shapeFeature of
        DFeat Offset   -> EqnNum amountVal
        XFeat EndPoint -> EqnNum endXVal
        YFeat EndPoint -> EqnNum endYVal
        _              -> Debug.crash <| "widgetFeatureEquation WOffset1D only supports DFeat Offset, XFeat EndPoint, and YFeat EndPoint; but asked for " ++ toString shapeFeature
    WCall callEId funcVal argVals retVal retWs ->
      Debug.crash <| "WCall does not have any feature val equations, but asked for " ++ toString shapeFeature
    WList val ->
      Debug.crash <| "WList does not have any feature val equations, but asked for " ++ toString shapeFeature


boxyFeatureEquations : ShapeKind -> List Attr -> BoxyFeatureEquations
boxyFeatureEquations kind attrs =
  let get attr  = EqnNum <| getAttrVal attr attrs in
  case kind of

    "rect" ->
      { left     = get "x"
      , top      = get "y"
      , right    = plus (get "x") (get "width")
      , bottom   = plus (get "y") (get "height")
      , cx       = plus (get "x") (div (get "width") eqnNumTwo)
      , cy       = plus (get "y") (div (get "height") eqnNumTwo)
      , mWidth   = Just <| get "width"
      , mHeight  = Just <| get "height"
      , mRadius  = Nothing
      , mRadiusX = Nothing
      , mRadiusY = Nothing
      }

    "BOX" ->
      { left     = get "LEFT"
      , top      = get "TOP"
      , right    = get "RIGHT"
      , bottom   = get "BOT"
      , cx       = div (plus (get "LEFT") (get "RIGHT")) eqnNumTwo
      , cy       = div (plus (get "TOP") (get "BOT")) eqnNumTwo
      , mWidth   = Just <| minus (get "RIGHT") (get "LEFT")
      , mHeight  = Just <| minus (get "BOT") (get "TOP")
      , mRadius  = Nothing
      , mRadiusX = Nothing
      , mRadiusY = Nothing
      }

    "OVAL" ->
      { left     = get "LEFT"
      , top      = get "TOP"
      , right    = get "RIGHT"
      , bottom   = get "BOT"
      , cx       = div (plus (get "LEFT") (get "RIGHT")) eqnNumTwo
      , cy       = div (plus (get "TOP") (get "BOT")) eqnNumTwo
      , mWidth   = Nothing
      , mHeight  = Nothing
      , mRadius  = Nothing
      , mRadiusX = Just <| div (minus (get "RIGHT") (get "LEFT")) eqnNumTwo
      , mRadiusY = Just <| div (minus (get "BOT") (get "TOP")) eqnNumTwo
      }

    "circle" ->
      { left     = minus (get "cx") (get "r")
      , top      = minus (get "cy") (get "r")
      , right    = plus (get "cx") (get "r")
      , bottom   = plus (get "cy") (get "r")
      , cx       = get "cx"
      , cy       = get "cy"
      , mWidth   = Nothing
      , mHeight  = Nothing
      , mRadius  = Just <| get "r"
      , mRadiusX = Nothing
      , mRadiusY = Nothing
      }

    "ellipse" ->
      { left     = minus (get "cx") (get "rx")
      , top      = minus (get "cy") (get "ry")
      , right    = plus (get "cx") (get "rx")
      , bottom   = plus (get "cy") (get "ry")
      , cx       = get "cx"
      , cy       = get "cy"
      , mWidth   = Nothing
      , mHeight  = Nothing
      , mRadius  = Nothing
      , mRadiusX = Just <| get "rx"
      , mRadiusY = Just <| get "ry"
      }

    _ -> Debug.crash <| "boxyFeatureEquations: " ++ kind


evaluateFeatureEquation : FeatureEquation -> Maybe Val
evaluateFeatureEquation eqn =
  case eqn of
    EqnNum val ->
      Just val

    EqnOp op [left, right] ->
      let maybeLeftResult = evaluateFeatureEquation left in
      let maybeRightResult = evaluateFeatureEquation right in
      case (maybeLeftResult, maybeRightResult) of
        (Just leftResult, Just rightResult) -> Eval.simpleEvalToMaybeVal (eOp op [eHoleVal leftResult, eHoleVal rightResult])
        _                                   -> Nothing

    _ -> Nothing


evaluateFeatureEquation_ : FeatureEquation -> Val
evaluateFeatureEquation_ =
  Utils.fromJust_ "evaluateFeatureEquation_" << evaluateFeatureEquation


evaluateLineFeatures : List Attr -> (Num, Num, Num, Num, Num, Num)
evaluateLineFeatures attrs =
  [ XFeat (Point 1), YFeat (Point 1)
  , XFeat (Point 2), YFeat (Point 2)
  , XFeat Center, YFeat Center
  ]
  |> List.map (\shapeFeature -> shapeFeatureEquation shapeFeature "line" attrs |> evaluateFeatureEquation_ |> valToNum)
  |> Utils.unwrap6


type alias BoxyNums =
  { left : Num , top : Num , right : Num , bot : Num , width : Num , height : Num
  , cx : Num , cy : Num
  , rx : Num , ry : Num , r : Num
  }


evaluateBoxyNums : ShapeKind -> List Attr -> BoxyNums
evaluateBoxyNums kind attrs =
  let equations = boxyFeatureEquations kind attrs in
  let (left, top, right, bot, cx, cy) =
    ( evaluateFeatureEquation_ equations.left   |> valToNum
    , evaluateFeatureEquation_ equations.top    |> valToNum
    , evaluateFeatureEquation_ equations.right  |> valToNum
    , evaluateFeatureEquation_ equations.bottom |> valToNum
    , evaluateFeatureEquation_ equations.cx     |> valToNum
    , evaluateFeatureEquation_ equations.cy     |> valToNum
    )
  in
  let
    width  = right - left
    height = bot - top
    rx     = width / 2
    ry     = height / 2
  in
  { left = left, top = top, right = right, bot = bot
  , width = width, height = height
  , cx = cx, cy = cy
  , rx = rx, ry = ry
  , r = rx
  }


------------------------------------------------------------------------------
-- Point Feature Equations

type alias PointEquations = (FeatureEquation, FeatureEquation)

getPointEquations : ShapeKind -> List Attr -> PointFeature -> PointEquations
getPointEquations kind attrs pointFeature =
  ( shapeFeatureEquation (XFeat pointFeature) kind attrs
  , shapeFeatureEquation (YFeat pointFeature) kind attrs )

-- Only used for some blob transform (may be able to discard sometime)
getPrimitivePointNumTrs : RootedIndexedTree -> NodeId -> List (NumTr, NumTr)
getPrimitivePointNumTrs (_, tree) nodeId =
  case Utils.justGet_ "LangSvg.getPrimitivePoints" nodeId tree |> .interpreted of
    LangSvg.SvgNode kind attrs _ ->
      List.concatMap (\pointFeature ->
        case getPointEquations kind attrs pointFeature of
          (EqnNum v1, EqnNum v2) -> [(valToNumTr v1, valToNumTr v2)]
          _                      -> []
      ) (pointFeaturesOfShape kind attrs)
    _ ->
      Debug.crash "LangSvg.getPrimitivePoints"


------------------------------------------------------------------------------
-- Shape Bounds

-- Enclosing bounding box
enclosureOfBoundsPair : (Num, Num, Num, Num) -> (Num, Num, Num, Num) -> (Num, Num, Num, Num)
enclosureOfBoundsPair (left1, top1, right1, bot1) (left2, top2, right2, bot2) =
  ( min  left1  left2
  , min   top1   top2
  , max right1 right2
  , max   bot1   bot2
  )

maybeBoundsIntersection : (Num, Num, Num, Num) -> (Num, Num, Num, Num) -> Maybe (Num, Num, Num, Num)
maybeBoundsIntersection (left1, top1, right1, bot1) (left2, top2, right2, bot2) =
  let (left, top, right, bot) =
    ( max  left1  left2
    , max   top1   top2
    , min right1 right2
    , min   bot1   bot2
    )
  in
  if left > right || top > bot
  then Nothing
  else Just (left, top, right, bot)


maybeEnclosureOfAllBounds : List (Num, Num, Num, Num) -> Maybe (Num, Num, Num, Num)
maybeEnclosureOfAllBounds bounds =
  case bounds of
    []          -> Nothing
    first::rest -> Just (rest |> List.foldl enclosureOfBoundsPair first)


maybeEnclosureOfAllPoints : List (Num, Num) -> Maybe (Num, Num, Num, Num)
maybeEnclosureOfAllPoints points =
  points
  |> List.map (\(x, y) -> (x, y, x, y))
  |> maybeEnclosureOfAllBounds


boundsCenter : (Num, Num, Num, Num) -> (Num, Num)
boundsCenter (left, top, right, bot) =
  ( (left + right) / 2.0
  , (top  + bot)   / 2.0
  )


boundsArea : (Num, Num, Num, Num) -> Num
boundsArea (left, top, right, bot) =
  (right - left) * (bot - top) |> abs


boundsContains : (Num, Num, Num, Num) -> (Num, Num) -> Bool
boundsContains (left, top, right, bot) (x, y) =
  left <= x && x <= right &&
  top  <= y && y <= bot


expandBounds : Num -> Num -> (Num, Num, Num, Num) -> (Num, Num, Num, Num)
expandBounds padding extraTopPadding (left, top, right, bot) =
  ( left  - padding
  , top   - padding - extraTopPadding
  , right + padding
  , bot   + padding
  )


valToMaybeBounds : Val -> Maybe (Num, Num, Num, Num)
valToMaybeBounds val =
  case (valToMaybeAnnotatedPoint val, valToMaybePoint val) of
    (Just (x, y), _) -> Just (x, y, x, y)
    (_, Just (x, y)) -> Just (x, y, x, y) -- (Assume pair of nums is a point.)
    _ ->
      case val.v_ of
        VList vals ->
          case LangSvg.svgValToIndexedTree val of
            Ok (_, shapeDict) ->
              let shapeNodes = Dict.values shapeDict in
              shapeNodes
              |> List.filterMap maybeShapeBounds
              |> maybeEnclosureOfAllBounds

            Err _ ->
              -- Maybe this is a list of shapes or points?
              vals
              |> List.filterMap valToMaybeBounds
              |> maybeEnclosureOfAllBounds -- Returns nothing if input list empty (no shapes found)

        _ -> Nothing


valToMaybeAnnotatedPoint : Val -> Maybe (Num, Num)
valToMaybeAnnotatedPoint val =
  case val.v_ of
    VConst (Just (X, (y, _), _)) (x, _) -> Just (x, y)
    VConst (Just (Y, (x, _), _)) (y, _) -> Just (x, y)
    VList vals ->
      case List.map .v_ vals of
        [ VConst (Just (X, _, _)) (x, _)
        , VConst (Just (Y, _, _)) (y, _) ] -> Just (x, y)
        _                             -> Nothing
    _ -> Nothing


-- Returns Maybe (left, top, right, bot)
maybeShapeBounds : LangSvg.IndexedTreeNode -> Maybe (Num, Num, Num, Num)
maybeShapeBounds svgNode =
  case svgNode.interpreted of
    LangSvg.TextNode _ -> Nothing
    LangSvg.SvgNode shapeKind shapeAttrs childIds ->
      pointFeaturesOfShape shapeKind shapeAttrs
      |> List.filterMap (maybeEvaluateShapePointFeature shapeKind shapeAttrs)
      |> pointsToMaybeBounds


heightForWCallPats     = 25
heightForWCallFuncName = 0 -- 25
heightForWListExp      = 0 -- 25
widgetBoundsPadding    = 12 -- 15


widgetTopSpaceNeeded : Widget -> Num
widgetTopSpaceNeeded widget =
  case widget of
    WCall _ _ _ _ _ -> heightForWCallFuncName
    WList _         -> heightForWListExp
    _               -> 0


-- Returns Maybe (left, top, right, bot)
maybeWidgetInitialBounds : Widget -> Maybe (Num, Num, Num, Num)
maybeWidgetInitialBounds widget =
  case widget of
    WCall callEId funcVal argVals retVal retWs ->
      retVal::argVals
      |> List.map valToMaybeBounds
      |> (++) (retWs |> List.map maybeWidgetInitialBounds)
      |> Utils.filterJusts
      |> maybeEnclosureOfAllBounds
      |> Maybe.map (expandBounds widgetBoundsPadding heightForWCallFuncName)

    WList val ->
      valToMaybeBounds val
      |> Maybe.map (expandBounds widgetBoundsPadding heightForWListExp)

    _ ->
      pointFeaturesOfWidget widget
      |> List.filterMap (maybeEvaluateWidgetPointFeature widget)
      |> pointsToMaybeBounds


-- Returns Nothing if list is empty; otherwise returns Just (left, top, right, bot)
pointsToMaybeBounds : List (Num, Num) -> Maybe (Num, Num, Num, Num)
pointsToMaybeBounds points =
  let (xs, ys) = List.unzip points in
  case Utils.projJusts [ List.minimum xs, List.minimum ys, List.maximum xs, List.maximum ys ] of
    Just [ left, top, right, bot ] -> Just (left, top, right, bot)
    _                              -> Nothing


computeAndRejiggerWidgetBounds : List Widget -> List (Maybe (Num, Num, Num, Num))
computeAndRejiggerWidgetBounds widgets =
  widgets
  |> List.map maybeWidgetInitialBounds
  |> rejiggerWidgetBounds widgets


-- Try to decrease the number of overlapping widgets.
--
-- 1. Build DAG declaring "x contains y"
--    "x contains y" here means x is larger in area than y and x covers at least 75% of y.
--    In case of area ties, the higher indexed bounds contains the lower indexed bounds but not vice versa.
-- 2. Grow widgets to enclose all the widgets they contain.
rejiggerWidgetBounds : List Widget -> List (Maybe (Num, Num, Num, Num)) -> List (Maybe (Num, Num, Num, Num))
rejiggerWidgetBounds widgets boundsMaybes =
  let
    boundsMaybesIndexed = Utils.zipi1 boundsMaybes -- |> Debug.log "boundsMaybesIndexed"

    containsDAG : List (List Int)
    containsDAG =
      boundsMaybesIndexed
      |> List.map
          (\(i, maybeBounds) ->
            case maybeBounds of
              Just thisBounds ->
                if boundsArea thisBounds == 0 then
                  []
                else
                  boundsMaybesIndexed
                  |> Utils.removei i
                  |> List.filterMap
                      (\(otherI, maybeOtherBounds) ->
                        case maybeOtherBounds of
                          Just otherBounds ->
                            if boundsArea otherBounds > 0 then
                              if boundsArea otherBounds == boundsArea thisBounds && (maybeBoundsIntersection thisBounds otherBounds |> Maybe.map boundsArea |> Maybe.withDefault 0) > 0.75 * boundsArea otherBounds then
                                if i > otherI -- Avoid cycles.
                                then Just otherI
                                else Nothing
                              else if boundsArea otherBounds <= boundsArea thisBounds && (maybeBoundsIntersection thisBounds otherBounds |> Maybe.map boundsArea |> Maybe.withDefault 0) > 0.75 * boundsArea otherBounds then
                                Just otherI
                              else
                                Nothing
                            else
                              Nothing
                          Nothing ->
                            Nothing
                      )

              Nothing ->
                []
          )
      -- |> Debug.log "containsDAG"

    -- Compute bounds after all descendent bounds have been computed.
    -- Iterate until fixpoint.
    computeMoreBounds : Dict Int (Num, Num, Num, Num) -> Dict Int (Num, Num, Num, Num)
    computeMoreBounds calculated =
      -- let _ = Debug.log "calculated" calculated in
      let newCalculated =
        List.range 1 (List.length boundsMaybes)
        |> Utils.foldl
            calculated
            (\i calculated ->
              if Dict.member i calculated then
                calculated
              else
                let
                  descendentIs = Utils.geti i containsDAG
                  maybeDescendentBounds =
                    descendentIs
                    |> List.map (flip Dict.get calculated)
                    |> Utils.projJusts
                    |> Maybe.map maybeEnclosureOfAllBounds
                in
                case (maybeDescendentBounds, Utils.geti i boundsMaybes) of
                  (Just (Just boundsToEnclose), Just thisBounds) ->
                    let
                      thisWidget = Utils.geti i widgets
                      newBounds =
                        enclosureOfBoundsPair
                            thisBounds
                            (expandBounds widgetBoundsPadding (widgetTopSpaceNeeded thisWidget) boundsToEnclose)
                    in
                    Dict.insert i newBounds calculated

                  (Just Nothing, Just thisBounds) -> -- No descendents.
                    Dict.insert i thisBounds calculated

                  _ -> -- Descendents not calculated yet or widget does not have bounds.
                    calculated

            )
      in
      if newCalculated == calculated
      then calculated
      else computeMoreBounds newCalculated

    indexToNewBounds = computeMoreBounds Dict.empty
  in
  boundsMaybes
  |> Utils.mapi1
      (\(i, maybeBounds) ->
        Utils.orMaybe
            (Dict.get i indexToNewBounds)
            maybeBounds
      )


------------------------------------------------------------------------------
-- Zones

type RealZone
  = ZInterior
  | ZPoint PointFeature
  | ZLineEdge
  | ZPolyEdge Int
  | ZOther OtherFeature   -- fill and stroke sliders
  | ZSlider               -- range annotations
  | ZOffset1D


realZoneDesc : RealZone -> String
realZoneDesc realZone =
  toString realZone
  |> Utils.stringReplace " " ""
  |> Utils.stringReplace "(" ""
  |> Utils.stringReplace ")" ""


------------------------------------------------------------------------------
-- Relating Zones and Shape Point Features

-- In View, may want to create a single SVG element for points
-- that double as selection and drag widgets. If so, then
-- eliminate this connection.
--
zoneToMaybePointFeature : RealZone -> Maybe PointFeature
zoneToMaybePointFeature realZone =
  case realZone of
    ZPoint pf -> Just pf
    _         -> Nothing


------------------------------------------------------------------------------
-- Params for Shape Widget Sliders (needed by Sync and View)

wColorSlider = 250
wStrokeWidthSlider = 60
wOpacitySlider = 20


------------------------------------------------------------------------------
-- Mapping ouput selections to code EIds for synthesis suggestions.

featureEquationToValTree : FeatureEquation -> Val
featureEquationToValTree valEqn =
  case valEqn of
    EqnNum val        -> val
    EqnOp op children ->
      let childVals = List.map featureEquationToValTree children in
      -- Only need Provenance basedOn list and the EId of the expression (dummy here)
      { v_         = VList []
      , provenance = Provenance [] (eTuple []) childVals childVals
      , parents    = Parents []
      }

-- Combinatorical explosion of interpretations.
featureEquationToEIdSets : (Exp -> Bool) -> FeatureEquation -> List (Set EId)
featureEquationToEIdSets expFilter valEqn =
  valEqn
  |> featureEquationToValTree
  |> Provenance.valTreeToAllProgramEIdInterpretationsIgnoringUninterpretedSubtrees expFilter
  -- |> Debug.log "eids"

featureEquationToProximalDistalEIdSets : (Exp -> Bool) -> FeatureEquation -> (Set EId, Set EId)
featureEquationToProximalDistalEIdSets expFilter valEqn =
  let valTree = valEqn |> featureEquationToValTree in
  ( Provenance.valTreeToMostProximalProgramEIdInterpretation expFilter valTree
  , Provenance.valTreeToMostDistalProgramEIdInterpretation expFilter valTree
  )

-- featureEquationToSingleEIds : Exp -> (Exp -> Bool) -> FeatureEquation -> List EId
-- featureEquationToSingleEIds program expFilter valEqn =
--   valEqn
--   |> featureEquationToValTree
--   |> Provenance.valTreeToSingleEIdInterpretations program expFilter

-- Only two interpretations: most proximal for each feature, and most distal.
selectionsProximalDistalEIdInterpretations : Exp -> RootedIndexedTree -> Widgets -> List SelectableFeature -> List NodeId -> Dict Int NodeId -> (Exp -> Bool) -> List (List EId)
selectionsProximalDistalEIdInterpretations program slate widgets selectedFeatures selectedShapes selectedBlobs expFilter =
  let (proximalInterps, distalInterps) =
    selectionsProximalDistalEIdInterpretations_ program slate widgets selectedFeatures selectedShapes selectedBlobs expFilter
  in
  proximalInterps ++ distalInterps |> Utils.dedup

selectionsProximalEIdInterpretations : Exp -> RootedIndexedTree -> Widgets -> List SelectableFeature -> List NodeId -> Dict Int NodeId -> (Exp -> Bool) -> List (List EId)
selectionsProximalEIdInterpretations program slate widgets selectedFeatures selectedShapes selectedBlobs expFilter =
  let (proximalInterps, _{- proximalInterps -}) =
    selectionsProximalDistalEIdInterpretations_ program slate widgets selectedFeatures selectedShapes selectedBlobs expFilter
  in
  proximalInterps

-- selectionsDistalEIdInterpretations : Exp -> RootedIndexedTree -> Widgets -> Set SelectableFeature -> Set NodeId -> Dict Int NodeId -> (Exp -> Bool) -> List (List EId)
-- selectionsDistalEIdInterpretations program slate widgets selectedFeatures selectedShapes selectedBlobs expFilter =
--   let (_{- proximalInterps -}, distalInterps) =
--     selectionsProximalDistalEIdInterpretations_ program slate widgets selectedFeatures selectedShapes selectedBlobs expFilter
--   in
--   distalInterps

selectionsUniqueProximalEIdInterpretations : Exp -> RootedIndexedTree -> Widgets -> List SelectableFeature -> List NodeId -> Dict Int NodeId -> (Exp -> Bool) -> List (List EId)
selectionsUniqueProximalEIdInterpretations program ((rootI, shapeTree) as slate) widgets selectedFeatures selectedShapes selectedBlobs expFilter =
  let eidsToNotSelect =
    -- If any shapes selected, diff against all other shapes.
    let shapeProvenanceEIdsNotToSelect =
      if List.length selectedShapes == 0 then
        Set.empty
      else
        let effectiveSelectedNodeIds =
          let selectedDescendentIds =
            selectedShapes
            |> List.filterMap (\nodeId -> Dict.get nodeId shapeTree)
            |> List.concatMap (LangSvg.descendantNodeIds shapeTree)
            |> Set.fromList
          in
          Set.union
              (Set.fromList selectedShapes)
              selectedDescendentIds
        in
        shapeTree
        |> Dict.toList
        |> List.filter (\(nodeId, shape) -> not <| Utils.anyOverlap [Set.singleton nodeId, Set.fromList (LangSvg.descendantNodeIds shapeTree shape), effectiveSelectedNodeIds])
        |> List.concatMap (\(nodeId, shape) -> Provenance.flattenValBasedOnTree shape.val)
        |> List.map (valExp >> .val >> .eid)
        -- |> List.map valExp
        -- |> List.filter (.val >> .eid >> FastParser.isProgramEId)
        -- |> List.map (\e -> let _ = Utils.log (LangUnparser.unparse e) in e)
        -- |> List.map (.val >> .eid)
        |> Set.fromList
    in
    -- TODO: If any features selected, diff against all other analogous features on other shapes.
    let featureProvenanceEIdsNotToSelect =
      if List.length selectedFeatures == 0 then
        Set.empty
      else
        Set.empty
    in
    Set.union shapeProvenanceEIdsNotToSelect featureProvenanceEIdsNotToSelect
  in
  let newExpFilter exp =
    -- Exclude expressions touched by shapes NOT selected
    (not <| Set.member exp.val.eid eidsToNotSelect)
    && expFilter exp
  in
  let (proximalInterps, _{- distalInterps -}) =
    selectionsProximalDistalEIdInterpretations_ program slate widgets selectedFeatures selectedShapes selectedBlobs newExpFilter
  in
  proximalInterps

-- EIds in interp must satisfy the predicate.
selectionsProximalDistalEIdInterpretations_ : Exp -> RootedIndexedTree -> Widgets -> List SelectableFeature -> List NodeId -> Dict Int NodeId -> (Exp -> Bool) -> (List (List EId), List (List EId))
selectionsProximalDistalEIdInterpretations_ program slate widgets selectedFeatures selectedShapes selectedBlobs expFilter =
  let (featureProximalEIds, featureDistalEIds) =
    selectedFeaturesToProximalDistalEIdInterpretations program slate widgets selectedFeatures expFilter
  in
  let (otherProximalEIds, otherDistalEIds) =
    [ selectedShapesToProximalDistalEIdInterpretations program slate widgets selectedShapes expFilter
    , selectedBlobsToProximalDistalEIdInterpretations  program slate         (Dict.toList selectedBlobs) expFilter
    ]
    |> List.unzip
    |> Utils.mapBoth Utils.unionAll
  in
  let (pointProximalInterps, pointDistalInterps) =
    selectedFeaturesToProximalDistalPointEIdInterpretations program slate widgets selectedFeatures expFilter
  in
  -- Point interps first.
  ( List.map (Set.union otherProximalEIds) pointProximalInterps ++ [Set.union featureProximalEIds otherProximalEIds] |> List.map Set.toList |> Utils.dedup
  , List.map (Set.union otherDistalEIds)   pointDistalInterps   ++ [Set.union featureDistalEIds   otherDistalEIds]   |> List.map Set.toList |> Utils.dedup
  )


-- -- Combinatorical explosion of interpretations.
-- selectionsEIdInterpretations : Exp -> RootedIndexedTree -> Widgets -> Set SelectableFeature -> Set NodeId -> Dict Int NodeId -> (Exp -> Bool) -> List (List EId)
-- selectionsEIdInterpretations program ((rootI, shapeTree) as slate) widgets selectedFeatures selectedShapes selectedBlobs expFilter =
--   selectedFeaturesToEIdInterpretationLists program slate widgets (Set.toList selectedFeatures) expFilter ++
--   selectedShapesToEIdInterpretationLists   program slate widgets (Set.toList selectedShapes)   expFilter ++
--   selectedBlobsToEIdInterpretationLists    program slate widgets (Dict.toList selectedBlobs)   expFilter
--   |> Utils.cartProdAll
--   |> List.map Utils.unionAll
--   |> List.map Set.toList
--   |> Utils.dedup


selectedValsInterpretingPoints : RootedIndexedTree -> Widgets -> List SelectableFeature -> List NodeId -> Dict Int NodeId -> List Val
selectedValsInterpretingPoints slate widgets selectedFeatures selectedShapes selectedBlobs =
  List.concat
      [ selectedFeaturesValTreesWithPoints slate widgets selectedFeatures
      , selectedShapesValTrees             slate widgets selectedShapes
      , selectedBlobsValTrees              slate         (Dict.toList selectedBlobs)
      ]


selectedVals : RootedIndexedTree -> Widgets -> List SelectableFeature -> List NodeId -> Dict Int NodeId -> List Val
selectedVals slate widgets selectedFeatures selectedShapes selectedBlobs =
  List.concat
      [ selectedFeaturesValTrees slate widgets selectedFeatures
      , selectedShapesValTrees   slate widgets selectedShapes
      , selectedBlobsValTrees    slate         (Dict.toList selectedBlobs)
      ]


selectionsEIdsTouched : Exp -> RootedIndexedTree -> Widgets -> List SelectableFeature -> List NodeId -> Dict Int NodeId -> (Exp -> Bool) -> List EId
selectionsEIdsTouched program slate widgets selectedFeatures selectedShapes selectedBlobs expFilter =
  selectedVals slate widgets selectedFeatures selectedShapes selectedBlobs
  |> List.concatMap Provenance.flattenValBasedOnTree
  |> List.map valExp
  |> List.filter expFilter
  |> List.map (.val >> .eid)
  |> Utils.dedup


-- Try to find single EIds in the program that explain everything selected.
selectionsSingleEIdInterpretations : Exp -> RootedIndexedTree -> Widgets -> List SelectableFeature -> List NodeId -> Dict Int NodeId -> (Exp -> Bool) -> List EId
selectionsSingleEIdInterpretations program slate widgets selectedFeatures selectedShapes selectedBlobs expFilter =
  let
    possibleExps = program |> flattenExpTree |> List.filter expFilter

    valTrees = selectedVals slate widgets selectedFeatures selectedShapes selectedBlobs

    directSingleEIdInterpretations =
      -- Checking exp-by-exp avoids combinatorical explosion of building all interpretations and then filtering.
      possibleExps
      |> List.filter (\exp -> valTrees |> List.all (Provenance.isPossibleSingleEIdInterpretation exp.val.eid))
      |> List.map (.val >> .eid)

    valExpIsInProgram val = FastParser.isProgramEId (valEId val)

    parentSingleEIdInterpretations =
      case valTrees of
        []          -> []
        first::_ ->
          let possibleParentVals = Provenance.flattenValBasedOnTree first |> List.concatMap valParents |> List.filter valExpIsInProgram |> Utils.dedup in
          let firstNonTrivialChildren val =
            case childVals val of
              []               -> [val]
              [singleChild]    -> firstNonTrivialChildren singleChild
              multipleChildren -> multipleChildren
          in
          possibleParentVals
          -- |> List.map (\parentVal -> let _ = Utils.log <| (++) "Possible parent: " <| LangUnparser.unparse <| valExp parentVal in parentVal)
          |> List.filter
              (\parentVal ->
                let domain = flattenValTree parentVal in
                let coveringsStillNeeded = [firstNonTrivialChildren parentVal] in
                -- Try to cover the parent's children with interpretations.
                let maybeNeededAfterCovering =
                  valTrees
                  |> Utils.foldlMaybe
                      (\valTree coveringsStillNeeded ->
                      --   let valInterps = Provenance.valInterpretationsAllInside domain valTree in
                      --   -- Remove interpretations subsumed by a more proximal interpretation.
                      --   let valInterpsSimplified =
                      --     valInterps
                      --     |> List.foldl
                      --         (\valInterp valInterps ->
                      --           valInterps
                      --           |> List.filter (\existingValInterp -> not (valInterp /= existingValInterp && Utils.isSublistAsSet existingValInterp (List.concatMap flattenValTree valInterp)))
                      --         )
                      --         valInterps
                      --   in
                        let valInterpsSimplified = Provenance.proximalValInterpretationsAllInside domain valTree in
                        case valInterpsSimplified of
                          [] -> Nothing -- Value can't be interpreted inside parent, give up on this parent.
                          _  ->
                            Utils.cartProd coveringsStillNeeded valInterpsSimplified
                            |> List.map (\(aCoveringNeeded, valInterp) -> Utils.diffAsSet aCoveringNeeded (List.concatMap flattenValTree valInterp))
                            |> Utils.dedup
                            |> Just
                      )
                      (Just coveringsStillNeeded)
                in
                case maybeNeededAfterCovering of
                  Nothing                   -> False
                  Just coveringsStillNeeded -> List.any ((==) []) coveringsStillNeeded
              )
          -- |> List.map (\parentVal -> let _ = Utils.log <| (++) "Passing parent: " <| LangUnparser.unparse <| valExp parentVal in parentVal)
          |> List.map valExp
          |> List.filter expFilter
          |> List.map (.val >> .eid)
  in
  directSingleEIdInterpretations ++ parentSingleEIdInterpretations


uniqueNonVarSingleExpressionInterpretations : Exp -> RootedIndexedTree -> Widgets -> List SelectableFeature -> List NodeId -> Dict Int NodeId -> (Exp -> Bool) -> List EId
uniqueNonVarSingleExpressionInterpretations program slate widgets selectedFeatures selectedShapes selectedBlobs expFilter =
  let
    singleExpressionInterpretationEIds =
      selectionsSingleEIdInterpretations
          program
          slate
          widgets
          selectedFeatures
          selectedShapes
          selectedBlobs
          (\e -> expFilter e && not (isVar (expEffectiveExp e)))
      |> Set.fromList
  in
  selectionsUniqueProximalEIdInterpretations
      program
      slate
      widgets
      selectedFeatures
      selectedShapes
      selectedBlobs
      (\e -> Set.member e.val.eid singleExpressionInterpretationEIds)
  |> List.filterMap Utils.maybeUnwrap1


-- Heuristic: Closest and farthest interpretation only.
selectedFeaturesToProximalDistalEIdInterpretations : Exp -> RootedIndexedTree -> Widgets -> List SelectableFeature -> (Exp -> Bool) -> (Set EId, Set EId)
selectedFeaturesToProximalDistalEIdInterpretations program ((rootI, shapeTree) as slate) widgets selectedFeatures expFilter =
  let (proximalEIdSets, distalEIdSets) =
    selectedFeatures
    |> List.map
        (\feature ->
          featureToEquation feature shapeTree widgets
          |> Utils.fromJust_ "selectedFeaturesToEIdLists: can't make feature into val equation"
          |> featureEquationToProximalDistalEIdSets expFilter
        )
    |> List.unzip
  in
  ( Utils.unionAll proximalEIdSets
  , Utils.unionAll distalEIdSets
  )

-- Returns multiple proximal and distal interpretations (b/c of some edge cases where most proximal/distal for x is not the most proximal/distal for y).
--
-- Features not part of points are still interpretated.
selectedFeaturesToProximalDistalPointEIdInterpretations : Exp -> RootedIndexedTree -> Widgets -> List SelectableFeature -> (Exp -> Bool) -> (List (Set EId), List (Set EId))
selectedFeaturesToProximalDistalPointEIdInterpretations program ((rootI, shapeTree) as slate) widgets selectedFeatures expFilter =
  let recurse remainingFeatures =
    selectedFeaturesToProximalDistalPointEIdInterpretations program slate widgets remainingFeatures expFilter
  in
  case selectedFeatures of
    [] -> ([Set.empty], [Set.empty])
    selectableFeature::rest ->
      let returnNotPartOfAPoint () =
        let (thisProximalInterp, thisDistalInterp) =
          featureToEquation selectableFeature shapeTree widgets
          |> Utils.fromJust_ "selectedFeaturesToProximalDistalPointEIdInterpretations0: can't make feature into val equation"
          |> featureEquationToProximalDistalEIdSets expFilter
        in
        let (remainingProximalInterps, remainingDistalInterps) = recurse rest in
        ( remainingProximalInterps |> List.map (Set.union thisProximalInterp)
        , remainingDistalInterps   |> List.map (Set.union thisDistalInterp)
        )
      in
      -- Try to interpret as point?
      case rest |> Utils.findFirst (\otherSelectableFeature -> featuresAreXYPairs selectableFeature otherSelectableFeature) of
        Just otherSelectableFeature ->
          let (xSelectableFeature, ySelectableFeature) =
            if featureIsX selectableFeature
            then (selectableFeature, otherSelectableFeature)
            else (otherSelectableFeature, selectableFeature)
          in
          let xValEqn = featureToEquation xSelectableFeature shapeTree widgets |> Utils.fromJust_ "selectedFeaturesToEIdLists1: can't make feature into val equation" in
          let yValEqn = featureToEquation ySelectableFeature shapeTree widgets |> Utils.fromJust_ "selectedFeaturesToEIdLists2: can't make feature into val equation" in
          let xValTree = featureEquationToValTree xValEqn in
          let yValTree = featureEquationToValTree yValEqn in
          let (proximalInterp1, proximalInterp2, distalInterp1, distalInterp2) =
            Provenance.valsToProximalDistalPointInterpretations expFilter xValTree yValTree
          in
          -- If code written correctly, there should be a proximal interp iff there is an distal interp.
          if (proximalInterp1 /= Set.empty || proximalInterp2 /= Set.empty) && (distalInterp1 /= Set.empty || distalInterp2 /= Set.empty) then
            let (remainingProximalInterps, remainingDistalInterps) = recurse (Utils.removeAsSet otherSelectableFeature rest) in
            ( remainingProximalInterps |> Utils.cartProd (List.filter ((/=) Set.empty) [proximalInterp1, proximalInterp2] |> Utils.dedup) |> List.map (uncurry Set.union)
            , remainingDistalInterps   |> Utils.cartProd (List.filter ((/=) Set.empty) [distalInterp1,   distalInterp2]   |> Utils.dedup) |> List.map (uncurry Set.union)
            )
          else if proximalInterp1 /= Set.empty || proximalInterp2 /= Set.empty then
            Debug.crash <| "selectedFeaturesToProximalDistalPointEIdInterpretations: proximal interpretation exists but distal does not!\n" ++ toString (proximalInterp1, proximalInterp2, distalInterp1, distalInterp2)
          else if distalInterp1 /= Set.empty || distalInterp2 /= Set.empty then
            Debug.crash <| "selectedFeaturesToProximalDistalPointEIdInterpretations: distal interpretation exists but proximal does not!\n" ++ toString (proximalInterp1, proximalInterp2, distalInterp1, distalInterp2)
          else
            returnNotPartOfAPoint ()

        Nothing ->
          returnNotPartOfAPoint ()


-- No special handling of points.
selectedFeaturesValTrees : RootedIndexedTree -> Widgets -> List SelectableFeature -> List Val
selectedFeaturesValTrees ((rootI, shapeTree) as slate) widgets selectedFeatures =
  selectedFeatures
  |> List.map
      (\feature ->
        featureToEquation feature shapeTree widgets
        |> Utils.fromJust_ "selectedFeaturesValTrees: can't make shape into val equation"
        |> featureEquationToValTree
      )


selectedFeaturesValTreesWithPoints : RootedIndexedTree -> Widgets -> List SelectableFeature -> List Val
selectedFeaturesValTreesWithPoints slate widgets selectedFeatures =
  selectedFeaturesValTrees slate widgets selectedFeatures
  |> Provenance.consolidatePointPartsIntoPoints


-- Combinatorical explosion of interpretations.
selectedFeaturesToEIdInterpretationLists : Exp -> RootedIndexedTree -> Widgets -> List SelectableFeature -> (Exp -> Bool) -> List (List (Set EId))
selectedFeaturesToEIdInterpretationLists program ((rootI, shapeTree) as slate) widgets selectedFeatures expFilter =
  let recurse remainingFeatures =
    selectedFeaturesToEIdInterpretationLists program slate widgets remainingFeatures expFilter
  in
  case selectedFeatures of
    [] -> []
    selectableFeature::rest ->
      let eidSets = featureEquationToEIdSets expFilter <| Utils.fromJust_ "selectedFeaturesToEIdLists: can't make feature into val equation" <| featureToEquation selectableFeature shapeTree widgets in
      -- Try to interpret as point?
      case rest |> Utils.findFirst (\otherSelectableFeature -> featuresAreXYPairs selectableFeature otherSelectableFeature) of
        Just otherSelectableFeature ->
          let otherEIdSets = featureEquationToEIdSets expFilter <| Utils.fromJust_ "selectedFeaturesToEIdLists2: can't make feature into val equation" <| featureToEquation otherSelectableFeature shapeTree widgets in
          let singletonEIdSets      = eidSets      |> List.filter (Set.size >> (==) 1) in
          let singletonOtherEIdSets = otherEIdSets |> List.filter (Set.size >> (==) 1) in
          let pointTuples =
            Utils.cartProd singletonEIdSets singletonOtherEIdSets
            |> List.filterMap
                (\(eidSingleton, otherEIdSingleton) ->
                  case ( parentByEId program (Utils.unwrapSingletonSet eidSingleton)
                       , parentByEId program (Utils.unwrapSingletonSet otherEIdSingleton) ) of
                    (Just (Just parent), Just (Just otherParent)) ->
                      if isPair parent && parent == otherParent
                      then Just parent.val.eid
                      else Nothing

                    _ -> Nothing
                )
          in
          case pointTuples of
            []   -> eidSets :: recurse rest
            _::_ -> List.map Set.singleton pointTuples :: recurse (Utils.removeAsSet otherSelectableFeature rest)

        Nothing ->
          eidSets :: recurse rest


selectedShapesToProximalDistalEIdInterpretations : Exp -> RootedIndexedTree -> Widgets -> List NodeId -> (Exp -> Bool) -> (Set EId, Set EId)
selectedShapesToProximalDistalEIdInterpretations program ((rootI, shapeTree) as slate) widgets selectedShapes expFilter =
  let (proximalEIdSets, distalEIdSets) =
    selectedShapes
    |> List.map
        (\nodeId ->
          selectedShapeToEquation nodeId shapeTree widgets
          |> Utils.fromJust_ "selectedShapesToProximalDistalEIdInterpretations: can't make shape into val equation"
          |> featureEquationToProximalDistalEIdSets expFilter
        )
    |> List.unzip
  in
  ( Utils.unionAll proximalEIdSets
  , Utils.unionAll distalEIdSets
  )


selectedShapesValTrees : RootedIndexedTree -> Widgets -> List NodeId -> List Val
selectedShapesValTrees ((rootI, shapeTree) as slate) widgets selectedShapes =
  selectedShapes
  |> List.map
      (\nodeId ->
        shapeIdToMaybeVal nodeId shapeTree widgets
        |> Utils.fromJust_ "selectedShapesValTrees: can't make shape into val equation"
      )


-- -- Combinatorical explosion of interpretations.
-- selectedShapesToEIdInterpretationLists : Exp -> RootedIndexedTree -> Widgets -> List NodeId -> (Exp -> Bool) -> List (List (Set EId))
-- selectedShapesToEIdInterpretationLists program ((rootI, shapeTree) as slate) widgets selectedShapes expFilter =
--   selectedShapes
--   |> List.map
--       (\nodeId ->
--         selectedShapeToEquation nodeId shapeTree
--         |> Utils.fromJust_ "selectedShapesToEIdInterpretationLists: can't make shape into val equation"
--         |> featureEquationToEIdSets expFilter
--       )


selectedBlobsToProximalDistalEIdInterpretations : Exp -> RootedIndexedTree -> List (Int, NodeId) -> (Exp -> Bool) -> (Set EId, Set EId)
selectedBlobsToProximalDistalEIdInterpretations _ _ _ _ =
  (Set.empty, Set.empty) -- blobs will go away sometime


selectedBlobsValTrees : RootedIndexedTree -> List (Int, NodeId) -> List Val
selectedBlobsValTrees _ _ =
  [] -- blobs will go away sometime


-- -- Unused. Combinatorical explosion of interpretations.
-- selectedBlobsToEIdInterpretationLists : Exp -> RootedIndexedTree -> Widgets -> List (Int, NodeId) -> (Exp -> Bool) -> List (List (Set EId))
-- selectedBlobsToEIdInterpretationLists program ((rootI, shapeTree) as slate) widgets selectedBlobs expFilter =
--   selectedBlobs
--   |> List.map (always []) -- blobs will go away sometime


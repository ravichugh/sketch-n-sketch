module Sync exposing
  ( Options, defaultOptions, syncOptionsOf
  , HeuristicMode(..)
  , expToUnfrozenLocIdSet, unfrozenTraceLocIdSet, locIsFrozen
  , LiveInfo, Triggers, LiveTrigger, ZoneKey
  , prepareLiveUpdates, prepareLiveTrigger
  , yellowAndGrayHighlights, hoverInfo
  )

import Lang exposing (..)
import ValUnparser exposing (..)
import Pos exposing (..)
import Info exposing (..)
import LangSvg exposing
  ( NodeId, ShapeKind, Attr, RootedIndexedTree, IndexedTree
  , AVal, AVal_(..), TransformCmd(..), PathCmd(..)
  )
import ShapeWidgets exposing
  ( RealZone, RealZone(..), PointFeature(..), OtherFeature(..)
  )
import ColorNum
import MathExp
import Solver
import SolverTypes
import FastParser exposing (isPreludeLoc, substPlusOf)
import Ace
import Config exposing (params)
import Utils
import Either exposing (Either(..))
import ImpureGoodies

import Dict exposing (Dict)
import Set exposing (Set)
import Debug
import String


------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugSync


------------------------------------------------------------------------------

type alias Canvas = (RootedIndexedTree, Widgets)


------------------------------------------------------------------------------
-- Sync.Options

type HeuristicMode
  = HeuristicsNone
  | HeuristicsFair
  | HeuristicsBiased

type alias Options =
  { thawedByDefault : Bool
  , heuristicsMode  : HeuristicMode
  }

defaultOptions =
  { thawedByDefault = True
  , heuristicsMode  = HeuristicsBiased
  }

syncOptionsOf oldOptions e =
  -- using oldOptions instead of defaultOptions, because want
  -- heuristicsMode to be a global flag, not per-example flag for now
  case Utils.maybeFind "unannotated-numbers" (getOptions e) of
    Nothing -> oldOptions
    Just s ->
      -- TODO decide whether to make heuristicsMode per-example or not
      --   if not, perhaps move it out of Options
      if s == "n?" then { oldOptions | thawedByDefault = True }
      else if s == "n!" then { oldOptions | thawedByDefault = False }
      else
        let _ = debugLog "invalid sync option: " s in
        oldOptions


------------------------------------------------------------------------------
-- Locations of Trace


-- Better to assume frozen (e.g. all of prelude)
expToUnfrozenLocIdSet : Options -> Exp -> Set LocId
expToUnfrozenLocIdSet options exp =
  flattenExpTree exp
  |> List.filterMap
      (\e ->
        case e.val.e__ of
          EConst _ n ((locId, _, _) as loc) _ -> if locIsFrozen options loc then Nothing else Just locId
          _                                   -> Nothing
      )
  |> Set.fromList

locIsFrozen : Options -> Loc -> Bool
locIsFrozen opts ((_,annot,_) as loc) =
  isPreludeLoc loc
  || (annot == frozen)
  || (annot == unann  && not opts.thawedByDefault)

unfrozenTraceLocIdSet : Set LocId -> Trace -> Set LocId
unfrozenTraceLocIdSet unfrozenLocIdSet trace =
  MathExp.mathExpToVarIds trace
  |> List.filter (\locId -> Set.member locId unfrozenLocIdSet)
  |> Set.fromList

-- locsOfTrace : Options -> Trace -> Set Loc
-- locsOfTrace opts trace =
  -- let locsOfTrace_ t = case t of
  --   TrLoc loc -> if locIsFrozen opts loc then [] else [loc]
  --   TrOp _ ts -> List.concatMap locsOfTrace_ ts
  -- in
  -- -- TODO do this filtering later if want gray highlights
  -- --   even when not feeling lucky
  -- let locSet = Set.fromList (locsOfTrace_ trace) in
  -- if opts.heuristicsMode == HeuristicsNone && Set.size locSet >= 2
  -- then Set.empty
  -- else locSet

unfrozenTracesLocIdSet : Set LocId -> List Trace -> Set LocId
unfrozenTracesLocIdSet unfrozenLocIdSet traces =
  traces
  |> List.map (unfrozenTraceLocIdSet unfrozenLocIdSet)
  |> Utils.unionAll


------------------------------------------------------------------------------
-- Counters

getCount x dict       = Utils.getWithDefault x 0 dict
incrementCount x dict = Dict.insert x (1 + getCount x dict) dict
addCount k x dict     = Dict.insert x (k + getCount x dict) dict


------------------------------------------------------------------------------
-- Counters for Biased Mode

type alias BiasCounts = Dict LocId Int


getLocationCounts : Set LocId -> Canvas -> BiasCounts
getLocationCounts unfrozenLocIdSet (slate, widgets) =
  let weightedScore kind (attrName, attrVal) acc =
    let k =
      -- because literal bounds will be unambiguously controlled by
      -- corner zones (and because bounding boxes are used often for
      -- stretchy shapes and groups), increase the bias against them
      case (kind, attrName, attrVal.interpreted) of
        ("BOX", "LEFT",  ANum (_, MathVar _)) -> 2
        ("BOX", "RIGHT", ANum (_, MathVar _)) -> 2
        ("BOX", "TOP",   ANum (_, MathVar _)) -> 2
        ("BOX", "BOT",   ANum (_, MathVar _)) -> 2
        _                                   -> 1
    in
    Set.foldl (addCount k) acc (unfrozenTracesLocIdSet unfrozenLocIdSet (tracesOfAVal attrVal))
  in
  {- for comparison to weightedScore:
  let unweightedScore _ (_, attrVal) acc =
    Set.foldl incrementCount acc (locsOfTraces options (tracesOfAVal attrVal))
  in
  -}
  let addTriggerNode nodeInfo acc =
    case nodeInfo of
      Left _                 -> acc
      Right (_, kind, attrs) -> List.foldl (weightedScore kind) acc attrs
  in
  let addTriggerWidget widget acc =
    case widget of
      WIntSlider _ _ _ _ _ (locId, _, _) _ -> incrementCount locId acc
      WNumSlider _ _ _ _ _ (locId, _, _) _ -> incrementCount locId acc
      WPoint (_, t1) _ (_, t2) _ _         -> Set.foldl incrementCount acc (unfrozenTracesLocIdSet unfrozenLocIdSet [t1, t2])
      WOffset1D _ _ _ _ (_, tr) _ _ _      -> Set.foldl incrementCount acc (unfrozenTraceLocIdSet unfrozenLocIdSet tr)
      WCall _ _ _ _ _                      -> acc
      WList _                              -> acc
  in
  let d  = LangSvg.foldSlateNodeInfo slate Dict.empty addTriggerNode in
  let d_ = List.foldl addTriggerWidget d widgets in
  d_


tracesOfAVals : List AVal -> List Trace
tracesOfAVals avals = List.foldl (\av acc -> tracesOfAVal av ++ acc) [] avals


tracesOfAVal : AVal -> List Trace
tracesOfAVal aval =
  case aval.interpreted of
    ANum (_,t) -> [t]

    AColorNum ((_,t), Nothing)     -> [t]
    AColorNum ((_,t), Just (_,t_)) -> [t, t_]

    APoints pts -> List.concatMap (\((_,t1),(_,t2)) -> [t1, t2]) pts

    -- TODO APath

    ATransform [Rot (_,t1) (_,t2) (_,t3)] -> [t1, t2, t3]

    AString _ -> []
    ARgba _   -> [] -- not collecting traces, because currently no RGBA widgets

    _ -> let _ = debugLog "tracesOfAVal?" (LangSvg.strAVal aval) in
         []


------------------------------------------------------------------------------
-- Counters for Fair Mode

type alias FairCounts = Dict (List (Maybe LocId)) Int


------------------------------------------------------------------------------
-- Choosing Locations for Triggers

type alias MaybeCounts = Maybe (Either BiasCounts FairCounts)


pickLocs : Subst -> Set LocId -> MaybeCounts -> List Trace -> (List (Maybe LocId), Set LocId, MaybeCounts)
pickLocs subst unfrozenLocIdSet maybeCounts traces =
  let possibleLocIdSets =
    traces
    |> List.map
        (\trace ->
          -- Validate that loc can affect trace (i.e. not multiplied by 0 or something).
          let mathExp = trace in
          unfrozenTraceLocIdSet unfrozenLocIdSet trace
          |> Set.filter
              (\locId ->
                let (_, concreteDerivative) = MathExp.applySubstAndEvaluateWithDerivative subst locId mathExp in
                not <| isNaN concreteDerivative || isInfinite concreteDerivative || 0.0 == concreteDerivative
              )
        )
  in
  let allLocs = Utils.unionAll possibleLocIdSets in
  let (assignedMaybeLocs, maybeCounts_) =
    case maybeCounts of

      Nothing ->
        (List.map (always Nothing) possibleLocIdSets, Nothing)

      Just (Left biasCounts) ->
        (List.map (chooseBiased biasCounts) possibleLocIdSets, Just (Left biasCounts))

      Just (Right fairCounts) ->
        Tuple.mapSecond (Just << Right) <|
          chooseFairLocationAssignment possibleLocIdSets fairCounts
  in
  (assignedMaybeLocs, allLocs, maybeCounts_)


chooseBiased biasCounts locIdSet =
  -- lower scores first
  -- if there are ties, pick arbitrarily
  locIdSet
    |> Set.toList
    |> List.sortBy (\locId -> getCount locId biasCounts)
    |> List.head


chooseFairLocationAssignment : List (Set LocId) -> FairCounts -> (List (Maybe LocId), FairCounts)
chooseFairLocationAssignment locSets fairCounts =

  let noAssignment () =
    let allNothings = List.repeat (List.length locSets) Nothing in
    (allNothings, fairCounts) in

  -- check if Cartesian product would be too large
  let numCandidates = List.foldl (*) 1 (List.map Set.size locSets) in

  if numCandidates > 100 then noAssignment ()
  else
    let sorted =
      locSets
        |> List.map Set.toList
        |> List.map convertEmptyToNonEmpty
        |> Utils.cartProdAll
        |> List.sortBy
             (\assignment -> getCount assignment fairCounts)
               -- lower counts first
               -- if there are ties, pick arbitrarily
    in
    case sorted of
      [] -> noAssignment ()

      assignment :: _ ->
        let fairCounts_ = incrementCount assignment fairCounts in
        (assignment, fairCounts_)


convertEmptyToNonEmpty locList =
  case locList of
    [] -> [Nothing]
    _  -> List.map Just locList


------------------------------------------------------------------------------
-- Prepare for Live Updates

type alias AttrName = String
  -- NOTE: AttrNames include "fake" attributes
  --   e.g. for polygons, x1,y1,x2,y2,x3,y3,...
  --   e.g. fillOpacity

type alias UpdateFunction
    = SolverTypes.SolutionsCache
   -> (Int, Int)   -- initial click (mx0, my0)
   -> (Int, Int)   -- change in mouse position (dx, dy)
   -> Maybe Num

type alias TriggerElement = (AttrName, String, LocId, Trace, UpdateFunction)
type alias Trigger        = List TriggerElement

-- keeping these separate (rather than an Either key, which isn't comparable)
--
type alias Triggers = Dict (NodeId, RealZone) (Trigger, Set LocId, Set LocId)

type alias LiveInfo =
  { triggers : Triggers
  , initSubstPlus : SubstPlus -- TODO this and/or initSubst should be in Model
  }


prepareLiveUpdates : Options -> Exp -> Canvas -> Result String LiveInfo
prepareLiveUpdates options e (slate, widgets) =
  -- ImpureGoodies.logTimedRun "Sync.prepareLiveUpdates" <| \_ ->
  prepareLiveUpdates_ options e (slate, widgets)

prepareLiveUpdates_ : Options -> Exp -> Canvas -> Result String LiveInfo
prepareLiveUpdates_ options e (slate, widgets) =

  let initSubstPlus = substPlusOf e in
  let initSubst = Dict.map (always .val) initSubstPlus in
  let unfrozenLocIdSet = expToUnfrozenLocIdSet options e in
  let maybeCounts =
    if options.heuristicsMode == HeuristicsFair then
      Just (Right Dict.empty)
    else if options.heuristicsMode == HeuristicsBiased then
      Just (Left (getLocationCounts unfrozenLocIdSet (slate, widgets)))
    else {- options.heuristicsMode == HeuristicsNone -}
      Nothing
  in
  let (shapeTriggers, maybeCounts_) =
    computeShapeTriggers (unfrozenLocIdSet, initSubst) slate maybeCounts
  in
  let (widgetTriggers, maybeCounts__) =
    computeWidgetTriggers (unfrozenLocIdSet, initSubst) widgets maybeCounts_
  in

  Ok { initSubstPlus = initSubstPlus
     , triggers = Dict.union shapeTriggers widgetTriggers
     }


------------------------------------------------------------------------------
-- Computing Triggers

computeShapeTriggers
    : (Set LocId, Subst) -> RootedIndexedTree -> MaybeCounts
   -> (Triggers, MaybeCounts)

computeShapeTriggers info slate initMaybeCounts =
  let processNode nodeInfo (dict, maybeCounts) =
    case nodeInfo of
      Left _ -> (dict, maybeCounts)

      Right shapeInfo ->
        let (directZoneTriggers, maybeCounts_) =
          let (_, kind, _) = shapeInfo in
          case kind of
            "line"     -> computeLineTriggers info maybeCounts shapeInfo
            "rect"     -> computeRectTriggers info maybeCounts shapeInfo
            "ellipse"  -> computeEllipseTriggers info maybeCounts shapeInfo
            "circle"   -> computeCircleTriggers info maybeCounts shapeInfo
            "BOX"      -> computeBoxOrOvalTriggers info maybeCounts shapeInfo
            "OVAL"     -> computeBoxOrOvalTriggers info maybeCounts shapeInfo
            "polygon"  -> computePolyTriggers info maybeCounts shapeInfo
            "polyline" -> computePolyTriggers info maybeCounts shapeInfo
            "path"     -> computePathTriggers info maybeCounts shapeInfo
            _          -> (Dict.empty, maybeCounts)
        in
        let (sliderZoneTriggers, maybeCounts__) =
          computeFillAndStrokeTriggers info maybeCounts shapeInfo
        in
        let dict_ =
          dict |> Dict.union directZoneTriggers
               |> Dict.union sliderZoneTriggers
        in
        (dict_, maybeCounts_)
  in
  LangSvg.foldSlateNodeInfo slate (Dict.empty, initMaybeCounts) processNode


computeWidgetTriggers
    : (Set LocId, Subst) -> Widgets -> MaybeCounts
   -> (Triggers, MaybeCounts)

computeWidgetTriggers (unfrozenLocIdSet, subst) widgets initMaybeCounts =
  let wSlider = params.mainSection.uiWidgets.wSlider in

  let processWidget (i, widget) accResult =
    let idAsShape = -2 - i in
    case widget of

      WNumSlider minVal maxVal _ curVal _ (locId, _, _) _ ->
        let updateX dx =
          curVal + (toFloat dx / toFloat wSlider) * (maxVal - minVal)
            |> clamp minVal maxVal
        in
        let unfrozenLocIdSet_ = Set.singleton locId in
        addTrigger subst unfrozenLocIdSet_ idAsShape ZSlider [MathVar locId]
        (Utils.unwrap1 >> \maybeLoc ->
          mapMaybeToList maybeLoc (\loc_ ->
            ( "", "dx", loc_, MathVar locId
            , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst loc_ (updateX dx) (MathVar locId)
            ))
        )
        accResult

      WIntSlider a b _ c _ (locId, _, _) _ ->
        let (minVal, maxVal, curVal) = (toFloat a, toFloat b, toFloat c) in
        let updateX dx =
          curVal + (toFloat dx / toFloat wSlider) * (maxVal - minVal)
            |> clamp minVal maxVal
            |> round
            |> toFloat
        in
        let unfrozenLocIdSet_ = Set.singleton locId in
        addTrigger subst unfrozenLocIdSet_ idAsShape ZSlider [MathVar locId]
        (Utils.unwrap1 >> \maybeLoc ->
          mapMaybeToList maybeLoc (\loc_ ->
            ( "", "dx", loc_, MathVar locId
            , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst loc_ (updateX dx) (MathVar locId)
            ))
        )
        accResult

      WPoint (x, xTrace) xProvenance (y, yTrace) yProvenance pairProvenance ->
        addTrigger subst unfrozenLocIdSet idAsShape (ZPoint LonePoint) [xTrace, yTrace]
        ( Utils.unwrap2 >> \(xMaybeLoc, yMaybeLoc) ->
            mapMaybeToList xMaybeLoc (\xLoc ->
              ( "", "dx", xLoc, xTrace
              , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst xLoc (x + toFloat dx) xTrace
              )) ++
            mapMaybeToList yMaybeLoc (\yLoc ->
              ( "", "dy", yLoc, yTrace
              , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst yLoc (y + toFloat dy) yTrace
              ))
        )
        accResult

      WOffset1D baseXNumTr baseYNumTr axis sign (amount, amountTrace) amountProvenance endXProvenance endYProvenance ->
        addTrigger subst unfrozenLocIdSet idAsShape ZOffset1D [amountTrace]
        (Utils.unwrap1 >> \maybeLoc ->
          mapMaybeToList maybeLoc (\loc_ ->
            ( "", if axis == X then "dx" else "dy", loc_, amountTrace
            , \solutionsCache _ (dx,dy) -> solveOne solutionsCache subst loc_ ((if sign == Positive then (+) else (-)) amount (if axis == X then toFloat dx else toFloat dy)) amountTrace
            ))
        )
        accResult

      WCall _ _ _ _ _ ->
        accResult

      WList _ ->
        accResult
  in
  Utils.foldli1 processWidget (Dict.empty, initMaybeCounts) widgets


-- Helpers --

addTrigger subst unfrozenLocIdSet id realZone traces makeTrigger (dict, maybeCounts) =
  let key = (id, realZone) in
  let (assignedMaybeLocs, allLocs, maybeCounts_) =
    pickLocs subst unfrozenLocIdSet maybeCounts traces in
  let trigger = makeTrigger assignedMaybeLocs in
  let yellowLocs =
     List.foldl (\triggerElt acc ->
       let (_, _, loc, _, _) = triggerElt in
       Set.insert loc acc
     ) Set.empty trigger
  in
  let grayLocs = Set.diff allLocs yellowLocs in
  let dict_ = Dict.insert key (trigger, yellowLocs, grayLocs) dict in
  (dict_, maybeCounts_)


solveOne : SolverTypes.SolutionsCache -> Subst -> LocId -> Num -> Trace -> Maybe Num
solveOne solutionsCache subst locId n_ t =
  let subst_ = Dict.remove locId subst in
  let maybeSolution = Solver.solveTrace solutionsCache subst_ t n_ in
  maybeSolution
  |> Utils.filterMaybe (not << isNaN)
  |> Utils.filterMaybe (not << isInfinite)


mapMaybeToList mx f =
  case mx of
    Nothing -> []
    Just x  -> [f x]


-- Rect Triggers --

computeRectTriggers
     : (Set LocId, Subst)
    -> MaybeCounts
    -> (NodeId, ShapeKind, List Attr)
    -> (Dict (NodeId, RealZone) (Trigger, Set LocId, Set LocId), MaybeCounts)

computeRectTriggers (unfrozenLocIdSet, subst) maybeCounts (id, _, attrs) =
  let finishTrigger = addTrigger subst unfrozenLocIdSet id in

  let ((x, xTrace), (y, yTrace), (w, wTrace), (h, hTrace)) =
    Utils.unwrap4 <|
      List.map (LangSvg.toNum << Utils.find_ attrs) <|
        ["x", "y", "width", "height"] in

  let leftEdge xMaybeLoc wMaybeLoc =
    mapMaybeToList xMaybeLoc (\xLoc ->
      ( "x", "dx", xLoc, xTrace
      , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst xLoc (x + toFloat dx) xTrace
      )) ++
    mapMaybeToList wMaybeLoc (\wLoc ->
      ( "width", "dx", wLoc, wTrace
      , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst wLoc (w - toFloat dx) wTrace
      )) in

  let rightEdge wMaybeLoc =
    mapMaybeToList wMaybeLoc (\wLoc ->
      ( "width", "dx", wLoc, wTrace
      , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst wLoc (w + toFloat dx) wTrace
      )) in

  let topEdge yMaybeLoc hMaybeLoc =
    mapMaybeToList yMaybeLoc (\yLoc ->
      ( "y", "dy", yLoc, yTrace
      , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst yLoc (y + toFloat dy) yTrace
      )) ++
    mapMaybeToList hMaybeLoc (\hLoc ->
      ( "height", "dy", hLoc, hTrace
      , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst hLoc (h - toFloat dy) hTrace
      )) in

  let botEdge hMaybeLoc =
    mapMaybeToList hMaybeLoc (\hLoc ->
      ( "height", "dy", hLoc, hTrace
      , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst hLoc (h + toFloat dy) hTrace
      )) in

  (Dict.empty, maybeCounts)

  |> finishTrigger ZInterior [xTrace, yTrace] (\assignedMaybeLocs ->
       let (xMaybeLoc, yMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       mapMaybeToList xMaybeLoc (\xLoc ->
         ( "x", "dx", xLoc, xTrace
         , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst xLoc (x + toFloat dx) xTrace
         )) ++
       mapMaybeToList yMaybeLoc (\yLoc ->
         ( "y", "dy", yLoc, yTrace
         , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst yLoc (y + toFloat dy) yTrace
         ))
     )

  |> finishTrigger (ZPoint TopLeft) [xTrace, yTrace, wTrace, hTrace] (\assignedMaybeLocs ->
       let (xMaybeLoc, yMaybeLoc, wMaybeLoc, hMaybeLoc) =
         Utils.unwrap4 assignedMaybeLocs in
       leftEdge xMaybeLoc wMaybeLoc ++ topEdge yMaybeLoc hMaybeLoc
     )

  |> finishTrigger (ZPoint TopEdge) [yTrace, hTrace] (\assignedMaybeLocs ->
       let (yMaybeLoc, hMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       topEdge yMaybeLoc hMaybeLoc
     )

  |> finishTrigger (ZPoint TopRight) [yTrace, wTrace, hTrace] (\assignedMaybeLocs ->
       let (yMaybeLoc, wMaybeLoc, hMaybeLoc) = Utils.unwrap3 assignedMaybeLocs in
       topEdge yMaybeLoc hMaybeLoc ++ rightEdge wMaybeLoc
     )

  |> finishTrigger (ZPoint RightEdge) [wTrace] (\assignedMaybeLocs ->
       let (wMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       rightEdge wMaybeLoc
     )

  |> finishTrigger (ZPoint BotRight) [wTrace, hTrace] (\assignedMaybeLocs ->
       let (wMaybeLoc, hMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       rightEdge wMaybeLoc ++ botEdge hMaybeLoc
     )

  |> finishTrigger (ZPoint BotEdge) [hTrace] (\assignedMaybeLocs ->
       let (hMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       botEdge hMaybeLoc
     )

  |> finishTrigger (ZPoint BotLeft) [xTrace, wTrace, hTrace] (\assignedMaybeLocs ->
       let (xMaybeLoc, wMaybeLoc, hMaybeLoc) = Utils.unwrap3 assignedMaybeLocs in
       leftEdge xMaybeLoc wMaybeLoc ++ botEdge hMaybeLoc
     )

  |> finishTrigger (ZPoint LeftEdge) [xTrace, wTrace] (\assignedMaybeLocs ->
       let (xMaybeLoc, wMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       leftEdge xMaybeLoc wMaybeLoc
     )


-- Line Triggers --

computeLineTriggers (unfrozenLocIdSet, subst) maybeCounts (id, _, attrs) =
  let finishTrigger = addTrigger subst unfrozenLocIdSet id in

  let ((x1, x1Trace), (y1, y1Trace), (x2, x2Trace), (y2, y2Trace)) =
    Utils.unwrap4 <|
      List.map (LangSvg.toNum << Utils.find_ attrs) ["x1", "y1", "x2", "y2"] in

  let point i xMaybeLoc yMaybeLoc =
    let (x, xTrace, y, yTrace) =
      if i == 1 then    (x1, x1Trace, y1, y1Trace)
      else {- i == 2 -} (x2, x2Trace, y2, y2Trace)
    in
    mapMaybeToList xMaybeLoc (\xLoc ->
      ( "x" ++ toString i, "dx", xLoc, xTrace
      , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst xLoc (x + toFloat dx) xTrace
      )) ++
    mapMaybeToList yMaybeLoc (\yLoc ->
      ( "y" ++ toString i, "dy", yLoc, yTrace
      , \solutionsCache _ (_, dy) -> solveOne solutionsCache subst yLoc (y + toFloat dy) yTrace
      )) in

  (Dict.empty, maybeCounts)

  |> finishTrigger (ZPoint (Point 1)) [x1Trace, y1Trace] (\assignedMaybeLocs ->
       let (xMaybeLoc1, yMaybeLoc1) = Utils.unwrap2 assignedMaybeLocs in
       point 1 xMaybeLoc1 yMaybeLoc1
     )

  |> finishTrigger (ZPoint (Point 2)) [x2Trace, y2Trace] (\assignedMaybeLocs ->
       let (xMaybeLoc2, yMaybeLoc2) = Utils.unwrap2 assignedMaybeLocs in
       point 2 xMaybeLoc2 yMaybeLoc2
     )

  |> finishTrigger ZLineEdge [x1Trace, y1Trace, x2Trace, y2Trace] (\assignedMaybeLocs ->
       let (xMaybeLoc1, yMaybeLoc1, xMaybeLoc2, yMaybeLoc2) =
         Utils.unwrap4 assignedMaybeLocs in
       point 1 xMaybeLoc1 yMaybeLoc1 ++ point 2 xMaybeLoc2 yMaybeLoc2
     )


-- Ellipse Triggers --

-- NOTE: choosing not to update center with eight point zones
-- TODO: add an option

computeEllipseTriggers (unfrozenLocIdSet, subst) maybeCounts (id, _, attrs) =
  let finishTrigger = addTrigger subst unfrozenLocIdSet id in

  let ((cx, cxTrace), (cy, cyTrace), (rx, rxTrace), (ry, ryTrace)) =
    Utils.unwrap4 <|
      List.map (LangSvg.toNum << Utils.find_ attrs) ["cx", "cy", "rx", "ry"] in

  let leftEdge rxMaybeLoc =
    mapMaybeToList rxMaybeLoc (\rxLoc ->
      ( "rx", "dx", rxLoc, rxTrace
      , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst rxLoc (rx - toFloat dx) rxTrace
      )) in

  let rightEdge rxMaybeLoc =
    mapMaybeToList rxMaybeLoc (\rxLoc ->
      ( "rx", "dx", rxLoc, rxTrace
      , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst rxLoc (rx + toFloat dx) rxTrace
      )) in

  let topEdge ryMaybeLoc =
    mapMaybeToList ryMaybeLoc (\ryLoc ->
      ( "ry", "dy", ryLoc, ryTrace
      , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst ryLoc (ry - toFloat dy) ryTrace
      )) in

  let botEdge ryMaybeLoc =
    mapMaybeToList ryMaybeLoc (\ryLoc ->
      ( "ry", "dy", ryLoc, ryTrace
      , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst ryLoc (ry + toFloat dy) ryTrace
      )) in

  (Dict.empty, maybeCounts)

  |> finishTrigger ZInterior [cxTrace, cyTrace] (\assignedMaybeLocs ->
       let (cxMaybeLoc, cyMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       mapMaybeToList cxMaybeLoc (\cxLoc ->
         ( "cx", "dx", cxLoc, cxTrace
         , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst cxLoc (cx + toFloat dx) cxTrace
         )) ++
       mapMaybeToList cyMaybeLoc (\cyLoc ->
         ( "cy", "dy", cyLoc, cyTrace
         , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst cyLoc (cy + toFloat dy) cyTrace
         ))
     )

  |> finishTrigger (ZPoint TopLeft) [rxTrace, ryTrace] (\assignedMaybeLocs ->
       let (leftMaybeLoc, topMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       leftEdge leftMaybeLoc ++ topEdge topMaybeLoc
     )

  |> finishTrigger (ZPoint TopEdge) [ryTrace] (\assignedMaybeLocs ->
       let (topMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       topEdge topMaybeLoc
     )

  |> finishTrigger (ZPoint TopRight) [rxTrace, ryTrace] (\assignedMaybeLocs ->
       let (rightMaybeLoc, topMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       rightEdge rightMaybeLoc ++ topEdge topMaybeLoc
     )

  |> finishTrigger (ZPoint RightEdge) [rxTrace] (\assignedMaybeLocs ->
       let (rightMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       rightEdge rightMaybeLoc
     )

  |> finishTrigger (ZPoint BotRight) [rxTrace, ryTrace] (\assignedMaybeLocs ->
       let (rightMaybeLoc, botMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       rightEdge rightMaybeLoc ++ botEdge botMaybeLoc
     )

  |> finishTrigger (ZPoint BotEdge) [ryTrace] (\assignedMaybeLocs ->
       let (botMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       botEdge botMaybeLoc
     )

  |> finishTrigger (ZPoint BotLeft) [rxTrace, ryTrace] (\assignedMaybeLocs ->
       let (leftMaybeLoc, botMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       leftEdge leftMaybeLoc ++ botEdge botMaybeLoc
     )

  |> finishTrigger (ZPoint LeftEdge) [rxTrace] (\assignedMaybeLocs ->
       let (leftMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       leftEdge leftMaybeLoc
     )


-- Circle Triggers --

-- NOTE: choosing not to update center with eight point zones
-- TODO: add an option

computeCircleTriggers (unfrozenLocIdSet, subst) maybeCounts (id, _, attrs) =
  let finishTrigger = addTrigger subst unfrozenLocIdSet id in

  let co = (*) 1 in
  let contra = (*) -1 in

  let ((cx, cxTrace), (cy, cyTrace), (r, rTrace)) =
    Utils.unwrap3 <|
      List.map (LangSvg.toNum << Utils.find_ attrs) ["cx", "cy", "r"] in

  let leftEdge rMaybeLoc =
    mapMaybeToList rMaybeLoc (\rLoc ->
      ( "r", "dx", rLoc, rTrace
      , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst rLoc (r - toFloat dx) rTrace
      )) in

  let rightEdge rMaybeLoc =
    mapMaybeToList rMaybeLoc (\rLoc ->
      ( "r", "dx", rLoc, rTrace
      , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst rLoc (r + toFloat dx) rTrace
      )) in

  let topEdge rMaybeLoc =
    mapMaybeToList rMaybeLoc (\rLoc ->
      ( "r", "dy", rLoc, rTrace
      , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst rLoc (r - toFloat dy) rTrace
      )) in

  let botEdge rMaybeLoc =
    mapMaybeToList rMaybeLoc (\rLoc ->
      ( "r", "dy", rLoc, rTrace
      , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst rLoc (r + toFloat dy) rTrace
      )) in

  let corner rMaybeLoc fx fy =
    mapMaybeToList rMaybeLoc (\rLoc ->
      ( "r", "dxy", rLoc, rTrace
      , \solutionsCache _ (dx,dy) ->
          let d = max (fx dx) (fy dy) in
          solveOne solutionsCache subst rLoc (r + toFloat d) rTrace
      )) in

  (Dict.empty, maybeCounts)

  |> finishTrigger ZInterior [cxTrace, cyTrace] (\assignedMaybeLocs ->
       let (cxMaybeLoc, cyMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       mapMaybeToList cxMaybeLoc (\cxLoc ->
         ( "cx", "dx", cxLoc, cxTrace
         , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst cxLoc (cx + toFloat dx) cxTrace
         )
       ) ++
       mapMaybeToList cyMaybeLoc (\cyLoc ->
         ( "cy", "dy", cyLoc, cyTrace
         , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst cyLoc (cy + toFloat dy) cyTrace
         )
       )
     )

  |> finishTrigger (ZPoint TopLeft) [rTrace] (\assignedMaybeLocs ->
       let (rMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       corner rMaybeLoc contra contra
     )

  |> finishTrigger (ZPoint TopEdge) [rTrace] (\assignedMaybeLocs ->
       let (rMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       topEdge rMaybeLoc
     )

  |> finishTrigger (ZPoint TopRight) [rTrace] (\assignedMaybeLocs ->
       let (rMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       corner rMaybeLoc co contra
     )

  |> finishTrigger (ZPoint RightEdge) [rTrace] (\assignedMaybeLocs ->
       let (rMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       rightEdge rMaybeLoc
     )

  |> finishTrigger (ZPoint BotRight) [rTrace] (\assignedMaybeLocs ->
       let (rMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       corner rMaybeLoc co co
     )

  |> finishTrigger (ZPoint BotEdge) [rTrace] (\assignedMaybeLocs ->
       let (rMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       botEdge rMaybeLoc
     )

  |> finishTrigger (ZPoint BotLeft) [rTrace] (\assignedMaybeLocs ->
       let (rMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       corner rMaybeLoc contra co
     )

  |> finishTrigger (ZPoint LeftEdge) [rTrace] (\assignedMaybeLocs ->
       let (rMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       leftEdge rMaybeLoc
     )


-- Box/Oval Triggers --

computeBoxOrOvalTriggers (unfrozenLocIdSet, subst) maybeCounts (id, _, attrs) =
  let finishTrigger = addTrigger subst unfrozenLocIdSet id in

  let ((left, leftTrace), (top, topTrace), (right, rightTrace), (bot, botTrace)) =
    Utils.unwrap4 <|
      List.map (LangSvg.toNum << Utils.find_ attrs) <|
        ["LEFT", "TOP", "RIGHT", "BOT"] in

  let leftEdge leftMaybeLoc =
    mapMaybeToList leftMaybeLoc (\leftLoc ->
      ( "LEFT", "dx", leftLoc, leftTrace
      , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst leftLoc (left + toFloat dx) leftTrace
      )) in

  let rightEdge rightMaybeLoc =
    mapMaybeToList rightMaybeLoc (\rightLoc ->
      ( "RIGHT", "dx", rightLoc, rightTrace
      , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst rightLoc (right + toFloat dx) rightTrace
      )) in

  let topEdge topMaybeLoc =
    mapMaybeToList topMaybeLoc (\topLoc ->
      ( "TOP", "dy", topLoc, topTrace
      , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst topLoc (top + toFloat dy) topTrace
      )) in

  let botEdge botMaybeLoc =
    mapMaybeToList botMaybeLoc (\botLoc ->
      ( "BOT", "dy", botLoc, botTrace
      , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst botLoc (bot + toFloat dy) botTrace
      )) in

  (Dict.empty, maybeCounts)

  |> finishTrigger ZInterior [leftTrace, topTrace, rightTrace, botTrace] (\assignedMaybeLocs ->
       let (leftMaybeLoc, topMaybeLoc, rightMaybeLoc, botMaybeLoc) =
         Utils.unwrap4 assignedMaybeLocs in
       leftEdge leftMaybeLoc ++ topEdge topMaybeLoc ++
       rightEdge rightMaybeLoc ++ botEdge botMaybeLoc
     )

  |> finishTrigger (ZPoint TopLeft) [leftTrace, topTrace] (\assignedMaybeLocs ->
       let (leftMaybeLoc, topMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       leftEdge leftMaybeLoc ++ topEdge topMaybeLoc
     )

  |> finishTrigger (ZPoint TopEdge) [topTrace] (\assignedMaybeLocs ->
       let (topMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       topEdge topMaybeLoc
     )

  |> finishTrigger (ZPoint TopRight) [rightTrace, topTrace] (\assignedMaybeLocs ->
       let (rightMaybeLoc, topMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       rightEdge rightMaybeLoc ++ topEdge topMaybeLoc
     )

  |> finishTrigger (ZPoint RightEdge) [rightTrace] (\assignedMaybeLocs ->
       let (rightMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       rightEdge rightMaybeLoc
     )

  |> finishTrigger (ZPoint BotRight) [rightTrace, botTrace] (\assignedMaybeLocs ->
       let (rightMaybeLoc, botMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       rightEdge rightMaybeLoc ++ botEdge botMaybeLoc
     )

  |> finishTrigger (ZPoint BotEdge) [botTrace] (\assignedMaybeLocs ->
       let (botMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       botEdge botMaybeLoc
     )

  |> finishTrigger (ZPoint BotLeft) [leftTrace, botTrace] (\assignedMaybeLocs ->
       let (leftMaybeLoc, botMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       leftEdge leftMaybeLoc ++ botEdge botMaybeLoc
     )

  |> finishTrigger (ZPoint LeftEdge) [leftTrace] (\assignedMaybeLocs ->
       let (leftMaybeLoc) = Utils.unwrap1 assignedMaybeLocs in
       leftEdge leftMaybeLoc
     )


-- Polygon/Polyline/Path Triggers --

pointX_ subst i xMaybeLoc x xTrace =
  mapMaybeToList xMaybeLoc (\xLoc ->
    ( "X" ++ toString i, "dx", xLoc, xTrace
    , \solutionsCache _ (dx,_) -> solveOne solutionsCache subst xLoc (x + toFloat dx) xTrace
    ))

pointY_ subst i yMaybeLoc y yTrace =
  mapMaybeToList yMaybeLoc (\yLoc ->
    ( "Y" ++ toString i, "dy", yLoc, yTrace
    , \solutionsCache _ (_,dy) -> solveOne solutionsCache subst yLoc (y + toFloat dy) yTrace
    ))

addPointZones_ finishTrigger pointX pointY indexedPoints result =
  List.foldl
     (\(i, ((x, xTrace), (y, yTrace))) acc -> acc |>
        finishTrigger (ZPoint (Point i)) [xTrace, yTrace]
        ( Utils.unwrap2 >> \(xMaybeLoc, yMaybeLoc) ->
            pointX i xMaybeLoc x xTrace ++
            pointY i yMaybeLoc y yTrace
        )
     )
     result
     indexedPoints

addEdgeZones_ finishTrigger pointX pointY edges result =
  List.foldl
     (\((i, ((xi, xiTrace), (yi, yiTrace))),
        (j, ((xj, xjTrace), (yj, yjTrace)))) acc -> acc |>
        finishTrigger (ZPolyEdge i) [xiTrace, yiTrace, xjTrace, yjTrace]
        ( Utils.unwrap4 >> \(xiMaybeLoc, yiMaybeLoc, xjMaybeLoc, yjMaybeLoc) ->
            pointX i xiMaybeLoc xi xiTrace ++
            pointY i yiMaybeLoc yi yiTrace ++
            pointX j xjMaybeLoc xj xjTrace ++
            pointY j yjMaybeLoc yj yjTrace
        )
     )
     result
     edges

addInteriorZone_ finishTrigger pointX pointY indexedPoints result =
  let xTraces = List.map (Tuple.second << Tuple.first << Tuple.second) indexedPoints in
  let yTraces = List.map (Tuple.second << Tuple.second << Tuple.second) indexedPoints in

  finishTrigger ZInterior (xTraces ++ yTraces) (\assignedMaybeLocs ->
    case Utils.projJusts assignedMaybeLocs of
      -- only add Interior if locations chosen for all points
      Nothing -> []
      Just assignedLocs ->
        let n = List.length assignedLocs in
        let xLocs = List.take (n//2) assignedLocs in
        let yLocs = List.drop (n//2) assignedLocs in
        let stuff = Utils.zip (Utils.zip xLocs yLocs) indexedPoints in
        List.foldl (\((xLoc, yLoc), (i, ((x, xTrace), (y, yTrace)))) acc ->
           pointX i (Just xLoc) x xTrace ++
           pointY i (Just yLoc) y yTrace ++
           acc
        )
        []
        stuff
  ) result


-- Koch depth 3 is 20sec (28%) faster if we don't compute/display the poly point/edge zones.
--
-- See also Canvas.makeZonesPoly (also has the 50 point limit hardcoded).
computePolyTriggers (unfrozenLocIdSet, subst) maybeCounts (id, kind, attrs) =

  let finishTrigger = addTrigger subst unfrozenLocIdSet id in
  let pointX = pointX_ subst in
  let pointY = pointY_ subst in

  let addInteriorZone = addInteriorZone_ finishTrigger pointX pointY in

  let indexedPoints = Utils.mapi1 identity (LangSvg.getPolyPoints attrs) in

  if List.length indexedPoints > 50 then
    -- Koch depth 3
    (Dict.empty, maybeCounts)
      |> addInteriorZone indexedPoints
  else
    -- not Koch depth 3
    let addPointZones = addPointZones_ finishTrigger pointX pointY in
    let addEdgeZones = addEdgeZones_ finishTrigger pointX pointY in
    let edges =
      if kind == "polygon" then Utils.circOverlappingAdjacentPairs indexedPoints
      else {- if kind == "polyline" -}
        let n = List.length indexedPoints in
        Utils.circOverlappingAdjacentPairs (List.take (n-1) indexedPoints)
    in
    (Dict.empty, maybeCounts)
      |> addPointZones indexedPoints
      |> addEdgeZones edges
      |> addInteriorZone indexedPoints


computePathTriggers (unfrozenLocIdSet, subst) maybeCounts (id, _, attrs) =

  let finishTrigger = addTrigger subst unfrozenLocIdSet id in
  let pointX = pointX_ subst in
  let pointY = pointY_ subst in

  let addPointZones = addPointZones_ finishTrigger pointX pointY in
  let addEdgeZones = addEdgeZones_ finishTrigger pointX pointY in
  let addInteriorZone = addInteriorZone_ finishTrigger pointX pointY in

  let indexedPoints = LangSvg.pathIndexPoints attrs in

  (Dict.empty, maybeCounts)
    |> addPointZones indexedPoints
    |> addInteriorZone indexedPoints


-- Fill and Stroke Triggers --

computeFillAndStrokeTriggers (unfrozenLocIdSet, subst) maybeCounts (id, _, attrs) =
  let finishTrigger = addTrigger subst unfrozenLocIdSet id in

  let maybeAddColorTrigger realZone fillOrStroke (dict, maybeCounts) =
    case Utils.maybeFind fillOrStroke attrs |> Maybe.map .interpreted of

      Just (AColorNum ((color, colorTrace), _)) ->
        finishTrigger realZone [colorTrace] (\assignedMaybeLocs ->
          let (maybeLoc) = Utils.unwrap1 assignedMaybeLocs in
          mapMaybeToList maybeLoc (\colorLoc ->
            ( fillOrStroke, "dx", colorLoc, colorTrace
            , \solutionsCache _ (dx,_) ->
                let color_ = colorNumPlus color dx in
                solveOne solutionsCache subst colorLoc color_ colorTrace
            ))
        ) (dict, maybeCounts)

      _ -> (dict, maybeCounts) in

  let maybeAddOpacityTrigger realZone fillOrStroke (dict, maybeCounts) =
    case Utils.maybeFind fillOrStroke attrs |> Maybe.map .interpreted of

      Just (AColorNum (_, (Just (opacity, opacityTrace)))) ->
        finishTrigger realZone [opacityTrace] (\assignedMaybeLocs ->
          let (maybeLoc) = Utils.unwrap1 assignedMaybeLocs in
          mapMaybeToList maybeLoc (\opacityLoc ->
            ( fillOrStroke ++ "Opacity", "dx", opacityLoc, opacityTrace
            , \solutionsCache _ (dx,_) ->
                let opacity_ = opacityNumPlus opacity dx in
                solveOne solutionsCache subst opacityLoc opacity_ opacityTrace
            ))
        ) (dict, maybeCounts)

      _ -> (dict, maybeCounts) in

  let maybeAddStrokeWidthTrigger realZone (dict, maybeCounts) =
    case Utils.maybeFind "stroke-width" attrs |> Maybe.map .interpreted of

      Just (ANum (width, widthTrace)) ->
        finishTrigger realZone [widthTrace] (\assignedMaybeLocs ->
          let (maybeLoc) = Utils.unwrap1 assignedMaybeLocs in
          mapMaybeToList maybeLoc (\widthLoc ->
            ( "stroke-width", "dx", widthLoc, widthTrace
            , \solutionsCache _ (dx,_) ->
                let width_ = strokeWidthNumPlus width dx in
                solveOne solutionsCache subst widthLoc width_  widthTrace
            ))
        ) (dict, maybeCounts)

      _ -> (dict, maybeCounts) in

  let maybeAddRotationTrigger realZone (dict, maybeCounts) =
    case Utils.maybeFind "transform" attrs |> Maybe.map .interpreted of

      Just (ATransform [Rot (rot, rotTrace) (cx, _) (cy ,_)]) ->
        finishTrigger realZone [rotTrace] (\assignedMaybeLocs ->
          let (maybeLoc) = Utils.unwrap1 assignedMaybeLocs in
          mapMaybeToList maybeLoc (\rotLoc ->
            ( "transformRot", "dxy", rotLoc, rotTrace
            , \solutionsCache (mx0,my0) (dx,dy) ->
                let (mx1, my1) = (mx0 + dx, my0 + dy) in
                let
                  radToDeg = Utils.radiansToDegrees
                  a0 = radToDeg <| atan2 (cy - toFloat my0) (toFloat mx0 - cx)
                  a1 = radToDeg <| atan2 (cy - toFloat my1) (toFloat mx1 - cx)
                in
                solveOne solutionsCache subst rotLoc (rot + (a0 - a1)) rotTrace
            ))
        ) (dict, maybeCounts)

      _ -> (dict, maybeCounts) in

  (Dict.empty, maybeCounts)
    |> maybeAddColorTrigger (ZOther FillColor) "fill"
    |> maybeAddColorTrigger (ZOther StrokeColor) "stroke"
    |> maybeAddColorTrigger (ZOther FillOpacity) "fill"
    |> maybeAddColorTrigger (ZOther StrokeOpacity) "stroke"
    |> maybeAddStrokeWidthTrigger (ZOther StrokeWidth)
    |> maybeAddRotationTrigger (ZOther Rotation)


-- Scale Updates Based on UI Params --

colorNumPlus n dx =
  let scale = 1 / (ShapeWidgets.wColorSlider / ColorNum.maxColorNum) in
  let clamp = Utils.clamp 0 (ColorNum.maxColorNum - 1) in
  clamp (n + scale * toFloat dx)

strokeWidthNumPlus n dx =
  let scale = 1 / (ShapeWidgets.wStrokeWidthSlider / LangSvg.maxStrokeWidthNum) in
  let clamp = toFloat << round << Utils.clamp 0 LangSvg.maxStrokeWidthNum in
  clamp (n + scale * toFloat dx)

opacityNumPlus n dx =
  let clamp = Utils.clamp 0.0 1.0 in
  let scale = 1 / ShapeWidgets.wOpacitySlider in
  clamp (n + scale * toFloat dx)


------------------------------------------------------------------------------
-- Preparing Live Triggers

type alias LiveTrigger = SolverTypes.SolutionsCache -> (Int, Int) -> (Int, Int) -> (Exp, List Ace.Highlight)

-- ShapeKind in zone key is used for display in caption, but not for keying the triggers dictionary.
type alias ZoneKey = (NodeId, ShapeKind, RealZone) -- node id for a widget is -2 - (widget number starting from 1)

lookupZoneKey : ZoneKey -> LiveInfo -> (Trigger, Set LocId, Set LocId)
lookupZoneKey zoneKey info =
  let errorString = "lookupZoneKey: " ++ toString zoneKey in
  let default =
    let _ = debugLog errorString ("able to avoid this?") in
    ([], Set.empty, Set.empty)
  in
  let (id, _, zoneName) = zoneKey in
  Dict.get (id, zoneName) info.triggers
  |> Maybe.withDefault default

prepareLiveTrigger : LiveInfo -> Exp -> ZoneKey -> LiveTrigger
prepareLiveTrigger info exp zoneKey solutionsCache (mx0,my0) (dx,dy) =

  -- ImpureGoodies.logTimedRun "prepareLiveTrigger" <| \_ ->

  let (trigger, yellowLocs, _) = lookupZoneKey zoneKey info in
  let initSubst = Dict.map (always .val) info.initSubstPlus in

  let updates =
     List.foldl (\triggerElement acc ->
       let (_, _, locId, _, updateFunction) = triggerElement in
       case (Dict.get locId acc, updateFunction solutionsCache (mx0,my0) (dx,dy)) of

         (Nothing, maybeSolution) -> Dict.insert locId maybeSolution acc

         (Just Nothing, maybeSolution) -> Dict.insert locId maybeSolution acc

         (Just (Just oldSolution), Nothing) ->
           -- keep oldSolution even if these solution failed.
           acc

         (Just (Just oldSolution), Just newSolution) ->
           -- letting first solution win.
           -- could check that solutions agree.
           acc

     ) Dict.empty trigger
  in
  let newSubst =
     Dict.foldl (\locId maybeNum acc ->
       case maybeNum of
         Nothing -> acc
         Just num -> Dict.insert locId num acc
     ) initSubst updates
  in
  let
    exp_       = applyLocSubst newSubst exp
    highlights = highlightChanges info.initSubstPlus yellowLocs updates
  in
  (exp_, highlights)


------------------------------------------------------------------------------
-- Highlights for Locations

gray   = "lightgray"
yellow = "khaki"
green  = "limegreen"
red    = "salmon"

acePos : Pos  -> Ace.Pos
acePos p = { row = p.line, column = p.col }

aceRange : WithInfo a -> Ace.Range
aceRange x = { start = acePos x.start, end = acePos x.end }

makeHighlight : SubstPlus -> String -> LocId -> Maybe Ace.Highlight
makeHighlight subst color locId =
  Dict.get locId subst
  |> Maybe.map (\n -> { color = color, range = aceRange n })


-- Colors and Captions for Zone Locations, Before Direct Manipulation --

yellowAndGrayHighlights zoneKey info =
  let subst = info.initSubstPlus in
  let (_, yellowLocs, grayLocs) = lookupZoneKey zoneKey info in
  List.filterMap (makeHighlight subst yellow) (Set.toList yellowLocs)
  ++ List.filterMap (makeHighlight subst gray) (Set.toList grayLocs)

hoverInfo zoneKey info =
  let line1 =
    let (nodeId, shapeKind, realZone) = zoneKey in
    let displayId =
      if nodeId < -2
      then -nodeId - 2 -- Widget
      else nodeId
    in
    (shapeKind ++ toString displayId) ++ " " ++ ShapeWidgets.realZoneDesc realZone
  in
  let maybeLine2 =
    let (triggerElements, _, _) = lookupZoneKey zoneKey info in
    if triggerElements == [] then Nothing
    else
      let (dxElements, list2) =
        List.partition (\(_,s,_,_,_) -> s == "dx") triggerElements
      in
      let (dyElements, list3) =
        List.partition (\(_,s,_,_,_) -> s == "dy") list2
      in
      let (dxyElements, otherElements) =
        List.partition (\(_,s,_,_,_) -> s == "dxy") list3
      in
      let strElements caption elements =
        let foo (_,_,locId,_,_) =
          let n = Utils.justGet_ ("hoverInfo: " ++ toString locId) locId info.initSubstPlus in
          let locName = "loc_" ++ toString locId in
          locName ++ Utils.parens (String.left 4 (toString n.val))
        in
        case elements of
          [] -> []
          _  -> [Utils.bracks caption ++ " " ++ Utils.spaces (List.map foo elements)]
      in
      Just <| Utils.spaces <| List.concat <|
        [ strElements "dx" dxElements
        , strElements "dy" dyElements
        , strElements "dxy" dxyElements
        , strElements "..." otherElements
        ]
  in
  (line1, maybeLine2)


-- Colors for Zone Locations, During Direct Manipulation --

highlightChanges : SubstPlus -> Set LocId -> SubstMaybeNum -> List Ace.Highlight
highlightChanges initSubstPlus locIds changes =

  let (hi,stringOffsets) =
    -- hi : List Highlight, stringOffsets : List (Pos, Int)
    --   where Pos is start pos of a highlight to offset by Int chars
    let f locId (acc1,acc2) =
      let highlight c = makeHighlight initSubstPlus c locId |> Utils.fromJust_ "highlightChanges highlight: should not happen, function only called if locid in initSubstsPlus" in
      case (Dict.get locId initSubstPlus, Dict.get locId changes) of
        (Nothing, _)             -> (acc1, acc2)
        (Just n, Nothing)        -> (highlight yellow :: acc1, acc2)
        (Just n, Just Nothing)   -> (highlight red :: acc1, acc2)
        (Just n, Just (Just n_)) ->
          if n_ == n.val then
            (highlight yellow :: acc1, acc2)
          else
            let (s, s_) = (strNum n.val, strNum n_) in
            let x = (acePos n.start, String.length s_ - String.length s) in
            (highlight green :: acc1, x :: acc2)
    in
    List.foldl f ([],[]) (Set.toList locIds)
  in

  let hi_ =
    let g (startPos,extraChars) (old,new) =
      let bump pos = { pos | column = pos.column + extraChars } in
      let ret new_ = (old, new_) in
      ret <|
        if startPos.row    /= old.start.row         then new
        else if startPos.column >  old.start.column then new
        else if startPos.column == old.start.column then { start = new.start, end = bump new.end }
        else if startPos.column <  old.start.column then { start = bump new.start, end = bump new.end }
        else
          Debug.crash "highlightChanges"
    in
    -- hi has <= 4 elements, so not worrying about the redundant processing
    flip List.map hi <| \{color,range} ->
      let (_,range_) = List.foldl g (range,range) stringOffsets in
      { color = color, range = range_ }
  in

  hi_

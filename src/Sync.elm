module Sync
  ( Options, defaultOptions, syncOptionsOf
  , heuristicsNone, heuristicsFair, heuristicsBiased, toggleHeuristicMode
  , locsOfTrace
  , LiveInfo, ShapeTriggers, prepareLiveUpdates, applyTrigger
  ) where

import Lang exposing (..)
import LangSvg exposing
  ( NodeId, ShapeKind, Attr, RootedIndexedTree, IndexedTree
  , AVal, AVal_(..), TransformCmd(..), PathCmd(..)
  )
import ShapeWidgets exposing
  ( Zone, RealZone(..), PointFeature(..), OtherFeature(..)
  )
import Solver exposing (Equation)
import LangParser2 as Parser
import Config
import Utils
import Either exposing (Either(..))

import Dict exposing (Dict)
import Set exposing (Set)
import Debug
import String


------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugSync


getCount x dict    = Maybe.withDefault 0 (Dict.get x dict)
updateCount x dict = Dict.insert x (1 + getCount x dict) dict


------------------------------------------------------------------------------

type alias Canvas = (RootedIndexedTree, Widgets)


------------------------------------------------------------------------------
-- Sync.Options

type alias HeuristicModes = Int

heuristicModes = 3

(heuristicsNone, heuristicsFair, heuristicsBiased) =
  Utils.unwrap3
    [ 0 .. (heuristicModes - 1) ]

toggleHeuristicMode x =
  let i = (1 + x) % heuristicModes in
  i

type alias Options =
  { thawedByDefault : Bool
  , feelingLucky : HeuristicModes
  }

defaultOptions =
  { thawedByDefault = True
  , feelingLucky = heuristicsBiased
  }

syncOptionsOf oldOptions e =
  -- using oldOptions instead of defaultOptions, because want
  -- feelingLucky to be a global flag, not per-example flag for now
  case Utils.maybeFind "unannotated-numbers" (getOptions e) of
    Nothing -> oldOptions
    Just s ->
      -- TODO decide whether to make feelingLucky per-example or not
      --   if not, perhaps move it out of Options
      if s == "n?" then { oldOptions | thawedByDefault = True }
      else if s == "n!" then { oldOptions | thawedByDefault = False }
      else
        let _ = debugLog "invalid sync option: " s in
        oldOptions


------------------------------------------------------------------------------
-- Locations of Trace

locsOfTrace : Options -> Trace -> Set Loc
locsOfTrace opts =
  let frozenByDefault = not opts.thawedByDefault in
  let foo t = case t of
    TrLoc l ->
      let (_,b,_) = l in
      if      Parser.isPreludeLoc l         then Set.empty
      else if b == frozen                   then Set.empty
      else if b == unann && frozenByDefault then Set.empty
      else                                       Set.singleton l
    TrOp _ ts -> List.foldl Set.union Set.empty (List.map foo ts)
  in
  -- TODO do this filtering later if want gray highlights
  --   even when not feeling lucky
  \tr ->
    let s = foo tr in
    if opts.feelingLucky == heuristicsNone then
      if List.length (Set.toList s) <= 1 then s else Set.empty
    else
      s

locsOfTraces : Options -> List Trace -> Set Loc
locsOfTraces options traces =
  List.foldl (\t acc -> Set.union acc (locsOfTrace options t)) Set.empty traces


------------------------------------------------------------------------------
-- Counters for Biased Mode

type alias BiasCounts = Dict Loc Int


getLocationCounts : Options -> Canvas -> BiasCounts
getLocationCounts options (slate, widgets) =
  let addTriggerNode nodeInfo acc =
    case nodeInfo of
      Left _ -> acc
      Right (_, _, attrs) ->
        List.foldl (\(_,aval) acc' ->
          Set.foldl updateCount acc' (locsOfTraces options (tracesOfAVal aval))
        ) acc attrs
  in
  let addTriggerWidget widget acc =
    case widget of
      WIntSlider _ _ _ _ loc -> updateCount loc acc
      WNumSlider _ _ _ _ loc -> updateCount loc acc
      WPointSlider (_, t1) (_, t2) ->
        Set.foldl updateCount acc (locsOfTraces options [t1, t2])
  in
  let d  = LangSvg.foldSlateNodeInfo slate Dict.empty addTriggerNode in
  let d' = List.foldl addTriggerWidget d' widgets in
  d'


tracesOfAVals : List AVal -> List Trace
tracesOfAVals avals = List.foldl (\av acc -> tracesOfAVal av ++ acc) [] avals


tracesOfAVal : AVal -> List Trace
tracesOfAVal aval =
  case aval.av_ of
    ANum (_,t) -> [t]

    AColorNum ((_,t), Nothing)     -> [t]
    AColorNum ((_,t), Just (_,t')) -> [t, t']

    APoints pts -> List.concatMap (\((_,t1),(_,t2)) -> [t1, t2]) pts

    ATransform [Rot (_,t1) (_,t2) (_,t3)] -> [t1, t2, t3]

    AString _ -> []
    ARgba _   -> [] -- not collecting traces, because currently no RGBA widgets

    _ -> let _ = debugLog "tracesOfAVal?" (LangSvg.strAVal aval) in
         []


------------------------------------------------------------------------------
-- Choosing Locations for Triggers

type alias MaybeCounts = Maybe (Either BiasCounts FairCounts)
type alias FairCounts = Dict Loc Int


-- TODO for now, simply picking first loc of trace
pickLocs : Options -> MaybeCounts -> List Trace -> (List (Maybe Loc), Set Loc, MaybeCounts)
pickLocs options maybeCounts traces =
  let locSets = List.map (locsOfTrace options) traces in
  -- TODO
  let chooseFirst locs =
    case Set.toList locs of
      loc :: _ -> Just loc
      []       -> Nothing
  in
  let assignedLocs = List.map chooseFirst locSets in
  let allLocs = List.foldl Set.union Set.empty locSets in
  -- TODO
  let maybeCounts' = maybeCounts in
  (assignedLocs, allLocs, maybeCounts)


------------------------------------------------------------------------------
-- Prepare for Live Updates

type alias AttrName = String
  -- NOTE: AttrNames include "fake" attributes
  --   e.g. for polygons, x1,y1,x2,y2,x3,y3,...
  --   e.g. fillOpacity

type alias UpdateFunction
    = (Int, Int)   -- initial click (mx0, my0)
   -> (Int, Int)   -- change in mouse position (dx, dy)
   -> Maybe Num

type alias TriggerElement = (AttrName, String, Loc, Trace, UpdateFunction)
type alias Trigger = List TriggerElement

type alias ShapeTriggers = Dict (NodeId, Zone) (Trigger, Set Loc, Set Loc)

type alias LiveInfo =
  { shapeTriggers : ShapeTriggers
  , widgetTriggers : () -- TODO
  , initSubstPlus : SubstPlus -- TODO this and/or initSubst should be in Model
  }


prepareLiveUpdates : Options -> Int -> Int -> Float -> Exp -> Canvas -> Result String LiveInfo
prepareLiveUpdates options slideNumber movieNumber movieTime e (slate, widgets) =
  let initSubstPlus = Parser.substPlusOf e in
  let initSubst = Dict.map (always .val) initSubstPlus in
  let shapeTriggers = computeShapeTriggers (options, initSubst) slate in
  Ok { initSubstPlus = initSubstPlus
     , shapeTriggers = shapeTriggers
     , widgetTriggers = ()
     }


------------------------------------------------------------------------------
-- Computing Triggers

computeShapeTriggers : (Options, Subst) -> RootedIndexedTree -> ShapeTriggers
computeShapeTriggers info slate =
  let processNode nodeInfo (dict, maybeCounts) =
    case nodeInfo of
      Left _ -> (dict, maybeCounts)

      Right shapeInfo ->
        let (directZoneTriggers, maybeCounts') =
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
        let (sliderZoneTriggers, maybeCounts'') =
          computeFillAndStrokeTriggers info maybeCounts shapeInfo
        in
        let dict' =
          dict |> Dict.union directZoneTriggers
               |> Dict.union sliderZoneTriggers
        in
        (dict', maybeCounts')
  in
  let maybeCounts = Nothing in -- TODO
  let result =
    LangSvg.foldSlateNodeInfo slate (Dict.empty, maybeCounts) processNode
  in
  fst result


type alias ComputeTriggersResult = (ShapeTriggers, MaybeCounts)


addShapeZoneTrigger options id realZone traces makeTrigger (dict, maybeCounts) =
  let zone = ShapeWidgets.unparseZone realZone in
  let (assignedMaybeLocs, allLocs, maybeCounts') =
    pickLocs options maybeCounts traces in
  let trigger = makeTrigger assignedMaybeLocs in
  let yellowLocs =
     List.foldl (\triggerElt acc ->
       let (_, _, loc, _, _) = triggerElt in
       Set.insert loc acc
     ) Set.empty trigger
  in
  let grayLocs = Set.diff allLocs yellowLocs in
  let dict' = Dict.insert (id, zone) (trigger, yellowLocs, grayLocs) dict in
  (dict', maybeCounts')


solveOne subst (k,_,_) n' t =
  let subst' = Dict.remove k subst in
  let maybeSolution = Solver.solve subst' (n',t) in
  maybeSolution


mapMaybeToList mx f =
  case mx of
    Nothing -> []
    Just x  -> [f x]


-- Rect Triggers --

computeRectTriggers
     : (Options, Subst)
    -> MaybeCounts
    -> (NodeId, ShapeKind, List Attr)
    -> (Dict (NodeId, Zone) (Trigger, Set Loc, Set Loc), MaybeCounts)

computeRectTriggers (options, subst) maybeCounts (id, _, attrs) =
  let finishTrigger = addShapeZoneTrigger options id in

  let ((x, xTrace), (y, yTrace), (w, wTrace), (h, hTrace)) =
    Utils.unwrap4 <|
      List.map (LangSvg.toNum << Utils.find_ attrs) <|
        ["x", "y", "width", "height"] in

  let leftEdge xMaybeLoc wMaybeLoc =
    mapMaybeToList xMaybeLoc (\xLoc ->
      ( "x", "dx", xLoc, xTrace
      , \_ (dx,_) -> solveOne subst xLoc (x + toFloat dx) xTrace
      )) ++
    mapMaybeToList wMaybeLoc (\wLoc ->
      ( "width", "dx", wLoc, wTrace
      , \_ (dx,_) -> solveOne subst wLoc (w - toFloat dx) wTrace
      )) in

  let rightEdge wMaybeLoc =
    mapMaybeToList wMaybeLoc (\wLoc ->
      ( "width", "dx", wLoc, wTrace
      , \_ (dx,_) -> solveOne subst wLoc (w + toFloat dx) wTrace
      )) in

  let topEdge yMaybeLoc hMaybeLoc =
    mapMaybeToList yMaybeLoc (\yLoc ->
      ( "y", "dy", yLoc, yTrace
      , \_ (_,dy) -> solveOne subst yLoc (y + toFloat dy) yTrace
      )) ++
    mapMaybeToList hMaybeLoc (\hLoc ->
      ( "height", "dy", hLoc, hTrace
      , \_ (_,dy) -> solveOne subst hLoc (h - toFloat dy) hTrace
      )) in

  let botEdge hMaybeLoc =
    mapMaybeToList hMaybeLoc (\hLoc ->
      ( "height", "dy", hLoc, hTrace
      , \_ (_,dy) -> solveOne subst hLoc (h + toFloat dy) hTrace
      )) in

  (Dict.empty, maybeCounts)

  |> finishTrigger ZInterior [xTrace, yTrace] (\assignedMaybeLocs ->
       let (xMaybeLoc, yMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       mapMaybeToList xMaybeLoc (\xLoc ->
         ( "x", "dx", xLoc, xTrace
         , \_ (dx,_) -> solveOne subst xLoc (x + toFloat dx) xTrace
         )) ++
       mapMaybeToList yMaybeLoc (\yLoc ->
         ( "y", "dy", yLoc, yTrace
         , \_ (_,dy) -> solveOne subst yLoc (y + toFloat dy) yTrace
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

computeLineTriggers (options, subst) maybeCounts (id, _, attrs) =
  let finishTrigger = addShapeZoneTrigger options id in

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
      , \_ (dx,_) -> solveOne subst xLoc (x + toFloat dx) xTrace
      )) ++
    mapMaybeToList yMaybeLoc (\yLoc ->
      ( "y" ++ toString i, "dy", yLoc, yTrace
      , \_ (_, dy) -> solveOne subst yLoc (y + toFloat dy) yTrace
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

computeEllipseTriggers (options, subst) maybeCounts (id, _, attrs) =
  let finishTrigger = addShapeZoneTrigger options id in

  let ((cx, cxTrace), (cy, cyTrace), (rx, rxTrace), (ry, ryTrace)) =
    Utils.unwrap4 <|
      List.map (LangSvg.toNum << Utils.find_ attrs) ["cx", "cy", "rx", "ry"] in

  let leftEdge rxMaybeLoc =
    mapMaybeToList rxMaybeLoc (\rxLoc ->
      ( "rx", "dx", rxLoc, rxTrace
      , \_ (dx,_) -> solveOne subst rxLoc (rx - toFloat dx) rxTrace
      )) in

  let rightEdge rxMaybeLoc =
    mapMaybeToList rxMaybeLoc (\rxLoc ->
      ( "rx", "dx", rxLoc, rxTrace
      , \_ (dx,_) -> solveOne subst rxLoc (rx + toFloat dx) rxTrace
      )) in

  let topEdge ryMaybeLoc =
    mapMaybeToList ryMaybeLoc (\ryLoc ->
      ( "ry", "dy", ryLoc, ryTrace
      , \_ (_,dy) -> solveOne subst ryLoc (ry - toFloat dy) ryTrace
      )) in

  let botEdge ryMaybeLoc =
    mapMaybeToList ryMaybeLoc (\ryLoc ->
      ( "ry", "dy", ryLoc, ryTrace
      , \_ (_,dy) -> solveOne subst ryLoc (ry + toFloat dy) ryTrace
      )) in

  (Dict.empty, maybeCounts)

  |> finishTrigger ZInterior [cxTrace, cyTrace] (\assignedMaybeLocs ->
       let (cxMaybeLoc, cyMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       mapMaybeToList cxMaybeLoc (\cxLoc ->
         ( "cx", "dx", cxLoc, cxTrace
         , \_ (dx,_) -> solveOne subst cxLoc (cx + toFloat dx) cxTrace
         )) ++
       mapMaybeToList cyMaybeLoc (\cyLoc ->
         ( "cy", "dy", cyLoc, cyTrace
         , \_ (_,dy) -> solveOne subst cyLoc (cy + toFloat dy) cyTrace
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

computeCircleTriggers (options, subst) maybeCounts (id, _, attrs) =
  let finishTrigger = addShapeZoneTrigger options id in

  let co = (*) 1 in
  let contra = (*) -1 in

  let ((cx, cxTrace), (cy, cyTrace), (r, rTrace)) =
    Utils.unwrap3 <|
      List.map (LangSvg.toNum << Utils.find_ attrs) ["cx", "cy", "r"] in

  let leftEdge rMaybeLoc =
    mapMaybeToList rMaybeLoc (\rLoc ->
      ( "r", "dx", rLoc, rTrace
      , \_ (dx,_) -> solveOne subst rLoc (r - toFloat dx) rTrace
      )) in

  let rightEdge rMaybeLoc =
    mapMaybeToList rMaybeLoc (\rLoc ->
      ( "r", "dx", rLoc, rTrace
      , \_ (dx,_) -> solveOne subst rLoc (r + toFloat dx) rTrace
      )) in

  let topEdge rMaybeLoc =
    mapMaybeToList rMaybeLoc (\rLoc ->
      ( "r", "dy", rLoc, rTrace
      , \_ (_,dy) -> solveOne subst rLoc (r - toFloat dy) rTrace
      )) in

  let botEdge rMaybeLoc =
    mapMaybeToList rMaybeLoc (\rLoc ->
      ( "r", "dy", rLoc, rTrace
      , \_ (_,dy) -> solveOne subst rLoc (r + toFloat dy) rTrace
      )) in

  let corner rMaybeLoc fx fy =
    mapMaybeToList rMaybeLoc (\rLoc ->
      ( "r", "dxy", rLoc, rTrace
      , \_ (dx,dy) ->
          let d = max (fx dx) (fy dy) in
          solveOne subst rLoc (r + toFloat d) rTrace
      )) in

  (Dict.empty, maybeCounts)

  |> finishTrigger ZInterior [cxTrace, cyTrace] (\assignedMaybeLocs ->
       let (cxMaybeLoc, cyMaybeLoc) = Utils.unwrap2 assignedMaybeLocs in
       mapMaybeToList cxMaybeLoc (\cxLoc ->
         ( "cx", "dx", cxLoc, cxTrace
         , \_ (dx,_) -> solveOne subst cxLoc (cx + toFloat dx) cxTrace
         )
       ) ++
       mapMaybeToList cyMaybeLoc (\cyLoc ->
         ( "cy", "dy", cyLoc, cyTrace
         , \_ (_,dy) -> solveOne subst cyLoc (cy + toFloat dy) cyTrace
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

computeBoxOrOvalTriggers (options, subst) maybeCounts (id, _, attrs) =
  let finishTrigger = addShapeZoneTrigger options id in

  let ((left, leftTrace), (top, topTrace), (right, rightTrace), (bot, botTrace)) =
    Utils.unwrap4 <|
      List.map (LangSvg.toNum << Utils.find_ attrs) <|
        ["LEFT", "TOP", "RIGHT", "BOT"] in

  let leftEdge leftMaybeLoc =
    mapMaybeToList leftMaybeLoc (\leftLoc ->
      ( "LEFT", "dx", leftLoc, leftTrace
      , \_ (dx,_) -> solveOne subst leftLoc (left + toFloat dx) leftTrace
      )) in

  let rightEdge rightMaybeLoc =
    mapMaybeToList rightMaybeLoc (\rightLoc ->
      ( "RIGHT", "dx", rightLoc, rightTrace
      , \_ (dx,_) -> solveOne subst rightLoc (right + toFloat dx) rightTrace
      )) in

  let topEdge topMaybeLoc =
    mapMaybeToList topMaybeLoc (\topLoc ->
      ( "TOP", "dy", topLoc, topTrace
      , \_ (_,dy) -> solveOne subst topLoc (top + toFloat dy) topTrace
      )) in

  let botEdge botMaybeLoc =
    mapMaybeToList botMaybeLoc (\botLoc ->
      ( "BOT", "dy", botLoc, botTrace
      , \_ (_,dy) -> solveOne subst botLoc (bot + toFloat dy) botTrace
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
    , \_ (dx,_) -> solveOne subst xLoc (x + toFloat dx) xTrace
    ))

pointY_ subst i yMaybeLoc y yTrace =
  mapMaybeToList yMaybeLoc (\yLoc ->
    ( "Y" ++ toString i, "dy", yLoc, yTrace
    , \_ (_,dy) -> solveOne subst yLoc (y + toFloat dy) yTrace
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
  let xTraces = List.map (snd << fst << snd) indexedPoints in
  let yTraces = List.map (snd << snd << snd) indexedPoints in

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


computePolyTriggers (options, subst) maybeCounts (id, kind, attrs) =

  let finishTrigger = addShapeZoneTrigger options id in
  let pointX = pointX_ subst in
  let pointY = pointY_ subst in

  let addPointZones = addPointZones_ finishTrigger pointX pointY in
  let addEdgeZones = addEdgeZones_ finishTrigger pointX pointY in
  let addInteriorZone = addInteriorZone_ finishTrigger pointX pointY in

  let indexedPoints = Utils.mapi identity (LangSvg.getPolyPoints attrs) in
  let edges =
    if kind == "polygon" then Utils.selfZipCircConsecPairs indexedPoints
    else {- if kind == "polyline" -}
      let n = List.length indexedPoints in
      Utils.selfZipCircConsecPairs (List.take (n-1) indexedPoints)
  in

  (Dict.empty, maybeCounts)
    |> addPointZones indexedPoints
    |> addEdgeZones edges
    |> addInteriorZone indexedPoints


computePathTriggers (options, subst) maybeCounts (id, _, attrs) =

  let finishTrigger = addShapeZoneTrigger options id in
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

computeFillAndStrokeTriggers (options, subst) maybeCounts (id, _, attrs) =
  let finishTrigger = addShapeZoneTrigger options id in

  let maybeAddColorTrigger realZone fillOrStroke (dict, maybeCounts) =
    case Utils.mapMaybe .av_ (Utils.maybeFind fillOrStroke attrs) of

      Just (AColorNum ((color, colorTrace), _)) ->
        finishTrigger realZone [colorTrace] (\assignedMaybeLocs ->
          let (maybeLoc) = Utils.unwrap1 assignedMaybeLocs in
          mapMaybeToList maybeLoc (\colorLoc ->
            ( fillOrStroke, "dx", colorLoc, colorTrace
            , \_ (dx,_) ->
                let color' = colorNumPlus color dx in
                solveOne subst colorLoc color' colorTrace
            ))
        ) (dict, maybeCounts)

      _ -> (dict, maybeCounts) in

  let maybeAddOpacityTrigger realZone fillOrStroke (dict, maybeCounts) =
    case Utils.mapMaybe .av_ (Utils.maybeFind fillOrStroke attrs) of

      Just (AColorNum (_, (Just (opacity, opacityTrace)))) ->
        finishTrigger realZone [opacityTrace] (\assignedMaybeLocs ->
          let (maybeLoc) = Utils.unwrap1 assignedMaybeLocs in
          mapMaybeToList maybeLoc (\opacityLoc ->
            ( fillOrStroke ++ "Opacity", "dx", opacityLoc, opacityTrace
            , \_ (dx,_) ->
                let opacity' = opacityNumPlus opacity dx in
                solveOne subst opacityLoc opacity' opacityTrace
            ))
        ) (dict, maybeCounts)

      _ -> (dict, maybeCounts) in

  let maybeAddStrokeWidthTrigger realZone (dict, maybeCounts) =
    case Utils.mapMaybe .av_ (Utils.maybeFind "stroke-width" attrs) of

      Just (ANum (width, widthTrace)) ->
        finishTrigger realZone [widthTrace] (\assignedMaybeLocs ->
          let (maybeLoc) = Utils.unwrap1 assignedMaybeLocs in
          mapMaybeToList maybeLoc (\widthLoc ->
            ( "stroke-width", "dx", widthLoc, widthTrace
            , \_ (dx,_) ->
                let width' = strokeWidthNumPlus width dx in
                solveOne subst widthLoc width'  widthTrace
            ))
        ) (dict, maybeCounts)

      _ -> (dict, maybeCounts) in

  let maybeAddRotationTrigger realZone (dict, maybeCounts) =
    case Utils.mapMaybe .av_ (Utils.maybeFind "transform" attrs) of

      Just (ATransform [Rot (rot, rotTrace) (cx, _) (cy ,_)]) ->
        finishTrigger realZone [rotTrace] (\assignedMaybeLocs ->
          let (maybeLoc) = Utils.unwrap1 assignedMaybeLocs in
          mapMaybeToList maybeLoc (\rotLoc ->
            ( "transformRot", "dxy", rotLoc, rotTrace
            , \(mx0,my0) (dx,dy) ->
                let (mx1, my1) = (mx0 + dx, my0 + dy) in
                let
                  radToDeg = Utils.radiansToDegrees
                  a0 = radToDeg <| atan2 (cy - toFloat my0) (toFloat mx0 - cx)
                  a1 = radToDeg <| atan2 (cy - toFloat my1) (toFloat mx1 - cx)
                in
                solveOne subst rotLoc (rot + (a0 - a1)) rotTrace
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
  let scale = 1 / (ShapeWidgets.wColorSlider / LangSvg.maxColorNum) in
  let clamp = Utils.clamp 0 (LangSvg.maxColorNum - 1) in
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
-- Applying Triggers

type alias MouseTrigger2 a = (Int, Int) -> (Int, Int) -> a

applyTrigger
    : Exp -> SubstPlus
   -> Trigger
   -> MouseTrigger2 (Exp, SubstMaybeNum)
        -- i.e. (Int, Int) -> (Int, Int) -> (Exp, SubstMaybeNum)
applyTrigger exp initSubstPlus trigger (mx0,my0) (dx,dy) =
  let initSubst = Dict.map (always .val) initSubstPlus in
  let updates =
     List.foldl (\triggerElement acc ->
       let (_, _, (k,_,_), _, updateFunction) = triggerElement in
       case (Dict.get k acc, updateFunction (mx0,my0) (dx,dy)) of

         (Nothing, maybeSolution) -> Dict.insert k maybeSolution acc

         (Just Nothing, maybeSolution) -> Dict.insert k maybeSolution acc

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
     Dict.foldl (\k maybeNum acc ->
       case maybeNum of
         Nothing -> acc
         Just num -> Dict.insert k num acc
     ) initSubst updates
  in
  (applyLocSubst newSubst exp, updates)

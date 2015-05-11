module Sync (sync, prepareLiveUpdates, printZoneTable) where

import Dict exposing (Dict)
import Set
import Utils
import Debug
import String

import Lang exposing (..)
import Eval
import LangParser


------------------------------------------------------------------------------
-- Value Contexts

multiLeafDiffs = True

type alias VContext = Val
  -- a VContext is a Val with exactly one VHole in single-leaf diff mode
  -- or multiple VHoles in multi-leaf diff mode

type alias HoleSubst = Dict.Dict Int (Val,Val)

fillHole : VContext -> HoleSubst -> Val
fillHole = fillHole_ True

fillHole_ new vc subst = case vc of
  VHole i          -> case Dict.get i subst of
                        Just (vOld,vNew) -> if new then vNew else vOld
  VConst _ _       -> vc
  VBase _          -> vc
  VClosure _ _ _ _ -> vc   -- not recursing into closures
  VList vs         -> VList (List.map (\v -> fillHole_ new v subst) vs)

type VDiff = Same Val | Diff VContext HoleSubst

diff : Val -> Val -> Maybe VDiff
diff v1 v2 =
  let res = diff_ 0 v1 v2 in
  case res of
    Just (_, Diff vc subst) ->
      let (v1',v2') = (fillHole_ False vc subst, fillHole_ True vc subst) in
      if | eqV (v1,v1') && eqV (v2,v2') -> Just (Diff vc subst)
         | otherwise ->
             let f (i,(vOld,vNew)) = [toString i, strVal vOld, strVal vNew] in
             Debug.crash <| Utils.lines <|
               ("bad diff" :: strVal vc :: List.concatMap f (Dict.toList subst))
    _ ->
      Utils.mapMaybe snd res

eqV (v1,v2) = case (v1, v2) of            -- equality modulo traces
  (VConst i tr, VConst j _) -> i == j
  (VList vs1, VList vs2) ->
    case Utils.maybeZip vs1 vs2 of
      Nothing -> False
      Just l  -> List.all eqV l
  _ -> v1 == v2

diffNoCheck v1 v2 =
  Utils.mapMaybe snd (diff_ 0 v1 v2)

-- assuming that v1 is the value resulting from eval (so it has proper locs)
-- and that v2 has dummy locs

diff_ : Int -> Val -> Val -> Maybe (Int, VDiff)
diff_ k v1 v2 = case (v1, v2) of
  (VBase Star, VConst _ _) -> Just (k, Same v2)
  (VConst i tr, VConst j _) ->
    if | i == j    -> Just (k, Same (VConst i tr))  -- cf. comment above
       | otherwise -> let d = Dict.singleton k (v1, (VConst j tr)) in
                      Just (k+1, Diff (VHole k) d)
  (VList vs1, VList vs2) ->
    case Utils.maybeZip vs1 vs2 of
      Nothing -> Nothing
      Just l ->
        List.foldr (\(vi1,vi2) acc ->
          case acc of
            Nothing -> Nothing
            Just (k, Same (VList vs)) ->
              case diff_ k vi1 vi2 of
                Nothing                 -> Nothing
                Just (k, Same v)        -> Just (k, Same (VList (v::vs)))
                Just (k, Diff vc subst) -> Just (k, Diff (VList (vc::vs)) subst)
            Just (k, Diff (VList vs) subst) ->
              case diff_ k vi1 vi2 of
                Nothing                 -> Nothing
                Just (k, Same v)        -> Just (k, Diff (VList (v::vs)) subst)
                Just (k, Diff vc sub')  ->
                  if | not multiLeafDiffs -> Nothing
                     | otherwise ->
                         let d = Dict.union subst sub' in
                         Just (k, Diff (VList (vc::vs)) d)
            Just (_, Diff _ _) ->
              Debug.crash "diff_: error?"
        ) (Just (k, Same (VList []))) l
  _ ->
    if | v1 == v2  -> Just (k, Same v1)
       | otherwise -> Nothing


------------------------------------------------------------------------------

type Equation = Equation Num Trace

locsOfTrace : Trace -> Set.Set Loc
locsOfTrace =
  let foo t = case t of
    TrLoc l   -> if | LangParser.isPreludeLoc l -> Set.empty
                    | otherwise                 -> Set.singleton l
    TrOp _ ts -> List.foldl Set.union Set.empty (List.map foo ts)
  in
  foo

solveOneLeaf : Subst -> Val -> List (LocId, Num)
solveOneLeaf s (VConst i tr) =
  List.map
    (\l -> let s' = Dict.remove l s in
           let n  = solve s' (Equation i tr) in
           (l, n))
    (List.map fst <| Set.toList <| locsOfTrace tr)

inferSubsts : Subst -> List Val -> List Subst
inferSubsts s0 vs =
  List.map (solveOneLeaf s0) vs
    |> Utils.oneOfEach
    |> List.map combine
    |> List.map (Utils.mapMaybe (\s' -> Dict.union s' s0))  -- pref to s'
    |> List.filterMap identity

combine : List (LocId, Num) -> Maybe Subst
combine solutions =
  let f (l,n) msubst =
    let g subst =
      case Dict.get l subst of
        Nothing -> Just (Dict.insert l n subst)
        Just i  -> if | i == n    -> Just (Dict.insert l n subst)
                      | otherwise -> Nothing
    in
    Utils.bindMaybe g msubst
  in
  List.foldl f (Just Dict.empty) solutions

-- assumes that a single variable is being solved for
solve : Subst -> Equation -> Num
solve subst (Equation sum tr) =
  let evalTrace t = case t of
    TrLoc (k,_)  -> case Dict.get k subst of
                      Nothing -> (0, 1)
                      Just i  -> (i, 0)
    TrOp Plus ts -> List.foldl plusplus (0,0) (List.map evalTrace ts)
  in
  let (partialSum,n) = evalTrace tr in
  (sum - partialSum) / n

plusplus = Utils.lift_2_2 (+)

compareVals : (Val, Val) -> Num
compareVals (v1, v2) = case (v1, v2) of
  (VConst i _, VConst j _) -> abs (i-j)
  (VList vs1, VList vs2)   -> case Utils.maybeZip vs1 vs2 of
                                Nothing -> largeInt
                                Just l  -> Utils.sum (List.map compareVals l)
  _                        -> if | v1 == v2  -> 0
                                 | otherwise -> largeInt

largeInt = 99999999

------------------------------------------------------------------------------

getFillers : HoleSubst -> List Val
getFillers = List.map (snd << snd) << Dict.toList

leafToStar v = case v of {VConst _ _ -> VBase Star; _ -> v}

sync : Exp -> Val -> Val -> Result String (List ((Exp, Val), Num))
sync e v v' =
  case diff v v' of
    Nothing       -> Err "bad change"
    Just (Same _) -> Err "no change"
    Just (Diff vc holeSubst) ->
      let newNew = getFillers holeSubst in
      let subst0 = LangParser.substOf e in
      let substs = inferSubsts subst0 newNew in
      Ok <| List.sortBy snd <|
        List.filterMap (\s ->
          let e1 = applySubst s e in
          let v1 = Eval.run e1 in
          let vcStar = mapVal leafToStar vc in
          case diffNoCheck (fillHole vcStar holeSubst) v1 of
            Nothing -> Debug.crash "sync: shouldn't happen?"
            Just (Same _) ->
              let n = compareVals (v, v1) in
              Just ((e1, v1), n)
            Just (Diff _ holeSubst') ->
              let oldNew = getFillers holeSubst' in
              if | newNew /= oldNew -> Nothing
                 | otherwise ->
                     let n = compareVals (v, v1) in
                     Just ((e1, v1), n)
        ) substs


------------------------------------------------------------------------------
-- Zones

zones = [
    ("circle",
      [ ("Interior", ["cx", "cy"])
      , ("Edge", ["r"])
      ])
  , ("ellipse",
      [ ("Interior", ["cx", "cy"])
      , ("Edge", ["rx", "ry"])
      ])
  , ("rect",
      [ ("Interior", ["x", "y"])
      , ("TopLeftCorner", ["x", "y", "width", "height"])
      , ("TopRightCorner", ["y", "width", "height"])
      , ("BotRightCorner", ["width", "height"])
      , ("BotLeftCorner", ["x", "width", "height"])
      , ("LeftEdge", ["x", "width"])
      , ("TopEdge", ["y", "height"])
      , ("RightEdge", ["width"])
      , ("BotEdge", ["height"])
      ])
  , ("line",
      [ ("Point1", ["x1", "y1"])
      , ("Point2", ["x2", "y2"])
      , ("Edge", ["x1", "y1", "x2", "y2"])
      ])
  , ("polygon",
      [ ("TODO", [])
      ])
  ]


------------------------------------------------------------------------------
-- Zones and Triggers

type alias ShapeId = Int
type alias ShapeKind = String
type alias Attr = String
type alias LocSet = Set.Set Loc
type alias Locs = List Loc
type alias Zone = String
type ExtraInfo = None | NumPoints Int

type alias Dict0 = Dict ShapeId (ShapeKind, ExtraInfo, Dict Attr Trace)
type alias Dict1 = Dict ShapeId (ShapeKind, List (Zone, (List Locs)))
type alias Dict2 = Dict ShapeId (ShapeKind, List (Zone, (Locs, List Locs)))

printZoneTable : Val -> String
printZoneTable v =
  shapesToAttrLocs v         -- Step 1: Val   -> Dict0
    |> shapesToZoneTable     -- Step 2: Dict0 -> Dict1
    |> assignTriggers        -- Step 3: Dict1 -> Dict2
    |> strTable              -- Step 4: Dict2 -> String

-- Step 1 --

shapesToAttrLocs : Val -> Dict0
shapesToAttrLocs v = case v of
  VList (VList [VBase (String "svgAttrs"), _] :: vs) ->
    shapesToAttrLocs (VList vs)
  VList vs ->
    let processShape (i,shape) dShapes = case shape of
      VList (VBase (String shape) :: vs') ->
        let processAttr v' (extra,dAttrs) = case v' of
          VList [VBase (String a), VConst _ tr] ->
            (extra, Dict.insert a tr dAttrs)
          VList [VBase (String "points"), VList pts] ->
            let acc' =
              Utils.foldli (\(i,vPt) acc ->
                case vPt of
                  VList [VConst _ trx, VConst _ try] ->
                    let (ax,ay) = ("x" ++ toString i, "y" ++ toString i) in
                    acc |> Dict.insert ax trx
                        |> Dict.insert ay try) dAttrs pts in
            (NumPoints (List.length pts), acc')
          -- NOTE:
          --   string-valued and RGBA attributes are ignored.
          --   see LangSvg.valToSvg for spec of attributes.
          _ ->
            (extra, dAttrs)
        in
        let (extra,attrs) = List.foldl processAttr (None, Dict.empty) vs' in
        Dict.insert i (shape, extra, attrs) dShapes
    in
    Utils.foldli processShape Dict.empty vs

-- Step 2 --

shapesToZoneTable : Dict0 -> Dict1
shapesToZoneTable d0 =
  let foo i stuff acc =
    let (kind,_,_) = stuff in
    Dict.insert i (kind, shapeToZoneInfo stuff) acc in
  Dict.foldl foo Dict.empty d0

shapeToZoneInfo :
  (ShapeKind, ExtraInfo, Dict Attr Trace) -> List (Zone, (List Locs))
shapeToZoneInfo (kind, extra, d) =
  let zones = getZones kind extra in
  let f (s,l) acc =
    let sets =
      l |> List.map (\a -> locsOfTrace <| justGet a d)
        |> Utils.cartProdWithDiff in
    (s, sets) :: acc
  in
  List.foldr f [] zones

justGet k d = Utils.fromJust (Dict.get k d)

getZones : ShapeKind -> ExtraInfo -> List (Zone, List Attr)
getZones kind extra =
  let foo s i = s ++ toString i in
  let xy i    = [foo "x" i, foo "y" i] in
  let pt i    = (foo "Point" i, xy i) in
  case (kind, extra) of
    ("polygon", NumPoints n) ->
      List.map pt [1..n] ++ [("Interior", List.concatMap xy [1..n])]
    _ ->
      Utils.fromJust (Utils.maybeFind kind zones)

-- Step 3 --

-- NOTE: choosing same name setSeen for both accumulators leads
--       to JS undefined error. perhaps due to a shadowing bug?

assignTriggers : Dict1 -> Dict2
assignTriggers d1 =
  let f i (kind,zoneLists) (setSeen1,acc) =
    let g (zone,sets) (setSeen2,acc) =
      case (Utils.findFirst (not << flip Set.member setSeen2) sets, sets) of
        (Nothing, [])         -> (setSeen2, (zone,([],[]))::acc)
        (Nothing, set::sets') -> (setSeen2, (zone,(set,sets'))::acc)
        (Just x,  _)          ->
          let setSeen3 = Set.insert x setSeen2 in
          let acc' = (zone, (x, Utils.removeFirst x sets)) :: acc in
          (setSeen3, acc')
    in
    let (setSeen,zoneLists') = List.foldl g (setSeen1,[]) zoneLists in
    (setSeen, Dict.insert i (kind, List.reverse zoneLists') acc)
  in
  snd <| Dict.foldl f (Set.empty, Dict.empty) d1

-- Step 4 --

strTable : Dict2 -> String
strTable d =
  Dict.toList d
    |> List.map (\(i,(kind,di)) ->
         let s1 = "Shape " ++ toString i ++ " " ++ Utils.parens kind in
         let sRows = List.map strRow di in
         Utils.lines (s1::sRows))
    |> String.join "\n\n"

strRow (zone,(set,sets)) =
     String.padRight 18 ' ' zone
  ++ String.padRight 25 ' ' (if set == [] then "" else strLocs set)
  ++ Utils.spaces (List.map strLocs sets)

strLocs = Utils.braces << Utils.commas << List.map strLoc_

strLoc_ l =
  let (_,mx) = l in
  if | mx == ""  -> strLoc l
     | otherwise -> mx

------------------------------------------------------------------------------

type alias Triggers = Dict ShapeId (Dict Zone Trigger)
type alias Trigger  = List (Attr, Num) -> (Exp, Dict ShapeId (Dict Attr Num))

prepareLiveUpdates : Exp -> Val -> Triggers
prepareLiveUpdates e v =
  let d0 = shapesToAttrLocs v in
  let d1 = shapesToZoneTable d0 in
  let d2 = assignTriggers d1 in
  makeTriggers e d0 d2

-- TODO refactor Dict data structures above to make this more efficient

makeTriggers : Exp -> Dict0 -> Dict2 -> Triggers
makeTriggers e d0 d2 =
  let subst = LangParser.substOf e in
  let f i (_,zones) =
    let g (zone,_) = Dict.insert zone (makeTrigger e d0 d2 subst i zone) in
    List.foldl g Dict.empty zones in
  Dict.map f d2

makeTrigger : Exp -> Dict0 -> Dict2 -> Subst -> ShapeId -> Zone -> Trigger
makeTrigger e d0 d2 subst i zone = \newAttrs ->
  let (subst',changedLocs) =
    let f (attr,newNum) (acc1,acc2) =
      let k = whichLoc d0 d2 i zone attr in
      let subst' = Dict.remove k subst in
      let tr = justGet attr (Utils.thd3 (justGet i d0)) in
      let kSolution = solve subst' (Equation newNum tr) in
      (Dict.insert k kSolution acc1, Set.insert k acc2) in
    List.foldl f (Dict.empty, Set.empty) newAttrs in
  let g i (_,_,di) acc =
    let h attr tr acc =
      let locs = Set.map fst (locsOfTrace tr) in
      if | Utils.setIsEmpty (locs `Set.intersect` changedLocs) -> acc
         | otherwise -> Dict.insert attr (evalTr subst' tr) acc
    in
    let di' = Dict.foldl h Dict.empty di in
    if | Utils.dictIsEmpty di' -> acc
       | otherwise -> Dict.insert i di' acc in
  let e' = applySubst subst' e in
  (e', Dict.foldl g Dict.empty d0)

-- TODO sloppy way of doing this for now...
whichLoc : Dict0 -> Dict2 -> ShapeId -> Zone -> Attr -> LocId
whichLoc d0 d2 i z attr =
  let trLocs =
    justGet i d0 |> Utils.thd3 |> justGet attr |> locsOfTrace in
  let zoneLocs =
    justGet i d2
      |> snd |> Utils.maybeFind z |> Utils.fromJust |> fst
      |> Set.fromList in
  let [(k,_)] = Set.toList (trLocs `Set.intersect` zoneLocs) in
  k

evalTr : Subst -> Trace -> Num
evalTr subst tr = case tr of
  TrLoc (k,_)  -> justGet k subst
  TrOp Plus ts -> List.foldl (+) 0 (List.map (evalTr subst) ts)


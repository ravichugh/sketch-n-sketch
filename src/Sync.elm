module Sync (Options, defaultOptions, syncOptionsOf,
             heuristicsNone, heuristicsFair, heuristicsBiased, toggleHeuristicMode,
             inferLocalUpdates, prepareLiveUpdates,
             printZoneTable, LiveInfo, Triggers, tryToBeSmart,
             locsOfTrace
             ) where

import Dict exposing (Dict)
import Set
import Utils exposing (justGet_)
import Debug
import String

import Lang exposing (..)
-- import LangSvg exposing (NodeId, ShapeKind, Zone, addi)
import LangSvg exposing (..)
import ShapeWidgets exposing (Zone)
import Eval
import OurParser2 as P
import LangParser2 as Parser
import Config
import LangUnparser as Un
import LocEqn exposing (..)

------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugSync

------------------------------------------------------------------------------

addi s i = s ++ toString i

------------------------------------------------------------------------------
-- Sync.Options

-- TODO make general enum functions in Utils
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
-- Value Contexts

multiLeafDiffs = True

type alias VContext = Val
  -- a VContext is a Val with exactly one VHole in single-leaf diff mode
  -- or multiple VHoles in multi-leaf diff mode

type alias HoleSubst = Dict.Dict Int (Val,Val)

fillHole : VContext -> HoleSubst -> Val
fillHole = fillHole_ True

fillHole_ new vc subst = case vc.v_ of
  VHole i          -> case Dict.get i subst of
                        Just (vOld,vNew) -> if new then vNew else vOld
                        Nothing          -> Debug.crash "fillHole_"
  VConst _         -> vc
  VBase _          -> vc
  VClosure _ _ _ _ -> vc   -- not recursing into closures
  VList vs         -> vList (List.map (\v -> fillHole_ new v subst) vs)
  VDict d          -> vDict (Dict.map (\_ v -> fillHole_ new v subst) d)

type VDiff = Same Val | Diff VContext HoleSubst

diff : Val -> Val -> Maybe VDiff
diff v1 v2 =
  let res = diff_ 0 v1 v2 in
  case res of
    Just (_, Diff vc subst) ->
      let (v1',v2') = (fillHole_ False vc subst, fillHole_ True vc subst) in
      if eqV (v1,v1') && eqV (v2,v2') then
        Just (Diff vc subst)
      else
        let f (i,(vOld,vNew)) = [toString i, strVal vOld, strVal vNew] in
        Debug.crash <| Utils.lines <|
          ("bad diff" :: strVal vc :: List.concatMap f (Dict.toList subst))
    _ ->
      Utils.mapMaybe snd res

eqV (v1,v2) = case (v1.v_, v2.v_) of -- equality modulo traces
  (VConst it, VConst jt) -> fst it == fst jt
  (VList vs1, VList vs2) ->
    case Utils.maybeZip vs1 vs2 of
      Nothing -> False
      Just l  -> List.all eqV l
  (VDict d1, VDict d2) ->
    case Utils.maybeZipDicts d1 d2 of
      Nothing -> False
      Just dd -> List.all eqV (Dict.values dd)
  _ -> v1 == v2

diffNoCheck v1 v2 =
  Utils.mapMaybe snd (diff_ 0 v1 v2)

-- assuming that v1 is the value resulting from eval (so it has proper locs)
-- and that v2 has dummy locs

diff_ : Int -> Val -> Val -> Maybe (Int, VDiff)
diff_ k v1 v2 = case (v1.v_, v2.v_) of
  (VBase VStar, VConst _) -> Just (k, Same v2)
  (VConst (i,tr), VConst (j,_)) ->
    if i == j then
      Just (k, Same (vConst (i,tr)))  -- cf. comment above
    else
      let d = Dict.singleton k (v1, (vConst (j,tr))) in
      Just (k+1, Diff (vHole k) d)
  (VList vs1, VList vs2) ->
    case Utils.maybeZip vs1 vs2 of
      Nothing -> Nothing
      Just l ->
        List.foldr (\(vi1,vi2) acc ->
          case acc of
            Nothing -> Nothing
            Just (k, Same vUgh) ->
              case vUgh.v_ of
                VList vs ->
                  case diff_ k vi1 vi2 of
                    Nothing                 -> Nothing
                    Just (k, Same v)        -> Just (k, Same (vList (v::vs)))
                    Just (k, Diff vc subst) -> Just (k, Diff (vList (vc::vs)) subst)
                _ -> Debug.crash "diff_"
            Just (k, Diff vUgh subst) ->
              case vUgh.v_ of
                VList vs ->
                  case diff_ k vi1 vi2 of
                    Nothing                 -> Nothing
                    Just (k, Same v)        -> Just (k, Diff (vList (v::vs)) subst)
                    Just (k, Diff vc sub')  ->
                      if not multiLeafDiffs
                        then Nothing
                        else let d = Dict.union subst sub' in
                             Just (k, Diff (vList (vc::vs)) d)
                _ -> Debug.crash "diff_"
        ) (Just (k, Same (vList []))) l
  (VDict d1, VDict d2) ->
    case Utils.maybeZipDicts d1 d2 of
      Nothing -> Nothing
      Just dd ->
        Dict.foldr (\key (vi1,vi2) acc ->
          case acc of
            Nothing -> Nothing
            Just (k, Same vUgh) ->
              case vUgh.v_ of
                VDict accDict ->
                  case diff_ k vi1 vi2 of
                    Nothing                 -> Nothing
                    Just (k, Same v)        -> Just (k, Same (vDict <| Dict.insert key v accDict))
                    Just (k, Diff vc subst) -> Just (k, Diff (vDict <| Dict.insert key vc accDict) subst)
                _ -> Debug.crash "diff_"
            Just (k, Diff vUgh subst) ->
              case vUgh.v_ of
                VDict accDict ->
                  case diff_ k vi1 vi2 of
                    Nothing                 -> Nothing
                    Just (k, Same v)        -> Just (k, Diff (vDict <| Dict.insert key v accDict) subst)
                    Just (k, Diff vc sub')  ->
                      if not multiLeafDiffs
                        then Nothing
                        else let sub'' = Dict.union subst sub' in
                             Just (k, Diff (vDict <| Dict.insert key vc accDict) sub'')
                _ -> Debug.crash "diff_"
        ) (Just (k, Same (vDict Dict.empty))) dd
  _ ->
    if v1 == v2
      then Just (k, Same v1)
      else Nothing


------------------------------------------------------------------------------

type alias Equation = (Num, Trace)

locsOfTrace : Options -> Trace -> Set.Set Loc
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

{-
    else
      -- want to count the number of non-frozen, non-assignOnce locs
      let keep = (\(_,ann,_) -> ann /= assignOnlyOnce) in
      if List.length (List.filter keep (Set.toList s)) <= 1 then
        -- let _ = Debug.log "dropping" s in
        Set.empty
      else
        s
-}

solveOneLeaf : Options -> Subst -> Val -> List (LocId, Num)
solveOneLeaf opts s v = case v.v_ of
  VConst (i, tr) ->
    List.filterMap
      (\k -> let s' = Dict.remove k s in
             Utils.mapMaybe (\n -> (k,n)) (solve s' (i, tr)))
      (List.map Utils.fst3 <| Set.toList <| locsOfTrace opts tr)
  _ ->
    Debug.crash "solveOneLeaf"

inferSubsts : Options -> Subst -> List Val -> List Subst
inferSubsts opts s0 vs =
  List.map (solveOneLeaf opts s0) vs
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
        Just i  -> if i == n
                     then Just (Dict.insert l n subst)
                     else Nothing
    in
    Utils.bindMaybe g msubst
  in
  List.foldl f (Just Dict.empty) solutions

-- useful for debugging
traceToExp : Subst -> Trace -> Exp
traceToExp subst tr = case tr of
  TrLoc l ->
    case Dict.get (Utils.fst3 l) subst of
      Nothing -> eVar (strLoc l)
      Just n  -> eConst n l
  TrOp op ts ->
    withDummyPos (EOp " " (withDummyRange op) (List.map (traceToExp subst) ts) "")

solve : Subst -> Equation -> Maybe Num
solve subst eqn =
{-
  let (n,t) = eqn in
  (\ans ->
    let _ = Debug.log "solveTopDown" (n, sExp (traceToExp subst t), ans)
    in ans) <|
-}
  (termSolve subst eqn) `Utils.plusMaybe` (solveTopDown subst eqn)

  -- both solveTopDown and termSolve
  -- assumes that a single variable is being solved for


-- Use the Make Equal solver
termSolve : Subst -> Equation -> Maybe Num
termSolve subst (newN, trace) =
  -- The locId missing from subst is what we are solving for
  let locEqn = traceToLocEquation trace in
  let locIds = locEqnLocIds locEqn |> Set.toList in
  let targetLocId =
    locIds
    |> Utils.findFirst (\locId -> Dict.get locId subst == Nothing)
    |> Utils.fromJust_ "subst should be missing a locId"
  in
  case locEqnTerms targetLocId (LocEqnOp Minus [locEqn, LocEqnConst newN]) of
    Just (locPow, locCoeff, rest) ->
      -- We have: coeff*x^pow + rest = 0
      -- We want: x = (-rest / coeff)^(1/pow)
      let coeffEvaled = locEqnEval subst locCoeff in
      let restEvaled  = locEqnEval subst rest in
      let newLocValue = (-restEvaled / coeffEvaled)^(1/locPow) in
      if (isNaN newLocValue) || (isInfinite newLocValue) then
        Nothing
      else
        Just newLocValue

    Nothing ->
      Nothing


evalTrace : Subst -> Trace -> Maybe Num
evalTrace subst t = case t of
  TrLoc (k,_,_) -> Dict.get k subst
  TrOp op ts ->
    Utils.mapMaybe
      (Eval.evalDelta [] op)
      (Utils.projJusts (List.map (evalTrace subst) ts))

evalLoc : Subst -> Trace -> Maybe (Maybe Num)
  -- Just (Just i)   tr is a location bound in subst
  -- Just Nothing    tr is a location not bound (i.e. it's being solved for)
  -- Nothing         tr is not a location
evalLoc subst tr =
  case tr of
    TrOp _ _    -> Nothing
    TrLoc (k,_,_) -> Just (Dict.get k subst)

solveTopDown subst (n, t) = case t of

  TrLoc (k,_,_) ->
    case Dict.get k subst of
      Nothing -> Just n
      Just _  -> Nothing

  TrOp op [t1,t2] ->
    let left  = (evalTrace subst t1, evalLoc   subst t2) in
    let right = (evalLoc   subst t1, evalTrace subst t2) in
    case (isNumBinop op, left, right) of

      -- four cases are of the following form,
      -- where k is the single location variable being solved for:
      --
      --    1.   n =  i op k
      --    2.   n =  i op t2
      --    3.   n =  k op j
      --    4.   n = t1 op j

      (True, (Just i, Just Nothing), _) -> solveR op n i
      (True, (Just i, Nothing), _)      -> Utils.bindMaybe
                                             (\n -> solveTopDown subst (n, t2))
                                             (solveR op n i)
      (True, _, (Just Nothing, Just j)) -> solveL op n j
      (True, _, (Nothing, Just j))      -> Utils.bindMaybe
                                             (\n -> solveTopDown subst (n, t1))
                                             (solveL op n j)

      _ ->
        let _ = debugLog "Sync.solveTopDown" <| strTrace t in
        Nothing

  TrOp op [t1] ->
    case evalTrace subst t1 of
      Just _  -> Nothing
      Nothing ->
        case op of
          Cos     -> maybeFloat <| acos n
          Sin     -> maybeFloat <| asin n
          ArcCos  -> Just <| cos n
          ArcSin  -> Just <| sin n
          Sqrt    -> Just <| n * n
          Round   -> Nothing
          Floor   -> Nothing
          Ceil    -> Nothing
          _       -> let _ = debugLog "TODO solveTopDown" t in
                     Nothing

  _ ->
    let _ = debugLog "TODO solveTopDown" t in
    Nothing

isNumBinop = (/=) Lt

maybeFloat n =
  let thresh = 1000 in
  if isNaN n || isInfinite n then debugLog "maybeFloat Nothing" Nothing
  else if abs n > thresh     then debugLog "maybeFloat (above thresh)" Nothing
  else                            Just n

-- n = i op j
solveR op n i = case op of
  Plus    -> maybeFloat <| n - i
  Minus   -> maybeFloat <| i - n
  Mult    -> maybeFloat <| n / i
  Div     -> maybeFloat <| i / n
  Pow     -> Just <| logBase i n
  Mod     -> Nothing
  ArcTan2 -> maybeFloat <| tan(n) * i
  _       -> Debug.crash "solveR"

-- n = i op j
solveL op n j = case op of
  Plus  -> maybeFloat <| n - j
  Minus -> maybeFloat <| j + n
  Mult  -> maybeFloat <| n / j
  Div   -> maybeFloat <| j * n
  Pow   -> Just <| n ^ (1/j)
  Mod   -> Nothing
  ArcTan2 -> maybeFloat <| j / tan(n)
  _     -> Debug.crash "solveL"


simpleSolve subst (sum, tr) =
  let walkTrace t = case t of
    TrLoc (k,_,_) ->
      case Dict.get k subst of
        Nothing -> Just (0, 1)
        Just i  -> Just (i, 0)
    TrOp Plus ts ->
      let foo mx macc =
        case (mx, macc) of
          (Just (a,b), Just (acc1,acc2)) -> Just (a+acc1, b+acc2)
          _                              -> Nothing
      in
        List.foldl foo (Just (0,0)) (List.map walkTrace ts)
    _ ->
      let _ = debugLog "Sync.simpleSolve" <| strTrace tr in
      Nothing
  in
  Utils.mapMaybe
    (\(partialSum,n) -> (sum - partialSum) / n)
    (walkTrace tr)

compareVals : (Val, Val) -> Num
compareVals (v1, v2) = case (v1.v_, v2.v_) of
  (VConst it, VConst jt) -> abs (fst it - fst jt)
  (VList vs1, VList vs2) -> case Utils.maybeZip vs1 vs2 of
                              Nothing -> largeInt
                              Just l  -> Utils.sum (List.map compareVals l)
  (VDict d1, VDict d2)   -> case Utils.maybeZipDicts d1 d2 of
                              Nothing -> largeInt
                              Just dd -> Utils.sum (Dict.values dd |> List.map compareVals)
  _                      -> if v1 == v2 then 0 else largeInt

largeInt = 99999999

------------------------------------------------------------------------------

getFillers : HoleSubst -> List Val
getFillers = List.map (snd << snd) << Dict.toList

leafToStar v = case v.v_ of
  VConst _ -> vBase VStar
  _        -> v

-- historically, inferLocalUpdates was called "sync"

inferLocalUpdates : Options -> Exp -> Val -> Val -> Result String (List (Exp, Val))
inferLocalUpdates opts e v v' =
  case diff v v' of
    Nothing       -> Err "bad change"
    -- Just (Same _) -> Err "no change"
    Just (Same _) -> Ok []
    Just (Diff vc holeSubst) ->
      let newNew = getFillers holeSubst in
      let subst0 = Parser.substOf e in
      let substs = inferSubsts opts subst0 newNew in
      let res =
        List.map fst <|
          List.sortBy snd <|
            List.filterMap (\s ->
              let e1 = applyLocSubst s e in
              let v1 = fst (Utils.fromOk "Sync.inferLocalUpdates" <| Eval.run e1) in
              let vcStar = mapVal leafToStar vc in
              case diffNoCheck (fillHole vcStar holeSubst) v1 of
                -- TODO 9/24: one of the last few commits affected this
                --   on RelateRects0...
                -- Nothing -> Debug.crash "sync: shouldn't happen?"
                Nothing -> Debug.log "sync: shouldn't happen?" Nothing
                Just (Same _) ->
                  let n = compareVals (v, v1) in
                  Just ((e1, v1), n)
                Just (Diff _ holeSubst') ->
                  let oldNew = getFillers holeSubst' in
                  if newNew /= oldNew
                    then Nothing
                    else
                      let n = compareVals (v, v1) in
                      Just ((e1, v1), n)
            ) substs
      in
      -- TODO: is this a good idea?
      if res == [] then Err "bad change 2" else Ok res


------------------------------------------------------------------------------
-- Triggers

-- NOTE: AttrNames include "fake" attributes
--   e.g. for polygons, x1,y1,x2,y2,x3,y3,...

type alias AttrName = String
type alias Locs = List Loc

-- band-aids for extra metadata...
type ExtraInfo = None | NumPoints Int | NumsPath LangSvg.PathCounts
type alias ExtraExtraInfo = List (AttrName, (Zone, Trace))

type alias NumAttrs = Int

type alias Dict0 = Dict NodeId (ShapeKind, ExtraInfo, ExtraExtraInfo, Dict AttrName Trace)
type alias Dict1 = Dict NodeId (ShapeKind, List (Zone, (NumAttrs, List Locs)))
type alias Dict2 = Dict NodeId (ShapeKind, List (Zone, Maybe (Locs, List Locs)))

printZoneTable : Val -> String
printZoneTable v =
  Debug.crash "printZoneTable not called anywhere"
{-
  let so = defaultOptions in
  nodeToAttrLocs v           -- Step 1: Val   -> Dict0
    |> shapesToZoneTable so  -- Step 2: Dict0 -> Dict1
    |> assignTriggers        -- Step 3: Dict1 -> Dict2
    |> strTable              -- Step 4: Dict2 -> String
-}

-- Step 1 --

-- TODO: assigning IDs is now redundant with valToIndexedTree.
-- so start with IndexedTree rather than Val.

nodeToAttrLocs : Val -> Dict0
nodeToAttrLocs = snd << flip nodeToAttrLocs_ (1, Dict.empty)

nodeToAttrLocs_ : Val -> (Int, Dict0) -> (Int, Dict0)
nodeToAttrLocs_ v (nextId,dShapes) = case v.v_ of

  VList vsUgh -> case List.map .v_ vsUgh of

    [VBase (VString "TEXT"), VBase (VString s)] ->
      (1 + nextId, Dict.insert 1 ("DUMMYTEXT", None, [], Dict.empty) dShapes)

    [VBase (VString kind), VList vs', VList children] ->

      -- processing attributes of current node
      let processAttr v' (extra,extraextra,dAttrs) = case v'.v_ of

        VList vsUghUgh -> case List.map .v_ vsUghUgh of

          [VBase (VString "fill"), VConst (_,tr)] ->
            let ee = ("fill", ("FillBall", tr)) :: extraextra in
            (extra, ee, Dict.insert "fill" tr dAttrs)

          [VBase (VString "stroke"), VConst (_,tr)] ->
            let ee = ("stroke", ("StrokeBall", tr)) :: extraextra in
            (extra, ee, Dict.insert "stroke" tr dAttrs)

          [VBase (VString "fill"), VList [v1, v2]] ->
            case (v1.v_, v2.v_) of
              (VConst (_,tr1), VConst (_,tr2)) ->
                let ee = ("fill", ("FillBall", tr1)) ::
                         ("fillOpacity", ("FillOpacityBall", tr2)) :: extraextra in
                (extra, ee, Dict.insert "fillOpacity" tr2 (Dict.insert "fill" tr1 dAttrs))
              _ -> Debug.crash "nodeToAttrLocs_"

          [VBase (VString "stroke"), VList [v1, v2]] ->
            case (v1.v_, v2.v_) of
              (VConst (_,tr1), VConst (_,tr2)) ->
                let ee = ("stroke", ("StrokeBall", tr1)) ::
                         ("strokeOpacity", ("StrokeOpacityBall", tr2)) :: extraextra in
                (extra, ee, Dict.insert "strokeOpacity" tr2 (Dict.insert "stroke" tr1 dAttrs))
              _ -> Debug.crash "nodeToAttrLocs_"

          [VBase (VString "stroke-width"), VConst (_,tr)] ->
            let ee = ("stroke-width", ("StrokeWidthBall", tr)) :: extraextra in
            (extra, ee, Dict.insert "stroke-width" tr dAttrs)

          -- NOTE: requires for a single cmd, and "transformRot" is a fake attr....
          [VBase (VString "transform"), VList [vBlah]] ->
            case vBlah.v_ of
              VList vsBlah ->
                case List.map .v_ vsBlah of
                  [VBase (VString "rotate"), VConst (_, tr), _, _] ->
                    let ee = ("transformRot", ("RotateBall", tr)) :: extraextra in
                    (extra, ee, Dict.insert "transformRot" tr dAttrs)
                  _ -> Debug.crash "nodeToAttrLocs_"
              _ -> Debug.crash "nodeToAttrLocs_"

          [VBase (VString a), VConst (_,tr)] ->
            (extra, extraextra, Dict.insert a tr dAttrs)

          [VBase (VString "points"), VList pts] ->
            let acc' =
              Utils.foldli (\(i,vPt) acc ->
                case vPt.v_ of
                  VList vsUghUghUgh ->
                    case List.map .v_ vsUghUghUgh of
                      [VConst (_,trx), VConst (_,try)] ->
                        let (ax,ay) = (addi "x" i, addi "y" i) in
                        acc |> Dict.insert ax trx
                            |> Dict.insert ay try
                      _ -> Debug.crash "nodeToAttrLocs_"
                  _ -> Debug.crash "nodeToAttrLocs_"
               ) dAttrs pts in
            (NumPoints (List.length pts), extraextra, acc')

          [VBase (VString "d"), VList vs] ->
            let addPt (mi,(xt,yt)) dict =
              case mi of
                Nothing -> dict
                Just i  -> dict |> Dict.insert (addi "x" i) (snd xt)
                                |> Dict.insert (addi "y" i) (snd yt)
            in
            let addPts pts dict = List.foldl addPt dict pts in
            let (cmds,counts) = LangSvg.valsToPath2 vs in
            let dAttrs' =
              List.foldl (\c acc -> case c of
                LangSvg.CmdZ   s              -> acc
                LangSvg.CmdMLT s pt           -> acc |> addPt pt
                LangSvg.CmdHV  s n            -> acc
                LangSvg.CmdC   s pt1 pt2 pt3  -> acc |> addPts [pt1,pt2,pt3]
                LangSvg.CmdSQ  s pt1 pt2      -> acc |> addPts [pt1,pt2]
                LangSvg.CmdA   s a b c d e pt -> acc |> addPt pt) dAttrs cmds
            in
            (NumsPath counts, extraextra, dAttrs')

          -- NOTE:
          --   string-valued and RGBA attributes are ignored.
          --   see LangSvg.valToSvg for spec of attributes.
          _ ->
            (extra, extraextra, dAttrs)

        _ -> Debug.crash "nodeToAttrLocs_"
      in
      let (extra,ee,attrs) = List.foldl processAttr (None, [], Dict.empty) vs' in

      -- recursing into sub-nodes
      let (nextId',dShapes') =
        List.foldl nodeToAttrLocs_ (nextId,dShapes) children in

      (nextId' + 1, Dict.insert nextId' (kind, extra, ee, attrs) dShapes')

    _ -> Debug.crash "nodeToAttrLocs_"

  _ -> Debug.crash <| "Sync.nodeToAttrLocs_: " ++ strVal v

-- Step 2 --

-- TODO
--   equations are no longer always solvable.
--   so perhaps (symbolically) take into account whether a
--   solution may be Nothing (e.g. because of a division)
--   when computing which Locs may be assigned to a zone.
--   this would go after the cartProdWithDiff...
--   would also need to take into account whether an equation
--   is "top-down solvable" w.r.t to the desired location...

shapesToZoneTable : Options -> Dict0 -> Dict1
shapesToZoneTable opts d0 =
  let foo i stuff acc =
    let (kind,_,_,_) = stuff in
    Dict.insert i (kind, shapeToZoneInfo opts stuff) acc in
  Dict.foldl foo Dict.empty d0

shapeToZoneInfo :
  Options ->
  (ShapeKind, ExtraInfo, ExtraExtraInfo, Dict AttrName Trace) ->
  List (Zone, (NumAttrs, List Locs))
shapeToZoneInfo opts (kind, extra, ee, d) =
  let zones = getZones kind extra ee in
  let f (s,l) acc =
    let numAttrs = List.length l in
    let sets =
      -- temporary way to ignore numbers specified as strings
      -- l |> List.map (\a -> locsOfTrace opts <| justGet_ "%1" a d)
      l |> List.map (\a -> case Dict.get a d of
                             Just tr -> locsOfTrace opts tr
                             Nothing -> Set.empty)
        |> createLocLists opts in
    (s, (numAttrs, sets)) :: acc
  in
  List.foldr f [] zones

allowOverConstrained = True -- CONFIG

createLocLists opts sets =
  -- let foo = Utils.cartProdWithDiff sets in
  let removeEmpties = List.filter ((/=) 0 << Utils.setCardinal) in
  let foo = Utils.cartProdWithDiff (removeEmpties sets) in
  let bar =
    if not allowOverConstrained then []
    else if opts.feelingLucky == heuristicsNone ||
            opts.feelingLucky == heuristicsFair then
      sets |> Utils.intersectMany |> Set.toList |> List.map Utils.singleton
    else if opts.feelingLucky == heuristicsBiased then
      let l = sets |> Utils.intersectMany |> Set.toList in
      Utils.oneOfEach [l,l]
    else
      Debug.crash "createLocLists"
  in
  let baz =
    if foo == bar then
      foo
    else
      foo ++ bar
  in
  List.filter ((/=) []) baz

-- TODO refactor/move all of this to ShapeWidgets...
getZones : ShapeKind -> ExtraInfo -> ExtraExtraInfo -> List (Zone, List AttrName)
getZones kind extra ee =
  let xy i = [addi "x" i, addi "y" i] in
  let pt i = (addi "Point" i, xy i) in
  let edge n i =
    if i <  n then (addi "Edge" i, xy i ++ xy (i+1)) else
    if i == n then (addi "Edge" i, xy i ++ xy 1)
    else Debug.crash "getZones"
  in
  let interior n = ("Interior", List.concatMap xy [1..n]) in
  let basicZones =
    case (kind, extra) of
      ("polyline", NumPoints n) ->
        List.map pt [1..n] ++ List.map (edge n) [1..n-1]
      ("polygon", NumPoints n) ->
        List.map pt [1..n] ++ List.map (edge n) [1..n] ++ [interior n]
      ("path", NumsPath {numPoints}) ->
        List.map pt [1..numPoints] ++ [interior numPoints]
      _ ->
        case Utils.maybeFind kind ShapeWidgets.zones of
          Just zones -> zones
          Nothing    -> []
{-
        Utils.fromJust_
          ("Sync.getZones " ++ kind)
          (Utils.maybeFind kind LangSvg.zones)
-}
  in
  basicZones ++ widgetZones ee

widgetZones = List.map <| \x -> case x of
  ("fill"         , ("FillBall"   , _)) -> ("FillBall"   , ["fill"])
  ("stroke"       , ("StrokeBall" , _)) -> ("StrokeBall" , ["stroke"])
  ("fillOpacity"  , ("FillOpacityBall"   , _)) -> ("FillOpacityBall"   , ["fillOpacity"])
  ("strokeOpacity", ("StrokeOpacityBall" , _)) -> ("StrokeOpacityBall" , ["strokeOpacity"])
  ("stroke-width" , ("StrokeWidthBall" , _)) -> ("StrokeWidthBall" , ["stroke-width"])
  ("transformRot" , ("RotateBall" , _)) -> ("RotateBall" , ["transformRot"])
  _                                     -> Debug.crash "widgetZones"

-- Step 3 --

-- NOTE: choosing same name setSeen for both accumulators leads
--       to JS undefined error. perhaps due to a shadowing bug?

getTriggerType numAttrs locs =
  let n = List.length locs in
  if n == numAttrs then ()
  else if n == 1 then ()
  else Debug.crash "getTriggerType"

{-
  old approach:
    if all locsets in rankedSets have been assigned at least once,
    then just pick the first set in rankedSets.possible sets have already.

  new approach:
    evenly distribute the number of times each locset is assigned.
-}

assignTriggers : Options -> Dict0 -> Dict1 -> Dict2
assignTriggers opts d0 d1 =
  let hm = opts.feelingLucky in
  if hm == heuristicsNone then assignTriggersV2 d1
  else if hm == heuristicsFair then assignTriggersV2 d1
  else assignTriggersV3 d0 d1

assignTriggersV2 d1 =
  let f i (kind,zoneLists) (dictSetSeen1,acc) =
    let g (zone,(numAttrs,sets)) (dictSetSeen2,acc) =
      -- let rankedSets = List.sortBy scoreOfLocs sets in
      let rankedSets = sets in
      let maybeChosenSet =
        List.foldl (\thisSet acc ->
          let thisSet' = removeAlreadyAssignedOnce thisSet dictSetSeen2 in
          case acc of
            Nothing -> Just thisSet'
            Just bestSet ->
            if getCount bestSet dictSetSeen2 < getCount thisSet' dictSetSeen2
              then acc
              else Just thisSet') Nothing rankedSets in
      case maybeChosenSet of
        Nothing -> (dictSetSeen2, (zone, Nothing) :: acc)
        Just chosenSet ->
          (updateCount chosenSet dictSetSeen2, (zone, Just (chosenSet, rankedSets)) :: acc)
    in
    let (dictSetSeen,zoneLists') = List.foldl g (dictSetSeen1,[]) zoneLists in
    (dictSetSeen, Dict.insert i (kind, List.reverse zoneLists') acc)
  in
  snd <| Dict.foldl f (Dict.empty, Dict.empty) d1

assignTriggersV3 d0 d1 =
  let dLocCounts = countLocs d0 in
  let f i (kind,zoneLists) (dictSetSeen1,acc) =
    let g (zone,(numAttrs,sets)) (dictSetSeen2,acc) =
      let rankedSets = List.sortBy (scoreOfLocs2 dLocCounts) sets in
      let maybeChosenSet =
        List.foldl (\thisSet acc ->
          let thisSet' = removeAlreadyAssignedOnce thisSet dictSetSeen2 in
          -- let _ = Debug.log "consider" (zone, scoreOfLocs2 dLocCounts thisSet', thisSet') in
          case acc of
            Nothing -> Just thisSet'
            Just bestSet -> Just bestSet) Nothing rankedSets in
            -- TODO not using dictSetSeen (as in V2), so can get rid of them
      case maybeChosenSet of
        Nothing -> (dictSetSeen2, (zone, Nothing) :: acc)
        Just chosenSet ->
          (dictSetSeen2, (zone, Just (chosenSet, rankedSets)) :: acc)
    in
    let (dictSetSeen,zoneLists') = List.foldl g (dictSetSeen1,[]) zoneLists in
    (dictSetSeen, Dict.insert i (kind, List.reverse zoneLists') acc)
  in
  snd <| Dict.foldl f (Dict.empty, Dict.empty) d1

getCount x dict      = Maybe.withDefault 0 (Dict.get x dict)
updateCount x dict   = Dict.insert x (1 + getCount x dict) dict

-- removeAlreadyAssignedOnce : Locs -> Dict Locs Int -> Locs
-- NOTE:
--   important _not_ to annotate with Locs,
--   b/c that will jeopardize comparable-ness..
removeAlreadyAssignedOnce thisSet counters =
  let coveredLocs =
    -- TODO compute this incrementally in assignTriggers
    Dict.foldl
       (\locs i acc -> Set.union (Set.fromList locs) acc)
       Set.empty counters
  in
  List.filter (\l ->
    let (_,ann,_) = l in
    not (ann == assignOnlyOnce && Set.member l coveredLocs)
  ) thisSet

assignTriggersV1 : Dict1 -> Dict2
assignTriggersV1 d1 =
  let f i (kind,zoneLists) (setSeen1,acc) =
    let g (zone,(numAttrs,sets)) (setSeen2,acc) =
      -- let rankedSets = List.sortBy scoreOfLocs sets in
      let rankedSets = sets in
      let pred = not << flip Set.member setSeen2 in
      case (Utils.findFirst pred rankedSets, rankedSets) of
        (Nothing, [])         -> (setSeen2, (zone,Nothing)::acc)
        (Nothing, set::sets') ->
          let _ = getTriggerType numAttrs set in
          (setSeen2, (zone, Just (set, sets'))::acc)
        (Just x,  _)          ->
          let _ = getTriggerType numAttrs x in
          let setSeen3 = Set.insert x setSeen2 in
          let acc' = (zone, Just (x, Utils.removeFirst x rankedSets)) :: acc in
          (setSeen3, acc')
    in
    let (setSeen,zoneLists') = List.foldl g (setSeen1,[]) zoneLists in
    (setSeen, Dict.insert i (kind, List.reverse zoneLists') acc)
  in
  snd <| Dict.foldl f (Set.empty, Dict.empty) d1

scoreOfLocs : Locs -> Int
scoreOfLocs locs =
  let foo (_,b,mx) acc =
    let _ = Utils.assert "scoreOfLocs" (b == unann) in
    if mx == ""
      then acc
      else acc + 1
  in
  -1 * (List.foldl foo 0 locs)

scoreOfLocs2 : Dict LocId Int -> Locs -> Int
scoreOfLocs2 dLocCounts locs_ =
  let locs = Set.fromList locs_ in
  -- could use log to keep absolute numbers smaller.
  let foo (i,_,_) acc = acc * getCount i dLocCounts in
  let score = Set.foldl foo (1) locs in
  if Utils.setCardinal locs == 1
    then score * score * score
    else score

-- TODO compute these counts along with Dict0
countLocs : Dict0 -> Dict LocId Int
countLocs d0 =
  Dict.foldl (\_ (_,_,_,dAttrNameTrace) acc1 ->
    Dict.foldl (\_ tr acc2 ->
      -- subtle, but should be okay to use defaultOptions,
      -- since countLocs only gets called in hmBiased
      let locSet = locsOfTrace defaultOptions tr in
      Set.foldl (\(locid,_,_) acc3 ->
        updateCount locid acc3
      ) acc2 locSet
    ) acc1 dAttrNameTrace
  ) Dict.empty d0

-- Step 4 --

strTable : Dict2 -> String
strTable d =
  Dict.toList d
    |> List.map (\(i,(kind,di)) ->
         let s1 = addi "Shape " i ++ " " ++ Utils.parens kind in
         let sRows = List.map strRow di in
         Utils.lines (s1::sRows))
    |> String.join "\n\n"

strRow (zone, m) = case m of
  Nothing -> String.padRight 18 ' ' zone
  Just (set,sets) ->
       String.padRight 18 ' ' zone
    ++ String.padRight 25 ' ' (if set == [] then "" else strLocs set)
    ++ Utils.spaces (List.map strLocs sets)

strLocs = Utils.braces << Utils.commas << List.map strLoc_

strLoc_ l =
  let (_,_,mx) = l in
  if mx == ""
    then strLoc l
    else mx

------------------------------------------------------------------------------

-- move this to Model
type alias MouseTrigger2 a = (Int, Int) -> (Int, Int) -> a

type alias Triggers = Dict NodeId (Dict Zone (Maybe Trigger))
type alias Trigger  = MouseTrigger2 (Exp, SubstMaybeNum)

type alias LiveInfo =
  { triggers    : Triggers
  , assignments : Dict NodeId (Dict Zone (LocSet, LocSet))
  , initSubst   : SubstPlus
  }

tryToBeSmart = False

prepareLiveUpdates : Options -> Int -> Int -> Float -> Exp -> Val -> IndexedTree -> Result String LiveInfo
prepareLiveUpdates opts slideNumber movieNumber movieTime e v slate =
  LangSvg.resolveToMovieFrameVal slideNumber movieNumber movieTime v
  |> Result.map (\v' ->
      let d0 = nodeToAttrLocs v' in
      let d1 = shapesToZoneTable opts d0 in
      -- let d2 = assignTriggers d1 in
      let d2 = assignTriggers opts d0 d1 in
      let initSubstPlus = Parser.substPlusOf e in
      let initSubst = Dict.map (always .val) initSubstPlus in
        { triggers    = makeTriggers initSubst opts e d0 d2 slate
        , assignments = zoneAssignments d2
        , initSubst   = initSubstPlus
        }
    )

-- TODO refactor Dict data structures above to make this more efficient

makeTriggers : Subst -> Options -> Exp -> Dict0 -> Dict2 -> IndexedTree -> Triggers
makeTriggers subst opts e d0 d2 slate =
  let f i (kind,zones) =
    let g (zone,m) =
      Dict.insert zone <|
        case m of
          Nothing -> Nothing
          Just _  -> Just (makeTrigger opts e d0 d2 slate subst i kind zone) in
    List.foldl g Dict.empty zones in
  Dict.map f d2

makeTrigger
  : Options -> Exp -> Dict0 -> Dict2 -> IndexedTree
 -> Subst -> NodeId -> ShapeKind -> Zone
 -> Trigger
makeTrigger opts e d0 d2 slate subst id kind zone =
  let trigger_ = makeTrigger_ opts d0 d2 slate subst id kind zone in
  \(mx0,my0) (dx,dy) ->
    let updates = trigger_ (mx0,my0) (dx,dy) in
    let (entireSubst, changedSubst) =
      List.foldl
         (\(k,maybeSolution) (acc1,acc2) ->
           let acc1' =
             case maybeSolution of
               Nothing        -> acc1
               Just kSolution -> Dict.insert k kSolution acc1
           in
           let acc2' =
             -- if this solution failed but previous one succeeded,
             -- don't shadow the previous one
             case (maybeSolution, Dict.member k acc2) of
               (Nothing, True)  -> acc2
               (Nothing, False) -> Dict.insert k maybeSolution acc2
               (Just _, _)      -> Dict.insert k maybeSolution acc2
           in
           (acc1', acc2')
         )
         (subst, Dict.empty)
         updates
    in
    (applyLocSubst entireSubst e, changedSubst)

makeTrigger_
  : Options -> Dict0 -> Dict2 -> IndexedTree
 -> Subst -> NodeId -> ShapeKind -> Zone
 -> MouseTrigger2 (List (LocId, Maybe Num))
makeTrigger_ opts d0 d2 slate subst id kind zone =
  let solveOne = solveOne_ opts d0 d2 subst id zone in
  let offset d = updateBy (toFloat d) in
  case (kind, zone) of

    -- first match zones that can be attached to different shape kinds...

    (_, "FillBall")   ->
      \_ (dx,dy) ->
        let (n,t) = getAColorNum slate id "fill" in
        let n' = LangSvg.clampColorNum (n + scaleColorBall * toFloat dx) in
        solveOne "fill" (n', t)

    (_, "StrokeBall") ->
      \_ (dx,dy) ->
        let (n,t) = getAColorNum slate id "stroke" in
        let n' = LangSvg.clampColorNum (n + scaleColorBall * toFloat dx) in
        solveOne "stroke" (n', t)

    (_, "FillOpacityBall")   ->
      \_ (dx,dy) ->
        let (n,t) = getAColorNumOpacity slate id "fill" in
        let n' = Utils.clamp 0.0 1.0 (n + scaleOpacityBall * toFloat dx) in
        solveOne "fillOpacity" (n', t)

    (_, "StrokeOpacityBall")   ->
      \_ (dx,dy) ->
        let (n,t) = getAColorNumOpacity slate id "stroke" in
        let n' = Utils.clamp 0.0 1.0 (n + scaleOpacityBall * toFloat dx) in
        solveOne "strokeOpacity" (n', t)

    (_, "StrokeWidthBall") ->
      \_ (dx,dy) ->
        let (n,t) = getANum slate id "stroke-width" in
        let n' = LangSvg.clampStrokeWidthNum (n + scaleStrokeWidthBall * toFloat dx) in
        solveOne "stroke-width" (n', t)

    (_, "RotateBall") ->
      let (rot,cx,cy) = getATransformRot slate id "transform" in
      \(mx0,my0) (dx,dy) ->
        let (mx1, my1) = (mx0 + dx, my0 + dy) in
        let a0 = Utils.radiansToDegrees <| atan2 (fst cy - toFloat my0) (toFloat mx0 - fst cx) in
        let a1 = Utils.radiansToDegrees <| atan2 (fst cy - toFloat my1) (toFloat mx1 - fst cx) in
        solveOne "transformRot" (fst rot + (a0 - a1), snd rot)

    -- ... and then match each kind of shape separately

    ("rect", "Interior") ->
      \_ (dx,dy) ->
        solveOne "x"      (offset  dx (rectX slate id)) ++
        solveOne "y"      (offset  dy (rectY slate id))
    ("rect", "RightEdge") ->
      \_ (dx,dy) ->
        solveOne "width"  (offset  dx (rectW slate id))
    ("rect", "BotEdge") ->
      \_ (dx,dy) ->
        solveOne "height" (offset  dy (rectH slate id))
    ("rect", "LeftEdge") ->
      \_ (dx,dy) ->
        solveOne "x"      (offset  dx (rectX slate id)) ++
        solveOne "width"  (offset -dx (rectW slate id))
    ("rect", "TopEdge") ->
      \_ (dx,dy) ->
        solveOne "y"      (offset  dy (rectY slate id)) ++
        solveOne "height" (offset -dy (rectH slate id))
    ("rect", "BotRight") ->
      \_ (dx,dy) ->
        solveOne "width"  (offset  dx (rectW slate id)) ++
        solveOne "height" (offset  dy (rectH slate id))
    ("rect", "TopRight") ->
      \_ (dx,dy) ->
        solveOne "y"      (offset  dy (rectY slate id)) ++
        solveOne "width"  (offset  dx (rectW slate id)) ++
        solveOne "height" (offset -dy (rectH slate id))
    ("rect", "TopLeft") ->
      \_ (dx,dy) ->
        solveOne "x"      (offset  dx (rectX slate id)) ++
        solveOne "y"      (offset  dy (rectY slate id)) ++
        solveOne "width"  (offset -dx (rectW slate id)) ++
        solveOne "height" (offset -dy (rectH slate id))
    ("rect", "BotLeft") ->
      \_ (dx,dy) ->
        solveOne "x"      (offset  dx (rectX slate id)) ++
        solveOne "width"  (offset -dx (rectW slate id)) ++
        solveOne "height" (offset  dy (rectH slate id))

    ("line", "Edge") ->
      \_ (dx,dy) ->
        solveOne "x1" (offset dx (getANum slate id "x1")) ++
        solveOne "y1" (offset dy (getANum slate id "y1")) ++
        solveOne "x2" (offset dx (getANum slate id "x2")) ++
        solveOne "y2" (offset dy (getANum slate id "y2"))
    ("line", "Point1") ->
      \_ (dx,dy) ->
        solveOne "x1" (offset dx (getANum slate id "x1")) ++
        solveOne "y1" (offset dy (getANum slate id "y1"))
    ("line", "Point2") ->
      \_ (dx,dy) ->
        solveOne "x2" (offset dx (getANum slate id "x2")) ++
        solveOne "y2" (offset dy (getANum slate id "y2"))

    ("BOX", _)      -> makeTriggerBoxOrOval solveOne offset slate id zone
    ("OVAL", _)     -> makeTriggerBoxOrOval solveOne offset slate id zone
    ("ellipse", _)  -> makeTriggerEllipse solveOne offset slate id zone
    ("circle", _)   -> makeTriggerCircle solveOne offset slate id zone
    ("polygon", _)  -> makeTriggerPoly solveOne offset slate id kind zone
    ("polyline", _) -> makeTriggerPoly solveOne offset slate id kind zone
    ("path", _)     -> makeTriggerPath solveOne offset slate id zone

    _ -> Debug.crash ("makeTrigger: " ++ kind ++ " " ++ zone)

makeTriggerBoxOrOval solveOne offset slate id zone =
  case zone of
    "Interior" ->
      \_ (dx,dy) ->
        solveOne "LEFT"  (offset dx (getLeft slate id)) ++
        solveOne "RIGHT" (offset dx (getRight slate id)) ++
        solveOne "TOP"   (offset dy (getTop slate id)) ++
        solveOne "BOT"   (offset dy (getBot slate id))
    "RightEdge" ->
      \_ (dx,dy) ->
        solveOne "RIGHT" (offset dx (getRight slate id))
    "BotEdge" ->
      \_ (dx,dy) ->
        solveOne "BOT"   (offset dy (getBot slate id))
    "LeftEdge" ->
      \_ (dx,dy) ->
        solveOne "LEFT"  (offset dx (getLeft slate id))
    "TopEdge" ->
      \_ (dx,dy) ->
        solveOne "TOP"   (offset dy (getTop slate id))
    "BotRight" ->
      \_ (dx,dy) ->
        solveOne "RIGHT" (offset dx (getRight slate id)) ++
        solveOne "BOT"   (offset dy (getBot slate id))
    "TopRight" ->
      \_ (dx,dy) ->
        solveOne "TOP"   (offset dy (getTop slate id)) ++
        solveOne "RIGHT" (offset dx (getRight slate id))
    "TopLeft" ->
      \_ (dx,dy) ->
        solveOne "LEFT"  (offset dx (getLeft slate id)) ++
        solveOne "TOP"   (offset dy (getTop slate id))
    "BotLeft" ->
      \_ (dx,dy) ->
        solveOne "LEFT"  (offset dx (getLeft slate id)) ++
        solveOne "BOT"   (offset dy (getBot slate id))
    _ ->
      Debug.crash ("makeTriggerBoxOrOval: " ++ zone)

makeTriggerEllipse solveOne offset slate id zone =
  let
    left (dx,dy) =
      solveOne "cx" (offset  (dx//2) (getANum slate id "cx")) ++
      solveOne "rx" (offset -(dx//2) (getANum slate id "rx"))
    right (dx,dy) =
      solveOne "cx" (offset  (dx//2) (getANum slate id "cx")) ++
      solveOne "rx" (offset  (dx//2) (getANum slate id "rx"))
    top (dx,dy) =
      solveOne "cy" (offset  (dy//2) (getANum slate id "cy")) ++
      solveOne "ry" (offset -(dy//2) (getANum slate id "ry"))
    bot (dx,dy) =
      solveOne "cy" (offset  (dy//2) (getANum slate id "cy")) ++
      solveOne "ry" (offset  (dy//2) (getANum slate id "ry"))
  in

  case zone of
    "Interior" ->
      \_ (dx,dy) ->
        solveOne "cx" (offset dx (getANum slate id "cx")) ++
        solveOne "cy" (offset dy (getANum slate id "cy"))

    "LeftEdge"  -> \_ dxy -> left dxy
    "RightEdge" -> \_ dxy -> right dxy
    "TopEdge"   -> \_ dxy -> top dxy
    "BotEdge"   -> \_ dxy -> bot dxy

    "TopLeft"   -> \_ dxy -> top dxy ++ left dxy
    "TopRight"  -> \_ dxy -> top dxy ++ right dxy
    "BotLeft"   -> \_ dxy -> bot dxy ++ left dxy
    "BotRight"  -> \_ dxy -> bot dxy ++ right dxy

    _ -> Debug.crash ("makeTriggerEllipse: " ++ zone)

makeTriggerCircle solveOne offset slate id zone =
  let
    left (dx,dy) =
      solveOne "cx" (offset  (dx//2) (getANum slate id "cx")) ++
      solveOne "r"  (offset -(dx//2) (getANum slate id "r"))
    right (dx,dy) =
      solveOne "cx" (offset  (dx//2) (getANum slate id "cx")) ++
      solveOne "r"  (offset  (dx//2) (getANum slate id "r"))
    top (dx,dy) =
      solveOne "cy" (offset  (dy//2) (getANum slate id "cy")) ++
      solveOne "r"  (offset -(dy//2) (getANum slate id "r"))
    bot (dx,dy) =
      solveOne "cy" (offset  (dy//2) (getANum slate id "cy")) ++
      solveOne "r"  (offset  (dy//2) (getANum slate id "r"))
    corner n fx fy =
      solveOne "r"  (offset     (n//2)  (getANum slate id "r")) ++
      solveOne "cx" (offset (fx (n//2)) (getANum slate id "cx")) ++
      solveOne "cy" (offset (fy (n//2)) (getANum slate id "cy"))
  in

  case zone of
    "Interior" ->
      \_ (dx,dy) ->
        solveOne "cx" (offset dx (getANum slate id "cx")) ++
        solveOne "cy" (offset dy (getANum slate id "cy"))

    "LeftEdge"  -> \_ dxy -> left dxy
    "RightEdge" -> \_ dxy -> right dxy
    "TopEdge"   -> \_ dxy -> top dxy
    "BotEdge"   -> \_ dxy -> bot dxy

    "TopLeft"   -> \_ (dx,dy) -> corner (max -dx -dy) ((*) -1) ((*) -1)
    "TopRight"  -> \_ (dx,dy) -> corner (max  dx -dy) ((*)  1) ((*) -1)
    "BotLeft"   -> \_ (dx,dy) -> corner (max -dx  dy) ((*) -1) ((*)  1)
    "BotRight"  -> \_ (dx,dy) -> corner (max  dx  dy) ((*)  1) ((*)  1)

    _ -> Debug.crash ("makeTriggerCircle: " ++ zone)

makeTriggerPoly solveOne offset slate id kind zone =
  case ShapeWidgets.parseZone zone of

    ShapeWidgets.ZInterior ->
      \_ (dx,dy) ->
        let points = getAPoints slate id "points" in
        Utils.foldli
          (\(i,(xt,yt)) acc ->
            solveOne (addi "x" i) (offset dx xt) ++
            solveOne (addi "y" i) (offset dy yt) ++
            acc
          )
          []
          points

    ShapeWidgets.ZPoint (ShapeWidgets.Point i) ->
      let points = getAPoints slate id "points" in
      let (xt,yt) = Utils.geti i points in
      \_ (dx,dy) ->
        solveOne (addi "x" i) (offset dx xt) ++
        solveOne (addi "y" i) (offset dy yt)

    ShapeWidgets.ZPolyEdge i ->
      let points = getAPoints slate id "points" in
      let n = List.length points in
      let (xt1,yt1) = Utils.geti i points in
      if kind == "polyline" && i == n then
        \_ (dx,dy) ->
          solveOne (addi "x" i) (offset dx xt1) ++
          solveOne (addi "y" i) (offset dy yt1)
      else
        let j = if i == n then 1 else i + 1 in
        let (xt2,yt2) = Utils.geti j points in
        \_ (dx,dy) ->
          solveOne (addi "x" i) (offset dx xt1) ++
          solveOne (addi "y" i) (offset dy yt1) ++
          solveOne (addi "x" j) (offset dx xt2) ++
          solveOne (addi "y" j) (offset dy yt2)

    _ ->
     Debug.crash "makeTrigger_: polygon"

makeTriggerPath solveOne offset slate id zone =
  let solvePoint i (dx,dy) (xt,yt) =
    solveOne (addi "x" i) (offset dx xt) ++
    solveOne (addi "y" i) (offset dy yt)
  in

  case ShapeWidgets.parseZone zone of

    ShapeWidgets.ZPoint (ShapeWidgets.Point i) ->
      let cmds = getAPathCmds slate id "d" in
      case findPathPoint cmds i of
        Nothing -> Debug.crash ("makeTrigger path Point " ++ toString i)
        Just pt -> \_ dxy -> solvePoint i dxy pt

    ShapeWidgets.ZInterior ->
      \_ dxy ->
        let cmds = getAPathCmds slate id "d" in
        -- TODO if path is closed with Bezier segment,
        -- don't add equation for last point
{-
        let pt1 =
          case findPathPoint cmds 1 of
            Just pt1 -> pt1
            Nothing  -> Debug.crash "makeTrigger path Interior"
        in
        let maybeSolvePoint i dxy pti =
          if pt1 == pti -- if path is closed, don't add equation for last point
            then []
            else solvePoint i dxy pti
        in
-}
        List.foldl
          (\cmd acc ->
            acc ++ case cmd of
              CmdMLT _ (Just i1, pt1) ->
                solvePoint i1 dxy pt1
              CmdC _ (Just i1, pt1) (Just i2, pt2) (Just i3, pt3) ->
                solvePoint i1 dxy pt1 ++ solvePoint i2 dxy pt2 ++ solvePoint i3 dxy pt3
              CmdSQ _ (Just i1, pt1) (Just i2, pt2) ->
                solvePoint i1 dxy pt1 ++ solvePoint i2 dxy pt2
              CmdA _ _ _ _ _ _ (Just i1, pt1) ->
                solvePoint i1 dxy pt1
              _ ->
                []
          )
          []
          cmds

    _ ->
      Debug.crash ("makeTrigger path " ++ zone)

findPathPoint cmds i =
  let maybeKeepPoint (mj,pt) = if mj == Just i then Just pt else Nothing in
  List.foldl
     (\cmd acc ->
       let idPoints =
         case cmd of
           CmdMLT _ pt1          -> [pt1]
           CmdC   _ pt1 pt2 pt3  -> [pt1, pt2, pt3]
           CmdSQ  _ pt1 pt2      -> [pt1, pt2]
           CmdA   _ _ _ _ _ _ pt -> [pt]
           _                     -> []
       in
       List.foldl Utils.plusMaybe acc (List.map maybeKeepPoint idPoints)
     )
     Nothing
     cmds

updateBy : Num -> NumTr -> Equation
updateBy offset (n,t) = let n' = n + offset in (n', t)

solveOne_ opts d0 d2 subst i zone attr (n',t) =
  case whichLoc opts d0 d2 i zone attr of
    Nothing -> []
    Just k ->
      let subst' = Dict.remove k subst in
      let maybeSolution = solve subst' (n',t) in
      [(k, maybeSolution)]

-- some of these helpers overlap with LangSvg

getANum = getAVal <| \aval ->
  case aval.av_ of
    ANum nt -> nt
    _       -> Debug.crash "getANum"

getAPoints = getAVal <| \aval ->
  case aval.av_ of
    APoints pts -> pts
    _           -> Debug.crash "getAPoints"

getAPathCmds = getAVal <| \aval ->
  case aval.av_ of
    APath2 (cmds, _) -> cmds
    _                -> Debug.crash "getAPathCmds"

getATransformRot = getAVal <| \aval ->
  case aval.av_ of
    ATransform [Rot n1 n2 n3] -> (n1,n2,n3)
    _                         -> Debug.crash "getATransformRot"

getAColorNum = getAVal <| \aval ->
  case aval.av_ of
    AColorNum (nt, _) -> nt
    _                 -> Debug.crash "getAColorNum"

getAColorNumOpacity = getAVal <| \aval ->
  case aval.av_ of
    AColorNum (_, Just nt) -> nt
    _                      -> Debug.crash "getAColorNumOpacity"

getAVal foo slate i attrName =
  case Dict.get i slate of
    Just (LangSvg.SvgNode _ attrs _) ->
      case Utils.maybeFind attrName attrs of
        Just aval -> foo aval
        Nothing   -> Debug.crash ("getAVal: " ++ toString (i, attrName))
    Nothing                   -> Debug.crash "getAVal"
    Just (LangSvg.TextNode _) -> Debug.crash "getAVal"

rectX slate i = getANum slate i "x"
rectY slate i = getANum slate i "y"
rectW slate i = getANum slate i "width"
rectH slate i = getANum slate i "height"

getLeft slate i  = getANum slate i "LEFT"
getTop slate i   = getANum slate i "TOP"
getRight slate i = getANum slate i "RIGHT"
getBot slate i   = getANum slate i "BOT"

{-
rectRightEdge slate i =
  getANum slate i "x" `plusNumTr` getANum slate i "width"

rectBotEdge slate i =
  getANum slate i "y" `plusNumTr` getANum slate i "height"
-}

-- TODO sloppy way of doing this for now...
whichLoc : Options -> Dict0 -> Dict2 -> NodeId -> Zone -> AttrName -> Maybe LocId
whichLoc opts d0 d2 i z attr =
  let trLocs =
    -- temporary way to ignore numbers specified as strings
    -- justGet_ "%4" i d0 |> Utils.thd3 |> justGet_ "%5" attr |> locsOfTrace opts in
    justGet_ "%4" i d0 |> Utils.fourth4 |> Dict.get attr |>
      \m -> case m of
        Just tr -> locsOfTrace opts tr
        Nothing -> Set.empty in
  let zoneLocs =
    justGet_ "%6" i d2
      |> snd |> Utils.maybeFind z |> Utils.fromJust
      |> Utils.fromJust_ "guaranteed not to fail b/c of check in makeTriggers"
      |> fst |> Set.fromList in
  case Set.toList (trLocs `Set.intersect` zoneLocs) of
    [(k,_,_)] -> Just k
    []        -> Nothing
    -- _         -> Debug.crash "whichLoc"
    locs ->
      if opts.feelingLucky == heuristicsBiased then
        Just <| case (locs, String.left 1 attr) of
                  ([loc1,loc2], "x") -> Utils.fst3 loc2
                  ([loc1,loc2], "y") -> Utils.fst3 loc1
                  (loc1::_,_) -> Utils.fst3 loc1
                  _ -> Debug.crash "whichLoc"
      else
        Debug.crash "whichLoc"

evalTr subst tr = Utils.fromJust_ "evalTr" (evalTrace subst tr)

-- duplicated from InterfaceView2 for now
wGradient = 250
scaleColorBall = 1 / (wGradient / LangSvg.maxColorNum)

wStrokeWidthBox = 60
scaleStrokeWidthBall = 1 / (wStrokeWidthBox / LangSvg.maxStrokeWidthNum)

wOpacityBox = 20
scaleOpacityBall = 1 / wOpacityBox

------------------------------------------------------------------------------

setFromLocLists : List Locs -> LocSet
setFromLocLists = List.foldl (flip Set.union << Set.fromList) Set.empty

-- TODO compute this along with everything else
-- could also make this a single dictionary: Dict (NodeId, Zone) Locs
zoneAssignments : Dict2 -> Dict NodeId (Dict Zone (LocSet, LocSet))
zoneAssignments =
  Dict.map <| \i (_,l) ->
    List.foldl (\(z,m) acc ->
      case m of
        Just (locs,otherLocs) ->
          let yellowLocs = Set.fromList locs in
          let grayLocs   = setFromLocLists otherLocs `Set.diff` yellowLocs in
          Dict.insert z (yellowLocs, grayLocs) acc
        Nothing       -> acc
    ) Dict.empty l


module Sync (Options, defaultOptions, syncOptionsOf,
             heuristicsNone, heuristicsFair, heuristicsBiased, toggleHeuristicMode,
             inferLocalUpdates, inferStructuralUpdates, prepareLiveUpdates,
             inferDeleteUpdates,
             inferNewRelationships,
             relateSelectedAttrs,
             relate,
             strCall,
             printZoneTable, LiveInfo, Triggers, tryToBeSmart,
             locsOfTrace
             ) where

import Dict exposing (Dict)
import Set
import Utils exposing (justGet_)
import Debug
import String

import Lang exposing (..)
import LangSvg exposing (NodeId, ShapeKind, Zone, addi)
import Eval
import OurParser2 as P
import LangParser2 as Parser
import Config
import LangUnparser as Un
import LocEqn exposing (..)

------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugSync

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

eqV (v1,v2) = case (v1.v_, v2.v_) of            -- equality modulo traces
  (VConst it, VConst jt) -> fst it == fst jt
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
diff_ k v1 v2 = case (v1.v_, v2.v_) of
  (VBase Star, VConst _) -> Just (k, Same v2)
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
      (Eval.evalDelta op)
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
  RangeOffset _ -> Nothing
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
  RangeOffset _ -> Nothing
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
  (VConst it, VConst jt)   -> abs (fst it - fst jt)
  (VList vs1, VList vs2)   -> case Utils.maybeZip vs1 vs2 of
                                Nothing -> largeInt
                                Just l  -> Utils.sum (List.map compareVals l)
  _                        -> if v1 == v2 then 0 else largeInt

largeInt = 99999999

------------------------------------------------------------------------------

getFillers : HoleSubst -> List Val
getFillers = List.map (snd << snd) << Dict.toList

leafToStar v = case v.v_ of
  VConst _ -> vBase Star
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
              let v1 = fst (Eval.run e1) in
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
-- Naive Structural Update

stripSvg v =
  case v.v_ of
    VList vs ->
      case List.map .v_ vs of
        [VBase (String "svg"), VList vs1, VList vs2] ->
          (vs1, vs2)
        _ -> Debug.crash "stripSvg"
    _ -> Debug.crash "stripSvg"

idOldShapes  = "oldCanvas"
idNewShape i = "newShape" ++ toString i
eOldShapes   = eVar idOldShapes
eNewShape i  = eVar (idNewShape i)

addComments = False -- CONFIG

comment s e =
  if addComments
    then eComment s e
    else e

inferStructuralUpdates : Exp -> Val -> Val -> List (Exp, Val)
inferStructuralUpdates eOld v v' =
  let (attrs1,children1) = stripSvg v in
  let (attrs2,children2) = stripSvg v' in
  let _ = Utils.assert "Sync.inferStruct" (attrs1 == attrs2) in

  let diff =
    let foo (i,(vi,vi')) acc =
      if vi == vi'
        then acc
        else (i,vi') :: acc
    in
    List.reverse (Utils.foldli foo [] (Utils.zip children1 children2)) in

  let eNewCanvas =
    let es =
      List.map (\(i,_) ->
        let n = toFloat i in
        ePair (eConst n dummyLoc) (eNewShape i)) diff in
      eApp (eVar "updateCanvas") [eOldShapes, eList es Nothing] in

  let bindings =
    List.map (\(i,vi) ->
      -- going through parser to avoid adding EVal
      let ei = Utils.fromOk "Sync.addNew" (Parser.parseE (strVal vi)) in
      (idNewShape i, ei)) diff in

  let eNew_ =
    comment " Here's your original program..." <|
    comment "" <|
      eLets [(idOldShapes, eOld)] <|
        comment "" <|
        comment " ... and here are the hard-coded updates:" <|
        comment "" <|
          eLets bindings <|
            comment "" <|
            comment " Refactor if you'd like!" <|
            comment "" <|
              eNewCanvas in

  -- going through parser so that new location ids are assigned
  let eNew = Utils.fromOk "Sync.inferStruct" (Parser.parseE (sExp eNew_)) in
  [(eNew, fst (Eval.run eNew))]


------------------------------------------------------------------------------
-- "Dead Code Elimination"

type alias IndexTrace = (Loc, Loc, Int)

indexTraces : Trace -> List IndexTrace
indexTraces t = case t of
  TrOp (RangeOffset i) [TrLoc l1, TrLoc l2] -> [(l1, l2, i)]
  TrOp _ ts -> List.concatMap indexTraces ts
  _ -> []

sortIndexTraces (l1,u1,i1) (l2,u2,i2) =
  case compare (l1,u1) (l2,u2) of
    EQ  -> compare i1 i2
    ord -> ord

indexTracesOfVal : Val -> List IndexTrace
indexTracesOfVal v =
  let f v acc = case v.v_ of
    VConst (_, t) -> indexTraces t ++ acc
    _             -> acc
  in
  List.sortWith sortIndexTraces <| foldVal f v []

removeDeadIndices e v' =
  let idxTraces = indexTracesOfVal v' in
  let foo e__ = case e__ of
    EIndList _ rs _ -> EList " " (List.concatMap (expandRange idxTraces) rs) "" Nothing ""
    _               -> e__
  in
  mapExpViaExp__ foo e

expandRange idxTraces r =
  let mem = flip List.member idxTraces in
  case r.val of
    Point e -> case e.val.e__ of
      EConst _ n l _ -> if mem (l,l,0) then [e] else []
      _               -> Debug.crash "expandRange"
    Interval e1 ws e2 -> case (e1.val.e__, e2.val.e__) of
      -- TODO may be better to just put exactly one space in
      -- between each element and ignore existing positions
      (EConst _ n1 l1 _, EConst _ n2 l2 _) ->
        let d = ceiling (n2 - n1) in
        let foo i (nextStart,acc) =
          let return a =
            let end = Un.bumpCol (String.length (strNum a)) nextStart in
            let ei  = P.WithInfo (exp_ (EConst " " a dummyLoc noWidgetDecl)) nextStart end in
            (Un.incCol end, ei :: acc)
          in
          let m = n1 + toFloat i in
          if mem (l1,l2,i) && m > n2 then return n2 else
          if mem (l1,l2,i)           then return m
          else                            (nextStart, acc)
        in
        snd <| List.foldr foo (e1.start, []) [0..d]
      _ -> Debug.crash "expandRange"

type alias MaybeOne a = List a
nothing               = []
just                  = Utils.singleton
maybeToMaybeOne mx    = case mx of
  Nothing -> nothing
  Just x  -> just x

inferDeleteUpdates : Exp -> Val -> Val -> MaybeOne (Exp, Val)
inferDeleteUpdates eOld v v' =
  let (attrs1,children1) = stripSvg v in
  let (attrs2,children2) = stripSvg v' in
  let _ = Utils.assert "Sync.inferDeleteUpdates" (attrs1 == attrs2) in

  let onlyDeletes =
    let foo (vi,vi') =
      if vi == vi'                       then Nothing
      else if vi' == LangSvg.dummySvgVal then Just True
      else                                    Just False
    in
    let l = List.map foo (Utils.zip children1 children2) in
    List.length (List.filter ((==) (Just True)) l) > 0
      && List.length (List.filter ((==) (Just False)) l) == 0
  in

  if not onlyDeletes then nothing
  else
    -- freshen is needed b/c EConsts have been added (and removed)
    let eNew = Parser.freshen <| removeDeadIndices eOld v' in
    just (eNew, fst <| Eval.run eNew)


------------------------------------------------------------------------------
-- "Relate"

stripSvgNode : Bool -> Bool -> (String -> Bool) -> Val -> Maybe (List Val)
stripSvgNode b1 b2 kPred v =
  case v.v_ of
    VList vsUgh ->
      case List.map .v_ vsUgh of
        [VBase (String k'), VList vs1, VList vs2] ->
          case (kPred k', b1, vs1, b2, vs2) of
            (True, True, _, False, []) -> Just vs1
            (True, False, [], True, _) -> Just vs2
            _                          -> Nothing
        _ ->
          Debug.crash "stripSvgNode"
    _ ->
      Nothing

stripAttrs    = stripSvgNode True False
stripChildren = stripSvgNode False True

justBind = flip Utils.bindMaybe

getAttr : List Val -> String -> Maybe Val
getAttr l k = case l of
  [] -> Nothing
  v0::l' -> case v0.v_ of
    VList [vk, v] ->
      let k' = unwrapVBaseString_ "getAttr" vk.v_ in
      if k == k'
        then Just v
        else getAttr l' k
    _ ->
      Debug.crash "getAttr"

getAttrs : List String -> List Val -> Maybe (List Val)
getAttrs ks l = Utils.projJusts <| List.map (getAttr l) ks

basicRectAttrs     = ["x","y","width","height","fill"]
getBasicRectAttrs  = getAttrs basicRectAttrs

pluckOut attrLists i = List.map (Utils.geti i) attrLists

unzipBasicRectAttrs attrLists =
  List.map (pluckOut attrLists) [1 .. List.length basicRectAttrs]

sortRectsByX = List.sortBy (valToNum << Utils.fromJust << flip getAttr "x")
sortRectsByY = List.sortBy (valToNum << Utils.fromJust << flip getAttr "y")

collectExtraRectAttrs rects =
  let f attrs acc0 =
    let g v acc1 =
      let (vk_, _) = Utils.unwrap2 <| unwrapVList_ "collectExtraRectAttrs" v in
      let k = unwrapVBaseString_ "collectExtraRectAttrs" vk_ in
      if List.member k basicRectAttrs
        then acc1
        else k :: acc1
    in
    List.foldl g acc0 attrs
  in
  Utils.removeDupes <| List.foldl f [] rects

makeExtraRectAttrDicts rects =
  let ks = collectExtraRectAttrs rects in
  let processKey k =
    let processRect (i,attrs) acc =
      case getAttr attrs k of
        Nothing -> acc
        Just v  -> (i,v) :: acc
    in
    let indexedVals = Utils.foldri processRect [] rects in
    let table = strDictOfIndexedVals indexedVals in
    (k, table)
  in
  Utils.bracks <|
  String.join "\n                 " <|
    List.map
      (\(k,strTable) -> Utils.bracks (Utils.spaces [strVal (vStr k), strTable]))
      (List.map processKey ks)

pluckOutExtra attrLists k =
  Utils.foldri <| \(i,attrs) acc ->
    case getAttr attrs k of
      Nothing -> acc
      Just v  -> (i,v) :: acc

nl a b = a ++ "\n" ++ b

strCall f xs = Utils.parens (Utils.spaces (f::xs))

strInferred cap x ys =
  strCall "inferred" [x, cap, Utils.bracks (Utils.spaces ys)]

-- could switch to most common element if desired
--
chooseFirst list =
  let (v,vs) = Utils.uncons list in
  if Utils.allSame (v::vs)
    then strVal v
    else strInferred "'first of'" (strVal v) (List.map strVal (v::vs))

-- TODO this is duplicating toNum for Val rather than AVal...
valToNum v = case v.v_ of
  VConst (n,_) -> n
  VBase (String s) ->
    case String.toFloat s of
      Ok n -> n
      _    -> Debug.crash "valToNum"
  _ -> Debug.crash "valToNum"

chooseAvg_ : List Val -> (Int, String)
chooseAvg_ vals =
  let nums = List.map valToNum vals in
  if Utils.allSame nums then
    (round <| Utils.head_ nums, toString (Utils.head_ nums))
  else
    let avg = round <| Utils.avg nums in
    let s = strInferred "'average of'" (toString avg) (List.map toString nums) in
    (avg, s)

chooseAvg = snd << chooseAvg_

{-
lookupWithDefault def vals =
  let foo (i,v) = Utils.bracks (Utils.spaces [toString (i-1), strVal v]) in
  let s = Utils.bracks (Utils.spaces (Utils.mapi foo vals)) in
  strCall "lookupWithDefault" [toString def, "i", s]
-}

strDictOf vals =
  let foo (i,v) = Utils.bracks (Utils.spaces [(toString (i-1)) ++ "!", strVal v]) in
  Utils.bracks (Utils.spaces (Utils.mapi foo vals))

strDictOfIndexedVals indexedVals =
  Utils.bracks <| Utils.spaces <|
    List.map
      (\(i,v) -> Utils.bracks <| Utils.spaces [(toString (i-1)) ++ "!", strVal v])
      indexedVals

-- returns Nothing if not sorted (either non-decreasing or non-increasing)
--
baseAndOffset vals =
  let nums   = List.map valToNum vals in
  let pairs  = Utils.adjacentPairs False nums in
  let deltas = List.map (\(a,b) -> b-a) pairs in
  if not (List.all ((<=) 0) deltas || List.all ((>=) 0) deltas) then Nothing
  else
    let
      base   = strInferred "'smallest'"
                 (toString <| Utils.head_ nums)
                 (List.map toString nums)
      offset = strInferred "'average delta between'"
                 (toString <| round <| Utils.avg <| List.reverse deltas)
                 (List.map toString nums)
     in
     Just (base, offset)

inferXY xy vals =
  let
    xyBase          = xy ++ "Base"
    xyOff           = xy ++ "Off"
    xyBasePlusOff   = "(+ " ++ xyBase ++ " (mult i " ++ xyOff ++ "))"
    xyTable         = xy ++ "Table"
    xyLookup        = strCall "lookupWithDefault" ["10", "i", xyTable]
  in
  case baseAndOffset vals of
    Just (base,off) ->
      let
        s1 = "    (let " ++ xyBase ++ "  " ++ base            `nl`
             "    (let " ++ xyOff ++ "   " ++ off             `nl` ""
        s2 = "    (let " ++ xy ++ "      " ++ xyBasePlusOff   `nl` ""
      in
      (s1, s2, ")))")
    Nothing ->
      let
        s1 = "    (let " ++ xyTable ++ " " ++ strDictOf vals  `nl` ""
        s2 = "    (let " ++ xy ++ "      " ++ xyLookup        `nl` ""
      in
      (s1, s2, "))")

inferRelatedRectsX : Exp -> Val -> Val -> Maybe (Exp, Val)
inferRelatedRectsX = inferRelatedRects sortRectsByX "'right'"

inferRelatedRectsY : Exp -> Val -> Val -> Maybe (Exp, Val)
inferRelatedRectsY = inferRelatedRects sortRectsByY "'down'"

inferRelatedRects sortRectsByXY flow _ _ v' =
  stripChildren ((==) "svg") v' `justBind` (\shapes ->
  let mRects = List.map (stripAttrs ((==) "rect")) shapes in
  Utils.projJusts mRects `justBind` (\rects_ ->
  let rects = sortRectsByXY rects_ in
  Utils.projJusts (List.map getBasicRectAttrs rects) `justBind` (\attrLists ->
    let n = List.length attrLists in
    let indices = Utils.ibracks (Utils.spaces (List.map toString [0..n-1])) in
    let flowIndices = strCall "flow" [flow, "indices"] in
    let (xs, ys, widths, heights, fills) = Utils.unwrap5 <| unzipBasicRectAttrs attrLists in
    let (let_xBaseAndOff, let_x, xParens) = inferXY "x" xs in
    let (let_yBaseAndOff, let_y, yParens) = inferXY "y" ys in
    let xyParens = xParens ++ yParens in
    let extraAttrDicts = makeExtraRectAttrDicts rects in
    let theRect =
      if extraAttrDicts == "[]" then
        "      (rect fill x y width height)"
      else
        "    (let extras " ++ extraAttrDicts                    `nl`
        "      (addExtras i extras"                             `nl`
        "        (rect fill x y width height)))"
    in
    let s =
      "(def newGroup"                                           `nl`
      "  (let indices " ++ indices                              `nl`
      "  (groupMap " ++ flowIndices ++ " (\\i"                  `nl`
            let_xBaseAndOff                                      ++
            let_yBaseAndOff                                      ++
            let_x                                                ++
            let_y                                                ++
      "    (let width  " ++ chooseAvg widths                    `nl`
      "    (let height " ++ chooseAvg heights                   `nl`
      "    (let fill   " ++ chooseFirst fills                   `nl`
             theRect ++ ")))))))" ++ xyParens                   `nl`
      ""                                                        `nl`
      "(svg newGroup)"
    in
    let eNew = Utils.fromOk_ <| Parser.parseE s in
    let vNew = fst <| Eval.run eNew in
    Just (eNew, vNew)
  )))

basicCircleAttrs     = ["cx","cy","r","fill"]
getBasicCircleAttrs  = getAttrs basicCircleAttrs

unzipBasicCircleAttrs attrLists =
  List.map (pluckOut attrLists) [1 .. List.length basicCircleAttrs]

sortCirclesByCX = List.sortBy (valToNum << Utils.fromJust << flip getAttr "cx")

inferCircleOfCircles : Bool -> Exp -> Val -> Val -> Maybe (Exp, Val)
inferCircleOfCircles groupBox _ _ v' =
  stripChildren ((==) "svg") v' `justBind` (\shapes ->
  let mCircles = List.map (stripAttrs ((==) "circle")) shapes in
  Utils.projJusts mCircles `justBind` (\circles_ ->
  let circles = sortCirclesByCX circles_ in
  Utils.projJusts (List.map getBasicCircleAttrs circles) `justBind` (\attrLists ->
    let n = List.length attrLists in
    let indices =
      Utils.ibracks (Utils.spaces (List.map (flip (++) "!" << toString) [0..n-1])) in
    let flowIndices =
      if groupBox
      then strCall "flow" ["'ccw and groupbox'", "indices"]
      else strCall "flow" ["'ccw'", "indices"] in
    let (cxs, cys, rs, fills) = Utils.unwrap4 <| unzipBasicCircleAttrs attrLists in
    let (gx, sgx) = chooseAvg_ cxs in
    let (gy, sgy) = chooseAvg_ cys in
    let cxys = Utils.zip (List.map valToNum cxs) (List.map valToNum cys) in
    let dists = List.map (Utils.distance (toFloat gx, toFloat gy)) cxys in
    let gr = round <| Utils.avg dists in
    let rot =
      let (cx,cy) = -- (cx,cy) of rightmost circle
        Utils.unwrap2 <| List.map (valToNum << Utils.last_) [cxs,cys] in
      if cy <= toFloat gy
      then atan ((toFloat gy - cy) / (cx - toFloat gx))        -- quad I
      else -1 * (atan ((cy - toFloat gy) / (cx - toFloat gx))) -- quad IV
    in
    let theShapes =
      if groupBox then
        "  (let box"                                              `nl`
        "    (let padding " ++ "10!"                              `nl`
        "    (let len       (+ (* 2! gr) (* 2! padding))"         `nl`
        "      (rectCenter [0! 0! 0! 0!] gcx gcy len len)))"      `nl`
        "  (let circles"                                          `nl`
        "    (groupMap " ++ flowIndices ++ " (\\i"                `nl`
        "      (let cx (+ gcx (* gr (cos (* i theta))))"          `nl`
        "      (let cy (- gcy (* gr (sin (* i theta))))"          `nl`
        "      (let r       " ++ chooseAvg rs                     `nl`
        "      (let fill    " ++ chooseFirst fills                `nl`
        "        (circle fill cx cy r)))))))"                     `nl`
        "  (basicZonesTail"                                       `nl`
        "    (map (rotateAround rot gcx gcy)"                     `nl`
        "         (cons box circles)))))"
      else
        "  (groupMap " ++ flowIndices ++ " (\\i"                  `nl`
        "    (let cx   (+ gcx (* gr (cos (+ rot (* i theta)))))"  `nl`
        "    (let cy   (- gcy (* gr (sin (+ rot (* i theta)))))"  `nl`
        "    (let r    " ++ chooseAvg rs                          `nl`
        "    (let fill " ++ chooseFirst fills                     `nl`
        "      (circle fill cx cy r)))))))"
    in
    let s =
      "(def newGroup"                                           `nl`
      "  (let gcx     " ++ sgx                                  `nl`
      "  (let gcy     " ++ sgy                                  `nl`
      "  (let gr      " ++ toString gr                          `nl`
      "  (let theta   " ++ toString (2*pi / toFloat n) ++ "!"   `nl`
      "  (let rot     " ++ toString rot                         `nl`
      "  (let indices " ++ indices                              `nl`
            theShapes ++ ")))))))"                              `nl`
      ""                                                        `nl`
      "(svg newGroup)"
    in
    let eNew = Utils.fromOk_ <| Parser.parseE s in
    let vNew = fst <| Eval.run eNew in
    Just (eNew, vNew)
  )))

basicLineAttrs     = ["x1","y1","x2","y2","stroke","stroke-width"]
getBasicLineAttrs  = getAttrs basicLineAttrs

unzipBasicLineAttrs attrLists =
  List.map (pluckOut attrLists) [1 .. List.length basicLineAttrs]

type Corner = TL | TR | BL | BR

nearestCorner : (Num,Num) -> List (Corner, (Num,Num)) -> (Corner, (Num,Num))
nearestCorner pt l0 =
  let l1 = List.map (\(corner,cornerPt) -> ((corner, cornerPt), Utils.distance pt cornerPt)) l0 in
  let l2 = List.sortBy snd l1 in
  fst (Utils.head_ l2)

relativeTo (x',y') (corner,(x,y)) =
  let sx = case corner of
    TL -> Utils.parens ("+ x0 " ++ toString (x' - x) ++ "!")
    BL -> Utils.parens ("+ x0 " ++ toString (x' - x) ++ "!")
    _  -> Utils.parens ("- xw " ++ toString (x - x') ++ "!")
  in
  let sy = case corner of
    TL -> Utils.parens ("+ y0 " ++ toString (y' - y) ++ "!")
    TR -> Utils.parens ("+ y0 " ++ toString (y' - y) ++ "!")
    _  -> Utils.parens ("- yh " ++ toString (y - y') ++ "!")
  in
  (sx, sy)

clampX s = Utils.parens ("clampX " ++ s)
clampY s = Utils.parens ("clampY " ++ s)

placeX n = Utils.parens ("placeX " ++ toString n ++ "!")
placeY n = Utils.parens ("placeY " ++ toString n ++ "!")

-- True: elastic; False: sticky
inferGroupOfLines : Bool -> Exp -> Val -> Val -> Maybe (Exp, Val)
inferGroupOfLines elastic _ _ v' =
  stripChildren ((==) "svg") v' `justBind` (\shapes ->
  let mLines = List.map (stripAttrs ((==) "line")) shapes in
  Utils.projJusts mLines `justBind` (\lines ->
  Utils.projJusts (List.map getBasicLineAttrs lines) `justBind` (\attrLists_ ->
    -- let attrLists = List.map (List.map valToNum) attrLists_ in
    let attrLists =
      List.map
         (\values ->
           let (v1,v2,v3,v4,v5,v6) = Utils.unwrap6 values in
           (List.map valToNum [v1,v2,v3,v4], [v5,v6]))
         attrLists_ in
    let (x0,maxX,y0,maxY) =
      let foo (x1y1x2y2,_) (minX,maxX,minY,maxY) =
        let (x1,y1,x2,y2) = Utils.unwrap4 x1y1x2y2 in
        let minX' = if min x1 x2 < minX then min x1 x2 else minX in
        let maxX' = if max x2 x2 > maxX then max x1 x2 else maxX in
        let minY' = if min y1 y2 < minY then min y1 y2 else minY in
        let maxY' = if max y2 y2 > maxY then max y1 y2 else maxY in
        (minX', maxX', minY', maxY')
      in
      let min_ = 99999 in
      let max_ = -1 * max_ in
      List.foldl foo (min_, max_, min_, max_) attrLists
    in
    let (w,h) = (maxX - x0, maxY - y0) in
    let (xTL,xTR,xBL,xBR) = (x0, x0 + w, x0, x0 + w) in
    let (yTL,yTR,yBL,yBR) = (y0, y0, y0 + h, y0 + h) in
    let corners = [ (TL, (xTL,yTL))
                  , (TR, (xTR,yTR))
                  , (BL, (xBL,yBL))
                  , (BR, (xBR,yBR)) ] in
    let newLines =
      let goo (x1y1x2y2,strokesw) acc =
        let (x1,y1,x2,y2) = Utils.unwrap4 x1y1x2y2 in
        let (stroke,sw) = Utils.unwrap2 strokesw in
        let l1 = [ "line", strVal stroke, strVal sw ] in
        let l2 =
          if elastic then
            [ "\n      ", placeX ((x1-x0)/w), placeY ((y1-y0)/h)
            , "\n      ", placeX ((x2-x0)/w), placeY ((y2-y0)/h) ]
          else
            let (x1',y1') = relativeTo (x1,y1) <| nearestCorner (x1,y1) corners in
            let (x2',y2') = relativeTo (x2,y2) <| nearestCorner (x2,y2) corners in
            [ "\n      ", clampX x1', clampY y1'
            , "\n      ", clampX x2', clampY y2' ]
        in
        Utils.parens (Utils.spaces (l1 ++ l2)) :: acc
      in
      List.foldr goo [] attrLists
    in
    let sBounds = Utils.spaces (List.map toString [x0,y0,w,h]) in
    let sHelpers =
      if elastic then
        "[placeX placeY]   " ++ "[(\\p (+ x0 (* p w))) (\\p (+ y0 (* p h)))]"
      else
        "[clampX clampY]   " ++ "[(clamp x0 xw) (clamp y0 yh)]"
    in
    let s =
      "(def newGroup (\\(x0 y0 w h)"                            `nl`
      "  (let [xw yh]           " ++ "[(+ x0 w) (+ y0 h)]"      `nl`
      "  (let gcx               " ++ " (+ x0 (/ (- xw x0) 2))"  `nl`
      "  (let gcy               " ++ " (+ y0 (/ (- yh y0) 2))"  `nl`
      "  (let rot               " ++ "0"                        `nl`
      "  (let padding           " ++ "10!"                      `nl`
      "  (let " ++ sHelpers                                     `nl`
      "  (let box"                                              `nl`
      "    (let [gx gy] [(- x0 padding) (- y0 padding)]"        `nl`
      "    (let gw (+ w (mult 2! padding))"                     `nl`
      "    (let gh (+ h (mult 2! padding))"                     `nl`
      "      (rect [0! 0! 0! 0!] gx gy gw gh))))"               `nl`
      "  (let lines"                                            `nl`
      "    " ++ Utils.bracks (String.join "\n     " newLines)   `nl`
      "  (basicZonesTail"                                       `nl`
      "    (map (rotateAround rot gcx gcy)"                     `nl`
      "         (cons box lines)))))))))))))"                   `nl`
      ""                                                        `nl`
      "(svg (newGroup " ++ sBounds ++ "))"
      -- "  (let [xTL xTR xBL xBR] " ++ "[x0 xw x0 xw]"            `nl`
      -- "  (let [yTL yTR yBL yBR] " ++ "[y0 y0 yh yh]"            `nl`
      -- "  (cons box lines))))))))))"                             `nl`
    in
    let eNew = Utils.fromOk_ <| Parser.parseE s in
    let vNew = fst <| Eval.run eNew in
    Just (eNew, vNew)
  )))

dummyFrozenLoc = dummyLoc_ frozen

-- TODO relate in terms of AVals instead

relate : Int -> Exp -> List Val -> (Int, List (Exp, Val))
relate k0 e vs =
  let (k1,l1) = relateNums k0 e vs in
  (k1, l1)

type alias NTT = (NumTr, VTrace)

relateNums genSymK e vs =
  let noResults = (genSymK, []) in
  let foo v = case v.v_ of
    VConst nt -> Just (nt, v.vtrace)
    _         -> Nothing
  in
  case Utils.projJusts (List.map foo vs) of
    Nothing   -> noResults
    Just []   -> noResults
    Just ntts ->
      -- not sorting ntts here right now
      let (k1,l1) = relateNumsWithVar genSymK e (List.map fst ntts) in
      let (k2,l2) = relateBaseOffset k1 e ntts in
      (k2, l1 ++ l2)

relateBaseOffset : Int -> Exp -> List NTT -> (Int, List (Exp, Val))
relateBaseOffset genSymK e ntts_ =
  -- sorting by the number, so that base is always first
  let ntts = List.sortBy (fst << fst) ntts_ in
  let noResults = (genSymK, []) in
  let projBase t = case snd (fst t) of
    TrLoc (_,_,"") -> Nothing
    TrLoc loc      -> Just loc
    -- _              -> Debug.crash "relateBaseOffset"
    tr             -> Debug.crash <| "relateBaseOffset: " ++ strTrace tr
    -- TODO need to take static variable into account...
    -- the TrLoc cases should be the last resort...
  in
  let projBaseOff baseLoc ntt = case ntt of
    -- TODO check that t2 is a constant (loc w/o var)
    ((_, TrOp Plus [TrLoc loc, t2]), vtrace) ->
      if loc == baseLoc
        then Just vtrace
        else Nothing
    _ -> Nothing
  in
  case ntts of
    [] -> noResults
    ntt0::ntts' ->
      case projBase ntt0 of
        Nothing -> noResults
        Just baseLoc ->
          let (_,_,baseVar) = baseLoc in
          case Utils.projJusts (List.map (projBaseOff baseLoc) ntts') of
            Nothing -> noResults
            Just vtraces ->
              let esubst =
                let foo is acc =
                  case Utils.findFirst (not << Parser.isPreludeEId) is of
                    Nothing -> acc
                    Just i  -> Dict.insert i (EVar " " baseVar) acc
                in
                List.foldl foo Dict.empty vtraces
              in
              -- let _ = Debug.log "applied" (toString (Dict.toList esubst)) in
              let eNew = applyESubst esubst e in
              let vNew = fst <| Eval.run eNew in
              (genSymK, [(eNew, vNew)])

relateNumsWithVar : Int -> Exp -> List NumTr -> (Int, List (Exp, Val))
relateNumsWithVar genSymK e nts =
  let noResults = (genSymK, []) in
  let gensym = "gensym" ++ toString genSymK in
  let ((n0,t0),rest) = Utils.uncons <| List.sortBy fst nts in
  case (t0, rest) of
    (TrOp _ _, _) -> noResults
    (TrLoc _, []) -> noResults
    (TrLoc l0, _) ->
      let esubstAndNumsMaybe =
        let foo (ni,ti) acc =
          case (acc, ti) of
            (Nothing, _)       -> Nothing
            (Just _, TrOp _ _) -> Nothing
            (Just (d,all,someDiffLoc), TrLoc li) ->
              let e__ =
                if n0 == ni
                then EVar " " gensym
                else
                  (ePlus (eVar <| " " ++ gensym ++ " ") -- TODO spacing hack...
                         (eConst (ni-n0) dummyFrozenLoc)).val.e__
              in
              let someDiffLoc' = someDiffLoc || li /= l0 in
              Just (Dict.insert (Utils.fst3 li) e__ d, ni::all, someDiffLoc')
        in
        let init = (Dict.singleton (Utils.fst3 l0) (EVar " " gensym), [n0], False) in
        List.foldl foo (Just init) rest
      in
      case esubstAndNumsMaybe of
        Nothing -> noResults
        Just (eSubst1, allNums, someDiffLoc) ->
          let mkAnswer esubst gensymVal =
            let eNew =
              applyESubst esubst e
                |> Un.unparse
                |> (++) ("(def " ++ gensym ++ " " ++ gensymVal ++ ")\n")
                |> Parser.parseE
                |> Utils.fromOk "Sync.relate"
            in
            let vNew = fst <| Eval.run eNew in
            (eNew, vNew)
          in
          let ans1 =
            if someDiffLoc
            then [mkAnswer eSubst1 (toString n0)]
            else []
          in
          let ans2 =
            if Utils.allSame allNums then []
            else
              let nAvg = Utils.avg allNums in
              let eSubst2 = Dict.map (\_ _ -> EVar " " gensym) eSubst1 in
              let eAvg = strInferred "'average of'" (toString nAvg) (List.map toString allNums) in
              [mkAnswer eSubst2 eAvg]
          in
          (1 + genSymK, ans1 ++ ans2)

inferRelated : Int -> Exp -> Val -> Val -> (Int, List (Exp, Val))
inferRelated genSymK e _ v' =
  let noResults = (genSymK, []) in
  case stripChildren ((==) "svg") v' of
    Nothing -> noResults
    Just canvas ->
      let mShapes = List.map (stripAttrs (always True)) canvas in
      let shapes = Utils.filterJusts mShapes in
      let selectedAttrs =
        let foo attrList acc =
          case getAttr attrList "SELECTED" of
            Nothing -> acc
            Just s  ->
              case s.v_ of
                VBase (String "") -> acc
                VBase (String s)  ->
                  let goo k = Utils.fromJust_ "inferRelated" (getAttr attrList k) in
                  List.map goo (String.split " " s) ++ acc
                _ ->
                  Debug.crash "inferRelated"
        in
        List.foldl foo [] shapes
      in
      relate genSymK e selectedAttrs

inferNewRelationships e v v' =
     maybeToMaybeOne (inferRelatedRectsX e v v')
  ++ maybeToMaybeOne (inferRelatedRectsY e v v')
  -- ++ maybeToMaybeOne (inferCircleOfCircles False e v v')
  ++ maybeToMaybeOne (inferCircleOfCircles True e v v')
  ++ maybeToMaybeOne (inferGroupOfLines True e v v')
  ++ maybeToMaybeOne (inferGroupOfLines False e v v')

relateSelectedAttrs genSymK e v v' =
  inferRelated genSymK e v v'


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

    [VBase (String "TEXT"), VBase (String s)] ->
      (1 + nextId, Dict.insert 1 ("DUMMYTEXT", None, [], Dict.empty) dShapes)

    [VBase (String kind), VList vs', VList children] ->

      -- processing attributes of current node
      let processAttr v' (extra,extraextra,dAttrs) = case v'.v_ of

        VList vsUghUgh -> case List.map .v_ vsUghUgh of

          [VBase (String "fill"), VConst (_,tr)] ->
            let ee = ("fill", ("FillBall", tr)) :: extraextra in
            (extra, ee, Dict.insert "fill" tr dAttrs)

          [VBase (String "stroke"), VConst (_,tr)] ->
            let ee = ("stroke", ("StrokeBall", tr)) :: extraextra in
            (extra, ee, Dict.insert "stroke" tr dAttrs)

          -- NOTE: requires for a single cmd, and "transformRot" is a fake attr....
          [VBase (String "transform"), VList [vBlah]] ->
            case vBlah.v_ of
              VList vsBlah ->
                case List.map .v_ vsBlah of
                  [VBase (String "rotate"), VConst (_, tr), _, _] ->
                    let ee = ("transformRot", ("RotateBall", tr)) :: extraextra in
                    (extra, ee, Dict.insert "transformRot" tr dAttrs)
                  _ -> Debug.crash "nodeToAttrLocs_"
              _ -> Debug.crash "nodeToAttrLocs_"

          [VBase (String a), VConst (_,tr)] ->
            (extra, extraextra, Dict.insert a tr dAttrs)

          [VBase (String "points"), VList pts] ->
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

          [VBase (String "d"), VList vs] ->
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
        List.map pt [1..numPoints]
      _ ->
        case Utils.maybeFind kind LangSvg.zones of
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

type alias Triggers = Dict NodeId (Dict Zone (Maybe Trigger))
type alias Trigger  = List (AttrName, Num) -> (Exp, SubstMaybeNum)
-- type alias Trigger  = List (AttrName, Num) -> (Exp, Dict NodeId (Dict AttrName Num))

type alias LiveInfo =
  { triggers    : Triggers
  , assignments : Dict NodeId (Dict Zone (LocSet, LocSet))
  , initSubst   : SubstPlus
  }

tryToBeSmart = False

prepareLiveUpdates : Options -> Int -> Int -> Float -> Exp -> Val -> LiveInfo
prepareLiveUpdates opts slideNumber movieNumber movieTime e v =
  let v' = LangSvg.resolveToMovieFrameVal slideNumber movieNumber movieTime v in
  let d0 = nodeToAttrLocs v' in
  let d1 = shapesToZoneTable opts d0 in
  -- let d2 = assignTriggers d1 in
  let d2 = assignTriggers opts d0 d1 in
  let initSubstPlus = Parser.substPlusOf e in
  let initSubst = Dict.map (always .val) initSubstPlus in
    { triggers    = makeTriggers initSubst opts e d0 d2
    , assignments = zoneAssignments d2
    , initSubst   = initSubstPlus
    }

-- TODO refactor Dict data structures above to make this more efficient

makeTriggers : Subst -> Options -> Exp -> Dict0 -> Dict2 -> Triggers
makeTriggers subst opts e d0 d2 =
  let f i (_,zones) =
    let g (zone,m) =
      Dict.insert zone <|
        case m of
          Nothing -> Nothing
          Just _  -> Just (makeTrigger opts e d0 d2 subst i zone) in
    List.foldl g Dict.empty zones in
  Dict.map f d2

makeTrigger : Options -> Exp -> Dict0 -> Dict2 -> Subst -> NodeId -> Zone -> Trigger
makeTrigger opts e d0 d2 subst i zone = \newAttrs ->
  -- TODO symbolically compute changes !!!
  -- once this is done, might be able to rank trigger sets by int/float
  let (entireSubst, changedSubst, changedLocs) =
    let f (attr,newNum) (acc1,acc2,acc3) =
      {- 6/25: now that assigned locs do not appear in every attribute,
               whichLoc may return Nothing
      let k = whichLoc opts d0 d2 i zone attr in
      let subst' = Dict.remove k subst in
      let tr = justGet attr (Utils.thd3 (justGet i d0)) in
      case solve subst' (newNum, tr) of
        -- solve will no longer always return an answer, so one of
        -- the locations assigned to this trigger may not have an
        -- effect after all... (see Dict1 comment)
        Nothing -> (acc1, acc2)
        Just kSolution -> (Dict.insert k kSolution acc1, Set.insert k acc2)
      -}
      case whichLoc opts d0 d2 i zone attr of
        Nothing -> (acc1, acc2, acc3)
        Just k ->
          let subst' = Dict.remove k subst in
          let tr = justGet_ "%2" attr (Utils.fourth4 (justGet_ "%3" i d0)) in
          case solve subst' (newNum, tr) of
            -- solve will no longer always return an answer, so one of
            -- the locations assigned to this trigger may not have an
            -- effect after all... (see Dict1 comment)
            Nothing -> (acc1, Dict.insert k Nothing acc2, acc3)
            Just kSolution ->
              let acc1' = Dict.insert k kSolution acc1 in
              let acc2' = Dict.insert k (Just kSolution) acc2 in
              let acc3' = Set.insert k acc3 in
              (acc1', acc2', acc3')
    in
    List.foldl f (subst, Dict.empty, Set.empty) newAttrs in
    -- if using overconstrained triggers, then some of the newAttr values
    -- from the UI may be "immediately destroyed" by subsequent ones...

  (applyLocSubst entireSubst e, changedSubst)

{-
  let g i (_,_,_,di) acc =
    let h attr tr acc =
      let locs = Set.map Utils.fst3 (locsOfTrace opts tr) in
      if | Utils.setIsEmpty (locs `Set.intersect` changedLocs) -> acc
         | otherwise -> Dict.insert attr (evalTr subst' tr) acc

         -- updating "points" based on fake attributes would be easier
         -- if the Little representation simply kept separate attributes
         -- for each point...
         {-
         | otherwise ->
             let default () = Dict.insert attr (evalTr subst' tr) acc in
             case String.uncons attr of
               Nothing -> default ()
               Just (pre,suf) ->
                 if pre == 'x' || pre == 'y' then
                   case String.toInt suf of
                     Err _ -> default ()
                     Ok i  -> Debug.log "SYNC: change other point..." <| default ()
                 else
                   default()
        -}
    in
    -- let di' = Dict.foldl h Dict.empty di in
    let di' =
      if | tryToBeSmart -> Dict.foldl h Dict.empty di
         | otherwise    -> Dict.empty in
    if | Utils.dictIsEmpty di' -> acc
       | otherwise -> Dict.insert i di' acc in
  let e' = applyLocSubst subst' e in
  (e', Dict.foldl g Dict.empty d0)
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


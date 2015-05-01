module Sync (sync) where

import Dict
import Set
import Utils
import Debug

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

locsOfTrace : Trace -> List Loc
locsOfTrace =
  let foo t = case t of
    TrLoc l   -> if | LangParser.isPreludeLoc l -> Set.empty
                    | otherwise                 -> Set.singleton l
    TrOp _ ts -> List.foldl Set.union Set.empty (List.map foo ts)
  in
  Set.toList << foo

solveOneLeaf : Subst -> Val -> List (Loc, Num)
solveOneLeaf s (VConst i tr) =
  List.map
    (\l -> let s' = Dict.remove l s in
           let n  = solve s' (Equation i tr) in
           (l, n))
    (locsOfTrace tr)

inferSubsts : Subst -> List Val -> List Subst
inferSubsts s0 vs =
  List.map (solveOneLeaf s0) vs
    |> Utils.oneOfEach
    |> List.map combine
    |> List.map (Utils.mapMaybe (\s' -> Dict.union s' s0))  -- pref to s'
    |> List.filterMap identity

combine : List (Loc, Num) -> Maybe Subst
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
    TrLoc l      -> case Dict.get l subst of
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


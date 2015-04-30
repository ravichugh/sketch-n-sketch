module Sync (sync) where

import Dict
import Set
import Utils
import Debug

import Lang exposing (..)
import Eval
import LangParser

type Equation = Equation Int Trace

locsOfTrace : Trace -> List Loc
locsOfTrace =
  let foo t = case t of
    TrLoc l   -> Set.singleton l
    TrOp _ ts -> List.foldl Set.union Set.empty (List.map foo ts)
  in
  Set.toList << foo

inferSubsts : Subst -> Val -> List Subst
inferSubsts s (VConst i tr) =
  List.map
    (\l -> let s' = Dict.remove l s in
           let n  = solve s' (Equation i tr) in
           Dict.insert l n s')
    (locsOfTrace tr)

-- assumes that a single variable is being solved for
solve : Subst -> Equation -> Int
solve subst (Equation sum tr) =
  let evalTrace t = case t of
    TrLoc l      -> case Dict.get l subst of
                      Nothing -> (0, 1)
                      Just i  -> (i, 0)
    TrOp Plus ts -> List.foldl plusplus (0,0) (List.map evalTrace ts)
  in
  let (partialSum,n) = evalTrace tr in
  (sum - partialSum) // n
 
plusplus = Utils.lift_2_2 (+)

compareVals : (Val, Val) -> Int
compareVals (v1, v2) = case (v1, v2) of
  (VConst i _, VConst j _) -> abs (i-j)
  (VList vs1, VList vs2)   -> case Utils.maybeZip vs1 vs2 of
                                Nothing -> largeInt
                                Just l  -> Utils.sum (List.map compareVals l)
  _                        -> if | v1 == v2  -> 0
                                 | otherwise -> largeInt

largeInt = 99999999

------------------------------------------------------------------------------

sync : Exp -> Val -> Val -> Result String (List ((Exp, Val), Int))
sync e v v' =
  case diff v v' of
    Nothing       -> Err "bad change"
    Just (Same _) -> Err "no change"
    Just (Diff vc holeSubst) ->
      let [(_,(w,w'))] = Dict.toList holeSubst in -- TODO
      let subst0 = LangParser.substOf e in
      let substs = inferSubsts subst0 w' in
      Ok <| List.sortBy snd <|
        List.filterMap (\s ->
          let e1 = applySubst s e in
          let v1 = Eval.run e1 in
          let vcStar = mapVal (\x -> case x of
                                       VConst _ _ -> VBase Star
                                       _          -> x) vc
          in
          case diff_ (fillHoleWith vcStar holeSubst) v1 of
            Nothing -> Debug.crash "sync: shouldn't happen?"
            Just (Same _) ->
              let n = compareVals (v, v1) in
              Just ((e1, v1), n)
            Just (Diff _ holeSubst') ->
              let [(_,(_,w1))] = Dict.toList holeSubst' in -- TODO
              if | w1 /= w' -> Nothing
                 | otherwise ->
                     let n = compareVals (v, v1) in
                     Just ((e1, v1), n)
        ) substs


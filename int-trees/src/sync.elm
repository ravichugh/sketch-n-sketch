module Sync where

import List
import Dict

import Lang (..)

type Equation = Equation Int Trace

inferSubsts : Subst -> Val -> List Subst
inferSubsts s (VConst i tr) =
  List.map
    (\l -> let s' = Dict.remove l s in
           let n  = solve s' (Equation i tr) in
           Dict.insert l n s')
    (Dict.keys s)

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
 
-- TODO: weird bug when trying to abstract this over f
plusplus (i1,j1) (i2,j2) = (i1+i2, j1+j2)


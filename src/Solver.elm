module Solver
  ( Equation
  , solve
  ) where

import Lang exposing (..)
import Eval
import LocEqn exposing (LocEquation(..), locEqnEval, locEqnTerms, locEqnLocIds)
import Utils
import Config

import Set
import Dict
import Debug


--------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugSync

--------------------------------------------------------------------------------

-- TODO streamline Equation/LocEquation/Trace

type alias Equation = (Num, Trace)

{-
solveOneLeaf : Options -> Subst -> Val -> List (LocId, Num)
solveOneLeaf opts s v = case v.v_ of
  VConst (i, tr) ->
    List.filterMap
      (\k -> let s' = Dict.remove k s in
             Utils.mapMaybe (\n -> (k,n)) (solve s' (i, tr)))
      (List.map Utils.fst3 <| Set.toList <| locsOfTrace opts tr)
  _ ->
    Debug.crash "solveOneLeaf"
-}

{-
inferSubsts : Options -> Subst -> List Val -> List Subst
inferSubsts opts s0 vs =
  List.map (solveOneLeaf opts s0) vs
    |> Utils.oneOfEach
    |> List.map combine
    |> List.map (Utils.mapMaybe (\s' -> Dict.union s' s0))  -- pref to s'
    |> List.filterMap identity
-}

{-
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
-}

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


--------------------------------------------------------------------------------
-- "Make Equal" Solver

-- Use the Make Equal solver
termSolve : Subst -> Equation -> Maybe Num
termSolve subst (newN, trace) =
  -- The locId missing from subst is what we are solving for
  let locEqn = LocEqn.traceToLocEquation trace in
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


--------------------------------------------------------------------------------
-- "Top-Down" Solver (a.k.a. Solver B)

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


--------------------------------------------------------------------------------
-- "Addition-Only" Solver (a.k.a. Solver A)

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

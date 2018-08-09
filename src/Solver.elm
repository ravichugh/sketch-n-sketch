module Solver exposing (..)

import ImpureGoodies
import Lang exposing (Num, Op_, Trace(..), Subst)
import MathExp exposing (..)
import Utils

import Dict exposing (Dict)


--------------------------------------------------------------------------------

type alias Eqn = (MathExp, MathExp) -- LHS, RHS

type alias Problem  = (List Eqn, List Int) -- System of equations, and varIds to solve for (usually a singleton).
type alias Solution = List (MathExp, Int)

type alias SolutionsCache = Dict Problem (List Solution)

type NeedSolutionException = NeedSolution Problem


-- Some locId should be missing from the Subst: the missing locId will be solved for.
--
-- Side effect: throws exception if solution not in cache; controller should ask solver for solution and retry action.
solveTrace : SolutionsCache -> Subst -> Trace -> Num -> Maybe Num
solveTrace solutionsCache subst trace targetVal =
  let mathExp = traceToMathExp trace in
  let targetVarId = Utils.diffAsSet (mathExpVarIds mathExp) (Dict.keys subst) |> Utils.head "Solver.solveTrace: expected trace to have a locId remaining after applying subst" in
  -- Variablify everything to have the most general form of equations in the cache.
  -- (May push into solve in the future.)
  let targetValInsertedVarId = 1 + (mathExpVarIds mathExp |> List.maximum |> Maybe.withDefault 0) in
  case solveOne solutionsCache (mathExp, MathVar targetValInsertedVarId) targetVarId of
    solvedTerm::_ -> solvedTerm |> applySubst (Dict.insert targetValInsertedVarId targetVal subst) |> evalToMaybeNum
    _             -> Nothing


-- Solve one equation for one variable. Return a list of possible terms for that variable.
solveOne : SolutionsCache -> (MathExp, MathExp) -> Int -> List MathExp
solveOne solutionsCache eqn targetVarId =
  solve solutionsCache [eqn] [targetVarId]
  |> List.filterMap
      (\solution ->
        case solution of
          []             -> Nothing
          [(mathExp, _)] -> Just mathExp
          _              -> Debug.crash <| "Solver.solveOne why does a solution for one variable list multiple variables?? " ++ toString solution
      )


-- The targetVarIds had better occur inside of the eqns!
--
-- Maybe multiple solutions: returns a list.
--
-- Side effect: throws exception if solution not in cache; controller should ask solver for solution and retry action.
solve : SolutionsCache -> List (MathExp, MathExp) -> List Int -> List Solution
solve solutionsCache eqns targetVarIds =
  let allMathExps = List.concatMap Utils.pairToList eqns in
  let (oldToNormalizedVarIds, normalizedToOldVarIds) = normalizedVarIdMapping allMathExps in
  let normalizedEquations =
    eqns
    |> List.map
        (\(lhs, rhs) ->
          case (remapVarIds oldToNormalizedVarIds lhs, remapVarIds oldToNormalizedVarIds rhs) of
            (Just normalizedLHS, Just normalizedRHS) -> Just (normalizedLHS, normalizedRHS)
            _                                        -> Debug.crash "Shouldn't happen: Bug in Solver.solve/normalizedVarIdMapping"
        )
    |> Utils.projJusts
    |> Utils.fromJust_ "Also shouldn't happen: Bug in Solver.solve"
  in
  case targetVarIds |> List.map (\targetVarId -> Dict.get targetVarId oldToNormalizedVarIds) |> Utils.projJusts of
    Just normalizedTargetVarIds ->
      let problem = (List.map removeCommonSuperExps normalizedEquations, normalizedTargetVarIds) in
      case Dict.get problem solutionsCache of
        Just solutions ->
          -- Now convert back to given varIds
          solutions |> List.filterMap (remapSolutionVarIds normalizedToOldVarIds)

        Nothing ->
          ImpureGoodies.throw (NeedSolution problem)

    Nothing ->
      let _ = Debug.log "WARNING: Asked to solve for variable(s) not in equation! No solutions." (eqns, targetVarIds) in
      []


-- Let the first variable encountered be 1, second 2, etc...
--
-- Want our symbolic solutions to be general so we don't have to round-trip to the solver all the time.
normalizedVarIdMapping : List MathExp -> (Dict Int Int, Dict Int Int)
normalizedVarIdMapping mathExps =
  let (_, oldToNew, newToOld) =
    mathExps
    |> List.concatMap mathExpVarIds
    |> List.foldl
        (\oldVarId (i, oldToNormalizedVarIds, normalizedToOldVarIds) ->
          case Dict.get oldVarId oldToNormalizedVarIds of
            Just _  -> (i, oldToNormalizedVarIds, normalizedToOldVarIds)
            Nothing ->
              ( i+1
              , Dict.insert oldVarId i oldToNormalizedVarIds
              , Dict.insert i oldVarId normalizedToOldVarIds
              )
        )
        (1, Dict.empty, Dict.empty)
  in
  (oldToNew, newToOld)


remapVarIds : Dict Int Int -> MathExp -> Maybe MathExp
remapVarIds oldToNew mathExp =
  case mathExp of
    MathNum _             -> Just mathExp
    MathVar varId         -> Dict.get varId oldToNew |> Maybe.map MathVar
    MathOp op_ childTerms -> childTerms |> List.map (remapVarIds oldToNew) |> Utils.projJusts |> Maybe.map (MathOp op_)


-- REDUCE doesn't find the appropriate distance solution when everything is wrapped in "sqrt". Help it out.
removeCommonSuperExps : (MathExp, MathExp) -> (MathExp, MathExp)
removeCommonSuperExps eqn =
  case eqn of
    (MathOp lOp_ [lChild], MathOp rOp_ [rChild]) -> if lOp_ == rOp_ then removeCommonSuperExps (lChild, rChild) else eqn
    _                                            -> eqn


remapSolutionVarIds : Dict Int Int -> Solution -> Maybe Solution
remapSolutionVarIds oldToNew solution =
  solution
  |> List.map
      (\(mathExp, targetVarId) ->
        Maybe.map2
            (,)
            (remapVarIds oldToNew mathExp)
            (Dict.get targetVarId oldToNew)
      )
  |> Utils.projJusts


mapSolutionsExps : (MathExp -> MathExp) -> List Solution -> List Solution
mapSolutionsExps f solutions =
  solutions
  |> List.map (List.map (\(mathExp, varId) -> (f mathExp, varId)))


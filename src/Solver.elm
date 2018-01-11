module Solver exposing (..)

import ImpureGoodies
import Lang exposing (Num, Op_, Trace(..), Subst)
import Utils

import Dict exposing (Dict)


--------------------------------------------------------------------------------

-- TODO streamline Equation/LocEquation/Trace; see note in LocEqn.elm

type EqnTerm
  = EqnConst Num
  | EqnVar Int -- Variable identifiers, often locIds
  | EqnOp Op_ (List EqnTerm)

type alias Eqn = (EqnTerm, EqnTerm) -- LHS, RHS

type alias Problem  = (List Eqn, List Int) -- System of equations, and varIds to solve for (usually a singleton).
type alias Solution = List (EqnTerm, Int)

type alias SolutionsCache = Dict Problem (List Solution)

type NeedSolutionException = NeedSolution Problem


traceToEqnTerm : Trace -> EqnTerm
traceToEqnTerm trace =
  case trace of
    TrLoc (locId, _, _) -> EqnVar locId
    TrOp op_ children   -> EqnOp op_ (List.map traceToEqnTerm children)


-- Some locId should be missing from the Subst: the missing locId will be solved for.
--
-- Side effect: throws exception if solution not in cache; controller should ask solver for solution and retry action.
solveTrace : SolutionsCache -> Subst -> Trace -> Num -> Maybe Num
solveTrace solutionsCache subst trace targetVal =
  let eqnTerm = traceToEqnTerm trace in
  let targetVarId = Utils.diffAsSet (eqnTermVarIds eqnTerm) (Dict.keys subst) |> Utils.head "Solver.solveTrace: expected trace to have a locId remaining after applying subst" in
  -- Apply subst as late as possible so we can have the most general form of equations in the cache.
  case symbolicSolve solutionsCache [(eqnTerm, EqnConst targetVal)] [targetVarId] of
    [(solvedTerm, targetVarId)]::_ -> solvedTerm |> applySubst subst |> evalToMaybeNum
    _                              -> Nothing


-- Assumes no variables remain, otherwise returns Nothing with a debug message.
evalToMaybeNum : EqnTerm -> Maybe Num
evalToMaybeNum eqnTerm =
  case eqnTerm of
    EqnConst n        -> Just n
    EqnVar _          -> let _ = Utils.log ("Solver.evalToMaybeNum: Found " ++ toString eqnTerm ++ " in an EqnTerm that shouldn't have any variables.") in Nothing
    EqnOp op operands ->
      operands
      |> List.map evalToMaybeNum
      |> Utils.projJusts
      |> Maybe.andThen (\operandNums -> Lang.maybeEvalMathOp op operandNums)


-- The targetVarIds had better occur inside of the eqns!
--
-- Maybe multiple solutions: returns a list.
--
-- Side effect: throws exception if solution not in cache; controller should ask solver for solution and retry action.
symbolicSolve : SolutionsCache -> List (EqnTerm, EqnTerm) -> List Int -> List Solution
symbolicSolve solutionsCache eqns targetVarIds =
  let allEqnTerms = List.concatMap Utils.pairToList eqns in
  let (oldToNormalizedVarIds, normalizedToOldVarIds) = normalizedVarIdMapping allEqnTerms in
  let normalizedEquations =
    eqns
    |> List.map
        (\(lhs, rhs) ->
          case (remapVarIds oldToNormalizedVarIds lhs, remapVarIds oldToNormalizedVarIds rhs) of
            (Just normalizedLHS, Just normalizedRHS) -> Just (normalizedLHS, normalizedRHS)
            _                                        -> Debug.crash "Shouldn't happen: Bug in Solver.symbolicSolve/normalizedVarIdMapping"
        )
    |> Utils.projJusts
    |> Utils.fromJust_ "Also shouldn't happen: Bug in Solver.symbolicSolve"
  in
  case targetVarIds |> List.map (\targetVarId -> Dict.get targetVarId oldToNormalizedVarIds) |> Utils.projJusts of
    Just normalizedTargetVarIds ->
      let problem = (normalizedEquations, normalizedTargetVarIds) in
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
normalizedVarIdMapping : List EqnTerm -> (Dict Int Int, Dict Int Int)
normalizedVarIdMapping eqnTerms =
  let (_, oldToNew, newToOld) =
    eqnTerms
    |> List.concatMap eqnTermVarIds
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


eqnTermVarIds : EqnTerm -> List Int
eqnTermVarIds eqnTerm =
  case eqnTerm of
    EqnConst _         -> []
    EqnVar varId       -> [varId]
    EqnOp _ childTerms -> List.concatMap eqnTermVarIds childTerms


remapVarIds : Dict Int Int -> EqnTerm -> Maybe EqnTerm
remapVarIds oldToNew eqnTerm =
  case eqnTerm of
    EqnConst _           -> Just eqnTerm
    EqnVar varId         -> Dict.get varId oldToNew |> Maybe.map EqnVar
    EqnOp op_ childTerms -> childTerms |> List.map (remapVarIds oldToNew) |> Utils.projJusts |> Maybe.map (EqnOp op_)


remapSolutionVarIds : Dict Int Int -> Solution -> Maybe Solution
remapSolutionVarIds oldToNew solution =
  solution
  |> List.map
      (\(eqnTerm, targetVarId) ->
        Maybe.map2
            (,)
            (remapVarIds oldToNew eqnTerm)
            (Dict.get targetVarId oldToNew)
      )
  |> Utils.projJusts


applySubst : Dict Int Num -> EqnTerm -> EqnTerm
applySubst subst eqnTerm =
  case eqnTerm of
    EqnConst n   -> eqnTerm
    EqnVar varId ->
      case Dict.get varId subst of
        Just n  -> EqnConst n
        Nothing -> eqnTerm
    EqnOp op_ childTerms -> EqnOp op_ (childTerms |> List.map (applySubst subst))

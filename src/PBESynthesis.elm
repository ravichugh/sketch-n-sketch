--------------------------------------------------------------------------------
-- This module contains all the code for Programming by Example (PBE) synthesis.
--------------------------------------------------------------------------------

module PBESynthesis exposing
  ( guess
  , refine
  , solve
  )

import Dict exposing (Dict)
import Set

import UnLang as U exposing (..)
import TriEval

import Types2 as T exposing (..)
import Lang exposing (..)
import Info exposing (withDummyInfo)

import Utils

import NonDet exposing (NonDet)

import LeoUnparser

--------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------

maxDepth : Int
maxDepth =
  7

maxSolveDepth : Int
maxSolveDepth =
  5

--------------------------------------------------------------------------------
-- Satisfaction
--------------------------------------------------------------------------------

-- Nothing: False
-- Just constraints: True, provided that constraints are met
satisfiesWorlds : Worlds -> Exp -> Maybe Constraints
satisfiesWorlds worlds exp =
  let
    satisfiesWorld : World -> Exp -> Maybe Constraints
    satisfiesWorld (env, ex) =
      TriEval.evalWithEnv env
        >> TriEval.ensureConstraintFree
        >> Maybe.andThen (flip TriEval.backprop ex) 
  in
    worlds
      |> List.map satisfiesWorld
      |> flip Utils.applyList exp
      |> Utils.projJusts
      |> Maybe.map List.concat

--------------------------------------------------------------------------------
-- Type-Directed Synthesis
--------------------------------------------------------------------------------

guess_ : Int -> T.DatatypeEnv -> T.TypeEnv -> Type -> NonDet Exp
guess_ depth sigma gamma tau =
  if depth == 0 then
    NonDet.none
  else
    let
      typePairs =
        T.typePairs gamma

      -- EGuess-Var
      variableGuesses =
        typePairs
          |> List.filter (Tuple.second >> T.typeEquiv gamma tau)
          |> List.map (Tuple.first >> eVar)
          |> NonDet.fromList

      -- EGuess-App
      appGuesses =
        let
          arrowMatches : ArrowType -> Bool
          arrowMatches (_, _, returnType) =
            T.typeEquiv gamma tau returnType

          guessApps : (Exp, ArrowType) -> NonDet Exp
          guessApps (eFun, (_, argTypes, _)) =
            let
              possibleArgs =
                List.map
                  ( refine_ (depth - 1) sigma gamma []
                      >> NonDet.map Tuple.first -- Discards constraints
                  )
                  argTypes
            in
              possibleArgs
                |> NonDet.oneOfEach
                |> NonDet.map (eApp eFun)
        in
          typePairs
            |> List.map (Tuple.mapFirst eVar)
            |> List.map (Tuple.mapSecond T.matchArrowRecurse)
            |> List.map Utils.liftMaybePair2
            |> Utils.filterJusts
            |> List.filter (Tuple.second >> arrowMatches)
            |> NonDet.concatMap guessApps

      -- EGuess-Tuple
      tupleGuesses =
        let
          -- Returns list of (n, i) pairs that work
          extractGet : List Type -> List (Int, Int)
          extractGet ts =
            let
              n =
                List.length ts
            in
              ts
                |> Utils.zipWithIndex
                |> List.filter (Tuple.first >> T.typeEquiv gamma tau)
                |> List.map (\(_, i) -> (n, i + 1)) -- i should be 1-indexed

          makeGets : (Ident, List (Int, Int)) -> List Exp
          makeGets (ident, getInfos) =
            List.map
              (\(n, i) -> Lang.fromTupleGet (n, i, eVar ident))
              getInfos
        in
          typePairs
            |> List.map
                 ( Tuple.mapSecond <|
                     Lang.tupleTypeArguments >> Maybe.map extractGet
                 )
            |> List.map Utils.liftMaybePair2
            |> Utils.filterJusts
            |> List.concatMap makeGets
            |> NonDet.fromList
    in
      NonDet.concat
        [ variableGuesses
        , appGuesses
        , tupleGuesses
        ]

guess : T.DatatypeEnv -> T.TypeEnv -> Type -> NonDet Exp
guess =
  guess_ maxDepth

--------------------------------------------------------------------------------
-- Type-and-Example-Directed Synthesis
--------------------------------------------------------------------------------

refine_ :
  Int -> T.DatatypeEnv -> T.TypeEnv -> Worlds -> Type
    -> NonDet (Exp, Constraints)
refine_ depth sigma gamma unfilteredWorlds tau =
  if depth == 0 then
    NonDet.none
  else
    let
      -- Filter out all the don't care examples
      worlds =
        List.filter
          ( \(env, ex) ->
              case ex of
                ExDontCare ->
                  False
                _ ->
                  True
          )
          unfilteredWorlds

      -- IRefine-Guess
      guessRefinement =
        tau
          |> guess_ (depth - 1) sigma gamma
          |> NonDet.map
               (\e -> Utils.liftMaybePair2 (e, satisfiesWorlds worlds e))
          |> NonDet.collapseMaybe

      -- IRefine-Constant
      constantRefinement =
        let
          extractConstant ex =
            case (unwrapType tau, ex) of
              (TNum _, ExNum n) ->
                Just <| eConstDummyLoc n

              (TBool _, ExBool b) ->
                Just <| eBool b

              (TString _, ExString s) ->
                Just <| eStr s

              _ ->
                Nothing

        in
          worlds
            |> List.map Tuple.second
            |> Utils.collapseEqual
            |> Maybe.andThen extractConstant
            |> Utils.maybeToList
            |> List.map (\e -> (e, []))
            |> NonDet.fromList

      -- IRefine-Tuple
      tupleRefinement =
        case tupleTypeArguments tau of
          Nothing ->
            NonDet.none

          Just taus ->
            let
              tupleLength : Int
              tupleLength =
                List.length taus

              maybeTupleArgsWorlds : Maybe (List Worlds)
              maybeTupleArgsWorlds =
                let
                  extract : World -> Maybe Worlds
                  extract (env, ex) =
                    case ex of
                      ExTuple exArgs ->
                        -- Check may not be necessary with example typechecking
                        if List.length exArgs == tupleLength then
                          Just <|
                            List.map (\ex -> (env, ex)) exArgs
                        else
                          Nothing

                      ExDontCare ->
                        Just []

                      _ ->
                        Nothing
                in
                  worlds
                    |> List.map extract
                    |> Utils.projJusts
                    |> Maybe.map Utils.transpose
            in
              maybeTupleArgsWorlds
                |> Maybe.map
                     ( \tupleArgsWorlds ->
                         List.map2
                           (refine_ (depth - 1) sigma gamma)
                           tupleArgsWorlds
                           taus
                             |> NonDet.oneOfEach
                             |> NonDet.map
                                  ( List.unzip
                                      >> Tuple.mapFirst eTuple
                                      >> Tuple.mapSecond List.concat
                                  )
                     )
                |> Maybe.withDefault NonDet.none

      -- IRefine-Fun
      partialFunctionRefinement =
        case T.matchArrowRecurse tau of
          -- TODO Only support single-argument functions for now
          Just (_, [argType], returnType) ->
            let
              argName : Ident
              argName =
                "x"

              argNamePat : Pat
              argNamePat =
                pVar0 argName

              worldFromEntry :
                U.Env -> (List (UnExp ()), Example) -> Maybe World
              worldFromEntry env (args, output) =
                case args of
                  [arg] ->
                    Just
                      ( U.addVarBinding argName arg env
                      , output
                      )

                  -- TODO Only support single-argument functions for now
                  _ ->
                    Nothing

              branchWorlds : World -> Maybe Worlds
              branchWorlds (env, ex) =
                case ex of
                  ExPartialFunction entries ->
                    entries
                      |> List.map (worldFromEntry env)
                      |> Utils.projJusts

                  _ ->
                    Nothing

              newGamma : T.TypeEnv
              newGamma =
                T.addHasType (argNamePat, argType) gamma
            in
              worlds
                |> List.map branchWorlds
                |> Utils.projJusts
                |> Maybe.map List.concat
                |> Maybe.map
                     ( \allWorlds ->
                         returnType
                           |> refine_ (depth - 1) sigma newGamma allWorlds
                           |> NonDet.map (Tuple.mapFirst <| eFun [argNamePat])
                     )
                |> Maybe.withDefault NonDet.none

          _ ->
            NonDet.none

      -- IRefine-Constructor
      constructorRefinement =
        let
          extractConstructorArgWorlds : Ident -> Maybe Worlds
          extractConstructorArgWorlds ctorName =
            let
              extract : World -> Maybe World
              extract (env, ex) =
                  case ex of
                    ExConstructor exCtorName exInner ->
                      if exCtorName == ctorName then
                        Just (env, exInner)
                      else
                        Nothing

                    _ ->
                      Nothing
            in
              worlds
                |> List.map extract
                |> Utils.projJusts
        in
          case unwrapType tau of
            TVar _ datatypeName ->
              case Utils.maybeFind datatypeName sigma of
                Just datatypeConstructors ->
                  NonDet.do (NonDet.fromList datatypeConstructors) <|
                    \(ctorName, argTypes) ->
                      case argTypes of
                        -- Only support single arguments for now
                        [argType] ->
                          ctorName
                            |> extractConstructorArgWorlds
                            |> Maybe.map
                                 ( \constructorArgWorlds ->
                                     NonDet.map
                                       ( Tuple.mapFirst <|
                                           List.singleton
                                             >> eDatatype ctorName
                                       )
                                       ( refine_
                                           (depth - 1)
                                           sigma
                                           gamma
                                           constructorArgWorlds
                                           argType
                                       )
                                 )
                            |> Maybe.withDefault NonDet.none

                        _ ->
                          NonDet.none

                Nothing ->
                  NonDet.none

            _ ->
              NonDet.none

      -- IRefine-Match
      matchRefinement =
        let
          argName : Ident
          argName =
            "x"

          argNamePat : Pat
          argNamePat =
            pVar argName

          makeBranchWorlds : Exp -> Worlds -> Maybe (List (Ident, Worlds))
          makeBranchWorlds eScrutinee worlds =
            worlds
              |> List.map
                   ( \(env, ex) ->
                       eScrutinee
                         |> TriEval.evalWithEnv env
                         |> TriEval.ensureConstraintFree
                         |> Maybe.andThen U.expToVal
                         |> Maybe.andThen
                              ( \v ->
                                  case v of
                                    UVConstructor ctorName vInner ->
                                      Just
                                        ( ctorName
                                        , ( U.addVarBinding
                                              argName
                                              (U.valToExp vInner)
                                              env
                                          , ex
                                          )
                                        )

                                    _ ->
                                      Nothing
                              )
                   )
              |> Utils.projJusts
              |> Maybe.map (Utils.pairsToDictOfLists >> Dict.toList)

          makeBranch :
            List (Ident, List Type)
              -> (Ident, Worlds)
              -> NonDet (Pat, (Exp, Constraints))
          makeBranch constructors (ctorName, worlds) =
            case Utils.maybeFind ctorName constructors of
              -- Only support single arguments for now
              Just [ctorArgType] ->
                let
                  newGamma =
                    T.addHasType (argNamePat, ctorArgType) gamma

                  pat =
                    pDatatype ctorName [argNamePat]
                in
                  refine_ (depth - 1) sigma newGamma worlds tau
                    |> NonDet.map ((,) pat)

              _ ->
                NonDet.none

          makeCase :
            Exp -> List (Pat, (Exp, Constraints)) -> Maybe (Exp, Constraints)
          makeCase eScrutinee branchesWithConstraints =
            if List.isEmpty branchesWithConstraints then
              Nothing
            else
              let
                branches =
                  branchesWithConstraints
                    |> List.map
                       ( \(p, (e, _)) ->
                           withDummyInfo <|
                             Branch_ space0 p e space1
                       )

                constraints =
                  branchesWithConstraints
                    |> List.map (Tuple.second >> Tuple.second)
                    |> List.concat
              in
                Just (eCase eScrutinee branches, constraints)
        in
          NonDet.do (NonDet.fromList sigma) <| \(datatypeName, constructors) ->
          let
            dType =
              withDummyTypeInfo <| TVar space0 datatypeName
          in
          NonDet.do (guess_ (depth - 1) sigma gamma dType) <| \eScrutinee ->
            worlds
              |> makeBranchWorlds eScrutinee
              |> Maybe.map
                   ( List.map (makeBranch constructors)
                       >> NonDet.oneOfEach
                       >> NonDet.map (makeCase eScrutinee)
                       >> NonDet.collapseMaybe
                   )
              |> Maybe.withDefault NonDet.none

      -- IRefine-Hole
      holeRefinement =
        if List.isEmpty worlds then
          NonDet.pure (Lang.eEmptyHole, [])
        else
          NonDet.none
    in
      NonDet.concat
        [ guessRefinement
        , constantRefinement
        , tupleRefinement
        , partialFunctionRefinement
        , constructorRefinement
        , matchRefinement
        , holeRefinement
        ]

refine :
  T.DatatypeEnv -> T.TypeEnv -> Worlds -> Type -> NonDet (Exp, Constraints)
refine =
  refine_ maxDepth

--------------------------------------------------------------------------------
-- Iterative Constraint Solving
--------------------------------------------------------------------------------

solve_ : Int -> T.DatatypeEnv -> T.HoleEnv -> Constraints -> NonDet HoleFilling
solve_ depth sigma delta constraints =
  if depth == 0 then
    NonDet.none
  else
    let
      solveOne : HoleId -> Worlds -> NonDet (Exp, Constraints)
      solveOne holeId worlds =
        case T.holeEnvGet holeId delta of
          Just (gamma, tau) ->
            refine sigma gamma worlds tau

          Nothing ->
            NonDet.none

      solutions : NonDet (HoleFilling, Constraints)
      solutions =
        constraints
          |> Utils.pairsToDictOfLists -- "Group" operation
          |> Dict.map solveOne
          |> NonDet.oneOfEachDict
          |> NonDet.map
               ( Utils.unzipDict
                   >> Tuple.mapSecond (Dict.values >> List.concat)
               )

      oldConstraintSet =
        Set.fromList constraints
    in
      NonDet.do solutions <| \(holeFilling, newConstraints) ->
        let
          newConstraintSet =
            Set.fromList newConstraints
        in
        if Utils.isSubset newConstraintSet oldConstraintSet then
          NonDet.pure holeFilling
        else
          solve_
            (depth - 1)
            sigma
            delta
            (Set.toList (Set.union oldConstraintSet newConstraintSet))

solve : T.DatatypeEnv -> T.HoleEnv -> Constraints -> NonDet HoleFilling
solve =
  solve_ maxSolveDepth

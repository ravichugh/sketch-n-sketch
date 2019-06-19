--------------------------------------------------------------------------------
-- This module contains all the code for Programming by Example (PBE) synthesis.
--------------------------------------------------------------------------------

module PBESynthesis exposing
  ( guess
  , refine
  , solve

  , satisfiesWorlds
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
  8

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
      variableGuesses () =
        typePairs
          |> List.filter (Tuple.second >> T.typeEquiv gamma tau)
          |> List.map (Tuple.first >> eVar)
          |> NonDet.fromList

      -- EGuess-App
      appGuesses () =
        let
          arrowMatches : ArrowType -> Bool
          arrowMatches (_, _, returnType) =
            T.typeEquiv gamma tau returnType

          guessApps : (Exp, ArrowType) -> NonDet Exp
          guessApps (headExp, (_, argTypes, _)) =
            let
              possibleArgs =
                List.map
                  ( refine_ (depth - 1) sigma gamma []
                      -- There should be no constraints
                      >> NonDet.map
                           ( \(exp, constraints) ->
                               if List.isEmpty constraints then
                                 Just exp
                               else
                                 Debug.log
                                   ( "Non-empty constraints when synthesizing '"
                                       ++ LeoUnparser.unparse exp
                                       ++ "'"
                                   )
                                   Nothing
                           )
                      >> NonDet.collapseMaybe
                  )
                  argTypes
            in
              possibleArgs
                |> List.map (NonDet.map replacePrecedingWhitespace1)
                |> NonDet.oneOfEach
                |> NonDet.map (eApp0 headExp)
        in
          typePairs
            |> List.map (Tuple.mapFirst eVar0)
            |> List.map (Tuple.mapSecond T.matchArrowRecurse)
            |> List.map Utils.liftMaybePair2
            |> Utils.filterJusts
            |> List.filter (Tuple.second >> arrowMatches)
            |> NonDet.concatMap guessApps

      -- EGuess-Get
      getGuesses () =
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
        [ variableGuesses ()
        , appGuesses ()
        , getGuesses ()
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
refine_ depth sigma gamma worlds tau =
  if depth == 0 then
    NonDet.none
  else
    let
      -- Filter out all the don't care examples
      filteredWorlds =
        List.filter
          ( \(env, ex) ->
              case ex of
                ExDontCare ->
                  False
                _ ->
                  True
          )
          worlds

      -- IRefine-Guess
      guessRefinement () =
        tau
          |> guess_ (depth - 1) sigma gamma
          |> NonDet.map
               (\e -> Utils.liftMaybePair2 (e, satisfiesWorlds filteredWorlds e))
          |> NonDet.collapseMaybe

      -- IRefine-Constant
      constantRefinement () =
        let
          extractConstant ex =
            case (unwrapType tau, ex) of
              (TNum _, ExNum n) ->
                Just <| eConstDummyLoc0 n

              (TBool _, ExBool b) ->
                Just <| eBool0 b

              (TString _, ExString s) ->
                Just <| eStr0 s

              _ ->
                Nothing

        in
          filteredWorlds
            |> List.map Tuple.second
            |> Utils.collapseEqual
            |> Maybe.andThen extractConstant
            |> Utils.maybeToList
            |> List.map (\e -> (e, []))
            |> NonDet.fromList

      -- IRefine-Tuple
      tupleRefinement () =
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
                  filteredWorlds
                    |> List.map extract
                    |> Utils.projJusts
                    |> Maybe.map Utils.transpose
                    -- TODO This shouldn't be necessary?
                    |> Maybe.andThen
                         ( \worldsList ->
                             if List.length worldsList /= tupleLength then
                               Nothing
                             else
                               Just worldsList
                         )
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
                                      >> Tuple.mapFirst eTuple0
                                      >> Tuple.mapSecond List.concat
                                  )
                     )
                |> Maybe.withDefault NonDet.none

      -- IRefine-Fun
      partialFunctionRefinement () =
        case T.matchArrowRecurse tau of
          -- TODO Only support single-argument functions for now
          Just (_, [argType], returnType) ->
            let
              functionName : Ident
              functionName =
                "rec"

              functionNamePat : Pat
              functionNamePat =
                pVar0 functionName

              functionType : Type
              functionType =
                tFun0 argType returnType

              argName : Ident
              argName =
                "x"

              argNamePat : Pat
              argNamePat =
                pVar0 argName

              makeFunction : Exp -> Exp
              makeFunction functionBody =
                let
                  recursiveNameFinder e =
                    case unwrapExp e of
                      EVar _ name ->
                        name == functionName

                      _ ->
                        False
                in
                  case findFirstNode recursiveNameFinder functionBody of
                    -- Recursive
                    Just _ ->
                      replacePrecedingWhitespace1 <|
                        eLet
                          [(functionName, eFun [argNamePat] functionBody)]
                          (eVar0 functionName)

                    -- Non-recursive
                    Nothing ->
                      eFun0 [argNamePat] functionBody

              worldFromEntry :
                PartialFunction
                  -> U.Env
                  -> (UnExp (), Example)
                  -> World
              worldFromEntry partialFunction env (arg, output) =
                ( env
                    -- Recursive function binding
                    |> U.addVarBinding
                         functionName
                         (UPartialFunction () partialFunction)
                    -- Argument binding
                    |> U.addVarBinding argName arg
                , output
                )

              branchWorlds : World -> Maybe Worlds
              branchWorlds (env, ex) =
                case ex of
                  ExPartialFunction partialFunction ->
                    partialFunction
                      |> List.map (worldFromEntry partialFunction env)
                      |> Just

                  _ ->
                    Nothing

              newGamma : T.TypeEnv
              newGamma =
                -- NOTE: Structural recursion not handled
                gamma
                  |> T.addHasType (functionNamePat, functionType, Nothing)
                  |> T.addHasType (argNamePat, argType, Nothing)
            in
              filteredWorlds
                |> List.map branchWorlds
                |> Utils.projJusts
                |> Maybe.map List.concat
                |> Maybe.map
                     ( \allWorlds ->
                         returnType
                           |> refine_ (depth - 1) sigma newGamma allWorlds
                           |> NonDet.map (Tuple.mapFirst makeFunction)
                     )
                |> Maybe.withDefault NonDet.none

          _ ->
            NonDet.none

      -- IRefine-Constructor
      constructorRefinement () =
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
              filteredWorlds
                |> List.map extract
                |> Utils.projJusts
        in
          case unwrapType tau of
            TVar _ datatypeName ->
              case Utils.maybeFind datatypeName sigma of
                Just (typeArgNames, datatypeConstructors) ->
                  NonDet.do (NonDet.fromList datatypeConstructors) <|
                    \(ctorName, argTypes) ->
                      case argTypes of
                        -- -- Syntactic sugar for zero-argument constructors
                        -- [] ->
                        --   NonDet.pure
                        --     ( eDatatype ctorName []
                        --     , []
                        --     )

                        -- Only support single arguments for now
                        [argType] ->
                          ctorName
                            |> extractConstructorArgWorlds
                            |> Maybe.map
                                 ( \constructorArgWorlds ->
                                     NonDet.map
                                       ( Tuple.mapFirst <|
                                           replacePrecedingWhitespace1
                                             >> List.singleton
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
      matchRefinement () =
        let
          argName : Ident
          argName =
            "y"

          argNamePat : Pat
          argNamePat =
            pVar argName

          distributeWorlds : Exp -> Worlds -> Maybe (Dict Ident Worlds)
          distributeWorlds eScrutinee worlds =
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
              |> Maybe.map Utils.pairsToDictOfLists

          makeBranch :
            List (Ident, List Type)
              -> (Ident, Worlds)
              -> NonDet (Pat, (Exp, Constraints))
          makeBranch constructorDefs (ctorName, worlds) =
            case Utils.maybeFind ctorName constructorDefs of
              -- Only support single arguments for now
              Just [ctorArgType] ->
                let
                  newGamma =
                    -- NOTE: Structural recursion not handled
                    T.addHasType (argNamePat, ctorArgType, Nothing) gamma

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
          NonDet.do (NonDet.fromList sigma) <|
            \(datatypeName, (typeArgNames, constructorDefs)) ->
              let
                dType =
                  withDummyTypeInfo <|
                    TVar space0 datatypeName

                dontCareWorlds =
                  constructorDefs
                    |> List.map
                         ( \(name, _) ->
                             (name, [([], ExDontCare)])
                         )
                    |> Dict.fromList
              in
                NonDet.do (guess_ (depth - 1) sigma gamma dType) <|
                  \eScrutinee ->
                    Maybe.withDefault NonDet.none <|
                      flip Maybe.map (distributeWorlds eScrutinee worlds) <|
                        \distributedWorlds ->
                          dontCareWorlds
                            -- Gives preference to distributedWorlds
                            |> Dict.union distributedWorlds
                            |> Dict.toList
                            |> List.map (makeBranch constructorDefs)
                            |> NonDet.oneOfEach
                            |> NonDet.map (makeCase eScrutinee)
                            |> NonDet.collapseMaybe

      -- IRefine-Hole
      holeRefinement () =
        -- List of worlds is nonempty list of "don't care" examples
        if not (List.isEmpty worlds) && List.isEmpty filteredWorlds then
          NonDet.pure (Lang.eEmptyHole0, [])
        else
          NonDet.none
    in
      NonDet.concat <|
        [ guessRefinement ()
        , constantRefinement ()
        , tupleRefinement ()
        , partialFunctionRefinement ()
        , constructorRefinement ()
        ] ++
        ( if not <| List.isEmpty filteredWorlds then
            [ -- matchRefinement ()
            ]
          else
            []
        ) ++
        [ holeRefinement ()
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

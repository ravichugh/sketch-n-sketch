--------------------------------------------------------------------------------
-- This module contains all the code for Programming by Example (PBE) synthesis,
-- but with optimizations.
--------------------------------------------------------------------------------

module FastPBESynthesis exposing
  ( solve
  )

import Dict exposing (Dict)
import Set

import UnLang as U exposing (..)
import TriEval

import TermGen exposing (GenCached)

import Types2 as T exposing (..)
import Lang exposing (..)
import Info exposing (withDummyInfo)

import Utils

import NonDet exposing (NonDet)
import State exposing (State)

import LeoUnparser
import ImpureGoodies

--------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------

maxSolveDepth : Int
maxSolveDepth =
  5

--------------------------------------------------------------------------------
-- Type Helpers
--------------------------------------------------------------------------------

isBaseType : DatatypeEnv -> Type -> Bool
isBaseType sigma tau =
  case unwrapType tau of
    TVar _ datatypeName ->
      sigma
        |> Utils.maybeFind datatypeName
        |> Utils.maybeToBool

    _ ->
      False

--------------------------------------------------------------------------------
-- Data Structures
--------------------------------------------------------------------------------

type alias SynthesisProblem =
  { sigma : T.DatatypeEnv
  , gamma : T.TypeEnv
  , worlds : Worlds
  , goalType : Type
  }

type alias SynthesisSolution =
  NonDet (Exp, Constraints)

type alias RTree =
  { problem : SynthesisProblem
  , solution : Maybe SynthesisSolution
  , possibleRule : NonDet RRule
  }

type RRule
  = Ctor
      { ctorName : Ident
      , argRefinement : RTree
      }
  | Tuple
      { componentRefinements : List RTree
      }
  | Fix
      { fixName : Ident
      , argName : Ident
      , bodyRefinement : RTree
      }
  | Match
      { scrutinee : Exp
      , branchRefinements : List (Pat, RTree)
      }
  | Hole
      {}

showTree : RTree -> String
showTree =
  let
    showTree_ indentLevel tree =
      tree.possibleRule
        |> NonDet.toList
        |> List.map (showRule_ indentLevel)
        |> String.join "\n"

    showRule_ indentLevel rule =
      let
        singleIndent =
          "  "

        currentIndent =
          String.repeat indentLevel singleIndent

        nextIndentLevel =
          indentLevel + 1

        val =
          case rule of
            Ctor { ctorName, argRefinement } ->
              "Ctor "
                ++ ctorName
                ++ "\n"
                ++ showTree_ nextIndentLevel argRefinement

            Tuple { componentRefinements } ->
              let
                tupleName =
                  "Tuple" ++ toString (List.length componentRefinements)

                children =
                  componentRefinements
                    |> List.map (showTree_ nextIndentLevel)
                    |> List.indexedMap (\i s -> toString (i + 1) ++ ". " ++ s)
                    |> String.join "\n"
              in
                tupleName ++ "\n" ++ children

            Fix { fixName, argName, bodyRefinement } ->
              "Fix ("
                ++ fixName
                ++ ") \\"
                ++ argName
                ++ " ->\n"
                ++ showTree_ nextIndentLevel bodyRefinement

            Match { scrutinee, branchRefinements } ->
              let
                children =
                  branchRefinements
                    |> List.map
                         ( Tuple.mapFirst <| \p ->
                             currentIndent
                               ++ singleIndent
                               ++ LeoUnparser.unparsePattern p ++ " -> \n"
                         )
                    |> List.map
                         ( Tuple.mapSecond <|
                             showTree_ (indentLevel + 2)
                         )
                    |> List.map (\(a, b) -> a ++ b)
                    |> String.join "\n"
              in
                "Match " ++ LeoUnparser.unparse scrutinee ++ "\n" ++ children

            Hole _ ->
              "Hole"

--            Guess { sp, guess } ->
--              "Guess <"
--                ++ LeoUnparser.unparseType sp.goalType
--                ++ ">["
--                ++ showTypePairs sp.gamma
--                ++ "]"
--                ++ ( case guess of
--                       Nothing ->
--                         ""
--                       Just ss ->
--                         ss
--                           |> NonDet.toList
--                           |> List.map (Tuple.first >> LeoUnparser.unparse)
--                           |> String.join "; "
--                           |> (++) " with guesses: "
--                   )
      in
        currentIndent ++ val
  in
    showTree_ 0

--------------------------------------------------------------------------------
-- Satisfaction
--------------------------------------------------------------------------------

-- Non-deterministically returns a list of constraints that ensures `exp`
-- `worlds`. If the result is `NonDet.none`, then `exp` can never satisfy
-- `worlds`.
satisfiesWorlds : Worlds -> Exp -> NonDet Constraints
satisfiesWorlds worlds exp =
  let
    satisfiesWorld : World -> NonDet Constraints
    satisfiesWorld (env, ex) =
      exp
        |> TriEval.evalWithEnv env
        |> TriEval.ensureConstraintFree
        |> Maybe.map (TriEval.setNoData >> flip TriEval.backprop ex)
        |> Maybe.withDefault NonDet.none
  in
    worlds
      |> List.map satisfiesWorld
      |> NonDet.oneOfEach
      |> NonDet.map List.concat

--------------------------------------------------------------------------------
-- Synthesis
--------------------------------------------------------------------------------

guessAndCheck :
  { maxTermSize : Int } -> SynthesisProblem -> GenCached SynthesisSolution
guessAndCheck params ({ worlds } as sp) =
  let
    possibleGuessState =
      TermGen.upToE
        { termSize = params.maxTermSize
        , sigma = sp.sigma
        , gamma = sp.gamma
        , goalType = sp.goalType
        }
  in
    State.pureDo possibleGuessState <| \possibleGuess ->
    NonDet.do possibleGuess <| \guess ->
    NonDet.pureDo (satisfiesWorlds worlds guess) <| \constraints ->
      (guess, constraints)

arefine :
  { maxScrutineeSize : Int, maxMatchDepth : Int }
    -> SynthesisProblem
    -> RTree
arefine params ({ sigma, gamma, worlds, goalType } as sp) =
  let
    -- Filter out all the "don't care" examples
    filteredWorlds =
      List.filter
        (Tuple.second >> (/=) ExDontCare)
        worlds

    worldsEmpty =
      List.isEmpty worlds

    filteredWorldsEmpty =
      List.isEmpty filteredWorlds

    -- IRefine-Constructor
    constructorRule () =
      if filteredWorldsEmpty then
        NonDet.none
      else
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
          case unwrapType goalType of
            TVar _ datatypeName ->
              case Utils.maybeFind datatypeName sigma of
                Just (_, datatypeConstructors) ->
                  NonDet.do (NonDet.fromList datatypeConstructors) <|
                    \(ctorName, argTypes) ->
                      case argTypes of
                        -- Constructors only have single arguments
                        [argType] ->
                          ctorName
                            |> extractConstructorArgWorlds
                            |> Maybe.map
                                 ( \constructorArgWorlds ->
                                     let
                                        argRefinement =
                                          arefine
                                            params
                                            { sp
                                            | worlds = constructorArgWorlds
                                            , goalType = argType
                                            }
                                     in
                                       NonDet.pure <|
                                         Ctor
                                           { ctorName = ctorName
                                           , argRefinement = argRefinement
                                           }
                                 )
                            |> Maybe.withDefault NonDet.none

                        _ ->
                          NonDet.none

                Nothing ->
                  NonDet.none

            _ ->
              NonDet.none

    -- IRefine-Tuple
    tupleRule () =
      case Lang.tupleTypeArguments goalType of
        Nothing ->
          NonDet.none

        Just [] ->
          NonDet.pure <|
            Tuple
              { componentRefinements = []
              }

        Just argGoalTypes ->
          if filteredWorldsEmpty then
            NonDet.none
          else
            let
              tupleLength : Int
              tupleLength =
                List.length argGoalTypes

              maybeComponentsWorlds : Maybe (List Worlds)
              maybeComponentsWorlds =
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
              maybeComponentsWorlds
                |> Maybe.map
                     ( \componentsWorlds ->
                         let
                            componentRefinements =
                              List.map2
                                ( \argWorlds argGoalType ->
                                    arefine
                                      params
                                      { sp
                                      | worlds = argWorlds
                                      , goalType = argGoalType
                                      }
                                )
                                componentsWorlds
                                argGoalTypes
                         in
                           NonDet.pure <|
                             Tuple
                               { componentRefinements =
                                   componentRefinements
                               }
                     )
                |> Maybe.withDefault NonDet.none

    -- IRefine-Fix (actually IRefine-Fun for now)
    fixRule argType returnType =
      if filteredWorldsEmpty then
        NonDet.none
      else
        let
          functionType : Type
          functionType =
            tFun0 argType returnType

          argName : Ident
          argName =
            TermGen.freshIdent TermGen.varChar gamma

          argNamePat : Pat
          argNamePat =
            pVar0 argName

          worldFromEntry :
            PartialFunction
              -> U.Env
              -> (UnExp (), Example)
              -> World
          worldFromEntry partialFunction env (arg, output) =
            ( env
                -- Old fix rule for recursive function binding
                -- |> U.addVarBinding
                --      recFunctionName
                --      (UPartialFunction () partialFunction)
                -- Argument binding
                |> U.addVarBinding argName (arg, Nothing)
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
            gamma
              -- |> T.addHasType
              --      ( recFunctionNamePat
              --      , functionType
              --      , Just (Rec [recFunctionNamePat])
              --      )
              |> T.addHasType
                   ( argNamePat
                   , argType
                   -- , Just (Arg recFunctionNamePat)
                   , Nothing
                   )
        in
          filteredWorlds
            |> List.map branchWorlds
            |> Utils.projJusts
            |> Maybe.map List.concat
            |> Maybe.map
                 ( \allWorlds ->
                     let
                        bodyRefinement =
                          arefine
                            params
                            { sp
                            | gamma = newGamma
                            , worlds = allWorlds
                            , goalType = returnType
                            }
                     in
                       NonDet.pure <|
                         Fix
                           { fixName = "_"
                           , argName = argName
                           , bodyRefinement = bodyRefinement
                           }
                 )
            |> Maybe.withDefault NonDet.none

    -- IRefine-Match
    matchRule () =
      if params.maxMatchDepth == 0 then
        NonDet.none
      else
        if filteredWorldsEmpty then
          NonDet.none
        else
          let
            argName : Ident
            argName =
              TermGen.freshIdent TermGen.matchChar gamma

            distributeWorlds :
              Exp -> Maybe T.BindingSpecification -> Worlds ->
                Maybe (Dict Ident Worlds)
            distributeWorlds scrutinee maybeSubBindSpec worlds =
              worlds
                |> List.map
                     ( \(env, ex) ->
                         scrutinee
                           |> TriEval.evalWithEnv env
                           |> TriEval.ensureConstraintFree
                           |> Maybe.andThen U.expToVal
                           |> Maybe.andThen
                                ( \v ->
                                    case v of
                                      UVConstructor ctorName vInner ->
                                        Just
                                          ( ctorName
                                          , ( if vInner == UVTuple [] then
                                                env
                                              else
                                                U.addVarBinding
                                                  argName
                                                  ( U.valToExp vInner
                                                  , maybeSubBindSpec
                                                  )
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

            makeBranchRefinement :
              Maybe T.BindingSpecification
                -> List DataConDef -> Ident -> Worlds -> Maybe (Pat, RTree)
            makeBranchRefinement
             maybeSubBindSpec constructorDefs ctorName branchWorlds =
              -- Could possibly avoid lookup here, but not that big of a deal
              -- because the length of constructorDefs will almost always be
              -- very small.
              case Utils.maybeFind ctorName constructorDefs of
                Just [ctorArgType] ->
                  let
                    (argNamePat, newGamma) =
                      case Lang.tupleTypeArguments ctorArgType of
                        -- Empty tuples shouldn't get a name
                        Just [] ->
                          ( pVar "_"
                          , gamma
                          )

                        _ ->
                          let
                            argNamePat =
                              pVar argName
                          in
                            ( argNamePat
                            , T.addHasType
                                (argNamePat, ctorArgType, maybeSubBindSpec)
                                gamma
                            )

                    pat =
                      pDatatype
                        ctorName
                        [argNamePat]

                    branchBody =
                      arefine
                        { params | maxMatchDepth = params.maxMatchDepth - 1 }
                        { sp
                        | gamma = newGamma
                        , worlds = branchWorlds
                        , goalType = goalType
                        }
                  in
                     Just (pat, branchBody)

                _ ->
                  Nothing
          in
            NonDet.do (NonDet.fromList sigma) <|
              \(datatypeName, (_, constructorDefs)) ->
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
                  NonDet.do
                    ( Tuple.first << State.run TermGen.emptyCache <|
                        TermGen.upToE
                          { termSize = params.maxScrutineeSize
                          , sigma = sigma
                          , gamma = gamma
                          , goalType = dType
                          }
                    ) <| \scrutinee ->
                      let
                        maybeSubBindSpec =
                          scrutinee
                            |> T.bindSpec gamma
                            |> Maybe.andThen T.subBindSpec
                      in
                        Maybe.withDefault NonDet.none <|
                          flip Maybe.map
                            ( distributeWorlds
                                scrutinee
                                maybeSubBindSpec
                                worlds
                            ) <|  \distributedWorlds ->
                              if
                                List.length constructorDefs /= 1
                                  && Dict.size distributedWorlds < 2
                              then
                                -- Uninformative match by Restriction (A)
                                NonDet.none
                              else
                                let
                                  maybeBranchRefinements =
                                    dontCareWorlds
                                      -- Gives preference to distributedWorlds
                                      |> Dict.union
                                           distributedWorlds
                                      |> Dict.map
                                           ( makeBranchRefinement
                                               maybeSubBindSpec
                                               constructorDefs
                                           )
                                      |> Dict.values
                                      |> Utils.projJusts
                                in
                                  case maybeBranchRefinements of
                                    Just branchRefinements ->
                                      NonDet.pure <|
                                        Match
                                          { scrutinee =
                                              scrutinee
                                          , branchRefinements =
                                              branchRefinements
                                          }

                                    Nothing ->
                                      NonDet.none

    -- IRefine-Hole
    holeRule () =
      if
        not worldsEmpty
          && filteredWorldsEmpty
          -- If the goal type is unit, don't synthesize a hole
          && Lang.tupleTypeArguments goalType /= Just []
      then
        NonDet.pure <|
          Hole {}
      else
        NonDet.none

--    -- IRefine-Guess
--    guessRefinement () =
--      if not filteredWorldsEmpty && isBaseType sigma goalType then
--        NonDet.pure <|
--          Guess
--            { sp = sp
--            , guess = Nothing
--            }
--      else
--        NonDet.none
  in
    { problem =
        sp

    , solution =
        Nothing

    , possibleRule =
        -- Apply IRefine-Fix first
        case T.matchArrow goalType of
          Just (_, [argType], returnType) ->
            fixRule argType returnType

          _ ->
            NonDet.union
              [ constructorRule ()
              , tupleRule ()
              , matchRule ()
              , holeRule ()
              ]
    }

-- fillGuesses : { maxTermSize : Int } -> RTree -> GenCached RTree
-- fillGuesses params rtree =
--   case rtree of
--
--     -- This is the important case, the rest is just recursive plumbing
--
--     Guess { sp } ->
--       State.pureDo (guessAndCheck params sp) <| \guess ->
--         Guess
--           { sp = sp
--           , guess = Just guess
--           }
--
--     -- Recursive plumbing from here on down
--
--     Ctor ({ possibleArg } as info) ->
--       State.pureDo
--         ( possibleArg
--             |> NonDet.map (fillGuesses params)
--             |> State.nSequence
--         ) <| \newPossibleArg ->
--           Ctor
--             { info | possibleArg = newPossibleArg }
--
--     Tuple ({ possibleComponents } as info) ->
--       State.pureDo
--         ( possibleComponents
--             |> List.map (NonDet.map (fillGuesses params) >> State.nSequence)
--             |> State.sequence
--         ) <| \newPossibleComponents ->
--           Tuple
--             { info | possibleComponents = newPossibleComponents }
--
--     Fun ({ possibleBody } as info) ->
--       State.pureDo
--         ( possibleBody
--             |> NonDet.map (fillGuesses params)
--             |> State.nSequence
--         ) <| \newPossibleBody ->
--           Fun
--             { info | possibleBody = newPossibleBody }
--
--     Hole info ->
--       State.pure <|
--         Hole info
--
--     Match ({ possibleBranches } as info) ->
--       State.pureDo
--         ( possibleBranches
--             |> List.map
--                  ( Tuple.mapSecond <|
--                      NonDet.map (fillGuesses params) >> State.nSequence
--                  )
--             |> List.map (\(p, s) -> State.map (\n -> (p, n)) s)
--             |> State.sequence
--         ) <| \newPossibleBranches ->
--           Match
--             { info | possibleBranches = newPossibleBranches }
--
--     EarlyTermination info ->
--       State.pure <|
--         EarlyTermination info

-- This is all "propagate":
--
-- fillTree : RTree -> GenCached RTree
--   1. fill each rule
--   2. applyRule each possible rule, collect into variable X
--   3. if X is empty, solution = termgen, else solution = X

-- fillRule : RRule -> GenCached RRule
--   1. call fillTree on each child tree
--
-- applyRule : RRule -> SynthesisSolution
--   1. wrap the refinement in the correct constructor (simple; not recursive)

propagate : { maxTermSize : Int } -> RTree -> GenCached RTree
propagate params =
  let
    fillTree : RTree -> GenCached RTree
    fillTree { problem, solution, possibleRule } =
      State.do
        ( possibleRule
            |> NonDet.map fillRule
            |> State.nSequence
            |> State.map NonDet.join
        ) <| \possibleNewRule ->
      let
        possiblePropagationSolution =
          possibleNewRule
            |> NonDet.map applyRule
            |> NonDet.join
      in
        -- Only guess if we are at base type AND refinement fails
        if
          isBaseType problem.sigma problem.goalType
            && NonDet.isEmpty possiblePropagationSolution
        then
          guessAndCheck params problem
        else
          possiblePropagationSolution

    fillRule : RRule -> GenCached RRule
    fillRule rule =
      case rule of
        Ctor args ->
          State.map
            ( \newArgRefinement ->
                Ctor { args | argRefinement = newArgRefinement }
            )
            (fillTree args.argRefinement)

        Tuple args ->
          State.map
            ( \newComponentRefinements ->
                Tuple { args | componentRefinements = newComponentRefinements }
            )
            (State.sequence <| List.map fillTree args.componentRefinements)

        Fix args ->
          State.map
            ( \newBodyRefinement ->
                Fix { args | bodyRefinement = newBodyRefinement }
            )
            (fillTree args.bodyRefinement)

        Match args ->
          State.map
            ( \newBranchRefinements ->
                Match { args | branchRefinements = newBranchRefinements }
            )
            ( args.ranchRefinements
                |> List.map (Tuple.mapSecond fillTree)
                |> List.map (\(p, st) -> State.map (\t -> (p, t) st))
            )

        Hole args ->
          State.pure <|
            Hole args

    applyRule : RRule -> SynthesisSolution
    applyRule rule =
      case rule of
        Ctor { ctorName, argRefinement } ->
          NonDet.map
            ( Tuple.mapFirst <|
                replacePrecedingWhitespace1
                  >> List.singleton
                  >> eDatatype ctorName
            )
            ( argRefinement.solution
            )

        Tuple { componentRefinements } ->
          componentRefinements
            |> List.map .solution
            |> NonDet.oneOfEach
            |> NonDet.map
                 ( List.unzip
                     >> Tuple.mapFirst eTuple0
                     >> Tuple.mapSecond List.concat
                 )

        Fix { fixName, argName, bodyRefinement } ->
          let
            argNamePat : Pat
            argNamePat =
              pVar0 argName

            makeFunction : Exp -> Exp
            makeFunction functionBody =
              let
                recursiveNameFinder e =
                  case unwrapExp e of
                    EVar _ name ->
                      name == fixName

                    _ ->
                      False
              in
                case findFirstNode recursiveNameFinder functionBody of
                  -- Recursive
                  Just _ ->
                    replacePrecedingWhitespace1 <|
                      eLet
                        [(fixName, eFun [argNamePat] functionBody)]
                        (eVar fixName)

                  -- Non-recursive
                  Nothing ->
                    eFun0 [argNamePat] functionBody
          in
            NonDet.map
              (Tuple.mapFirst makeFunction)
              (bodyRefinement.solution)

        Match { scrutinee, branchRefinements } ->
          let
            makeCase :
              List (Pat, (Exp, Constraints)) -> (Exp, Constraints)
            makeCase branchesWithConstraints =
              let
                branches =
                  branchesWithConstraints
                    |> List.map
                         ( \(pat, (body, _)) ->
                             withDummyInfo <|
                               Branch_ space0 pat body space1
                         )

                constraints =
                  branchesWithConstraints
                    |> List.map (Tuple.second >> Tuple.second)
                    |> List.concat
              in
                (eCase scrutinee branches, constraints)
          in
            branchRefinements
              |> List.map (Tuple.mapSecond .solution)
              |> List.map (\(p, ne) -> NonDet.map (\e -> (p, e)) ne)
              |> NonDet.oneOfEach
              |> NonDet.map makeCase

        Hole _ ->
          NonDet.pure (Lang.eEmptyHole0, [])
  in
    fillTree

type SynthesisStage
  = Zero0
  | Zero1
  | Zero2
  | Zero3
  | One
  | Two
  | Three
  | Four
  | Five

synthesize :
  SynthesisStage -> SynthesisProblem -> GenCached RTree
synthesize stage sp =
  let
    (maxScrutineeSize, maxMatchDepth, maxTermSize) =
      case stage of
        Zero0 ->
          (1, 0, 1)

        Zero1 ->
          (1, 0, 3)

        Zero2 ->
          (1, 0, 5)

        Zero3 ->
          (1, 0, 7)

        One ->
          (1, 0, 10)

        Two ->
          (1, 1, 10)

        Three ->
          (1, 2, 10)

        Four ->
          (6, 2, 10)

        Five ->
          (6, 3, 10)

    rtree =
      arefine
        { maxScrutineeSize = maxScrutineeSize
        , maxMatchDepth = maxMatchDepth
        }
        sp
  in
    let () = Debug.log (showTree rtree) () in
    propagate { maxTermSize = maxTermSize } rtree

--------------------------------------------------------------------------------
-- Iterative Constraint Solving
--------------------------------------------------------------------------------

type alias SolveProblem =
  { depth : Int
  , stage : SynthesisStage
  , constraints : Constraints
  }

solve_ :
  T.DatatypeEnv
    -> T.HoleEnv
    -> List SolveProblem
    -> GenCached (List HoleFilling)
solve_ sigma delta problems =
  case problems of
    [] ->
      State.pure []

    ({ depth, stage, constraints } as problem) :: restProblems ->
      let
        _ =
          Debug.log
            "(depth, stage, numConstraints)"
            (depth, stage, List.length constraints)
      in
        if depth == maxSolveDepth then
          solve_ sigma delta restProblems
        else
          let
            solveOne : HoleId -> Worlds -> GenCached RTree
            solveOne holeId worlds =
              case T.holeEnvGet holeId delta of
                Just (gamma, goalType) ->
                  synthesize
                    stage
                    { sigma = sigma
                    , gamma = gamma
                    , worlds = worlds
                    , goalType = goalType
                    }

                Nothing ->
                  State.pure NonDet.none

            statefulSolutions : GenCached (NonDet (HoleFilling, Constraints))
            statefulSolutions =
              constraints
                |> Utils.pairsToDictOfLists -- "Group" operation
                |> Dict.map solveOne
                |> Dict.toList
                   --> L (h, S (N (e, k)))
                |> List.map
                     (\(h, gcss) -> State.map (\ss -> (h, ss)) gcss)
                   --> L (S (h, N (e, k)))
                |> State.sequence
                   --> S (L (h, N (e, k)))
                |> State.map
                     (List.map <| \(h, ss) -> NonDet.map (\ek -> (h, ek)) ss)
                  -- S (L (N (h, (e, k))))
                |> State.map
                     ( NonDet.oneOfEach
                       --> S N L (h, (e, k))
                         >> NonDet.map
                              ( Dict.fromList
                                  >> Utils.unzipDict
                                  >> Tuple.mapSecond
                                       (Dict.values >> List.concat)
                              )
                     )

            oldConstraintSet =
              Set.fromList constraints
          in
            State.do statefulSolutions <| \solutions ->
              let
                result =
                  Utils.partitionEithers << NonDet.toList <|
                    NonDet.pureDo solutions <|
                      \(holeFilling, newConstraints) ->
                      let
                        newConstraintSet =
                          Set.fromList newConstraints
                      in
                        if
                          Utils.isSubset newConstraintSet oldConstraintSet
                        then
                          Utils.Left holeFilling
                        else
                          Utils.Right
                            { depth =
                                depth + 1
                            , stage =
                                stage
                            , constraints =
                                Set.toList <|
                                  Set.union oldConstraintSet newConstraintSet
                            }
              in
                case result of
                  ([], newProblems) ->
                    solve_
                      sigma
                      delta
                      (newProblems ++ restProblems)

                  (trueSolutions, _) ->
                    State.pure trueSolutions

solve :
  T.DatatypeEnv
    -> T.HoleEnv
    -> NonDet Constraints
    -> (List HoleFilling, Float)
solve sigma delta possibleConstraints =
  let
    constraintsList =
      NonDet.toList possibleConstraints

    problems =
      List.concatMap
        ( \stage ->
            List.map
              ( \constraints ->
                  { depth = 0
                  , stage = stage
                  , constraints = constraints
                  }
              )
              constraintsList
        )
        [Zero0, Zero1, Zero2, Zero3, One, Two, Three, Four, Five]

    _ = Debug.log "Problems" (List.map (\p -> (p.stage, p.depth)) problems)
  in
    ImpureGoodies.timedRun <| \_ ->
      Tuple.first <|
        State.run TermGen.emptyCache <|
          solve_ sigma delta problems

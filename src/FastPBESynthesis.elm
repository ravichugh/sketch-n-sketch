--------------------------------------------------------------------------------
-- This module contains all the code for Programming by Example (PBE) synthesis,
-- but with the following additional optimizations:
--   * Refinement trees
--------------------------------------------------------------------------------
-- TODO:
--   * isBaseType
--   * Recursive functions
--   * Tuples seem not to be working

module FastPBESynthesis exposing
  ( solve
  )

import PBESynthesis exposing (satisfiesWorlds)

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

type alias RInfo =
  { sigma : T.DatatypeEnv
  , gamma : T.TypeEnv
  , worlds : Worlds
  , goalType : Type
  , guess : Maybe (NonDet (Exp, Constraints))
  , height : Int
  }

type EarlyTerminationReason
  = OutOfMatchBudget

-- One for each IRefine-* rule, except IRefine-Guess
type RTree
  = Constant Exp
  | Ctor RInfo Ident (NonDet RTree)
  | Tuple RInfo (List (NonDet RTree))
  | Fun RInfo Ident (NonDet RTree)
  | Match RInfo Exp (List (Pat, NonDet RTree))
  | EarlyTermination EarlyTerminationReason

height : RTree -> Int
height rt =
  case rt of
    Constant _ ->
      0

    Ctor { height } _ _ ->
      height

    Tuple { height } _ ->
      height

    Fun { height } _ _ ->
      height

    Match { height } _ _ ->
      height

    EarlyTermination _ ->
      0

nheight : NonDet RTree -> Int
nheight =
  NonDet.toList >> List.map height >> List.maximum >> Maybe.withDefault 0

modifyRefinementInfo : (RInfo -> RInfo) -> RTree -> RTree
modifyRefinementInfo f rtree =
  case rtree of
    Constant e ->
      Constant e

    Ctor rinfo ctorName arg ->
      Ctor (f rinfo) ctorName (NonDet.map (modifyRefinementInfo f) arg)

    Tuple rinfo args ->
      Tuple (f rinfo) (List.map (NonDet.map (modifyRefinementInfo f)) args)

    Fun rinfo argName body ->
      Fun (f rinfo) argName (NonDet.map (modifyRefinementInfo f) body)

    Match rinfo eScrutinee branches ->
      Match
        (f rinfo)
        eScrutinee
        ( List.map
            ( Tuple.mapSecond <|
                NonDet.map (modifyRefinementInfo f)
            )
            branches
        )

    EarlyTermination reason ->
      EarlyTermination reason

gen :
  { sigma : T.DatatypeEnv
  , gamma : T.TypeEnv
  , goalType : Type
  , maxTermSize : Int
  } ->
  NonDet Exp
gen args =
  let
    genE ({ sigma, gamma, goalType, termSize } as args) =
      case termSize of
        0 ->
          NonDet.none

        1 ->
          gamma
            |> T.typePairs
            |> List.filter (Tuple.second >> T.typeEquiv gamma goalType)
            |> List.map (Tuple.first >> eVar)
            |> NonDet.fromList

        _ ->
          let
            fromArrowType : ArrowType -> Maybe (Type, Type)
            fromArrowType (_, argTypes, returnType) =
              case argTypes of
                -- TODO Only support single arguments for now
                [argType] ->
                  if T.typeEquiv gamma goalType returnType then
                    Just (argType, returnType)
                  else
                    Nothing

                _ ->
                  Nothing

            possibleArrowType : NonDet (Type, Type)
            possibleArrowType =
              gamma
                |> T.typePairs
                |> List.map
                     ( Tuple.second
                         >> T.matchArrowRecurse
                         >> Maybe.andThen fromArrowType
                     )
                |> Utils.filterJusts
                |> NonDet.fromList

            possiblePartition : NonDet (List Int)
            possiblePartition =
              Utils.partitionInteger (termSize - 1) 2
                |> NonDet.fromList
          in
            NonDet.do possibleArrowType <| \(argType, returnType) ->
            NonDet.do possiblePartition <| \partition ->
              case partition of
                -- Will always happen
                [k1, k2] ->
                  let
                    possibleHead : NonDet Exp
                    possibleHead =
                      genE
                        { args
                        | goalType = T.rebuildArrow ([], [argType], returnType)
                        , termSize = k1
                        }

                    possibleArg : NonDet Exp
                    possibleArg =
                      genE
                        { args
                        | goalType = argType
                        , termSize = k2
                        }
                  in
                    -- TODO Check structural recursion
                    NonDet.do possibleHead <| \head ->
                    NonDet.do possibleArg <| \arg ->
                    NonDet.pure <|
                      eApp0 head [replacePrecedingWhitespace1 arg]

                _ ->
                  NonDet.none

    genI ({ sigma, gamma, goalType, termSize } as args) =
      case termSize of
        0 ->
          NonDet.none

        _ ->
          case T.matchArrowRecurse goalType of
            Just (_, argTypes, returnType) ->
              case argTypes of
                -- TODO Only support single arguments for now
                [argType] ->
                  let
                    argNamePat : Pat
                    argNamePat =
                      pVar0 "x"

                    possibleFunctionBody : NonDet Exp
                    possibleFunctionBody =
                      genI
                        { sigma =
                            sigma
                        , gamma =
                            T.addHasType
                              (argNamePat, argType)
                              gamma
                        , goalType =
                            returnType
                        , termSize =
                            termSize - 1
                        }
                  in
                    NonDet.map (eFun0 [argNamePat]) possibleFunctionBody

                _ ->
                  NonDet.none

            Nothing ->
              let
                eOption =
                  genE args

                constructorOption =
                  case unwrapType goalType of
                    TVar _ datatypeName ->
                      case Utils.maybeFind datatypeName sigma of
                        Just (_, datatypeConstructors) ->
                          NonDet.do (NonDet.fromList datatypeConstructors) <|
                            \(ctorName, argTypes) ->
                              -- TODO Only support single arguments for now
                              case argTypes of
                                [argType] ->
                                  NonDet.map
                                    ( replacePrecedingWhitespace1
                                        >> List.singleton
                                        >> eDatatype ctorName
                                    )
                                    ( genI
                                        { args
                                        | goalType = argType
                                        , termSize = termSize - 1
                                        }
                                    )

                                _ ->
                                  NonDet.none

                        Nothing ->
                          NonDet.none

                    _ ->
                      NonDet.none

                tupleOption =
                  case tupleTypeArguments goalType of
                    Nothing ->
                      NonDet.none

                    Just argGoalTypes ->
                      let
                        tupleLength =
                          List.length argGoalTypes

                        possiblePartition : NonDet (List Int)
                        possiblePartition =
                          NonDet.fromList <|
                            Utils.partitionInteger (termSize - 1) tupleLength
                      in
                        NonDet.do possiblePartition <| \partition ->
                          NonDet.map eTuple0 <|
                            NonDet.oneOfEach <|
                              List.map2
                                ( \tau k ->
                                    genI
                                      { args
                                      | goalType = tau
                                      , termSize = k
                                      }
                                )
                                argGoalTypes
                                partition
              in
                NonDet.concat
                  [ eOption
                  , constructorOption
                  , tupleOption
                  ]
  in
    NonDet.concat <|
      List.map
        ( \termSize ->
            genE
              { sigma = args.sigma
              , gamma = args.gamma
              , goalType = args.goalType
              , termSize = termSize
              }
        )
        ( List.range 1 args.maxTermSize
        )

guessAndCheck :
  { sigma : T.DatatypeEnv
  , gamma : T.TypeEnv
  , worlds : Worlds
  , goalType : Type
  , maxTermSize : Int
  } ->
  NonDet (Exp, Constraints)
guessAndCheck { sigma, gamma, worlds, goalType, maxTermSize } =
  let
    possibleSolution =
      gen
        { sigma = sigma
        , gamma = gamma
        , goalType = goalType
        , maxTermSize = maxTermSize
        }
  in
    possibleSolution
      |> NonDet.map
           (\e -> Utils.liftMaybePair2 (e, satisfiesWorlds worlds e))
      |> NonDet.collapseMaybe

arefine :
  { sigma : T.DatatypeEnv
  , gamma : T.TypeEnv
  , worlds : Worlds
  , goalType : Type
  , maxScrutineeSize : Int
  , maxMatchDepth : Int
  } ->
  NonDet RTree
arefine
  ( { sigma, gamma, worlds, goalType, maxScrutineeSize, maxMatchDepth }
      as args
  ) =
  let
    getRefinementInfo : Int -> RInfo
    getRefinementInfo h =
      { sigma = sigma
      , gamma = gamma
      , worlds = worlds
      , goalType = goalType
      , guess = Nothing
      , height = h
      }

    -- Filter out all the "don't care" examples
    filteredWorlds =
      List.filter
        (Tuple.second >> (/=) ExDontCare)
        worlds

    -- IRefine-Constant
    constantRefinement () =
      let
        extractConstant : Example -> Maybe Exp
        extractConstant ex =
          case (unwrapType goalType, ex) of
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
          |> Maybe.map Constant
          |> NonDet.fromMaybe

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
        case unwrapType goalType of
          TVar _ datatypeName ->
            case Utils.maybeFind datatypeName sigma of
              Just (typeArgNames, datatypeConstructors) ->
                NonDet.do (NonDet.fromList datatypeConstructors) <|
                  \(ctorName, argTypes) ->
                    case argTypes of
                      -- Only support single arguments for now
                      [argType] ->
                        ctorName
                          |> extractConstructorArgWorlds
                          |> Maybe.map
                               ( \constructorArgWorlds ->
                                   let
                                      ctorArg =
                                        arefine
                                          { args
                                          | worlds = constructorArgWorlds
                                          , goalType = argType
                                          }

                                      rinfo =
                                        getRefinementInfo (nheight ctorArg + 1)
                                   in
                                     NonDet.pure <|
                                       Ctor rinfo ctorName ctorArg
                               )
                          |> Maybe.withDefault NonDet.none

                      _ ->
                        NonDet.none

              Nothing ->
                NonDet.none

          _ ->
            NonDet.none

    -- IRefine-Tuple
    tupleRefinement () =
      case tupleTypeArguments goalType of
        Nothing ->
          NonDet.none

        Just argGoalTypes ->
          let
            tupleLength : Int
            tupleLength =
              List.length argGoalTypes

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
                       let
                          tupleArgs =
                            List.map2
                              ( \argWorlds argGoalType ->
                                  arefine
                                    { args
                                    | worlds = argWorlds
                                    , goalType = argGoalType
                                    }
                              )
                              tupleArgsWorlds
                              argGoalTypes

                          rinfo =
                            tupleArgs
                              |> List.map nheight
                              |> List.maximum
                              |> Maybe.withDefault 0
                              |> (+) 1
                              |> getRefinementInfo
                       in
                         NonDet.pure <|
                           Tuple rinfo tupleArgs
                   )
              |> Maybe.withDefault NonDet.none

    -- IRefine-Fun
    partialFunctionRefinement argType returnType =
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

        worldFromEntry :
          PartialFunction
            -> U.Env
            -> (List (UnExp ()), Example)
            -> Maybe World
        worldFromEntry partialFunction env (args, output) =
          case args of
            [arg] ->
              Just
                ( env
                    -- Recursive function binding
                    |> U.addVarBinding
                         functionName
                         (UPartialFunction () partialFunction)
                    -- Argument binding
                    |> U.addVarBinding argName arg
                , output
                )

            -- TODO Only support single-argument functions for now
            _ ->
              Nothing

        branchWorlds : World -> Maybe Worlds
        branchWorlds (env, ex) =
          case ex of
            ExPartialFunction partialFunction ->
              partialFunction
                |> List.map (worldFromEntry partialFunction env)
                |> Utils.projJusts

            _ ->
              Nothing

        newGamma : T.TypeEnv
        newGamma =
          gamma
            |> T.addHasType (functionNamePat, functionType)
            |> T.addHasType (argNamePat, argType)
      in
        filteredWorlds
          |> List.map branchWorlds
          |> Utils.projJusts
          |> Maybe.map List.concat
          |> Maybe.map
               ( \allWorlds ->
                   let
                      functionBody =
                        arefine
                          { args
                          | gamma = newGamma
                          , worlds = allWorlds
                          , goalType = returnType
                          }

                      rinfo =
                        getRefinementInfo (nheight functionBody + 1)
                   in
                     NonDet.pure <|
                       Fun rinfo argName functionBody
               )
          |> Maybe.withDefault NonDet.none

    -- IRefine-Match
    matchRefinement () =
      if maxMatchDepth == 0 then
        NonDet.pure <|
          EarlyTermination OutOfMatchBudget
      else
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
                  ( gen
                      { sigma = sigma
                      , gamma = gamma
                      , goalType = dType
                      , maxTermSize = maxScrutineeSize
                      }
                  ) <|
                  \eScrutinee ->
                    Maybe.withDefault NonDet.none <|
                      flip Maybe.map (distributeWorlds eScrutinee worlds) <|
                        \distributedWorlds ->
                          if Dict.size distributedWorlds < 2 then
                            -- Uninformative match
                            NonDet.none
                          else
                            let
                              branches =
                                dontCareWorlds
                                  -- Gives preference to distributedWorlds
                                  |> Dict.union distributedWorlds
                                  |> Dict.toList
                                  |> List.map
                                       ( \(ctorName, branchWorlds) ->
                                           case
                                             Utils.maybeFind
                                               ctorName
                                               constructorDefs
                                           of
                                             Just [ctorArgType] ->
                                               let
                                                 newGamma =
                                                   T.addHasType
                                                     (argNamePat, ctorArgType)
                                                     gamma

                                                 pat =
                                                   pDatatype
                                                     ctorName
                                                     [argNamePat]

                                                 branchBody =
                                                   arefine
                                                     { args
                                                     | gamma =
                                                         newGamma
                                                     , worlds =
                                                         branchWorlds
                                                     , goalType =
                                                         goalType
                                                     , maxMatchDepth =
                                                         maxMatchDepth - 1
                                                     }
                                               in
                                                  (pat, branchBody)

                                             _ ->
                                               (pVar0 "ERROR", NonDet.none)
                                       )

                              rinfo =
                                branches
                                  |> List.map (Tuple.second >> nheight)
                                  |> List.maximum
                                  |> Maybe.withDefault 0
                                  |> (+) 1
                                  |> getRefinementInfo
                            in
                              NonDet.pure <|
                                Match rinfo eScrutinee branches
  in
    -- Apply IRefine-Fun first
    case T.matchArrowRecurse goalType of
      Just (_, argTypes, returnType) ->
        case argTypes of
          -- TODO Only support single-argument functions for now
          [argType] ->
            partialFunctionRefinement argType returnType

          _ ->
            NonDet.none

      Nothing ->
        NonDet.concat
          [ constantRefinement ()
          , constructorRefinement ()
          , tupleRefinement ()
          , matchRefinement ()
          ]

-- TODO
isBaseType : Type -> Bool
isBaseType tau =
  True

-- Currently does not check height
fillGuesses : Int -> RTree -> RTree
fillGuesses maxTermSize =
  modifyRefinementInfo <| \rinfo ->
    if isBaseType rinfo.goalType then
      { rinfo
          | guess =
              Just <|
                guessAndCheck
                  { sigma = rinfo.sigma
                  , gamma = rinfo.gamma
                  , worlds = rinfo.worlds
                  , goalType = rinfo.goalType
                  , maxTermSize = maxTermSize
                  }
      }
    else
      rinfo

propagate : RTree -> NonDet (Exp, Constraints)
propagate rtree =
  case rtree of
    Constant e ->
      NonDet.pure (e, [])

    Ctor rinfo ctorName arg ->
      NonDet.concat
        [ Maybe.withDefault NonDet.none rinfo.guess
        , NonDet.map
            ( Tuple.mapFirst <|
                replacePrecedingWhitespace1
                  >> List.singleton
                  >> eDatatype ctorName
            )
            ( arg
                |> NonDet.andThen propagate
            )
        ]

    Tuple rinfo args ->
      NonDet.concat
        [ Maybe.withDefault NonDet.none rinfo.guess
        , args
            |> List.map (NonDet.andThen propagate)
            |> NonDet.oneOfEach
            |> NonDet.map
                 ( List.unzip
                     >> Tuple.mapFirst eTuple0
                     >> Tuple.mapSecond List.concat
                 )
        ]

    -- TODO Doesn't handle recursive functions?
    Fun rinfo argName body ->
      NonDet.concat
        [ Maybe.withDefault NonDet.none rinfo.guess
        , NonDet.map
            ( Tuple.mapFirst <|
                eFun0 [pVar0 argName]
            )
            ( body
                |> NonDet.andThen propagate
            )
        ]

    Match rinfo eScrutinee branches ->
      let
        makeCase :
          List (Pat, (Exp, Constraints)) -> (Exp, Constraints)
        makeCase branchesWithConstraints =
          let
            eBranches =
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
            (eCase eScrutinee eBranches, constraints)
      in
        NonDet.concat
          [ Maybe.withDefault NonDet.none rinfo.guess
          , branches
              |> List.map (Tuple.mapSecond (NonDet.andThen propagate))
              |> List.map (\(p, ne) -> NonDet.map (\e -> (p, e)) ne)
              |> NonDet.oneOfEach
              |> NonDet.map makeCase
          ]

    EarlyTermination reason ->
      NonDet.none

type SynthesisStage
  = One
  | Two
  | Three
  | Four
  | Five

nextStage : SynthesisStage -> Maybe SynthesisStage
nextStage s =
  case s of
    One -> Just Two
    Two -> Just Three
    Three -> Just Four
    Four -> Just Five
    Five -> Nothing

synthesize :
  { sigma : T.DatatypeEnv
  , gamma : T.TypeEnv
  , worlds : Worlds
  , goalType : Type
  } ->
  NonDet (Exp, Constraints)
synthesize { sigma, gamma, worlds, goalType } =
  let
    synthesize_ stage =
      let
        (maxScrutineeSize, maxMatchDepth, maxTermSize) =
          case stage of
            One ->
              (1, 0, 13)

            Two ->
              (1, 1, 13)

            Three ->
              (1, 2, 13)

            Four ->
              (6, 2, 13)

            Five ->
              (6, 3, 13)

        solution =
          { sigma = sigma
          , gamma = gamma
          , worlds = worlds
          , goalType = goalType
          , maxScrutineeSize = maxScrutineeSize
          , maxMatchDepth = maxMatchDepth
          }
            |> arefine
            |> NonDet.map (fillGuesses maxTermSize >> propagate)
            |> NonDet.collapse
      in
        if NonDet.isEmpty solution then
          case nextStage stage of
            Just newStage ->
              -- Synthesis failure, trying next stage
              synthesize_ newStage

            Nothing ->
              -- Synthesis failure, no more stages to try
              solution

        else
          -- Synthesis success
          solution
  in
    synthesize_ One

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
          Just (gamma, goalType) ->
            synthesize
              { sigma = sigma
              , gamma = gamma
              , worlds = worlds
              , goalType = goalType
              }

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

maxSolveDepth : Int
maxSolveDepth =
  5

solve : T.DatatypeEnv -> T.HoleEnv -> Constraints -> NonDet HoleFilling
solve =
  solve_ maxSolveDepth

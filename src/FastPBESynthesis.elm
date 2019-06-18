--------------------------------------------------------------------------------
-- This module contains all the code for Programming by Example (PBE) synthesis,
-- but with the following additional optimizations:
--   * Refinement trees
--------------------------------------------------------------------------------
-- TODO:
--   * isBaseType
--   * incomplete recursive functions (fancy new fix rule)
--   * projection is always size 2

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

--------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------

maxSolveDepth : Int
maxSolveDepth =
  5

recFunctionName : Ident
recFunctionName =
  "rec"

--------------------------------------------------------------------------------
-- Type Helpers
--------------------------------------------------------------------------------

-- TODO
isBaseType : Type -> Bool
isBaseType tau =
  T.matchArrowRecurse tau == Nothing

showTypePairs : T.TypeEnv -> String
showTypePairs =
  T.typePairs
    >> List.map (Tuple.mapSecond LeoUnparser.unparseType)
    >> List.map (\(x, tau) -> x ++ " : " ++ tau)
    >> String.join ", "

showTypeIdents : T.TypeEnv -> String
showTypeIdents =
  T.typePairs
    >> List.map Tuple.first
    >> String.join ", "

freshIdent : T.TypeEnv -> String
freshIdent gamma =
  let
    extractNumber : Ident -> Maybe Int
    extractNumber ident =
      case String.uncons ident of
        Just ('x', rest) ->
          Utils.natFromString rest

        _ ->
          Nothing

    freshNumber : Int
    freshNumber =
      gamma
        |> varsOfGamma
        |> List.map extractNumber
        |> Utils.filterJusts
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 0
  in
    "x" ++ toString freshNumber

-- Given a type
--   a -> b -> c -> ... -> z,
-- this function returns
--   Just (a, b -> c -> ... -> z).
-- (As a base case, given a type a -> b, this function returns Just (a, b).)
-- Otherwise, this function returns Nothing.
curryArrow : Type -> Maybe (Type, Type)
curryArrow tau =
  case T.matchArrowRecurse tau of
    Just (_, argTypes, returnType) ->
      case argTypes of
        argTypeHead :: argTypeRest ->
          Just
            ( argTypeHead
            , if List.isEmpty argTypeRest then
                returnType
              else
                T.rebuildArrow ([], argTypeRest, returnType)
            )

        -- Impossible
        [] ->
          Nothing

    Nothing ->
      Nothing

--------------------------------------------------------------------------------
-- Data Structures
--------------------------------------------------------------------------------

type alias HasGenInfo a =
  { a
  | sigma : T.DatatypeEnv
  , gamma : T.TypeEnv
  , goalType : Type
  }

type alias SynthesisProblem =
  { sigma : T.DatatypeEnv
  , gamma : T.TypeEnv
  , worlds : Worlds
  , goalType : Type
  }

type alias SynthesisSolution =
  NonDet (Exp, Constraints)

type EarlyTerminationReason
  = OutOfMatchBudget

type RTree
  = Constant
      { val : Exp }
  | Ctor
      { height : Int
      , ctorName : Ident
      , possibleArg : NonDet RTree
      }
  | Tuple
      { height : Int
      , possibleComponents : List (NonDet RTree)
      }
  | Fun
      { height : Int
      , argName : Ident
      , possibleBody : NonDet RTree
      }
  | Match
      { height : Int
      , scrutinee : Exp
      , possibleBranches : List (Pat, NonDet RTree)
      }
  | Guess
      { sp : SynthesisProblem
      , guess : Maybe SynthesisSolution
      }
  | EarlyTermination
      { reason : EarlyTerminationReason
      }

showTree : RTree -> String
showTree =
  let
    showTree_ indentLevel rtree =
      let
        indent =
          String.repeat indentLevel "  "

        val =
          case rtree of
            Constant { val } ->
              "Constant " ++ LeoUnparser.unparse val

            Ctor { ctorName, possibleArg } ->
              let
                children =
                  possibleArg
                    |> NonDet.toList
                    |> List.map (showTree_ (indentLevel + 1))
                    |> String.join "\n"
              in
                "Ctor " ++ ctorName ++ "\n" ++ children

            Tuple { possibleComponents } ->
              let
                tupleName =
                  "Tuple" ++ toString (List.length possibleComponents)

                children =
                  possibleComponents
                    |> List.map
                         ( NonDet.toList
                             >> List.map (showTree_ (indentLevel + 1))
                             >> String.join "\n"
                         )
                    |> List.indexedMap
                         (\i s -> toString (i + 1) ++ ". " ++ s)
                    |> String.join "\n"
              in
                tupleName ++ "\n" ++ children

            Fun { argName, possibleBody } ->
              let
                children =
                  possibleBody
                    |> NonDet.toList
                    |> List.map (showTree_ (indentLevel + 1))
                    |> String.join "\n"
              in
                "Fun \\" ++ argName ++ " ->\n" ++ children

            Match { scrutinee, possibleBranches } ->
              let
                children =
                  possibleBranches
                    |> List.map
                         ( Tuple.mapFirst <| \p ->
                             indent ++ LeoUnparser.unparsePattern p ++ " -> \n"
                         )
                    |> List.map
                         ( Tuple.mapSecond <|
                             NonDet.toList
                               >> List.map (showTree_ (indentLevel + 2))
                               >> String.join "\n"
                         )
                    |> List.map (\(a, b) -> a ++ b)
                    |> String.join "\n"
              in
                "Match " ++ LeoUnparser.unparse scrutinee ++ "\n" ++ children

            Guess { sp } ->
              "Guess [" ++ showTypePairs sp.gamma ++ "]"

            EarlyTermination { reason } ->
              let
                reasonString =
                  case reason of
                    OutOfMatchBudget ->
                      "OutOfMatchBudget"
              in
                "EarlyTermination " ++ reasonString
      in
        indent ++ val
  in
    showTree_ 0

height : RTree -> Int
height rtree =
  case rtree of
    Constant _ ->
      0

    Ctor { height } ->
      height

    Tuple { height } ->
      height

    Fun { height } ->
      height

    Match { height } ->
      height

    Guess _ ->
      0

    EarlyTermination _ ->
      0

nheight : NonDet RTree -> Int
nheight =
  NonDet.toList >> List.map height >> List.maximum >> Maybe.withDefault 0

gen : { maxTermSize : Int } -> HasGenInfo a -> NonDet Exp
gen { maxTermSize } initialInfo =
  let
    genE : { termSize : Int } -> HasGenInfo a -> NonDet Exp
    genE { termSize } ({ sigma, gamma, goalType } as info) =
      let
        typePairs =
          T.typePairs gamma
      in
        case termSize of
          0 ->
            NonDet.none

          1 ->
            typePairs
              |> List.filter (Tuple.second >> T.typeEquiv gamma goalType)
              |> List.map (Tuple.first >> eVar)
              |> NonDet.fromList

          2 ->
            let
              getOption =
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
                        |> List.filter
                             (Tuple.first >> T.typeEquiv gamma goalType)
                        -- i should be 1-indexed
                        |> List.map
                             (\(_, i) -> (n, i + 1))

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
              getOption

          _ ->
            let
              arrowOption =
                let
                  extractArrow : ArrowType -> Maybe (List Type, Type)
                  extractArrow (_, argTypes, returnType) =
                    if T.typeEquiv gamma goalType returnType then
                      Just (argTypes, returnType)
                    else
                      Nothing

                  possibleArrowType : NonDet (List Type, Type)
                  possibleArrowType =
                    typePairs
                      |> List.map
                           ( Tuple.second
                               >> T.matchArrowRecurse
                               >> Maybe.andThen extractArrow
                           )
                      |> Utils.filterJusts
                      |> NonDet.fromList
                in
                  NonDet.do possibleArrowType <| \(argTypes, returnType) ->
                  let
                    -- Number of sub-problems to split the budget for
                    -- (head + number of arguments)
                    subProblemCount =
                      1 + List.length argTypes

                    -- Number of applications in the resulting code
                    applicationCount =
                      subProblemCount - 1

                    possiblePartition : NonDet (List Int)
                    possiblePartition =
                      NonDet.fromList <|
                        Utils.partitionIntegerPermutations
                          (termSize - applicationCount)
                          subProblemCount
                  in
                  NonDet.do possiblePartition <| \partition ->
                    case partition of
                      -- Will always happen (subProblemCount >= 2 always)
                      -- Also, |kArgs| = |argTypes|
                      kHead :: kArgs ->
                        let
                          possibleHead : NonDet Exp
                          possibleHead =
                            let
                              arrowType =
                                -- Safe because argTypes comes directly from
                                -- matchArrowRecurse, so it cannot be empty.
                                T.rebuildArrow ([], argTypes, returnType)
                            in
                              genE
                                { termSize = kHead }
                                { info | goalType = arrowType }

                          possibleArgs : NonDet (List Exp)
                          possibleArgs =
                            NonDet.oneOfEach <|
                              List.map2
                                ( \argType kArg ->
                                    genI
                                      { termSize = kArg }
                                      { info | goalType = argType }
                                )
                                argTypes
                                kArgs
                        in
                          NonDet.do possibleHead <| \head ->
                          NonDet.do possibleArgs <| \args ->
                            if
                              List.any
                                (T.structurallyDecreasing gamma head)
                                args
                            then
                              NonDet.pure <|
                                eApp0
                                  head
                                  ( List.map replacePrecedingWhitespace1 args
                                  )

                            else
                              NonDet.none
                      _ ->
                        NonDet.none
            in
              arrowOption

    genI : { termSize : Int } -> HasGenInfo a -> NonDet Exp
    genI { termSize } ({ sigma, gamma, goalType } as info) =
      case termSize of
        0 ->
          NonDet.none

        _ ->
          case curryArrow goalType of
            Just (argType, returnType) ->
              let
                argNamePat : Pat
                argNamePat =
                  pVar0 (freshIdent gamma)

                possibleFunctionBody : NonDet Exp
                possibleFunctionBody =
                  genI
                    { termSize = termSize - 1 } -- -1 for lambda abstraction
                    { info
                    | gamma =
                        T.addHasType
                          (argNamePat, argType, Nothing)
                          gamma
                    , goalType =
                        returnType
                    }
              in
                NonDet.map (eFun0 [argNamePat]) possibleFunctionBody

            Nothing ->
              let
                eOption =
                  genE { termSize = termSize } info

                constructorOption =
                  case unwrapType goalType of
                    TVar _ datatypeName ->
                      case Utils.maybeFind datatypeName sigma of
                        Just (_, datatypeConstructors) ->
                          NonDet.do (NonDet.fromList datatypeConstructors) <|
                            \(ctorName, argTypes) ->
                              -- Constructors only have single arguments for now
                              case argTypes of
                                [argType] ->
                                  NonDet.map
                                    ( replacePrecedingWhitespace1
                                        >> List.singleton
                                        >> eDatatype ctorName
                                    )
                                    ( genI
                                        { termSize = termSize - 1 }
                                        { info | goalType = argType }
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
                            Utils.partitionIntegerPermutations
                              (termSize - 1)
                              tupleLength
                      in
                        NonDet.do possiblePartition <| \partition ->
                          NonDet.map eTuple0 <|
                            NonDet.oneOfEach <|
                              List.map2
                                ( \tau k ->
                                    genI
                                      { termSize = k}
                                      { info | goalType = tau }
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
        (\termSize -> genE { termSize = termSize } initialInfo)
        (List.range 1 maxTermSize)

guessAndCheck : { maxTermSize : Int } -> SynthesisProblem -> SynthesisSolution
guessAndCheck params ({ worlds } as sp) =
  let
    possibleSolution =
      gen params sp
  in
    possibleSolution
      |> NonDet.map
           (\e -> Utils.liftMaybePair2 (e, satisfiesWorlds worlds e))
      |> NonDet.collapseMaybe

arefine :
  { maxScrutineeSize : Int, maxMatchDepth : Int }
    -> SynthesisProblem
    -> NonDet RTree
arefine params ({ sigma, gamma, worlds, goalType } as sp) =
  let
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
          |> Maybe.map (\val -> Constant { val = val })
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
              Just (_, datatypeConstructors) ->
                NonDet.do (NonDet.fromList datatypeConstructors) <|
                  \(ctorName, argTypes) ->
                    case argTypes of
                      -- Constructors only have single arguments for now
                      [argType] ->
                        ctorName
                          |> extractConstructorArgWorlds
                          |> Maybe.map
                               ( \constructorArgWorlds ->
                                   let
                                      possibleArg =
                                        arefine
                                          params
                                          { sp
                                          | worlds = constructorArgWorlds
                                          , goalType = argType
                                          }
                                   in
                                     NonDet.pure <|
                                       Ctor
                                         { height = nheight possibleArg + 1
                                         , ctorName = ctorName
                                         , possibleArg = possibleArg
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
    tupleRefinement () =
      case tupleTypeArguments goalType of
        Nothing ->
          NonDet.none

        Just argGoalTypes ->
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
                          possibleComponents =
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
                             { height =
                                 possibleComponents
                                   |> List.map nheight
                                   |> List.maximum
                                   |> Maybe.withDefault 0
                                   |> (+) 1
                             , possibleComponents =
                                 possibleComponents
                             }
                   )
              |> Maybe.withDefault NonDet.none

    -- IRefine-Fun
    partialFunctionRefinement argType returnType =
      let
        recFunctionNamePat : Pat
        recFunctionNamePat =
          pVar0 recFunctionName

        functionType : Type
        functionType =
          tFun0 argType returnType

        argName : Ident
        argName =
          freshIdent gamma

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
                    -- TODO Fancy fix rule for recursive function binding
                    |> U.addVarBinding
                         recFunctionName
                         (UPartialFunction () partialFunction)
                    -- Argument binding
                    |> U.addVarBinding argName arg
                , output
                )

            -- Only support single-argument functions for now (should not be a
            -- problem because of currying).
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
            |> T.addHasType
                 ( recFunctionNamePat
                 , functionType
                 , Just Rec
                 )
            |> T.addHasType
                 ( argNamePat
                 , argType
                 , Just (Arg recFunctionNamePat)
                 )
      in
        filteredWorlds
          |> List.map branchWorlds
          |> Utils.projJusts
          |> Maybe.map List.concat
          |> Maybe.map
               ( \allWorlds ->
                   let
                      possibleBody =
                        arefine
                          params
                          { sp
                          | gamma = newGamma
                          , worlds = allWorlds
                          , goalType = returnType
                          }
                   in
                     NonDet.pure <|
                       Fun
                         { height = nheight possibleBody + 1
                         , argName = argName
                         , possibleBody = possibleBody
                         }
               )
          |> Maybe.withDefault NonDet.none

    -- IRefine-Match
    matchRefinement () =
      if params.maxMatchDepth == 0 then
        NonDet.pure <|
          EarlyTermination { reason = OutOfMatchBudget }
      else
        let
          argName : Ident
          argName =
            freshIdent gamma

          argNamePat : Pat
          argNamePat =
            pVar argName

          distributeWorlds : Exp -> Worlds -> Maybe (Dict Ident Worlds)
          distributeWorlds scrutinee worlds =
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

          makePossibleBranch :
            Maybe T.BindingSpecification
              -> List DataConDef -> Ident -> Worlds -> (Pat, NonDet RTree)
          makePossibleBranch
           maybeSubBindSpec constructorDefs ctorName branchWorlds =
            -- TODO Could possibly avoid lookup here, but not that big of a deal
            -- because the length of constructorDefs will almost always be very
            -- small.
            case Utils.maybeFind ctorName constructorDefs of
              Just [ctorArgType] ->
                let
                  newGamma =
                    T.addHasType
                      (argNamePat, ctorArgType, maybeSubBindSpec)
                      gamma

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
                   (pat, branchBody)

              _ ->
                (pVar0 "ERROR", NonDet.none)
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
                      { maxTermSize = params.maxScrutineeSize }
                      { sigma = sigma
                      , gamma = gamma
                      , goalType = dType
                      }
                  ) <|
                  \scrutinee ->
                    Maybe.withDefault NonDet.none <|
                      flip Maybe.map (distributeWorlds scrutinee worlds) <|
                        \distributedWorlds ->
                          if
                            List.length constructorDefs /= 1
                              && Dict.size distributedWorlds < 2
                          then
                            -- Uninformative match by Restriction (A)
                            NonDet.none
                          else
                            let
                              maybeSubBindSpec =
                                scrutinee
                                  |> T.bindSpec gamma
                                  |> Maybe.andThen T.subBindSpec

                              possibleBranches =
                                dontCareWorlds
                                  -- Gives preference to distributedWorlds
                                  |> Dict.union
                                       distributedWorlds
                                  |> Dict.map
                                       ( makePossibleBranch
                                           maybeSubBindSpec
                                           constructorDefs
                                       )
                                  |> Dict.values
                            in
                              NonDet.pure <|
                                Match
                                  { height =
                                      possibleBranches
                                        |> List.map (Tuple.second >> nheight)
                                        |> List.maximum
                                        |> Maybe.withDefault 0
                                        |> (+) 1
                                  , scrutinee = scrutinee
                                  , possibleBranches = possibleBranches
                                  }
    guessRefinement () =
      if isBaseType goalType then
        NonDet.pure <|
          Guess
            { sp = sp
            , guess = Nothing
            }
      else
        NonDet.none
  in
    -- Apply IRefine-Fun first
    case curryArrow goalType of
      Just (argType, returnType) ->
        partialFunctionRefinement argType returnType

      Nothing ->
        NonDet.concat
          [ constantRefinement ()
          , constructorRefinement ()
          , tupleRefinement ()
          , matchRefinement ()
          , guessRefinement ()
          ]

-- Currently does not check height; fills entire tree
fillGuesses : { maxTermSize : Int } -> RTree -> RTree
fillGuesses params rtree =
  case rtree of

    -- This is the important case, the rest is just recursive plumbing

    Guess { sp } ->
      Guess
        { sp = sp
        , guess = Just <| guessAndCheck params sp
        }

    -- Recursive plumbing from here on down

    Constant info ->
      Constant info

    Ctor ({ possibleArg } as info) ->
      Ctor
        { info
            | possibleArg =
                NonDet.map (fillGuesses params) possibleArg
        }

    Tuple ({ possibleComponents } as info) ->
      Tuple
        { info
            | possibleComponents =
                List.map (NonDet.map (fillGuesses params)) possibleComponents
        }

    Fun ({ possibleBody } as info) ->
      Fun
        { info
            | possibleBody =
                NonDet.map (fillGuesses params) possibleBody
        }

    Match ({ possibleBranches } as info) ->
      Match
        { info
            | possibleBranches =
                List.map
                  ( Tuple.mapSecond <|
                      NonDet.map (fillGuesses params)
                  )
                  possibleBranches
        }

    EarlyTermination info ->
      EarlyTermination info

propagate : RTree -> SynthesisSolution
propagate rtree =
  case rtree of
    Constant { val } ->
      NonDet.pure (val, [])

    Ctor { ctorName, possibleArg } ->
      NonDet.map
        ( Tuple.mapFirst <|
            replacePrecedingWhitespace1
              >> List.singleton
              >> eDatatype ctorName
        )
        ( NonDet.do possibleArg propagate
        )

    Tuple { possibleComponents } ->
      possibleComponents
        |> List.map (NonDet.andThen propagate)
        |> NonDet.oneOfEach
        |> NonDet.map
             ( List.unzip
                 >> Tuple.mapFirst eTuple0
                 >> Tuple.mapSecond List.concat
             )

    Fun { argName, possibleBody } ->
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
                  name == recFunctionName

                _ ->
                  False
          in
            case findFirstNode recursiveNameFinder functionBody of
              -- Recursive
              Just _ ->
                replacePrecedingWhitespace1 <|
                  eLet
                    [(recFunctionName, eFun [argNamePat] functionBody)]
                    (eVar recFunctionName)

              -- Non-recursive
              Nothing ->
                eFun0 [argNamePat] functionBody
      in
        NonDet.map
          (Tuple.mapFirst makeFunction)
          (NonDet.do possibleBody propagate)

    Match { scrutinee, possibleBranches } ->
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
        possibleBranches
          |> List.map (Tuple.mapSecond (NonDet.andThen propagate))
          |> List.map (\(p, ne) -> NonDet.map (\e -> (p, e)) ne)
          |> NonDet.oneOfEach
          |> NonDet.map makeCase

    Guess { guess } ->
      Maybe.withDefault NonDet.none guess

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

synthesize : SynthesisProblem -> SynthesisSolution
synthesize sp =
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

        rtree =
          arefine
            { maxScrutineeSize = maxScrutineeSize
            , maxMatchDepth = maxMatchDepth
            }
            sp

        solution =
          rtree
            |> (\rt -> let _ = Debug.log (List.map showTree (NonDet.toList rt) |> String.join "\n\n\n") () in rt)
            |> NonDet.map
                 ( fillGuesses { maxTermSize = maxTermSize }
                     >> propagate
                 )
            |> NonDet.join
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

solve : T.DatatypeEnv -> T.HoleEnv -> Constraints -> NonDet HoleFilling
solve =
  solve_ maxSolveDepth

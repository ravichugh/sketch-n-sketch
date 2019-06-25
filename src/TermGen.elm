module TermGen exposing
  ( freshIdent
  , functionChar
  , varChar
  , matchChar
  , TermKind(..)
  , GenCached
  , upToE
  )

import Lang exposing (..)
import Types2 as T

import NonDet exposing (NonDet)
import State exposing (State)

import Dict exposing (Dict)

import Utils

--------------------------------------------------------------------------------
-- Variable Names
--------------------------------------------------------------------------------

freshIdent : Char -> T.TypeEnv -> String
freshIdent firstChar gamma =
  let
    extractNumber : Ident -> Maybe Int
    extractNumber ident =
      case String.uncons ident of
        Just (head, rest) ->
          if head == firstChar then
            Utils.natFromString rest
          else
            Nothing

        _ ->
          Nothing

    freshNumber : Int
    freshNumber =
      gamma
        |> T.varsOfGamma
        |> List.map extractNumber
        |> Utils.filterJusts
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 1
  in
    String.cons firstChar (toString freshNumber)


functionChar : Char
functionChar =
  'f'

varChar : Char
varChar =
  'x'

matchChar : Char
matchChar =
  'y'

--------------------------------------------------------------------------------
-- Term Generation
--------------------------------------------------------------------------------

-- Relevance

type alias TypeBinding =
  (Ident, Type, Maybe T.BindingSpecification)

addTypeBinding : TypeBinding -> T.TypeEnv -> T.TypeEnv
addTypeBinding binding =
  T.addHasType (Utils.mapFst3 pVar binding)

type TermPermit
  = Must
  | May
  | Not

parts : Int -> NonDet (List TermPermit)
parts k =
  NonDet.do (NonDet.fromList (List.range 1 k)) <| \i ->
  NonDet.pure <|
    (List.repeat (i - 1) Not) ++ [Must] ++ (List.repeat (k - i) May)

-- Generation

type TermKind
  = E
  | I

type alias GenProblem =
  { termSize : Int
  , sigma : T.DatatypeEnv
  , gamma : T.TypeEnv
  , goalType : Type
  }

type alias GenInput =
  { termKind : TermKind
  , relBinding : Maybe TypeBinding
  , genProblem : GenProblem
  }

type alias GenCache =
  Dict GenInput (NonDet Exp)

type alias GenCached a =
  State GenCache a

record : GenInput -> NonDet Exp -> GenCached (NonDet Exp)
record gi exp =
  State.do State.get <| \cache ->
  State.do (State.put (Dict.insert gi exp cache)) <| \_ ->
  State.pure exp

genpI : TermPermit -> TypeBinding -> GenProblem -> GenCached (NonDet Exp)
genpI r relBinding gp =
  case r of
    Must ->
      gen
        { termKind = I
        , relBinding = Just relBinding
        , genProblem = gp
        }

    May ->
      gen
        { termKind = I
        , relBinding = Nothing
        , genProblem =
            { gp | gamma = addTypeBinding relBinding gp.gamma }
        }

    Not ->
      gen
        { termKind = I
        , relBinding = Nothing
        , genProblem = gp
        }

-- *** Important info about the gen helpers! ***
--
-- Do NOT call genE, relGenE, genI, or relGenI from anywhere EXCEPT inside
-- the actual gen function. The gen function handles caching; no other code
-- should worry about directly manipulating the cache.
--
-- So, if, for example, genE wants to recursively call itself, it should
-- actually do so indirectly via calling gen with the appropriate arguments.
--
-- ALSO: Make sure termSize > 0.

relGenE : TypeBinding -> GenProblem -> GenCached (NonDet Exp)
relGenE
 ((relName, relType, _) as relBinding)
 ({ termSize, sigma, gamma, goalType } as gp) =
  case termSize of
    1 ->
      if T.typeEquiv gamma goalType relType then
        State.pure <|
          NonDet.pure <|
            eVar relName
      else
        State.pure NonDet.none

    2 ->
      case Lang.tupleTypeArguments relType of
        Just componentTypes ->
          let
            n =
              List.length componentTypes
          in
            componentTypes
              |> Utils.zipWithIndex
              |> List.filter
                   (Tuple.first >> T.typeEquiv gamma goalType)
              -- Should be 1-indexed, so use i + 1
              |> List.map
                   ( \(_, i) ->
                       Lang.fromTupleGet (n, i + 1, eVar relName)
                   )
              |> NonDet.fromList
              |> State.pure

        Nothing ->
          State.pure NonDet.none

    -- All applications have size > 2
    _ ->
      let
        (revGoalTypeArgTypes, goalTypeReturnType) =
          T.matchArrowRecurse goalType
            |> Maybe.map (\(_, args, ret) -> (List.reverse args, ret))
            |> Maybe.withDefault ([], goalType)

        extractLastArgumentType : T.ArrowType -> Maybe Type
        extractLastArgumentType (_, argTypes, returnType) =
          if T.typeEquiv gamma goalType returnType then
            List.head (List.reverse argTypes)
          else
            let
              helper rGoalArgs rArgs =
                case (rGoalArgs, rArgs) of
                  (_, []) ->
                    Nothing

                  ([], rArgsHead :: _) ->
                    Just rArgsHead

                  (rGoalArgsHead :: rGoalArgsRest, rArgsHead :: rArgsRest) ->
                    if T.typeEquiv gamma rGoalArgsHead rArgsHead then
                      helper rGoalArgsRest rArgsRest
                    else
                      Nothing
            in
              helper revGoalTypeArgTypes (List.reverse argTypes)

        possibleArgumentType : NonDet Type
        possibleArgumentType =
          gamma
            |> T.typePairs
            |> List.map
                 ( Tuple.second
                     >> T.matchArrowRecurse
                     >> Maybe.andThen extractLastArgumentType
                 )
            |> Utils.filterJusts
            |> NonDet.fromList

        possiblePartition : NonDet (List Int)
        possiblePartition =
          NonDet.fromList <|
            Utils.partitionIntegerPermutations
              (termSize - 1) -- -1 for the application
              2

        combinedGamma : T.TypeEnv
        combinedGamma =
          addTypeBinding relBinding gamma

        appCombine : NonDet Exp -> NonDet Exp -> NonDet Exp
        appCombine possibleHead possibleArgument =
          NonDet.do possibleHead <| \head ->
          NonDet.do possibleArgument <| \argument ->
            if
              T.structurallyDecreasing combinedGamma head argument
            then
              NonDet.pure <|
                eApp0
                  head
                  [replacePrecedingWhitespace1 argument]

            else
              NonDet.none
      in
        State.map NonDet.join << State.nSequence <|
          NonDet.do possibleArgumentType <| \argumentType ->
          NonDet.do possiblePartition <| \partition ->
            case partition of
              -- Will always happen
              [kHead, kArg] ->
                let
                  headProblem : GenProblem
                  headProblem =
                    { gp
                        | termSize =
                            kHead
                        , goalType =
                            T.rebuildArrow
                              ([], [argumentType], goalType)
                    }

                  argProblem : GenProblem
                  argProblem =
                    { gp
                        | termSize =
                            kArg
                        , goalType =
                            argumentType
                    }
                in
                  NonDet.pure <|
                    State.do
                      ( gen
                          { termKind = E
                          , relBinding = Nothing
                          , genProblem = headProblem
                          }
                      ) <| \headSolution ->
                    State.do
                      ( gen
                          { termKind = E
                          , relBinding = Just relBinding
                          , genProblem = headProblem
                          }
                      ) <| \relHeadSolution ->
                    State.do
                      ( gen
                          { termKind = I
                          , relBinding = Nothing
                          , genProblem = argProblem
                          }
                      ) <| \argSolution ->
                    State.do
                      ( gen
                          { termKind = I
                          , relBinding = Just relBinding
                          , genProblem = argProblem
                          }
                      ) <| \relArgSolution ->
                    State.pure <|
                      NonDet.concat
                        [ appCombine relHeadSolution argSolution
                        , appCombine headSolution relArgSolution
                        , appCombine relHeadSolution relArgSolution
                        ]

              _ ->
                NonDet.pure <|
                  State.pure NonDet.none

relGenI : TypeBinding -> GenProblem -> GenCached (NonDet Exp)
relGenI
 ((relName, relType, _) as relBinding)
 ({ termSize, sigma, gamma, goalType } as gp) =
  let
    fixOption =
      case T.matchArrow goalType of
        Just (_, [argType], returnType) ->
          let
            recFunctionName : Ident
            recFunctionName =
              freshIdent functionChar gamma

            recFunctionNamePat : Pat
            recFunctionNamePat =
              pVar0 recFunctionName

            argName : Ident
            argName =
              freshIdent varChar gamma

            functionType : Type
            functionType =
              tFun0 argType returnType

            argNamePat : Pat
            argNamePat =
              pVar0 argName

            newGamma : T.TypeEnv
            newGamma =
              gamma
                |> T.addHasType
                     ( recFunctionNamePat
                     , functionType
                     , Just T.Rec
                     )
                |> T.addHasType
                     ( argNamePat
                     , argType
                     , Just (T.Arg recFunctionNamePat)
                     )

            possibleFunctionBody : GenCached (NonDet Exp)
            possibleFunctionBody =
              gen
                { termKind = I
                , relBinding = Just relBinding
                , genProblem =
                    { gp
                          -- -1 for lambda abstraction
                        | termSize =
                            termSize - 1
                        , gamma =
                            newGamma
                        , goalType =
                            returnType
                    }
                }
          in
            State.map
                (NonDet.map <| eFun0 [argNamePat])
                possibleFunctionBody

        _ ->
          State.pure NonDet.none

    eOption =
      gen
        { termKind = E
        , relBinding = Just relBinding
        , genProblem = gp
        }

    constructorOption =
      case unwrapType goalType of
        TVar _ datatypeName ->
          case Utils.maybeFind datatypeName sigma of
            Just (_, datatypeConstructors) ->
              State.map NonDet.join << State.nSequence <|
                NonDet.do (NonDet.fromList datatypeConstructors) <|
                  \(ctorName, argTypes) ->
                    NonDet.pure <|
                      -- Constructors only have single arguments
                      case argTypes of
                        [argType] ->
                          State.map
                            ( NonDet.map
                                ( replacePrecedingWhitespace1
                                    >> List.singleton
                                    >> eDatatype ctorName
                                )
                            )
                            ( gen
                                { termKind = I
                                , relBinding = Just relBinding
                                , genProblem =
                                    { gp
                                          -- -1 for constructor
                                          -- application
                                        | termSize =
                                            termSize - 1
                                        , goalType =
                                            argType
                                    }
                                }
                            )

                        _ ->
                          State.pure NonDet.none

            Nothing ->
              State.pure NonDet.none

        _ ->
          State.pure NonDet.none

    tupleOption =
      case Lang.tupleTypeArguments goalType of
        Nothing ->
          State.pure NonDet.none

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
            State.map NonDet.join << State.nSequence <|
              NonDet.do possiblePartition <| \partition ->
              NonDet.do (parts (List.length partition)) <| \part ->
                NonDet.pure <|
                  State.map
                    (NonDet.oneOfEach >> NonDet.map eTuple0) <|
                      State.sequence <|
                        ( List.map3
                            ( \tau n permit ->
                                genpI permit relBinding
                                  { gp
                                     | termSize = n
                                     , goalType = tau
                                  }
                            )
                            argGoalTypes
                            partition
                            part
                        )
  in
    [ fixOption
    , eOption
    , constructorOption
    , tupleOption
    ]
      |> State.sequence
      |> State.map (NonDet.fromList >> NonDet.join)

genE : GenProblem -> GenCached (NonDet Exp)
genE ({ termSize, sigma, gamma, goalType } as gp) =
  case T.varBindingUncons gamma of
    Just (binding, gammaRest) ->
      State.map
        (NonDet.join << NonDet.fromList) << State.sequence <|
          [ gen
              { termKind = E
              , relBinding = Just binding
              , genProblem = { gp | gamma = gammaRest }
              }
          , gen
              { termKind = E
              , relBinding = Nothing
              , genProblem = { gp | gamma = gammaRest }
              }
          ]

    Nothing ->
      State.pure NonDet.none

genI : GenProblem -> GenCached (NonDet Exp)
genI ({ termSize, sigma, gamma, goalType } as gp) =
  let
    maybeSplit =
      T.varBindingUncons gamma

    basicOption =
      case maybeSplit of
        Just (binding, gammaRest) ->
          State.map
            (NonDet.join << NonDet.fromList) << State.sequence <|
              [ gen
                  { termKind = I
                  , relBinding = Just binding
                  , genProblem = { gp | gamma = gammaRest }
                  }
              , gen
                  { termKind = I
                  , relBinding = Nothing
                  , genProblem = { gp | gamma = gammaRest }
                  }
              ]

        Nothing ->
          State.pure NonDet.none

    emptyFixOption =
      case maybeSplit of
        Just _ ->
          State.pure NonDet.none

        Nothing ->
          case T.matchArrow goalType of
            Just (_, [argType], returnType) ->
              let
                recFunctionName : Ident
                recFunctionName =
                  freshIdent functionChar gamma

                recFunctionNamePat : Pat
                recFunctionNamePat =
                  pVar0 recFunctionName

                functionType : Type
                functionType =
                  tFun0 argType returnType

                argName : Ident
                argName =
                  freshIdent varChar gamma

                argNamePat : Pat
                argNamePat =
                  pVar0 argName

                newGamma : T.TypeEnv
                newGamma =
                  -- gamma has no variable bindings
                  gamma
                    |> T.addHasType
                         ( recFunctionNamePat
                         , functionType
                         , Just T.Rec
                         )
                    |> T.addHasType
                         ( argNamePat
                         , argType
                         , Just (T.Arg recFunctionNamePat)
                         )

                possibleFunctionBody : GenCached (NonDet Exp)
                possibleFunctionBody =
                  gen
                    { termKind = I
                    , relBinding = Nothing
                    , genProblem =
                        { gp
                              -- -1 for lambda abstraction
                            | termSize =
                                termSize - 1
                            , gamma =
                                newGamma
                            , goalType =
                                returnType
                        }
                    }
              in
                State.map
                  (NonDet.map <| eFun0 [argNamePat])
                  possibleFunctionBody

            _ ->
              State.pure NonDet.none

    eOption =
      gen
        { termKind = E
        , relBinding = Nothing
        , genProblem = gp
        }

    constructorOption =
      case unwrapType goalType of
        TVar _ datatypeName ->
          case Utils.maybeFind datatypeName sigma of
            Just (_, datatypeConstructors) ->
              State.map NonDet.join << State.nSequence <|
                NonDet.do (NonDet.fromList datatypeConstructors) <|
                  \(ctorName, argTypes) ->
                    NonDet.pure <|
                      -- Constructors only have single arguments
                      case argTypes of
                        [argType] ->
                          State.map
                            ( NonDet.map
                                ( replacePrecedingWhitespace1
                                    >> List.singleton
                                    >> eDatatype ctorName
                                )
                            )
                            ( gen
                                { termKind = I
                                , relBinding = Nothing
                                , genProblem =
                                    { gp
                                          -- -1 for constructor
                                          -- application
                                        | termSize =
                                            termSize - 1
                                        , goalType =
                                            argType
                                    }
                                }
                            )

                        _ ->
                          State.pure NonDet.none

            Nothing ->
              State.pure NonDet.none

        _ ->
          State.pure NonDet.none

    tupleOption =
      case Lang.tupleTypeArguments goalType of
        Nothing ->
          State.pure NonDet.none

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
            State.map NonDet.join << State.nSequence <|
              NonDet.do possiblePartition <| \partition ->
                NonDet.pure <|
                  State.map
                    (NonDet.oneOfEach >> NonDet.map eTuple0) <|
                      State.sequence <|
                        ( List.map2
                            ( \tau k ->
                                gen
                                  { termKind = I
                                  , relBinding = Nothing
                                  , genProblem =
                                      { gp
                                         | termSize = k
                                         , goalType = tau
                                      }
                                  }
                            )
                            argGoalTypes
                            partition
                        )
  in
    [ basicOption
    , emptyFixOption
    , eOption
    , constructorOption
    , tupleOption
    ]
      |> State.sequence
      |> State.map (NonDet.fromList >> NonDet.join)

gen : GenInput -> GenCached (NonDet Exp)
gen ({ termKind, relBinding, genProblem } as genInput) =
  if genProblem.termSize == 0 then
    State.pure NonDet.none
  else
    State.do State.get <| \cache ->
      case Dict.get genInput cache of
        Just answer ->
          State.pure answer

        Nothing ->
          State.andThen (record genInput) <|
            case (termKind, relBinding) of
              (E, Just rb) ->
                relGenE rb genProblem

              (I, Just rb) ->
                relGenI rb genProblem

              (E, Nothing) ->
                genE genProblem

              (I, Nothing) ->
                genI genProblem

upToE : GenProblem -> GenCached (NonDet Exp)
upToE gp =
--  let
--    helper i =
--      if i > gp.termSize then
--        State.pure NonDet.none
--      else
--        State.do
--          ( gen
--              { termKind = E
--              , relBinding = Nothing
--              , genProblem = { gp | termSize = i }
--              }
--          ) <| \result ->
--            if NonDet.isEmpty result then
--              helper (i + 1)
--            else
--              -- State.pure result
--              helper (i + 1)
--  in
--    helper 1

  List.range 1 gp.termSize
    |> List.map
         ( \i ->
             gen
               { termKind = E
               , relBinding = Nothing
               , genProblem = { gp | termSize = i }
               }
         )
    |> State.sequence
    |> State.map (NonDet.fromList >> NonDet.join)

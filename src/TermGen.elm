module TermGen exposing
  ( freshIdent
  , functionChar
  , varChar
  , TermKind(..)
  , GenCached
  , upTo
  , gen, relGen
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

--------------------------------------------------------------------------------
-- Term Generation
--------------------------------------------------------------------------------

-- Relevance

type alias TypeBinding =
  (Ident, Type, Maybe T.BindingSpecification)

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
  { termKind : TermKind
  , termSize : Int
  , sigma : T.DatatypeEnv
  , gamma : T.TypeEnv
  , goalType : Type
  }

type alias GenCache =
  Dict GenProblem (NonDet Exp)

type alias GenCached a =
  State GenCache a

record : GenProblem -> NonDet Exp -> GenCached (NonDet Exp)
record gp x  =
  State.do State.get <| \cache ->
  -- State.do (State.put (Dict.insert gp x cache)) <| \_ ->
  State.pure x

genp : TermPermit -> TypeBinding -> GenProblem -> GenCached (NonDet Exp)
genp r relBinding gp =
  case r of
    Must ->
      relGen relBinding gp

    May ->
      gen
        { gp
            | gamma =
                T.addHasType
                  (Utils.mapFst3 pVar relBinding)
                  gp.gamma
        }

    Not ->
      gen gp

relGen : TypeBinding -> GenProblem -> GenCached (NonDet Exp)
relGen
 ((relName, relType, _) as relBinding)
 ({ termSize, sigma, gamma, goalType } as gp) =
  State.do State.get <| \cache ->
    case Dict.get gp cache of
      Just answer ->
        State.pure answer

      Nothing ->
        State.andThen (record gp) <|
          case gp.termKind of
            E ->
              case termSize of
                0 ->
                  State.pure NonDet.none

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
                    extractLastArgumentType : T.ArrowType -> Maybe Type
                    extractLastArgumentType (_, argTypes, returnType) =
                      if T.typeEquiv gamma goalType returnType then
                        List.head (List.reverse argTypes)
                      else
                        Nothing

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

                    appCombine : NonDet Exp -> NonDet Exp -> NonDet Exp
                    appCombine possibleHead possibleArgument =
                      NonDet.do possibleHead <| \head ->
                      NonDet.do possibleArgument <| \argument ->
                        if T.structurallyDecreasing gamma head argument then
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
                                    | termKind =
                                        E
                                    , termSize =
                                        kHead
                                    , goalType =
                                        T.rebuildArrow
                                          ([], [argumentType], goalType)
                                }

                              argProblem : GenProblem
                              argProblem =
                                { gp
                                    | termKind =
                                        I
                                    , termSize =
                                        kArg
                                    , goalType =
                                        argumentType
                                }
                            in
                              NonDet.pure <|
                                State.do (gen headProblem) <|
                                  \headSolution ->
                                State.do (relGen relBinding headProblem) <|
                                  \relHeadSolution ->
                                State.do (gen argProblem) <|
                                  \argSolution ->
                                State.do (relGen relBinding argProblem) <|
                                  \relArgSolution ->
                                State.pure <|
                                  NonDet.concat
                                    [ appCombine relHeadSolution argSolution
                                    , appCombine headSolution relArgSolution
                                    , appCombine relHeadSolution relArgSolution
                                    ]

                          _ ->
                            NonDet.pure <|
                              State.pure NonDet.none

            I ->
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
                          relGen relBinding
                            { gp
                                | termKind =
                                    I
                                  -- -1 for lambda abstraction
                                , termSize =
                                    termSize - 1
                                , gamma =
                                    newGamma
                                , goalType =
                                    returnType
                            }
                      in
                        State.map
                            (NonDet.map <| eFun0 [argNamePat])
                            possibleFunctionBody

                    _ ->
                      State.pure NonDet.none

                eOption =
                  relGen relBinding { gp | termKind = E }

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
                                        ( relGen relBinding
                                            { gp
                                                | termKind =
                                                    I
                                                  -- -1 for constructor
                                                  -- application
                                                , termSize =
                                                    termSize - 1
                                                , goalType =
                                                    argType
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
                                            genp permit relBinding
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

gen : GenProblem -> GenCached (NonDet Exp)
gen ({ termSize, sigma, gamma, goalType } as gp) =
  State.do State.get <| \cache ->
    case Dict.get gp cache of
      Just answer ->
        State.pure answer

      Nothing ->
        State.andThen (record gp) <|
          case gp.termKind of
            E ->
              if termSize == 0 then
                State.pure NonDet.none
              else
                case T.varBindingUncons gamma of
                  Just (binding, gammaRest) ->
                    State.map
                      (NonDet.join << NonDet.fromList) << State.sequence <|
                        [ relGen
                            binding
                            { gp | gamma = gammaRest }
                        , gen
                            { gp | gamma = gammaRest }
                        ]

                  Nothing ->
                    State.pure NonDet.none

            I ->
              if termSize == 0 then
                State.pure NonDet.none
              else
                let
                  maybeSplit =
                    T.varBindingUncons gamma

                  basicOption =
                    case maybeSplit of
                      Just (binding, gammaRest) ->
                        State.map
                          (NonDet.join << NonDet.fromList) << State.sequence <|
                            [ relGen
                                binding
                                { gp | gamma = gammaRest }
                            , gen
                                { gp | gamma = gammaRest }
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
                                  { gp
                                      | termKind =
                                          I
                                        -- -1 for lambda abstraction
                                      , termSize =
                                          termSize - 1
                                      , gamma =
                                          newGamma
                                      , goalType =
                                          returnType
                                  }
                            in
                              State.map
                                (NonDet.map <| eFun0 [argNamePat])
                                possibleFunctionBody

                          _ ->
                            State.pure NonDet.none

                  eOption =
                    gen { gp | termKind = E }

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
                                              { gp
                                                  | termKind =
                                                      I
                                                    -- -1 for constructor
                                                    -- application
                                                  , termSize =
                                                      termSize - 1
                                                  , goalType =
                                                      argType
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
                                                { gp
                                                   | termSize = k
                                                   , goalType = tau
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

upTo : GenProblem -> GenCached (NonDet Exp)
upTo gp =
  List.range 1 gp.termSize
    |> List.map (\i -> gen { gp | termSize = i })
    |> State.sequence
    |> State.map (NonDet.fromList >> NonDet.join)

module Synthesis exposing
  ( guess
  , refine
  , solve
  )

import Dict exposing (Dict)

import Example exposing (Example(..))
import UnExp exposing (UnExp(..), UnVal(..))
import UnDeclarations exposing (..)
import TriEval

import Types2 as T exposing (..)
import Lang exposing (..)

import Utils

import LeoUnparser

--------------------------------------------------------------------------------
-- Satisfaction
--------------------------------------------------------------------------------

satisfiesWorlds : List World -> Exp -> Bool
satisfiesWorlds worlds =
  let
    satisfiesWorld : World -> Exp -> Bool
    satisfiesWorld (env, ex) =
      TriEval.evalWithEnv env
        >> Result.toMaybe
        >> Maybe.andThen UnExp.asValue
        >> Maybe.map (satisfiesExample ex)
        >> Maybe.withDefault False
  in
    Utils.satisfiesAll (List.map satisfiesWorld worlds)

satisfiesExample : Example -> UnVal -> Bool
satisfiesExample ex v =
  case (ex, v) of
    (ExConstructor exIdent exArg, UVConstructor vIdent vArg) ->
      exIdent == vIdent && satisfiesExample exArg vArg

    (ExNum exN, UVNum vN) ->
      exN == vN

    (ExBool exB, UVBool vB) ->
      exB == vB

    (ExString exS, UVString vS) ->
      exS == vS

    (ExTuple exArgs, UVTuple vArgs) ->
      List.map2 satisfiesExample exArgs vArgs
        |> Utils.and

    (ExPartialFunction branches, UVFunClosure env params body) ->
      let
        paramLength =
          List.length params

        checkBranch (us, ex) =
          let
            envExtension =
              Utils.zip params us

            newEnv =
              envExtension ++ env

            lengthCondition =
              List.length us == paramLength

            evaluationCondition =
              body
                |> TriEval.evalWithEnv newEnv
                |> Result.toMaybe
                |> Maybe.andThen UnExp.asValue
                |> Maybe.map (satisfiesExample ex)
                |> Maybe.withDefault False
          in
            lengthCondition && evaluationCondition
      in
        List.map checkBranch branches
          |> Utils.and

    _ ->
      False

--------------------------------------------------------------------------------
-- Type-Directed Synthesis
--------------------------------------------------------------------------------

guess_ : Int -> T.TypeEnv -> Type -> List Exp
guess_ depth gamma tau =
  if depth == 0 then
    []
  else
    let
      typePairs =
        T.typePairs gamma

      -- EGuess-Var
      variableGuesses =
        typePairs
          |> List.filter (Tuple.second >> T.typeEquiv gamma tau)
          |> List.map (Tuple.first >> eVar0)

      -- EGuess-App
      appGuesses =
        let
          arrowMatches : ArrowType -> Bool
          arrowMatches (_, _, returnType) =
            T.typeEquiv gamma tau returnType

          guessApps : (Exp, ArrowType) -> List Exp
          guessApps (eFun, (_, argTypes, _)) =
            let
              possibleArgs =
                List.map (refine_ (depth - 1) gamma []) argTypes
            in
              List.map (eApp eFun) <|
                Utils.oneOfEach possibleArgs
        in
          typePairs
            |> List.map (Tuple.mapFirst eVar0)
            |> List.map (Tuple.mapSecond T.matchArrowRecurse)
            |> List.map Utils.liftMaybePair2
            |> Utils.filterJusts
            |> List.filter (Tuple.second >> arrowMatches)
            |> List.concatMap guessApps

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
              (\(n, i) -> Lang.fromTupleGet (n, i, eVar0 ident))
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
    in
      variableGuesses ++ appGuesses ++ tupleGuesses

guess : T.TypeEnv -> Type -> List Exp
guess =
  guess_ 5

--------------------------------------------------------------------------------
-- Type-and-Example-Directed Synthesis
--------------------------------------------------------------------------------

refine_ : Int -> T.TypeEnv -> List World -> Type -> List Exp
refine_ depth gamma worlds tau =
  if depth == 0 then
    []
  else
    let
      -- IRefine-Guess
      guessRefinement =
        List.filter
          (satisfiesWorlds worlds)
          (guess_ (depth - 1) gamma tau)

      -- IRefine-Constant
      constantRefinement =
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
          worlds
            |> List.map Tuple.second
            |> Utils.collapseEqual
            |> Maybe.andThen extractConstant
            |> Utils.maybeToList

      -- IRefine-Tuple
      tupleRefinement =
        case tupleTypeArguments tau of
          Nothing ->
            []

          Just taus ->
            let
              tupleLength : Int
              tupleLength =
                List.length taus

              extractTuple : Example -> Maybe (List Example)
              extractTuple ex =
                case ex of
                  ExTuple args ->
                    -- Check may not be necessary with example typechecking
                    if List.length args == tupleLength then
                      Just args
                    else
                      Nothing

                  _ ->
                    Nothing

              (envs, examples) =
                List.unzip worlds
            in
              examples
                |> List.map extractTuple
                |> Utils.projJusts
                |> Maybe.map
                     ( Utils.transpose
                         >> List.map (Utils.zip envs)
                         >> flip
                              (Utils.zipWith (refine_ (depth - 1) gamma))
                              taus
                         >> Utils.oneOfEach
                         >> List.map eTuple0
                     )
                |> Maybe.withDefault []

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

              extractPartialFunction :
                Example -> Maybe (List (UnExp.UnExp (), Example))
              extractPartialFunction ex =
                case ex of
                  ExPartialFunction entries ->
                    entries
                      -- TODO Only support single-argument functions for now
                      |> List.map
                           (Tuple.mapFirst List.head >> Utils.liftMaybePair1)
                      |> Utils.projJusts

                  _ ->
                    Nothing

              makeUniverse :
                UnExp.Env -> List (UnExp.UnExp (), Example) -> List World
              makeUniverse env =
                List.map (\(u, ex) -> ((argName, u) :: env, ex))

              (envs, examples) =
                List.unzip worlds

              newGamma =
                T.addHasType (argNamePat, argType) gamma
            in
              examples
                |> List.map extractPartialFunction
                |> Utils.projJusts
                |> Maybe.map
                     ( Utils.zipWith makeUniverse envs
                         >> List.concat
                         >> flip (refine_ (depth - 1) newGamma) returnType
                         >> List.map (eFun [argNamePat])
                     )
                |> Maybe.withDefault []

          _ ->
            []
    in
      List.concat
        [ guessRefinement
        , constantRefinement
        , tupleRefinement
        , partialFunctionRefinement
        ]

refine : T.TypeEnv -> List World -> Type -> List Exp
refine =
  refine_ 5

--------------------------------------------------------------------------------
-- Iterative Constraint Solving
--------------------------------------------------------------------------------

solve : T.HoleEnv -> List Constraint -> HoleFilling
solve delta constraints =
  let
    solveOne holeId worlds =
      case T.holeEnvGet holeId delta of
        Just (gamma, tau) ->
          refine gamma worlds tau

        Nothing ->
          []
  in
    constraints
      |> Utils.collect
      |> Dict.map solveOne

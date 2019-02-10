module Synthesis exposing
  ( World
  , guess
  , refine
  )

import Example exposing (Example(..))
import UnExp exposing (UnExp(..), UnVal(..))
import TriEval

import Types2 as T exposing (..)
import Lang exposing (..)

import Utils

import LeoUnparser

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

type alias World =
  (UnExp.Env, Example)

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

        checkBranch (vs, ex) =
          let
            envExtension =
              Utils.zip params vs
                |> List.map (\(x, v) -> (x, (UnExp.asExp v, ())))

            newEnv =
              envExtension ++ env

            lengthCondition =
              List.length vs == paramLength

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
          tupleArguments : Type -> Maybe (List Type)
          tupleArguments t =
            case unwrapType t of
              TRecord _ _ keyValues _ ->
                tupleEncodingUnapply keyValues
                  |> Maybe.map (List.map Tuple.second)

              _ ->
                Nothing

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
                 (Tuple.mapSecond <| tupleArguments >> Maybe.map extractGet)
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
    in
      guessRefinement ++ constantRefinement

refine : T.TypeEnv -> List World -> Type -> List Exp
refine =
  refine_ 5

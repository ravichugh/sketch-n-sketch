module Synthesis exposing
  ( World
  , guess
  , refine
  , hardCodedGamma
  )

import Example exposing (Example(..))
import UnExp exposing (UnExp(..), UnVal(..))
import TriEval

import Types2 as T exposing (..)
import Lang exposing (..)

import Utils

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

type alias World =
  (UnExp.Env, Example)

hardCodedGamma : T.TypeEnv
hardCodedGamma =
  [ HasType (pVar "n") <|
      Just <|
        withDummyTypeInfo (TNum space1)
  , HasType (pVar "b") <|
      Just <|
        withDummyTypeInfo (TBool space1)
  , HasType (pVar "s") <|
      Just <|
        withDummyTypeInfo (TString space1)
  ]

--------------------------------------------------------------------------------
-- Enumeration
--------------------------------------------------------------------------------

baseTypes : List Type
baseTypes =
  List.map (\f -> withDummyTypeInfo (f space1)) [TNum, TBool, TString]

enumerateTypes : Int -> List Type
enumerateTypes complexityCutoff =
  let
    argPossibilities =
      Utils.oneOfEach (List.repeat complexityCutoff baseTypes)

    arrows =
      List.map
        (\(args, returnType) -> T.rebuildArrow ([], args, returnType))
        (Utils.cartProd argPossibilities baseTypes)
  in
    arrows -- TODO add tuples

--------------------------------------------------------------------------------
-- Satisfaction
--------------------------------------------------------------------------------

satisfiesWorlds : List World -> Exp -> Bool
satisfiesWorlds worlds =
  let
    allSatisfy v =
      worlds
        |> List.map Tuple.second
        |> List.map (flip satisfiesExample v)
        |> Utils.and
  in
    TriEval.eval
      >> Result.toMaybe
      >> Maybe.andThen UnExp.asValue
      >> Maybe.map allSatisfy
      >> Maybe.withDefault False

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

    -- TODO Multiple arity
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

guess : T.TypeEnv -> Type -> List Exp
guess gamma tau =
  let
    -- EGuess-Var
    variableGuesses =
      let
        typePair : Ident -> Maybe (Ident, Type)
        typePair i =
          T.lookupVar gamma i
            |> Maybe.andThen (Maybe.map <| \t -> (i, t))
      in
        gamma
          |> T.varsOfGamma
          |> List.map typePair
          |> Utils.filterJusts
          |> List.filter (Tuple.second >> T.typeEquiv gamma tau)
          |> List.map (Tuple.first >> eVar0)

    -- EGuess-App
    appGuesses =
      let
        guessApps : Type -> List Exp
        guessApps tau2 =
          let
            tau2ArrTau =
              T.rebuildArrow ([], [tau2], tau)

            e1s =
              guess gamma tau2ArrTau

            e2s =
              refine gamma [] tau2ArrTau
          in
            Utils.cartProd e1s e2s
              |> List.map (\(e1, e2) -> eApp e1 [e2])

        typesToTry =
          enumerateTypes 5
      in
        List.concatMap guessApps typesToTry
  in
    variableGuesses ++ appGuesses

--------------------------------------------------------------------------------
-- Type-and-Example-Directed Synthesis
--------------------------------------------------------------------------------

refine : T.TypeEnv -> List World -> Type -> List Exp
refine gamma worlds tau =
  []

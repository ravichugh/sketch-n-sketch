module Synthesis exposing
  ( guess
  , refine
  )

import Types2 as T exposing (..)

type alias World =
  (Env, Example)

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
          []
      in
        List.concatMap guessApps typesToTry
  in
    variableGuesses ++ appGuesses

refine : T.TypeEnv -> List World -> Type -> List Exp
refine gamma worlds tau =
  []

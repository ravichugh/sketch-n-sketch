module Evaluator exposing
  ( Evaluator
  , map, andThen, succeed, fail, get, put, run
  , sequence, mapM, fromResult
  )

import Utils

-- An evaluator is stateful computation that may fail

type alias RawEvaluator s e a =
  s -> Result e (a, s)

type Evaluator s e a =
  E (RawEvaluator s e a)

unwrap : Evaluator s e a -> RawEvaluator s e a
unwrap (E evaluator) = evaluator

-- Core

map : (a -> b) -> Evaluator s e a -> Evaluator s e b
map f (E evaluator) =
  E <| \state ->
    evaluator state
      |> Result.map (\(x, newState) -> (f x, newState))

andThen : (a -> Evaluator s e b) -> Evaluator s e a -> Evaluator s e b
andThen f (E evaluator) =
  E <| \state ->
    evaluator state
      |> Result.andThen (\(x, newState) -> unwrap (f x) newState)

succeed : a -> Evaluator s e a
succeed x =
  E <| \state ->
    Ok (x, state)

fail : e -> Evaluator s e a
fail err =
  E <| \state ->
    Err err

get : Evaluator s e s
get =
  E <| \state ->
    Ok (state, state)

put : s -> Evaluator s e ()
put newState =
  E <| \state ->
    Ok ((), newState)

run : s -> Evaluator s e a -> Result e (a, s)
run state (E evaluator) =
  evaluator state

-- Library

sequence : List (Evaluator s e a) -> Evaluator s e (List a)
sequence evaluators =
  case evaluators of
    [] ->
      succeed []

    evaluator::rest ->
      evaluator |> andThen (\x ->
        map ((::) x) (sequence rest)
      )

mapM : (a -> Evaluator s e b) -> List a -> Evaluator s e (List b)
mapM f =
  sequence << List.map f

fromResult : Result e a -> Evaluator s e a
fromResult =
  Utils.handleResult fail succeed

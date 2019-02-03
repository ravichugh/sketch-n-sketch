module State exposing
  ( State
  , map, andThen, pure, get, put, run
  , sequence, mapM
  )

import Utils

type alias RawState s a =
  s -> (a, s)

type State s a =
  S (RawState s a)

unwrap : State s a -> RawState s a
unwrap (S state) = state

-- Core

map : (a -> b) -> State s a -> State s b
map f (S state) =
  S <| \s ->
    state s
      |> Tuple.mapFirst f

andThen : (a -> State s b) -> State s a -> State s b
andThen f (S state) =
  S <| \s ->
    let
      (x, newState) =
        state s
    in
      unwrap (f x) newState

pure : a -> State s a
pure x =
  S <| \s ->
    (x, s)

get : State s s
get =
  S <| \s ->
    (s, s)

put : s -> State s ()
put newState =
  S <| \s ->
    ((), newState)

run : s -> State s a -> (a, s)
run s (S state) =
  state s

-- Library

sequence : List (State s a) -> State s (List a)
sequence states =
  case states of
    [] ->
      pure []

    state::rest ->
      state |> andThen (\x ->
        map ((::) x) (sequence rest)
      )

mapM : (a -> State s b) -> List a -> State s (List b)
mapM f =
  sequence << List.map f

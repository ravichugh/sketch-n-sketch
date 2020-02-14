module Core.Sample exposing
  ( ..
  )

import Random.Generator exposing (Generator)

--------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------

trialCount : Int
trialCount =
  50

maxNat : Int
maxNat =
  5

maxNatListLength : Int
maxNatListLength =
  5

--------------------------------------------------------------------------------
-- Generic Sampling
--------------------------------------------------------------------------------

sequence : List (Generator a) -> Generator (List a)
sequence gens =
  case gens of
    [] ->
      Random.constant []

    gen :: rest ->
      Random.andThen
        (\x -> Random.map ((::) x) (sequence rest))
        gen

sampleUnique : Int -> Random.Generator a -> Random.Generator (Set a)
sampleUnique k gen =
  let
    helper acc =
      if Set.size acc == k then
        Random.constant acc
      else
        Random.andThen
          (\x -> helper (Set.insert x acc))
          gen
  in
    helper Set.empty

trial : Int -> Int -> (a -> b) -> Generator a -> Generator (Set (Set (a, b)))
trial n k ref =
  input
    |> Random.map (\x -> (x, ref x))
    |> sampleUnique k
    |> sampleUnique n

--------------------------------------------------------------------------------
-- Particular Sampling
--------------------------------------------------------------------------------

int : Generator Int
int =
  Random.int 0 maxNat

natList : Generator (List Int)
natList =
  Random.int 0 maxNatListLength
    |> Random.andThen (\len -> Random.list len int)

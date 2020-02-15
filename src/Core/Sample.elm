module Core.Sample exposing
  ( ..
  )

import Random exposing (Generator)
import Set exposing (Set)

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
-- Helpers
--------------------------------------------------------------------------------

constant : a -> Generator a
constant x =
  Random.map (\_ -> x) Random.bool

sequence : List (Generator a) -> Generator (List a)
sequence gens =
  case gens of
    [] ->
      constant []

    gen :: rest ->
      Random.andThen
        (\x -> Random.map ((::) x) (sequence rest))
        gen

--------------------------------------------------------------------------------
-- Generic Sampling
--------------------------------------------------------------------------------

sampleUnique : Int -> Random.Generator a -> Random.Generator (Set a)
sampleUnique k gen =
  let
    helper acc =
      if Set.size acc == k then
        constant acc
      else
        Random.andThen
          (\x -> helper (Set.insert x acc))
          gen
  in
    helper Set.empty

trial : Int -> Int -> (a -> b) -> Generator a -> Generator (Set (Set (a, b)))
trial n k ref =
  Random.map (\x -> (x, ref x))
    >> sampleUnique k
    >> sampleUnique n

--------------------------------------------------------------------------------
-- Particular Sampling
--------------------------------------------------------------------------------

nat : Generator Int
nat =
  Random.int 0 maxNat

natList : Generator (List Int)
natList =
  Random.int 0 maxNatListLength
    |> Random.andThen (\len -> Random.list len nat)

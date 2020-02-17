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
  1

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

fold : (a -> b -> Generator b) -> b -> List a -> Generator b
fold f baseAcc =
  List.foldl
    (\x -> Random.andThen (f x))
    (constant baseAcc)

--------------------------------------------------------------------------------
-- Generic Sampling
--------------------------------------------------------------------------------

sampleUnique : List (Int, Generator a) -> Generator (Set a)
sampleUnique =
  let
    helper (total, gen) acc =
      if Set.size acc >= total then
        constant acc
      else
        Random.andThen
          (\x -> helper (total, gen) (Set.insert x acc))
          gen
  in
    fold helper Set.empty

io : (a -> b) -> Generator a -> Generator (a, b)
io f =
  Random.map (\x -> (x, f x))

trial :
  Int -> Int -> (a -> b) -> Generator a -> Generator a
    -> Generator (Set (Set (a, b)))
trial n k ref input baseCase =
  sampleUnique
    [ (n, sampleUnique [(1, io ref baseCase), (k, io ref input)])
    ]

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

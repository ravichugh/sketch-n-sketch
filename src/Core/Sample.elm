module Core.Sample exposing
  ( ..
  )

import MyRandom as Random exposing (Generator)
import Set exposing (Set)

--------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------

trialCount : Int
trialCount =
  4

maxNat : Int
maxNat =
  3

maxListLength : Int
maxListLength =
  5

--------------------------------------------------------------------------------
-- Enumeration Sampling
--------------------------------------------------------------------------------

-- Generic

weight : Int -> Int -> Float
weight elementSize size =
  toFloat <|
    elementSize ^ size

all : a -> (Int -> List a) -> Int -> Int -> ((Float, a), List (Float, a))
all base shapes elementSize maxSize =
  ( (1, base)
  , List.concatMap
      ( \size ->
          size
            |> shapes
            |> List.map (\shape -> (weight elementSize size, shape))
      )
      (List.range 1 maxSize)
  )

-- Semi-Generic

type ListShape
  = Nil
  | Cons ListShape

listBase : ListShape
listBase =
  Nil

listShapes : Int -> List ListShape
listShapes n =
  if n == 0 then
    [listBase]
  else
    List.map Cons (listShapes (n - 1))

listFill : a -> ListShape -> List a
listFill x shape =
  case shape of
    Nil ->
      []

    Cons rest ->
      x :: listFill x rest

list : Int -> Generator a -> Generator (List a)
list elementSize elementGen =
  all listBase listShapes elementSize maxListLength
    |> uncurry Random.weighted
    |> Random.map (listFill elementGen)
    |> Random.andThen Random.sequence

-- Particular

nat : Generator Int
nat =
  Random.int 0 maxNat

bool : Generator Bool
bool =
  Random.bool

natList : Generator (List Int)
natList =
  list (maxNat + 1) nat

boolList : Generator (List Bool)
boolList =
  list 2 bool

--------------------------------------------------------------------------------
-- IO Sampling
--------------------------------------------------------------------------------

io : (a -> b) -> Generator a -> Generator (a, b)
io f =
  Random.map (\x -> (x, f x))

trial :
  Int -> Int -> (a -> b) -> Generator a -> Maybe (Generator a)
    -> Generator (List (Set (a, b)))
trial n k ref input maybeBaseCase =
  let
    amounts =
      ( case maybeBaseCase of
          Nothing ->
            []

          Just baseCase ->
            [ (1, io ref baseCase)
            ]
      ) ++
      [ (k, io ref input)
      ]
  in
    amounts
      |> Random.sampleUnique
      |> Random.list n

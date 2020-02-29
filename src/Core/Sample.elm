module Core.Sample exposing
  ( ..
  )

import MyRandom as Random exposing (Generator)
import Set exposing (Set)

import Utils

import Tree exposing (Tree)

--------------------------------------------------------------------------------
-- Parameters
--------------------------------------------------------------------------------

trialCount : Int
trialCount =
  50

maxNat : Int
maxNat =
  3

maxListLength : Int
maxListLength =
  4

maxInnerListLength : Int
maxInnerListLength =
  2

maxTreeSize : Int
maxTreeSize =
  4

--------------------------------------------------------------------------------
-- Enumeration Sampling
--------------------------------------------------------------------------------

-- Generic

weight : Int -> Int -> Float
weight elementSize size =
  1.0
  -- toFloat <|
  --   elementSize ^ size

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

nestedList : Int -> Generator a -> Generator (List (List a))
nestedList elementSize elementGen =
  let
    innerListSize =
      List.range 0 maxInnerListLength
        |> List.map (\len -> elementSize ^ len)
        |> List.sum
  in
    all listBase listShapes innerListSize maxListLength
      |> uncurry Random.weighted
      |> Random.map (listFill (list elementSize elementGen))
      |> Random.andThen Random.sequence

type TreeShape
  = Leaf
  | Node TreeShape TreeShape

treeBase : TreeShape
treeBase =
  Leaf

treeShapes : Int -> List TreeShape
treeShapes n =
  if n == 0 then
    [treeBase]
  else
    let
      sub =
        treeShapes (n - 1)

      subs =
        Utils.cartProd sub sub
    in
      List.map
        (\(left, right) -> Node left right)
        subs

treeFill : a -> TreeShape -> Tree a
treeFill x t =
  case t of
    Leaf ->
      Tree.Leaf

    Node left right ->
      Tree.Node (treeFill x left) x (treeFill x right)

tree : Int -> Generator a -> Generator (Tree a)
tree elementSize elementGen =
  all treeBase treeShapes elementSize maxTreeSize
    |> uncurry Random.weighted
    |> Random.map (treeFill elementGen)
    |> Random.andThen Random.sequenceTree

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

nestedNatList : Generator (List (List Int))
nestedNatList =
  nestedList (maxNat + 1) nat

boolList : Generator (List Bool)
boolList =
  list 2 bool

natTree : Generator (Tree Int)
natTree =
  tree (maxNat + 1) nat

boolTree : Generator (Tree Bool)
boolTree =
  tree 2 bool

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

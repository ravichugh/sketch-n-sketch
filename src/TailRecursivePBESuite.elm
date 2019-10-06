module TailRecursivePBESuite exposing (suite)

import Dict exposing (Dict)

list_fold = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type Boolean
  = F ()
  | T ()

type NatList
  = Nil ()
  | Cons (Nat, NatList)

let
  sum : Nat -> Nat -> Nat
  sum n1 n2 =
    case n1 of
      Z _ -> n2
      S m -> S (sum m n2)
in
let
  isOdd : Nat -> Boolean
  isOdd n =
    case n of
      Z _  -> F ()
      S m1 ->
        case m1 of
          Z _  -> T ()
          S m2 -> isOdd m2
in
let
  countOdd : Nat -> Nat -> Nat
  countOdd n1 n2 =
    case isOdd n2 of
      T _ -> S n1
      F _ -> n1
in
let
  listFold : (Nat -> Nat -> Nat) -> Nat -> NatList -> Nat
  listFold f acc =
    let
      fixListFold : NatList -> Nat
      fixListFold xs =
        case xs of
          Nil _ -> acc
          Cons p -> ?? : Nat
    in
      fixListFold
in

"""
  """specifyFunction3 listFold
  [ (sum, 0, [], 0)
  , (sum, 0, [1], 1)
  , (sum, 0, [2, 1], 3)
  , (sum, 0, [3, 2, 1], 6)
  , (sum, 1, [], 1)
  , (countOdd, 0, [], 0)
  , (countOdd, 0, [1], 1)
  , (countOdd, 0, [2, 1], 1)
  , (countOdd, 0, [3, 2, 1], 2)
  ]""" 9
  """specifyFunction3 listFold
  [ -- (sum, 0, [], 0)
    (sum, 0, [1], 1)
  , (sum, 0, [2, 1], 3)
  -- , (sum, 0, [3, 2, 1], 6)
  -- , (sum, 1, [], 1)
  -- , (countOdd, 0, [], 0)
  -- , (countOdd, 0, [1], 1)
  -- , (countOdd, 0, [2, 1], 1)
  -- , (countOdd, 0, [3, 2, 1], 2)
  ]""" 2

list_rev_tailcall = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listRevTailcall : NatList -> NatList -> NatList
listRevTailcall xs acc =
  case xs of
    Nil _ -> acc
    Cons p -> ?? : NatList

"""
  """specifyFunction2 listRevTailcall
  [ ([], [], [])
  , ([], [0], [0])
  , ([], [1], [1])
  , ([], [1, 0], [1, 0])
  , ([0], [], [0])
  , ([1], [], [1])
  , ([1], [0], [1, 0])
  , ([0, 1], [], [1, 0])
  ]""" 8
  """specifyFunction2 listRevTailcall
  [ -- ([], [], [])
  -- , ([], [0], [0])
  -- , ([], [1], [1])
  -- , ([], [1, 0], [1, 0])
  -- , ([0], [], [0])
  -- , ([1], [], [1])
  -- , ([1], [0], [1, 0])
    ([0, 1], [], [1, 0])
  ]""" 1

suite : Dict String (String, String, Int, String, Int)
suite =
  Dict.fromList
    [ ("list_fold", list_fold)
    , ("list_rev_tailcall", list_rev_tailcall)
    ]

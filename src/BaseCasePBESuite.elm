module BaseCasePBESuite exposing (suite)

import Dict exposing (Dict)

list_append = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

append : NatList -> NatList -> NatList
append xs ys =
  case xs of
    Nil _ ->
      ys

    Cons p ->
      ?? : NatList

"""
  """specifyFunction2 append
  [ ([], [], [])
  , ([], [0], [0])
  , ([0], [], [0])
  , ([0], [0], [0, 0])
  , ([1, 0], [], [1, 0])
  , ([1, 0], [0], [1, 0, 0])
  ]""" 6
  """specifyFunction2 append
  [ -- ([], [], [])
  -- , ([], [0], [0])
  -- , ([0], [], [0])
  -- , ([0], [0], [0, 0])
  , ([1, 0], [], [1, 0])
  -- , ([1, 0], [0], [1, 0, 0])
  ]""" 1

list_compress = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type Cmp
  = LT ()
  | EQ ()
  | GT ()

let
  compare : Nat -> Nat -> Cmp
  compare n1 n2 =
    case n1 of
      Z _ ->
        case n2 of
          Z _ -> EQ ()
          S _ -> LT ()
      S m1 ->
        case n2 of
          Z _  -> GT ()
          S m2 -> compare m1 m2
in
let
  compress : NatList -> NatList
  compress xs = ??
in

"""
  """specifyFunction compress
  [ ([], [])
  , ([0], [0])
  , ([1], [1])
  , ([0,0], [0])
  , ([1,1], [1])
  , ([2,0], [2,0])
  , ([1,0,0], [1,0])
  , ([0,1,1], [0,1])
  , ([2,1,0,0], [2,1,0])
  , ([2,2,1,0,0], [2,1,0])
  , ([2,2,0], [2,0])
  , ([2,2,2,0], [2,0])
  , ([1,2,2,2,0], [1,2,0])
  ]""" 13
  """specifyFunction2 compress
  [ ([], [])
  , ([0], [0])
  , ([1], [1])
  , ([0,0], [0])
  , ([1,1], [1])
  , ([2,0], [2,0])
  , ([1,0,0], [1,0])
  , ([0,1,1], [0,1])
  , ([2,1,0,0], [2,1,0])
  , ([2,2,1,0,0], [2,1,0])
  , ([2,2,0], [2,0])
  , ([2,2,2,0], [2,0])
  , ([1,2,2,2,0], [1,2,0])
  ]""" (-1)

list_concat = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type NatListList
  = LNil ()
  | LCons (NatList, NatListList)

let
  append : NatList -> NatList -> NatList
  append l1 l2 =
    case l1 of
      Nil _ ->
        l2
      Cons p ->
        Cons (get_2_1 p, append (get_2_2 p) l2)
in
let
  concat : NatListList -> NatList
  concat xss =
    case xss of
      LNil _ ->
        Nil ()

      LCons p ->
        ?? : NatList
in

"""
  """specifyFunction concat
  [ (LNil (), [])
  , (LCons ([], LNil ()), [])
  , (LCons ([0], LNil ()), [0])
  , (LCons ([0], LCons([0], LNil ())), [0,0])
  , (LCons ([1], LNil ()), [1])
  , (LCons ([1], LCons([1], LNil ())), [1,1])
  ]""" 6
  """specifyFunction concat
  [ -- (LNil (), [])
  , (LCons ([], LNil ()), [])
  , (LCons ([0], LNil ()), [0])
  -- , (LCons ([0], LCons([0], LNil ())), [0,0])
  -- , (LCons ([1], LNil ()), [1])
  -- , (LCons ([1], LCons([1], LNil ())), [1,1])
  ]""" 2

list_drop = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listDrop : NatList -> Nat -> NatList
listDrop xs n =
  case n of
    Z _ ->
      xs

    S n_ ->
      ?? : NatList

"""
  """specifyFunction2 listDrop
  [ ([], 0, [])
  , ([], 1, [])
  , ([0], 0, [0])
  , ([0], 1, [])
  , ([1], 0, [1])
  , ([1], 1, [])
  , ([1, 0], 0, [1, 0])
  , ([1, 0], 1, [0])
  , ([0, 1], 0, [0, 1])
  , ([0, 1], 1, [1])
  , ([0, 1], 2, [])
  ]""" 11
  """specifyFunction2 listDrop
  [ -- ([], 0, [])
    ([], 1, [])
  -- , ([0], 0, [0])
  -- , ([0], 1, [])
  -- , ([1], 0, [1])
  -- , ([1], 1, [])
  -- , ([1, 0], 0, [1, 0])
  , ([1, 0], 1, [0])
  -- , ([0, 1], 0, [0, 1])
  -- , ([0, 1], 1, [1])
  -- , ([0, 1], 2, [])
  ]""" 2

list_even_parity = (,,,,)
  """type Boolean
  = T ()
  | F ()

type BooleanList
  = Nil ()
  | Cons (Boolean, BooleanList)

evenParity : BooleanList -> Boolean
evenParity xs = ??

"""
  """specifyFunction evenParity
  [ ([], T ())
  , ([F ()], T ())
  , ([T ()], F ())
  , ([F (), F ()], T ())
  , ([F (), T ()], F ())
  , ([T (), F ()], F ())
  , ([T (), T ()], T ())
  ]""" 7
  """specifyFunction evenParity
  [ ([], T ())
  , ([F ()], T ())
  , ([T ()], F ())
  , ([F (), F ()], T ())
  -- , ([F (), T ()], F ())
  -- , ([T (), F ()], F ())
  , ([T (), T ()], T ())
  ]""" 5

list_filter = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type Boolean
  = F ()
  | T ()

let
  isEven : Nat -> Boolean
  isEven n =
    case n of
      Z _  -> T ()
      S m1 ->
        case m1 of
          Z _  -> F ()
          S m2 -> isEven m2

  isNonzero : Nat -> Boolean
  isNonzero n =
    case n of
      Z _ -> F ()
      S _ -> T ()
in
let
  listFilter : (Nat -> Boolean) -> NatList -> NatList
  listFilter predicate =
    let
      fixListFilter : NatList -> NatList
      fixListFilter xs = ??
    in
      fixListFilter
in

"""
  """specifyFunction2 listFilter
  [ (isEven, [], [])
  , (isEven, [0], [0])
  , (isEven, [1], [])
  , (isEven, [2], [2])
  , (isEven, [0, 0], [0, 0])
  , (isEven, [0, 1], [0])
  , (isNonzero, [], [])
  , (isNonzero, [0], [])
  ]""" 8
  """specifyFunction2 listFilter
  [ (isEven, [], [])
  -- , (isEven, [0], [0])
  -- , (isEven, [1], [])
  , (isEven, [2], [2])
  -- , (isEven, [0, 0], [0, 0])
  , (isEven, [0, 1], [0])
  -- , (isNonzero, [], [])
  , (isNonzero, [0], [])
  ]""" 4

list_last = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type NatOpt
  = None ()
  | Some Nat

listLast : NatList -> NatOpt
listLast xs =
  case xs of
    Nil _ ->
      None ()

    Cons p ->
      ?? : NatOpt

"""
  """specifyFunction listLast
  [ ([], None ())
  , ([1], Some 1)
  , ([2], Some 2)
  , ([2, 1], Some 1)
  , ([1, 2], Some 2)
  , ([3, 2, 1], Some 1)
  ]""" 6
  """specifyFunction listLast
  [ -- ([], None ())
  -- , ([1], Some 1)
    ([2], Some 2)
  -- , ([2, 1], Some 1)
  -- , ([1, 2], Some 2)
  , ([3, 2, 1], Some 1)
  ]""" 2

list_length = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listLength : NatList -> Nat
listLength xs =
  case xs of
    Nil _ ->
      Z ()

    Cons p ->
      ?? : Nat

"""
  """specifyFunction listLength
  [ ([], 0)
  , ([0], 1)
  , ([0, 0], 2)
  ]""" 3
  """specifyFunction listLength
  [ -- ([], 0)
    ([0], 1)
  -- , ([0, 0], 2)
  ]""" 1

list_map = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

let
  zero : Nat -> Nat
  zero n = Z ()

  inc : Nat -> Nat
  inc n = S n
in
let
  listMap : (Nat -> Nat) -> NatList -> NatList
  listMap f =
    let
      listMapFix : NatList -> NatList
      listMapFix xs =
        case xs of
          Nil _ ->
            Nil ()

          Cons p ->
            ?? : NatList
    in
      listMapFix
in

"""
  """specifyFunction2 listMap
  [ (inc, [], [])
  , (inc, [0], [1])
  , (inc, [0, 0], [1, 1])
  , (inc, [1], [2])
  , (inc, [1, 1], [2, 2])
  , (zero, [], [])
  , (zero, [0], [0])
  , (zero, [0, 0], [0, 0])
  ]""" 8
  """specifyFunction2 listMap
  [ -- (inc, [], [])
    (inc, [0], [1])
  -- , (inc, [0, 0], [1, 1])
  , (inc, [1], [2])
  -- , (inc, [1, 1], [2, 2])
  -- , (zero, [], [])
  -- , (zero, [0], [0])
  -- , (zero, [0, 0], [0, 0])
  ]""" 2

list_nth = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listNth : NatList -> Nat -> Nat
listNth xs n =
  case n of
    Z _ ->
      case xs of
        Nil _ ->
          Z ()

        Cons p ->
          get_2_1 p

    S n_ ->
      ?? : Nat

"""
  """specifyFunction2 listNth
  [ ([], 0, 0)
  , ([], 1, 0)
  , ([2], 0, 2)
  , ([2], 1, 0)
  , ([1, 2], 0, 1)
  , ([1, 2], 1, 2)
  , ([1], 0, 1)
  , ([1], 1, 0)
  , ([2, 1], 0, 2)
  , ([2, 1], 1, 1)
  , ([3, 2, 1], 0, 3)
  , ([3, 2, 1], 1, 2)
  , ([3, 2, 1], 2, 1)
  ]""" 13
  """specifyFunction2 listNth
  [ -- ([], 0, 0)
  , ([], 1, 0)
  -- , ([2], 0, 2)
  -- , ([2], 1, 0)
  -- , ([1, 2], 0, 1)
  -- , ([1, 2], 1, 2)
  -- , ([1], 0, 1)
  -- , ([1], 1, 0)
  -- , ([2, 1], 0, 2)
  -- , ([2, 1], 1, 1)
  -- , ([3, 2, 1], 0, 3)
  , ([3, 2, 1], 1, 2)
  -- , ([3, 2, 1], 2, 1)
  ]""" 2

list_pairwise_swap = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listPairwiseSwap : NatList -> NatList
listPairwiseSwap xs = ??

"""
  """specifyFunction listPairwiseSwap
  [ ([], [])
  , ([0], [])
  , ([1], [])
  , ([0, 1], [1, 0])
  , ([1, 0], [0, 1])
  , ([1, 0, 1], [])
  , ([0, 1, 0, 1], [1, 0, 1, 0])
  ]""" 7
  """specifyFunction listPairwiseSwap
  [ ([], [])
  , ([0], [])
  , ([1], [])
  , ([0, 1], [1, 0])
  , ([1, 0], [0, 1])
  , ([1, 0, 1], [])
  , ([0, 1, 0, 1], [1, 0, 1, 0])
  ]""" (-1)

list_rev_append = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

let
  append : NatList -> NatList -> NatList
  append l1 l2 =
    case l1 of
      Nil _ ->
        l2
      Cons p ->
        Cons (get_2_1 p, append (get_2_2 p) l2)
in
let
  listRevAppend : NatList -> NatList
  listRevAppend xs =
    case xs of
      Nil _ ->
        Nil ()

      Cons p ->
        ?? : NatList
in

"""
  """specifyFunction listRevAppend
  [ ([], [])
  , ([0], [0])
  , ([1], [1])
  , ([0, 1], [1, 0])
  , ([0, 0, 1], [1, 0, 0])
  ]""" 5
  """specifyFunction listRevAppend
  [ -- ([], [])
    ([0], [0])
  -- , ([1], [1])
  -- , ([0, 1], [1, 0])
  , ([0, 0, 1], [1, 0, 0])
  ]""" 2

list_rev_snoc = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

let
  snoc : NatList -> Nat -> NatList
  snoc xs n =
    case xs of
      Nil _ -> Cons (n, Nil ())
      Cons p -> Cons (get_2_1 p, snoc (get_2_2 p) n)
in
let
  listRevSnoc : NatList -> NatList
  listRevSnoc xs =
    case xs of
      Nil _ ->
        Nil ()

      Cons p ->
        ?? : natList
in

"""
  """specifyFunction listRevSnoc
  [ ([], [])
  , ([0], [0])
  , ([1], [1])
  , ([0, 1], [1, 0])
  , ([0, 0, 1], [1, 0, 0])
  ]""" 5
  """specifyFunction listRevSnoc
  [ --([], [])
  -- , ([0], [0])
  -- , ([1], [1])
    ([0, 1], [1, 0])
  -- , ([0, 0, 1], [1, 0, 0])
  ]""" 1

list_snoc = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listSnoc : NatList -> Nat -> NatList
listSnoc xs n =
  case xs of
    Nil _ ->
      Cons (n, Nil ())

    Cons p ->
      ?? : NatList

"""
  """specifyFunction2 listSnoc
  [ ([], 0, [0])
  , ([], 1, [1])
  , ([0], 0, [0, 0])
  , ([0], 1, [0, 1])
  , ([1, 0], 0, [1, 0, 0])
  , ([1, 0], 1, [1, 0, 1])
  , ([2, 1, 0], 0, [2, 1, 0, 0])
  , ([2, 1, 0], 1, [2, 1, 0, 1])
  ]""" 8
  """specifyFunction2 listSnoc
  [ -- ([], 0, [0])
  -- , ([], 1, [1])
  , ([0], 0, [0, 0])
  , ([0], 1, [0, 1])
  -- , ([1, 0], 0, [1, 0, 0])
  -- , ([1, 0], 1, [1, 0, 1])
  -- , ([2, 1, 0], 0, [2, 1, 0, 0])
  -- , ([2, 1, 0], 1, [2, 1, 0, 1])
  ]""" 2

list_sort_sorted_insert = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type Cmp
  = LT ()
  | EQ ()
  | GT ()

let
  compare : Nat -> Nat -> Cmp
  compare n1 n2 =
    case n1 of
      Z _ ->
        case n2 of
          Z _ -> EQ ()
          S _ -> LT ()
      S m1 ->
        case n2 of
          Z _  -> GT ()
          S m2 -> compare m1 m2

  insert : NatList -> Nat -> NatList
  insert xs n =
    case xs of
      Nil _ -> Cons (n, Nil ())
      Cons p ->
        case compare n (get_2_1 p) of
          LT _ -> Cons (n, Cons (get_2_1 p, get_2_2 p))
          EQ _ -> xs
          GT _ -> Cons (get_2_1 p, insert (get_2_2 p) n)
in
let
  listSortSortedInsert : NatList -> NatList
  listSortSortedInsert xs =
    case xs of
      Nil _ ->
        Nil ()

      Cons p ->
        ?? : NatList
in

"""
  """specifyFunction listSortSortedInsert
  [ ([], [])
  , ([0], [0])
  , ([1], [1])
  , ([0, 0], [0])
  , ([1, 0], [0, 1])
  , ([1, 1], [1])
  , ([0, 1, 1], [0, 1])
  ]""" 7
  """specifyFunction listSortSortedInsert
  [ -- ([], [])
  -- , ([0], [0])
  -- , ([1], [1])
  -- , ([0, 0], [0])
    ([1, 0], [0, 1])
  -- , ([1, 1], [1])
  -- , ([0, 1, 1], [0, 1])
  ]""" 1

list_sorted_insert = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type Cmp
  = LT ()
  | EQ ()
  | GT ()

let
  compare : Nat -> Nat -> Cmp
  compare n1 n2 =
    case n1 of
      Z _ ->
        case n2 of
          Z _ -> EQ ()
          S _ -> LT ()
      S m1 ->
        case n2 of
          Z _  -> GT ()
          S m2 -> compare m1 m2
in
let
  listSortedInsert : NatList -> Nat -> NatList
  listSortedInsert xs n = ??
in

"""
  """specifyFunction2 listSortedInsert
  [ ([], 0, [0])
  , ([], 1, [1])
  , ([], 2, [2])
  , ([0], 0, [0])
  , ([0], 1, [0, 1])
  , ([1], 0, [0, 1])
  , ([1], 1, [1])
  , ([1], 2, [1, 2])
  , ([2], 0, [0, 2])
  , ([2], 1, [1, 2])
  , ([0, 1], 0, [0, 1])
  , ([0, 1], 2, [0, 1, 2])
  ]""" 12
  """specifyFunction2 listSortedInsert
  [ ([], 0, [0])
  -- , ([], 1, [1])
  -- , ([], 2, [2])
  -- , ([0], 0, [0])
  -- , ([0], 1, [0, 1])
  -- , ([1], 0, [0, 1])
  , ([1], 1, [1])
  , ([1], 2, [1, 2])
  , ([2], 0, [0, 2])
  , ([2], 1, [1, 2])
  , ([0, 1], 0, [0, 1])
  , ([0, 1], 2, [0, 1, 2])
  ]""" 7

list_stutter = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listStutter : NatList -> NatList
listStutter xs =
  case xs of
    Nil _ ->
      Nil ()

    Cons p ->
      ?? : NatList

"""
  """specifyFunction listStutter
  [ ([], [])
  , ([0], [0, 0])
  , ([1, 0], [1, 1, 0, 0])
  ]""" 3
  """specifyFunction listStutter
  [ -- ([], [])
  -- , ([0], [0, 0])
    ([1, 0], [1, 1, 0, 0])
  ]""" 1

list_take = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listTake : Nat -> NatList -> NatList
listTake n xs =
  case n of
    Z _ ->
      Nil ()

    S n_ ->
      ?? : NatList

"""
  """specifyFunction2 listTake
  [ (0, [], [])
  , (0, [1], [])
  , (0, [0, 1], [])
  , (0, [1, 0, 1], [])
  , (1, [], [])
  , (1, [1], [1])
  , (1, [0, 1], [0])
  , (1, [1, 0, 1], [1])
  , (2, [], [])
  , (2, [1], [1])
  , (2, [0, 1], [0, 1])
  , (2, [1, 0, 1], [1, 0])
  ]""" 12
  """specifyFunction2 listTake
  [ -- (0, [], [])
  -- , (0, [1], [])
  -- , (0, [0, 1], [])
  -- , (0, [1, 0, 1], [])
    (1, [], [])
  , (1, [1], [1])
  , (1, [0, 1], [0])
  -- , (1, [1, 0, 1], [1])
  -- , (2, [], [])
  -- , (2, [1], [1])
  -- , (2, [0, 1], [0, 1])
  -- , (2, [1, 0, 1], [1, 0])
  ]""" 3

nat_iseven = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type Boolean
  = F ()
  | T ()

isEven : Nat -> Boolean
isEven n =
  case n of
    Z _ ->
      T ()

    S n_ ->
      ?? : Boolean

"""
  """specifyFunction isEven
  [ (0, T ())
  , (1, F ())
  , (2, T ())
  , (3, F ())
  ]""" 4
  """specifyFunction isEven
  [ -- (0, T ())
    (1, F ())
  , (2, T ())
  -- , (3, F ())
  ]""" 2

nat_max = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type Boolean
  = F ()
  | T ()

type Cmp
  = LT ()
  | EQ ()
  | GT ()

let
  compare : Nat -> Nat -> Cmp
  compare n1 n2 =
    case n1 of
      Z _ ->
        case n2 of
          Z _ -> EQ ()
          S _ -> LT ()
      S m1 ->
        case n2 of
          Z _  -> GT ()
          S m2 -> compare m1 m2
in
let
  natMax : Nat -> Nat -> Nat
  natMax m n =
    case m of
      Z _ ->
        n

      S m_ ->
        ?? : Nat
in

"""
  """specifyFunction2 natMax
  [ (0, 0, 0)
  , (0, 1, 1)
  , (0, 2, 2)
  , (1, 0, 1)
  , (1, 1, 1)
  , (1, 2, 2)
  , (2, 0, 2)
  , (2, 1, 2)
  , (2, 2, 2)
  ]""" 9
  """specifyFunction2 natMax
  [ -- (0, 0, 0)
  -- , (0, 1, 1)
  -- , (0, 2, 2)
  , (1, 0, 1)
  , (1, 1, 1)
  , (1, 2, 2)
  -- , (2, 0, 2)
  -- , (2, 1, 2)
  -- , (2, 2, 2)
  ]""" 3

nat_add = (,,,,)
  """type Nat
  = Z ()
  | S Nat

natAdd : Nat -> Nat -> Nat
natAdd m n =
  case m of
    Z _ ->
      n

    S m_ ->
      ?? : Nat

"""
  """specifyFunction2 natAdd
  [ (0, 0, 0)
  , (0, 1, 1)
  , (0, 2, 2)
  , (1, 0, 1)
  , (1, 1, 2)
  , (1, 2, 3)
  , (2, 0, 2)
  , (2, 1, 3)
  , (2, 2, 4)
  ]""" 9
  """specifyFunction2 natAdd
  [ -- (0, 0, 0)
  -- , (0, 1, 1)
  -- , (0, 2, 2)
    (1, 0, 1)
  -- , (1, 1, 2)
  -- , (1, 2, 3)
  -- , (2, 0, 2)
  -- , (2, 1, 3)
  -- , (2, 2, 4)
  ]""" 1

tree_binsert = (,,,,)
  """type Cmp
  = LT ()
  | EQ ()
  | GT ()

type Nat
= Z ()
| S Nat

type NatTree
  = Leaf ()
  | Node (NatTree, Nat, NatTree)

let
  compare : Nat -> Nat -> Cmp
  compare n1 n2 =
    case n1 of
      Z _ ->
        case n2 of
          Z _ -> EQ ()
          S _ -> LT ()
      S m1 ->
        case n2 of
          Z _  -> GT ()
          S m2 -> compare m1 m2
in
let
  treeBInsert : NatTree -> Nat -> NatTree
  treeBInsert t n = ??
in

"""
  """specifyFunction2 treeBInsert
  [ (Leaf (), 0, Node (Leaf (), 0, Leaf ()))
  , (Leaf (), 1, Node (Leaf (), 1, Leaf ()))
  , (Leaf (), 2, Node (Leaf (), 2, Leaf ()))
  , (Node (Leaf (), 1, Leaf ()), 0, Node (Node (Leaf (), 0, Leaf ()), 1, Leaf ()))
  , (Node (Leaf (), 1, Leaf ()), 1, Node (Leaf (), 1, Leaf ()))
  , (Node (Leaf (), 1, Leaf ()), 2, Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())))
  , (Node (Leaf (), 0, Leaf ()), 0, Node (Leaf (), 0, Leaf ()))
  , (Node (Leaf (), 0, Leaf ()), 1, Node (Leaf (), 0, Node (Leaf (), 1, Leaf ())))
  , (Node (Leaf (), 0, Leaf ()), 2, Node (Leaf (), 0, Node (Leaf (), 2, Leaf ())))
  , (Node (Leaf (), 2, Leaf ()), 0, Node (Node (Leaf (), 0, Leaf ()), 2, Leaf ()))
  , (Node (Leaf (), 2, Leaf ()), 1, Node (Node (Leaf (), 1, Leaf ()), 2, Leaf ()))
  , (Node (Leaf (), 2, Leaf ()), 2, Node (Leaf (), 2, Leaf ()))
  , (Node (Node (Leaf (), 0, Leaf ()), 1, Leaf ()), 0, Node (Node (Leaf (), 0, Leaf ()), 1, Leaf ()))
  , (Node (Node (Leaf (), 0, Leaf ()), 1, Leaf ()), 1, Node (Node (Leaf (), 0, Leaf ()), 1, Leaf ()))
  , (Node (Node (Leaf (), 0, Leaf ()), 1, Leaf ()), 2, Node (Node (Leaf (), 0, Leaf ()), 1, Node(Leaf (), 2, Leaf ())))
  , (Node (Leaf (), 0, Node (Leaf (), 1, Leaf ())), 2, Node (Leaf (), 0, Node (Leaf (), 1, Node(Leaf (), 2, Leaf ()))))
  , (Node (Node (Leaf (), 1, Leaf ()), 2, Leaf ()), 0, Node (Node (Node(Leaf (), 0, Leaf ()), 1, Leaf ()), 2, Leaf ()))
  , (Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())), 0, Node (Node (Leaf (), 0, Leaf ()), 1, Node (Leaf (), 2, Leaf ())))
  , (Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())), 1, Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())))
  , (Node (Node (Leaf (), 1, Leaf ()), 2, Leaf ()), 0, Node (Node (Node(Leaf (), 0, Leaf ()), 1, Leaf ()), 2, Leaf ()))
  ]""" 20
  """specifyFunction2 treeBInsert
  [ (Leaf (), 0, Node (Leaf (), 0, Leaf ()))
  , (Leaf (), 1, Node (Leaf (), 1, Leaf ()))
  , (Leaf (), 2, Node (Leaf (), 2, Leaf ()))
  , (Node (Leaf (), 1, Leaf ()), 0, Node (Node (Leaf (), 0, Leaf ()), 1, Leaf ()))
  , (Node (Leaf (), 1, Leaf ()), 1, Node (Leaf (), 1, Leaf ()))
  , (Node (Leaf (), 1, Leaf ()), 2, Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())))
  , (Node (Leaf (), 0, Leaf ()), 0, Node (Leaf (), 0, Leaf ()))
  , (Node (Leaf (), 0, Leaf ()), 1, Node (Leaf (), 0, Node (Leaf (), 1, Leaf ())))
  , (Node (Leaf (), 0, Leaf ()), 2, Node (Leaf (), 0, Node (Leaf (), 2, Leaf ())))
  , (Node (Leaf (), 2, Leaf ()), 0, Node (Node (Leaf (), 0, Leaf ()), 2, Leaf ()))
  , (Node (Leaf (), 2, Leaf ()), 1, Node (Node (Leaf (), 1, Leaf ()), 2, Leaf ()))
  , (Node (Leaf (), 2, Leaf ()), 2, Node (Leaf (), 2, Leaf ()))
  , (Node (Node (Leaf (), 0, Leaf ()), 1, Leaf ()), 0, Node (Node (Leaf (), 0, Leaf ()), 1, Leaf ()))
  , (Node (Node (Leaf (), 0, Leaf ()), 1, Leaf ()), 1, Node (Node (Leaf (), 0, Leaf ()), 1, Leaf ()))
  , (Node (Node (Leaf (), 0, Leaf ()), 1, Leaf ()), 2, Node (Node (Leaf (), 0, Leaf ()), 1, Node(Leaf (), 2, Leaf ())))
  , (Node (Leaf (), 0, Node (Leaf (), 1, Leaf ())), 2, Node (Leaf (), 0, Node (Leaf (), 1, Node(Leaf (), 2, Leaf ()))))
  , (Node (Node (Leaf (), 1, Leaf ()), 2, Leaf ()), 0, Node (Node (Node(Leaf (), 0, Leaf ()), 1, Leaf ()), 2, Leaf ()))
  , (Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())), 0, Node (Node (Leaf (), 0, Leaf ()), 1, Node (Leaf (), 2, Leaf ())))
  , (Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())), 1, Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())))
  , (Node (Node (Leaf (), 1, Leaf ()), 2, Leaf ()), 0, Node (Node (Node(Leaf (), 0, Leaf ()), 1, Leaf ()), 2, Leaf ()))
  ]""" (-1)

tree_collect_leaves = (,,,,)
  """type Boolean
  = F ()
  | T ()

type BooleanTree
  = Leaf ()
  | Node (BooleanTree, Boolean, BooleanTree)

type BooleanList
  = Nil ()
  | Cons (Boolean, BooleanList)

let
  append : BooleanList -> BooleanList -> BooleanList
  append l1 l2 =
    case l1 of
      Nil _ ->
        l2
      Cons p ->
        Cons (get_2_1 p, append (get_2_2 p) l2)
in
let
  treeCollectLeaves : BooleanTree -> BooleanList
  treeCollectLeaves tree =
    case tree of
      Leaf _ ->
        Nil ()

      Node node ->
        ?? : BooleanList
in

"""
  """specifyFunction treeCollectLeaves
  [ (Leaf (), [])
  , (Node (Leaf (), True, Leaf ()), [True])
  , (Node (Leaf (), False, Leaf ()), [False])
  , (Node (Node (Leaf (), True, Leaf ()), False, Leaf ()), [True, False])
  , (Node (Node (Leaf (), False, Leaf ()), True, Leaf ()), [False, True])
  , (Node (Leaf (), False, Node (Leaf (), True, Leaf ())), [False, True])
  ]""" 6
  """specifyFunction treeCollectLeaves
  [ -- (Leaf (), [])
  -- , (Node (Leaf (), True, Leaf ()), [True])
    (Node (Leaf (), False, Leaf ()), [False])
  , (Node (Node (Leaf (), True, Leaf ()), False, Leaf ()), [True, False])
  -- , (Node (Node (Leaf (), False, Leaf ()), True, Leaf ()), [False, True])
  -- , (Node (Leaf (), False, Node (Leaf (), True, Leaf ())), [False, True])
  ]""" 2

tree_count_leaves = (,,,,)
  """type Boolean
  = F ()
  | T ()

type BooleanTree
  = Leaf ()
  | Node (BooleanTree, Boolean, BooleanTree)

type Nat
= Z ()
| S Nat

let
  sum : Nat -> Nat -> Nat
  sum n1 n2 =
    case n1 of
      Z _ -> n2
      S m -> S (sum m n2)
in
let
  treeCountLeaves : BooleanTree -> Nat
  treeCountLeaves tree =
    case tree of
      Leaf _ ->
        S (Z ())

      Node node ->
        ?? : Nat
in

"""
  """specifyFunction treeCountLeaves
  [ (Leaf (), 1)
  , (Node (Leaf (), True, Leaf ()), 2)
  , (Node (Node (Leaf (), True, Leaf ()), True, Leaf ()), 3)
  , (Node (Leaf (), True, Node (Leaf (), True, Leaf ())), 3)
  , (Node (Node (Node (Leaf (), True, Leaf ()), True, Leaf ()), True, Leaf ()), 4)
  , (Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ())), 4)
  , (Node (Node (Leaf (), True, Leaf ()), True, Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ()))), 6)
  ]""" 7
  """specifyFunction treeCountLeaves
  [ -- (Leaf (), 1)
  -- , (Node (Leaf (), True, Leaf ()), 2)
  -- , (Node (Node (Leaf (), True, Leaf ()), True, Leaf ()), 3)
  -- , (Node (Leaf (), True, Node (Leaf (), True, Leaf ())), 3)
  -- , (Node (Node (Node (Leaf (), True, Leaf ()), True, Leaf ()), True, Leaf ()), 4)
  -- , (Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ())), 4)
  , (Node (Node (Leaf (), True, Leaf ()), True, Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ()))), 6)
  ]""" 1

tree_count_nodes = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatTree
  = Leaf ()
  | Node (NatTree, Nat, NatTree)

let
  sum : Nat -> Nat -> Nat
  sum n1 n2 =
    case n1 of
      Z _ -> n2
      S m -> S (sum m n2)
in
let
  treeCountNodes : NatTree -> Nat
  treeCountNodes tree =
    case tree of
      Leaf _ ->
        Z ()

      Node node ->
        ?? : Nat
in

"""
  """specifyFunction treeCountNodes
  [ (Leaf (), 0)
  , (Node (Leaf (), 0, Leaf ()), 1)
  , (Node (Node (Leaf (), 0, Leaf ()), 0, Leaf ()), 2)
  , (Node (Leaf (), 0, Node(Leaf (), 0, Leaf ())), 2)
  , (Node (Node (Leaf (), 0, Node (Leaf (), 0, Leaf ())), 0, Leaf ()), 3)
  , (Node (Leaf (), 0, Node (Leaf (), 0, Node (Leaf (), 0, Leaf ()))), 3)
  ]""" 6
  """specifyFunction treeCountNodes
  [ -- (Leaf (), 0)
  -- , (Node (Leaf (), 0, Leaf ()), 1)
    (Node (Node (Leaf (), 0, Leaf ()), 0, Leaf ()), 2)
  -- , (Node (Leaf (), 0, Node(Leaf (), 0, Leaf ())), 2)
  , (Node (Node (Leaf (), 0, Node (Leaf (), 0, Leaf ())), 0, Leaf ()), 3)
  -- , (Node (Leaf (), 0, Node (Leaf (), 0, Node (Leaf (), 0, Leaf ()))), 3)
  ]""" 2

tree_inorder = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type NatTree
  = Leaf ()
  | Node (NatTree, Nat, NatTree)

let
  append : NatList -> NatList -> NatList
  append l1 l2 =
    case l1 of
      Nil _ ->
        l2
      Cons p ->
        Cons (get_2_1 p, append (get_2_2 p) l2)
in
let
  treeInOrder : NatTree -> NatList
  treeInOrder xss =
    case xss of
      Leaf _ ->
        Nil ()

      Node node ->
        ?? : NatList
in

"""
  """specifyFunction treeInOrder
  [ (Leaf (), [])
  , (Node (Leaf (), 1, Leaf ()), [1])
  , (Node (Leaf (), 2, Leaf ()), [2])
  , (Node (Node (Leaf (), 1, Leaf ()), 2, Leaf ()), [1, 2])
  , (Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())), [1, 2])
  ]""" 5
  """specifyFunction treeInOrder
  [ -- (Leaf (), [])
  -- , (Node (Leaf (), 1, Leaf ()), [1])
    (Node (Leaf (), 2, Leaf ()), [2])
  , (Node (Node (Leaf (), 1, Leaf ()), 2, Leaf ()), [1, 2])
  -- , (Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())), [1, 2])
  ]""" 2

tree_map = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type NatTree
  = Leaf ()
  | Node (NatTree, Nat, NatTree)

let
  div2 : Nat -> Nat
  div2 n =
    case n of
      Z _ -> Z ()
      S m1 ->
        case m1 of
          Z _ -> Z ()
          S m2 -> S (div2 m2)

  inc : Nat -> Nat
  inc n =
    S n
in
let
  treeMap : (Nat -> Nat) -> NatTree -> NatTree
  treeMap f =
    let
      fixTreeMap : NatTree -> NatTree
      fixTreeMap tree =
        case tree of
          Leaf _ ->
            Leaf ()

          Node node ->
            ?? : NatTree
    in
      fixTreeMap
in

"""
  """specifyFunction2 treeMap
  [ (div2, Leaf (), Leaf ())
  , (div2, Node (Leaf (), 0, Leaf ()), Node (Leaf (), 0, Leaf ()))
  , (div2, Node (Leaf (), 2, Leaf ()), Node (Leaf (), 1, Leaf ()))
  , (div2, Node (Node (Leaf (), 2, Leaf ()), 2, Leaf ()), Node (Node (Leaf (), 1, Leaf ()), 1, Leaf ()))
  , (div2, Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())), Node (Leaf (), 0, Node (Leaf (), 1, Leaf ())))
  , (inc, Leaf (), Leaf ())
  , (inc, Node (Leaf (), 0, Leaf ()), Node (Leaf (), 1, Leaf ()))
  ]""" 7
  """specifyFunction2 treeMap
  [ -- (div2, Leaf (), Leaf ())
  -- , (div2, Node (Leaf (), 0, Leaf ()), Node (Leaf (), 0, Leaf ()))
  -- , (div2, Node (Leaf (), 2, Leaf ()), Node (Leaf (), 1, Leaf ()))
    (div2, Node (Node (Leaf (), 2, Leaf ()), 2, Leaf ()), Node (Node (Leaf (), 1, Leaf ()), 1, Leaf ()))
  , (div2, Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())), Node (Leaf (), 0, Node (Leaf (), 1, Leaf ())))
  -- , (inc, Leaf (), Leaf ())
  -- , (inc, Node (Leaf (), 0, Leaf ()), Node (Leaf (), 1, Leaf ()))
  ]""" 2

tree_nodes_at_level = (,,,,)
  """type Boolean
  = F ()
  | T ()

type BooleanTree
  = Leaf ()
  | Node (BooleanTree, Boolean, BooleanTree)

type Nat
= Z ()
| S Nat

let
  sum : Nat -> Nat -> Nat
  sum n1 n2 =
    case n1 of
      Z _ -> n2
      S m -> S (sum m n2)
in
let
  treeNodesAtLevel : BooleanTree -> Nat -> Nat
  treeNodesAtLevel tree n = ??
in

"""
  """specifyFunction2 treeNodesAtLevel
  [ (Leaf (), 0, 0)
  , (Leaf (), 1, 0)
  , (Node (Leaf (), True, Leaf ()), 0, 1)
  , (Node (Leaf (), True, Leaf ()), 1, 0)
  , (Node (Node (Leaf (), True, Leaf ()), True, Leaf ()), 0, 1)
  , (Node (Node (Leaf (), True, Leaf ()), True, Leaf ()), 1, 1)
  , (Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ())), 0, 1)
  , (Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ())), 1, 2)
  , (Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ())), 2, 0)
  , (Node (Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ())), True, Leaf ()), 0, 1)
  , (Node (Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ())), True, Leaf ()), 1, 1)
  ]""" 11
  """specifyFunction2 treeNodesAtLevel
  [ (Leaf (), 0, 0)
  , (Leaf (), 1, 0)
  , (Node (Leaf (), True, Leaf ()), 0, 1)
  , (Node (Leaf (), True, Leaf ()), 1, 0)
  , (Node (Node (Leaf (), True, Leaf ()), True, Leaf ()), 0, 1)
  , (Node (Node (Leaf (), True, Leaf ()), True, Leaf ()), 1, 1)
  , (Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ())), 0, 1)
  , (Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ())), 1, 2)
  , (Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ())), 2, 0)
  , (Node (Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ())), True, Leaf ()), 0, 1)
  , (Node (Node (Node (Leaf (), True, Leaf ()), True, Node (Leaf (), True, Leaf ())), True, Leaf ()), 1, 1)
  ]""" (-1)

tree_postorder = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type NatTree
  = Leaf ()
  | Node (NatTree, Nat, NatTree)

let
  append : NatList -> NatList -> NatList
  append l1 l2 =
    case l1 of
      Nil _ ->
        l2
      Cons p ->
        Cons (get_2_1 p, append (get_2_2 p) l2)
in
let
  treePostorder : NatTree -> NatList
  treePostorder tree = ??
in

"""
  """specifyFunction treePostorder
  [ (Leaf (), [])
  , (Node (Leaf (), 1, Leaf ()), [1])
  , (Node (Leaf (), 2, Leaf ()), [2])
  , (Node (Node (Leaf (), 1, Leaf ()), 2, Leaf ()), [1, 2])
  , (Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())), [2, 1])
  , (Node (Node (Leaf (), 1, Leaf ()), 0, Node (Leaf (), 2, Leaf ()) ), [1, 2, 0])
  , (Node (Node (Leaf (), 2, Leaf ()), 0, Node (Leaf (), 1, Leaf ()) ), [2, 1, 0])
  , (Node (Node (Node (Leaf (), 2, Leaf ()), 0, Node (Leaf (), 1, Leaf ()) ), 0, Leaf ()), [2, 1, 0, 0])
  , (Node (Leaf (), 2, Node (Node (Leaf (), 2, Leaf ()), 0, Node (Leaf (), 1, Leaf ()) )), [2, 1, 0, 2])
  ]""" 9
  """specifyFunction treePostorder
  [ (Leaf (), [])
  , (Node (Leaf (), 1, Leaf ()), [1])
  , (Node (Leaf (), 2, Leaf ()), [2])
  , (Node (Node (Leaf (), 1, Leaf ()), 2, Leaf ()), [1, 2])
  , (Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())), [2, 1])
  , (Node (Node (Leaf (), 1, Leaf ()), 0, Node (Leaf (), 2, Leaf ()) ), [1, 2, 0])
  , (Node (Node (Leaf (), 2, Leaf ()), 0, Node (Leaf (), 1, Leaf ()) ), [2, 1, 0])
  , (Node (Node (Node (Leaf (), 2, Leaf ()), 0, Node (Leaf (), 1, Leaf ()) ), 0, Leaf ()), [2, 1, 0, 0])
  , (Node (Leaf (), 2, Node (Node (Leaf (), 2, Leaf ()), 0, Node (Leaf (), 1, Leaf ()) )), [2, 1, 0, 2])
  ]""" (-1)

tree_preorder = (,,,,)
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type NatTree
  = Leaf ()
  | Node (NatTree, Nat, NatTree)

let
  append : NatList -> NatList -> NatList
  append l1 l2 =
    case l1 of
      Nil _ ->
        l2
      Cons p ->
        Cons (get_2_1 p, append (get_2_2 p) l2)
in
let
  treePreorder : NatTree -> NatList
  treePreorder tree =
    case tree of
      Leaf _ ->
        Nil ()

      Node node ->
        ?? : NatList
in

"""
  """specifyFunction treePreorder
  [ (Leaf (), [])
  , (Node (Leaf (), 1, Leaf ()), [1])
  , (Node (Leaf (), 2, Leaf ()), [2])
  , (Node (Node (Leaf (), 1, Leaf ()), 2, Leaf ()), [2, 1])
  , (Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())), [1, 2])
  ]""" 5
  """specifyFunction treePreorder
  [ -- (Leaf (), [])
  -- , (Node (Leaf (), 1, Leaf ()), [1])
  -- , (Node (Leaf (), 2, Leaf ()), [2])
    (Node (Node (Leaf (), 1, Leaf ()), 2, Leaf ()), [2, 1])
  , (Node (Leaf (), 1, Node (Leaf (), 2, Leaf ())), [1, 2])
  ]""" 2

suite : Dict String (String, String, Int, String, Int)
suite =
  Dict.fromList
    [ ("list_append", list_append)
    -- , ("list_compress", list_compress)
    , ("list_concat", list_concat)
    , ("list_drop", list_drop)
    -- , ("list_even_parity", list_even_parity)
    -- , ("list_filter", list_filter)
    , ("list_last", list_last)
    , ("list_length", list_length)
    , ("list_map", list_map)
    , ("list_nth", list_nth)
    -- , ("list_pairwise_swap", list_pairwise_swap)
    , ("list_rev_append", list_rev_append)
    , ("list_rev_snoc", list_rev_snoc)
    , ("list_snoc", list_snoc)
    , ("list_sort_sorted_insert", list_sort_sorted_insert)
    -- , ("list_sorted_insert", list_sorted_insert)
    , ("list_stutter", list_stutter)
    , ("list_take", list_take)
    , ("nat_iseven", nat_iseven)
    , ("nat_max", nat_max)
    , ("nat_add", nat_add)
    -- , ("tree_binsert", tree_binsert)
    , ("tree_collect_leaves", tree_collect_leaves)
    , ("tree_count_leaves", tree_count_leaves)
    , ("tree_count_nodes", tree_count_nodes)
    , ("tree_inorder", tree_inorder)
    , ("tree_map", tree_map)
    -- , ("tree_nodes_at_level", tree_nodes_at_level)
    -- , ("tree_postorder", tree_postorder)
    , ("tree_preorder", tree_preorder)
    ]

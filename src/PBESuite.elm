module PBESuite exposing (init, suite)

import Dict exposing (Dict)

init =
  """type NatList
  = Nil ()
  | Cons (Nat, NatList)

type Nat
  = Z ()
  | S Nat

?? : Nat"""

bool_band =
  """type Boolean
  = F ()
  | T ()

and : Boolean -> Boolean -> Boolean
and p q = ??

specifyFunction2 and
  [ (T (), T (), T ())
  , (T (), F (), F ())
  , (F (), T (), F ())
  , (F (), F (), F ())
  ]"""

bool_bor =
  """type Boolean
  = F ()
  | T ()

or : Boolean -> Boolean -> Boolean
or p q = ??

specifyFunction2 or
  [ (T (), T (), T ())
  , (T (), F (), T ())
  , (F (), T (), T ())
  , (F (), F (), F ())
  ]"""

bool_impl =
  """type Boolean
  = F ()
  | T ()

impl : Boolean -> Boolean -> Boolean
impl p q = ??

specifyFunction2 impl
  [ (T (), T (), T ())
  , (T (), F (), T ())
  , (F (), T (), T ())
  , (F (), F (), F ())
  ]"""

bool_neg =
  """type Boolean
  = F ()
  | T ()

neg : Boolean -> Boolean
neg p = ??

specifyFunction neg
  [ (T (), F ())
  , (F (), T ())
  ]"""

bool_xor =
  """type Boolean
  = F ()
  | T ()

xor : Boolean -> Boolean -> Boolean
xor p q = ??

specifyFunction2 xor
  [ (T (), T (), F ())
  , (T (), F (), T ())
  , (F (), T (), T ())
  , (F (), F (), F ())
  ]"""

list_append =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

append : NatList -> NatList -> NatList
append xs ys = ??

specifyFunction2 append
  [ ([], [], [])
  , ([], [0], [0])
  , ([0], [], [0])
  , ([0], [0], [0, 0])
  , ([1, 0], [], [1, 0])
  , ([1, 0], [0], [1, 0, 0])
  ]"""

list_compress =
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

specifyFunction2 compress
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
  ]"""

list_concat =
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
  concat xss = ??
in

specifyFunction concat
  [ (LNil (), [])
  , (LCons ([], LNil ()), [])
  , (LCons ([0], LNil ()), [0])
  , (LCons ([0], LCons([0], LNil ())), [0,0])
  , (LCons ([1], LNil ()), [1])
  , (LCons ([1], LCons([1], LNil ())), [1,1])
  ]"""

list_drop =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

drop : NatList -> Nat -> NatList
drop xs n = ??

specifyFunction2 drop
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
  ]"""

list_even_parity =
  """type Nat
  = Z ()
  | S Nat

type Boolean
  = T ()
  | F ()

type BooleanList
  = Nil ()
  | Cons (Boolean, BooleanList)

evenParity : BooleanList -> Boolean
evenParity xs = ??

specifyFunction evenParity
  [ ([], T ())
  , ([F ()], T ())
  , ([T ()], F ())
  , ([F (), F ()], T ())
  , ([F (), T ()], F ())
  , ([T (), F ()], F ())
  , ([T (), T ()], T ())
  ]"""

list_filter =
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

  isNonzero : Nat -> Bool
  isNonzero n =
    case n of
      Z _ -> F ()
      S _ -> T ()
in
let
  listFilter : (Nat -> Bool) -> NatList -> NatList
  listFilter predicate xs = ??
in

specifyFunction2 listFilter
  [ (isEven, [], [])
  , (isEven, [0], [0])
  , (isEven, [1], [])
  , (isEven, [2], [2])
  , (isEven, [0, 0], [0, 0])
  , (isEven, [0, 1], [0])
  , (isNonzero, [], [])
  , (isNonzero, [0], [])
  ]"""

list_fold =
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

  isOdd : Nat -> Boolean
  isOdd n =
    case n of
      Z _  -> F ()
      S m1 ->
        case m1 of
          Z _  -> T ()
          S m2 -> isOdd m2

  countOdd : Nat -> Nat -> Nat
  countOdd n1 n2 =
    case isOdd n2 of
      T _ -> S n1
      F _ -> n1
in
let
  listFold : (Nat -> Nat -> Nat) -> Nat -> NatList -> Nat
  listFold f acc xs = ??
in

specifyFunction (\\p -> listFold (get_3_1 p) (get_3_2 p) (get_3_3 p))
  [ ((sum, 0, []), 0)
  , ((sum, 0, [1]), 1)
  , ((sum, 0, [2, 1]), 3)
  , ((sum, 0, [3, 2, 1]), 6)
  , ((sum, 1, []), 1)
  , ((countOdd, 0, []), 0)
  , ((countOdd, 0, [1]), 1)
  , ((countOdd, 0, [2, 1]), 1)
  , ((countOdd, 0, [3, 2, 1]), 2)
  ]"""

list_hd =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listHead : NatList -> Nat
listHead xs = ??

specifyFunction listHead
  [ ([], 0)
  , ([0], 0)
  , ([1], 1)
  ]"""

list_inc =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

let
  map : NatList -> (Nat -> Nat) -> NatList
  map xs f =
    case xs of
      Nil _ -> Nil ()
      Cons p -> Cons (f (get_2_1 p), map (get_2_2 p) f)
in
let
  listInc : NatList -> NatList
  listInc xs = ??
in

specifyFunction listInc
  [ ([], [])
  , ([1, 2], [2, 3])
  , ([0, 0], [1, 1])
  , ([3, 4, 5], [4, 5, 6])
  ]"""

list_last =
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
listLast xs = ??

specifyFunction listLast
  [ ([], None ()
  , ([1], Some 1)
  , ([2], Some 2)
  , ([2, 1], Some 1)
  , ([1, 2], Some 2)
  , ([3, 2, 1], Some 1)
  ]"""

list_length =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listLength : NatList -> Nat
listLength xs = ??

specifyFunction listLength
  [ ([], 0)
  , ([0], 1)
  , ([0, 0], 2)
  ]"""

list_map =
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
  listMap f = let rec xs = ?? in xs
in

specifyFunction2 listMap
  [ (inc, [], [])
  , (inc, [0], [1])
  , (inc, [0, 0], [1, 1])
  , (inc, [1], [2])
  , (inc, [1, 1], [2, 2])
  , (zero, [], [])
  , (zero, [0], [0])
  , (zero, [0, 0], [0, 0])
  ]"""

list_nth =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listNth : NatList -> Nat -> Nat
listNth xs n = ??

specifyFunction2 listNth
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
  ]"""

list_pairwise_swap =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listPairwiseSwap : NatList -> NatList
listPairwiseSwap xs = ??

specifyFunction listPairwiseSwap
  [ ([], [])
  , ([0], [])
  , ([1], [])
  , ([0, 1], [1, 0])
  , ([1, 0], [0, 1])
  , ([1, 0, 1], [])
  , ([0, 1, 0, 1], [1, 0, 1, 0])
  ]"""

list_rev_append =
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
  listRevAppend = ??
in
let list_rev_append : list -> list |>
  { [] => []
  | [0] => [0]
  | [1] => [1]
  | [0;1] => [1;0]
  | [0;0;1] => [1;0;0]
  } = ?"""

list_rev_fold =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

let
  fold : NatList -> (NatList -> Nat -> NatList) -> NatList -> NatList
  fold xs f acc =
    case xs of
      Nil _ -> acc
      Cons p -> fold (get_2_2 p) f (f acc (get_2_1 p))

  snoc : NatList -> Nat -> NatList
  snoc xs n =
    case xs of
      Nil _ -> Cons (n, Nil ())
      Cons p -> Cons (get_2_1 p, snoc (get_2_2 p) n)
in
let
  listRevFold : NatList -> NatList
in
let list_rev_fold : list -> list |>
  { [] => []
  | [0] => [0]
  | [1] => [1]
  | [0;1] => [1;0]
  | [0;0;1] => [1;0;0]
  } = ?"""

list_rev_snoc =
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
  listRevSnoc = ??
in
let list_rev_snoc : list -> list |>
  { [] => []
  | [0] => [0]
  | [1] => [1]
  | [0;1] => [1;0]
  | [0;0;1] => [1;0;0]
  } = ?"""

list_rev_tailcall =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listRevTailcall : NatList -> NatList -> NatList
listRevTailcall = ??

let list_rev_tailcall : list -> list -> list |>
  { [] => ( [] => []
          | [0] => [0]
          | [1] => [1]
          | [1;0] => [1;0]
          )
  | [0] => ( [] => [0] )
  | [1] => ( [] => [1]
           | [0] => [1;0]
           )
  | [0;1] => ( [] => [1;0] )
  } = ?"""

list_snoc =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listSnoc : NatList -> Nat -> NatList
listSnoc = ??

let list_snoc : list -> nat -> list |>
  { [] => ( 0 => [0]
          | 1 => [1] )
  | [0] => ( 0 => [0; 0]
           | 1 => [0; 1] )
  | [1; 0] => ( 0 => [1; 0; 0]
              | 1 => [1; 0; 1] )
  | [2; 1; 0] => ( 0 => [2; 1; 0; 0]
                 | 1 => [2; 1; 0; 1] )
  } = ?"""

list_sort_sorted_insert =
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
          GT -> Cons (get_2_1 p, insert (get_2_2 p) n)
in
let
  listSortSortedInsert : NatList -> NatList
  listSortSortedInsert = ??
in
let list_sort_sorted_insert : list -> list |>
 { [] => []
 | [0] => [0]
 | [1] => [1]
 | [0;0] => [0]
 | [1;0] => [0;1]
 | [1;1] => [1]
 | [0;1;1] => [0;1]
 } = ?"""

list_sorted_insert =
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
  listSortedInsert = ??
in

let list_sorted_insert : list -> nat -> list |>
  { [] => ( 0 => [0]
          | 1 => [1]
          | 2 => [2] )
  | [0] => ( 0 => [0]
           | 1 => [0;1] )
  | [1] => ( 0 => [0;1]
           | 1 => [1]
           | 2 => [1;2] )
  | [2] => ( 0 => [0;2]
           | 1 => [1;2])
  | [0;1] => ( 0 => [0;1]
             | 2 => [0;1;2] )
  } = ?"""

list_stutter =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listStutter : NatList -> NatList
listStutter = ??

let list_stutter : list -> list |>
  { [] => []
  | [0] => [0;0]
  | [1;0] => [1;1;0;0]
  } = ?"""

list_sum =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

let
  fold : NatList -> (NatList -> Nat -> NatList) -> NatList -> NatList
  fold xs f acc =
    case xs of
      Nil _ -> acc
      Cons p -> fold (get_2_2 p) f (f acc (get_2_1 p))

  add : Nat -> Nat -> Nat
  add n1 n2 =
    case n1 of
      Z _ -> n2
      S m -> S (add m n2)
in
let
  listSum : NatList -> NatList
  listSum = ??
in
let list_sum : list -> nat |>
  { [] => 0
  | [1] => 1
  | [2; 1] => 3
  } = ?"""

list_take =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listTake : Nat -> NatList -> NatList
listTake = ??

let list_take : nat -> list -> list |>
  { 0 => ( [] => []
         | [1] => []
         | [0;1] => []
         | [1;0;1] => [] )
  | 1 => ( []    => []
         | [1]   => [1]
         | [0;1] => [0]
         | [1;0;1] => [1] )
  | 2 => ( []    => []
         | [1]   => [1]
         | [0;1] => [0;1]
         | [1;0;1] => [1;0] )
  } = ?"""

list_tl =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listTail : NatList -> NatList
listTail = ??

let list_tl : list -> list |>
  { [] => []
  | [0] => []
  | [0; 0] => [0] } = ?"""

nat_iseven =
  """type Nat
  = Z ()
  | S Nat

type Boolean
  = F ()
  | T ()

isEven : Nat -> Bool
isEven = ??

let nat_iseven : nat -> bool |>
  { 0 => True
  | 1 => False
  | 2 => True
  | 3 => False
  } = ?"""

nat_max =
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
  natMax : NatList -> NatList
  natMax = ??
in

let nat_max : nat -> nat -> nat |>
{
  0 => ( 0 => 0
       | 1 => 1
       | 2 => 2 )
| 1 => ( 0 => 1
       | 1 => 1
       | 2 => 2 )
| 2 => ( 0 => 2
       | 1 => 2
       | 2 => 2 )
} = ?"""

nat_pred =
  """type Nat
  = Z ()
  | S Nat

type Boolean
  = F ()
  | T ()

natPred : Nat -> Nat
natPred = ??

let nat_pred : nat -> nat |>
  { O => O
  ; S (O) => O
  ; S (S (O)) => S (O) } = ?"""

nat_add =
  """type Nat
  = Z ()
  | S Nat

type Boolean
  = F ()
  | T ()

natAdd : Nat -> Nat -> Nat
natAdd = ??

let nat_add : nat -> nat -> nat |>
  { 0 => ( 0 => 0
         | 1 => 1
         | 2 => 2 )
  | 1 => ( 0 => 1
         | 1 => 2
         | 2 => 3 )
  | 2 => ( 0 => 2
         | 1 => 3
         | 2 => 4 )
  } = ?"""

tree_binsert =
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
  treeBInsert = ??
in

let tree_binsert : tree -> nat -> tree |>
  { Leaf => ( 0 => Node (Leaf, 0, Leaf)
            | 1 => Node (Leaf, 1, Leaf)
            | 2 => Node (Leaf, 2, Leaf))
  | Node (Leaf, 1, Leaf) => ( 0 => Node (Node (Leaf, 0, Leaf), 1, Leaf)
                            | 1 => Node (Leaf, 1, Leaf)
                            | 2 => Node (Leaf, 1, Node (Leaf, 2, Leaf)))
  | Node (Leaf, 0, Leaf) => ( 0 => Node (Leaf, 0, Leaf)
                            | 1 => Node (Leaf, 0, Node (Leaf, 1, Leaf))
                            | 2 => Node (Leaf, 0, Node (Leaf, 2, Leaf)))
  | Node (Leaf, 2, Leaf) => ( 0 => Node (Node (Leaf, 0, Leaf), 2, Leaf)
                            | 1 => Node (Node (Leaf, 1, Leaf), 2, Leaf)
                            | 2 => Node (Leaf, 2, Leaf))
  | Node (Node (Leaf, 0, Leaf), 1, Leaf) =>
      ( 0 => Node (Node (Leaf, 0, Leaf), 1, Leaf)
      | 1 => Node (Node (Leaf, 0, Leaf), 1, Leaf)
      | 2 => Node (Node (Leaf, 0, Leaf), 1, Node(Leaf, 2, Leaf)))
  | Node (Leaf, 0, Node (Leaf, 1, Leaf)) =>
      ( 2 => Node (Leaf, 0, Node (Leaf, 1, Node(Leaf, 2, Leaf))))
  | Node (Node (Leaf, 1, Leaf), 2, Leaf) =>
      ( 0 => Node (Node (Node(Leaf, 0, Leaf), 1, Leaf), 2, Leaf))
  | Node (Leaf, 1, Node (Leaf, 2, Leaf)) =>
      ( 0 => Node (Node (Leaf, 0, Leaf), 1, Node (Leaf, 2, Leaf))
      | 1 => Node (Leaf, 1, Node (Leaf, 2, Leaf)))
  | Node (Node (Leaf, 1, Leaf), 2, Leaf) =>
      ( 0 => Node (Node (Node(Leaf, 0, Leaf), 1, Leaf), 2, Leaf))
  } = ?"""

tree_collect_leaves =
  """type Boolean
  = F ()
  | T ()

type BooleanTree
  = Leaf
  | Node (BooleanTree, Boolean, BooleanTree)

type BooleanList
  = Nil ()
  | Cons (Boolean, BooleanList)

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
  treeCollectLeaves : NatTree -> NatList
  treeCollectLeaves = ??
in

let tree_collect_leaves : tree -> list |>
  { Leaf => []
  | Node (Leaf, True, Leaf) => [True]
  | Node (Leaf, False, Leaf) => [False]
  | Node (Node (Leaf, True, Leaf), False, Leaf) => [True; False]
  | Node (Node (Leaf, False, Leaf), True, Leaf) => [False; True]
  | Node (Leaf, False, Node (Leaf, True, Leaf)) => [False; True]
  } = ?"""

tree_count_leaves =
  """type Boolean
  = F ()
  | T ()

type BooleanTree
  = Leaf
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
  treeCountLeaves : NatTree -> Nat
  treeCountLeaves = ??
in

let tree_count_leaves : tree -> nat |>
  { Leaf => 1
  | Node (Leaf, True, Leaf) => 2
  | Node (Node (Leaf, True, Leaf), True, Leaf) => 3
  | Node (Leaf, True, Node (Leaf, True, Leaf)) => 3
  | Node (Node (Node (Leaf, True, Leaf), True, Leaf), True, Leaf) => 4
  | Node (Node (Leaf, True, Leaf), True, Node (Leaf, True, Leaf)) => 4
  | Node (Node (Leaf, True, Leaf), True,
      Node (Node (Leaf, True, Leaf), True, Node (Leaf, True, Leaf))) => 6
  } = ?"""

tree_count_nodes =
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
  treeCountNodes : NatTree -> Nat
  treeCountNodes = ??
in

let tree_count_nodes : tree -> nat |>
  { Leaf => 0
  | Node(Leaf, 0, Leaf) => 1
  | Node(Node(Leaf, 0, Leaf), 0, Leaf) => 2
  | Node(Leaf, 0, Node(Leaf, 0, Leaf)) => 2
  | Node(Node(Leaf, 0, Node(Leaf, 0, Leaf)), 0, Leaf) => 3
  | Node(Leaf, 0, Node(Leaf, 0, Node(Leaf, 0, Leaf))) => 3
  } = ?"""

tree_inorder =
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
  treeInOrder : NatListList -> NatList
  treeInOrder = ??
in

let tree_inorder: tree -> list |>
{ Leaf => []
| Node (Leaf, 1, Leaf) => [1]
| Node (Leaf, 2, Leaf) => [2]
| Node (Node (Leaf, 1, Leaf), 2, Leaf) => [1;2]
| Node (Leaf, 1, Node (Leaf, 2, Leaf)) => [1;2]
} = ?"""

tree_map =
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
  treeMap = ??
in

let tree_map : (nat -> nat) -> tree -> tree |>
{ div2 => ( Leaf => Leaf
          | Node (Leaf, 0, Leaf) => Node (Leaf, 0, Leaf)
          | Node (Leaf, 2, Leaf) => Node (Leaf, 1, Leaf)
          | Node (Node (Leaf, 2, Leaf), 2, Leaf) =>
              Node (Node (Leaf, 1, Leaf), 1, Leaf)
          | Node (Leaf, 1, Node (Leaf, 2, Leaf)) =>
              Node (Leaf, 0, Node (Leaf, 1, Leaf))
          )
| inc =>  ( Leaf => Leaf
          | Node (Leaf, 0, Leaf) => Node (Leaf, 1, Leaf) )
} = ?"""

tree_nodes_at_level =
  """type Boolean
  = F ()
  | T ()

type tree =
  | Leaf
  | Node of tree * bool * tree

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
  treeNodesAtLevel : NatTree -> Nat -> Nat
  treeNodesAtLevel = ??
in

let tree_nodes_at_level : tree -> nat -> nat |>
  { Leaf =>
    ( 0 => 0
    | 1 => 0
    )
  | Node (Leaf, True, Leaf) =>
    ( 0 => 1
    | 1 => 0
    )
  | Node (Node (Leaf, True, Leaf), True, Leaf) =>
    ( 0 => 1
    | 1 => 1
    )
  | Node (Node (Leaf, True, Leaf), True, Node (Leaf, True, Leaf)) =>
    ( 0 => 1
    | 1 => 2
    | 2 => 0
    )
  | Node (Node
      (Node (Leaf, True, Leaf), True, Node (Leaf, True, Leaf)), True, Leaf) =>
    ( 0 => 1
    | 1 => 1
    )
  } = ?"""

tree_postorder =
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
  treePostOrder : NatTree -> NatList
  treePostOrder = ??
in

let tree_postorder : tree -> list |>
{  Leaf => []
| Node (Leaf, 1, Leaf) => [1]
| Node (Leaf, 2, Leaf) => [2]
| Node (Node (Leaf, 1, Leaf), 2, Leaf) => [1;2]
| Node (Leaf, 1, Node (Leaf, 2, Leaf)) => [2;1]
| Node (Node (Leaf, 1, Leaf), 0, Node (Leaf, 2, Leaf) ) => [1;2;0]
| Node (Node (Leaf, 2, Leaf), 0, Node (Leaf, 1, Leaf) ) => [2;1;0]
| Node (Node (Node (Leaf, 2, Leaf), 0, Node (Leaf, 1, Leaf) ), 0, Leaf) =>
    [2;1;0;0]
| Node (Leaf, 2, Node (Node (Leaf, 2, Leaf), 0, Node (Leaf, 1, Leaf) )) =>
    [2;1;0;2]
} = ?"""

tree_preorder =
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
  treePreOrder : NatTree -> NatList
  treePreOrder = ??
in

let tree_preorder : tree -> list |>
{ Leaf => []
| Node (Leaf, 1, Leaf) => [1]
| Node (Leaf, 2, Leaf) => [2]
| Node (Node (Leaf, 1, Leaf), 2, Leaf) => [2;1]
| Node (Leaf, 1, Node (Leaf, 2, Leaf)) => [1;2]
} = ?"""

suite : Dict String String
suite =
  Dict.fromList
    [ ("bool_band", bool_band)
    , ("bool_bor", bool_bor)
    , ("bool_impl", bool_impl)
    , ("bool_neg", bool_neg)
    , ("bool_xor", bool_xor)
    , ("list_append", list_append)
    -- , ("list_compress", list_compress)
    , ("list_concat", list_concat)
    , ("list_drop", list_drop)
    , ("list_even_parity", list_even_parity)
    , ("list_filter", list_filter)
    , ("list_fold", list_fold)
    , ("list_hd", list_hd)
    , ("list_inc", list_inc)
    , ("list_last", list_last)
    , ("list_length", list_length)
    , ("list_map", list_map)
    , ("list_nth", list_nth)
    , ("list_pairwise_swap", list_pairwise_swap)
    , ("list_rev_append", list_rev_append)
    , ("list_rev_fold", list_rev_fold)
    , ("list_rev_snoc", list_rev_snoc)
    , ("list_rev_tailcall", list_rev_tailcall)
    , ("list_snoc", list_snoc)
    , ("list_sort_sorted_insert", list_sort_sorted_insert)
    , ("list_sorted_insert", list_sorted_insert)
    , ("list_stutter", list_stutter)
    , ("list_sum", list_sum)
    , ("list_take", list_take)
    , ("list_tl", list_tl)
    , ("nat_iseven", nat_iseven)
    , ("nat_max", nat_max)
    , ("nat_pred", nat_pred)
    , ("nat_add", nat_add)
    , ("tree_binsert", tree_binsert)
    , ("tree_collect_leaves", tree_collect_leaves)
    , ("tree_count_leaves", tree_count_leaves)
    , ("tree_count_nodes", tree_count_nodes)
    , ("tree_inorder", tree_inorder)
    , ("tree_map", tree_map)
    , ("tree_nodes_at_level", tree_nodes_at_level)
    , ("tree_postorder", tree_postorder)
    , ("tree_preorder", tree_preorder)
    ]

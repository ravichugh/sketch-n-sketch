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
and = ??

PBE.constrain and <|
  PF \"\"\"
    { T () ->
        { T () -> T ()
        , F () -> F ()
        }
    , F () ->
        { T () -> F ()
        , F () -> F ()
        }
    }
  \"\"\""""

bool_bor =
  """type Boolean
  = F ()
  | T ()

or : Boolean -> Boolean -> Boolean
or = ??

PBE.constrain or <|
  PF \"\"\"
    { T () ->
        { T () -> T ()
        , F () -> T ()
        }
    , F () ->
        { T () -> T ()
        , F () -> F ()
        }
    }
  \"\"\""""

bool_impl =
  """type Boolean
  = F ()
  | T ()

impl : Boolean -> Boolean -> Boolean
impl = ??

PBE.constrain impl <|
  PF \"\"\"
    { T () ->
        { T () -> T ()
        , F () -> F ()
        }
    , F () ->
        { T () -> T ()
        , F () -> T ()
        }
    }
  \"\"\""""

bool_neg =
  """type Boolean
  = F ()
  | T ()

neg : Boolean -> Boolean
neg = ??

PBE.constrain neg <|
  PF \"\"\"
    { T () -> F ()
    , F () -> T ()
    }
  \"\"\""""

bool_xor =
  """type Boolean
  = F ()
  | T ()

xor : Boolean -> Boolean -> Boolean
xor = ??

PBE.constrain xor <|
  PF \"\"\"
    { T () ->
        { T () -> F ()
        , F () -> T ()
        }
    , F () ->
        { T () -> T ()
        , F () -> F ()
        }
    }
  \"\"\""""

list_append =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

append : NatList -> NatList -> NatList
append = ??

PBE.constrain append <|
  PF \"\"\"
    { [] ->
        { [] -> []
        , [0] -> [0]
        }
    , [0] ->
        { [] -> [0]
        , [0] -> [0, 0]
        }
    , [1, 0] ->
        { [] -> [1, 0]
        , [0] -> [1, 0, 0]
        }
    }
  \"\"\""""

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
  compress = ??
in

PBE.constrain compress <|
  PF \"\"\"
    { [] -> []
    , [0] -> [0]
    , [1] -> [1]
    , [0,0] -> [0]
    , [1,1] -> [1]
    , [2,0] -> [2,0]
    , [1,0,0] -> [1,0]
    , [0,1,1] -> [0,1]
    , [2,1,0,0] -> [2,1,0]
    , [2,2,1,0,0] -> [2,1,0]
    , [2,2,0] -> [2,0]
    , [2,2,2,0] -> [2,0]
    , [1,2,2,2,0] -> [1,2,0]
    }
  \"\"\""""

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
  concat = ??
in

PBE.constrain concat <|
  PF \"\"\"
    { LNil () -> []
    , LCons ([], LNil ()) -> []
    , LCons ([0], LNil ()) -> [0]
    , LCons ([0], LCons([0], LNil ())) -> [0,0]
    , LCons ([1], LNil ()) -> [1]
    , LCons ([1], LCons([1], LNil ())) -> [1,1]
    }
  \"\"\""""

list_drop =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

drop : NatList -> Nat -> NatList
drop = ??

PBE.constrain drop <|
  PF \"\"\"
    { [] ->
        { 0 -> []
        , 1 -> []
        }
    , [0] ->
        { 0 -> [0]
        , 1 -> []
        }
    , [1] ->
        { 0 -> [1]
        , 1 -> []
        }
    , [1, 0] ->
        { 0 -> [1, 0]
        , 1 -> [0]
        }
    , [0, 1] ->
        { 0 -> [0, 1]
        , 1 -> [1]
        , 2 -> []
        }
    }
  \"\"\""""

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
evenParity = ??

PBE.constrain evenParity <|
  PF \"\"\"
    { [] -> T ()
    , [F ()] -> T ()
    , [T ()] -> F ()
    , [F (), F ()] -> T ()
    , [F (), T ()] -> F ()
    , [T (), F ()] -> F ()
    , [T (), T ()] -> T ()
    }
  \"\"\""""

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
  listFilter = ??
in
{
  is_even => ( [] => []
             | [0] => [0]
             | [1] => []
             | [2] => [2]
             | [0;0] => [0;0]
             | [0;1] => [0] )
| is_nonzero => ( [] => []
                | [0] => [] )
} = ?"""

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
  listFold = ??
in

let list_fold : (nat -> nat -> nat) -> nat -> list -> nat |>
    { sum => ( 0 => ( [] => 0
                    | [1] => 1
                    | [2; 1] => 3
                    | [3; 2; 1] => 6 )
             | 1 => [] => 1 )
    | count_odd => ( 0 => ( [] => 0
                          | [1] => 1
                          | [2; 1] => 1
                          | [3; 2; 1] => 2 ) )
    } = ?"""

list_hd =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listHead : NatList -> NatList
listHead = ??

let list_hd : list -> nat |>
  { [] => 0
  | [0] => 0
  | [1] => 1
  } = ?"""

list_inc =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

let
  map : NatList -> (Nat -> Nat) -> NatList =
  map xs f =
    case xs of
      Nil _ -> Nil ()
      Cons p -> Cons (f (get_2_1 p), map (get_2_2 p) f)
in
let
  listInc : NatList -> NatList
  listInc = ??
in
let list_inc : list -> list |>
  { [] => []
  | [1;2] => [2;3]
  | [0;0] => [1;1]
  | [3;4;5] => [4;5;6]
  } = ?"""

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
listLast = ??

let list_last : list -> natopt |>
  { [] => None
  | [1] => Some (1)
  | [2] => Some (2)
  | [2; 1] => Some (1)
  | [1; 2] => Some (2)
  | [3; 2; 1] => Some (1)
  } = ?"""

list_length =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listLength : NatList -> Nat
listLength = ??

let list_length : list -> nat |>
  { [] => 0
  | [0] => 1
  | [0;0] => 2 } = ?"""

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
  listMap = ??
in
let list_map : (nat -> nat) -> list -> list |>
  { inc => ( [] => []
            | [0] => [1]
            | [0; 0] => [1; 1]
            | [1] => [2]
            | [1; 1] => [2; 2] )
  | zero => ( [] => []
            | [0] => [0]
            | [0; 0] => [0; 0] )
  } = ?"""

list_nth =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listNth : NatList -> Nat -> Nat
listNth = ??

let list_nth : list -> nat -> nat |>
  { [] => ( 0 => 0
          | 1 => 0 )
  | [2] => ( 0 => 2
           | 1 => 0 )
  | [1; 2] => ( 0 => 1
              | 1 => 2 )
  | [1] => ( 0 => 1
           | 1 => 0 )
  | [2; 1] => ( 0 => 2
              | 1 => 1 )
  | [3; 2; 1] => ( 0 => 3
                 | 1 => 2
                 | 2 => 1 )
  } = ?"""

list_pairwise_swap =
  """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

listPairwiseSwap : NatList -> NatList
listPairwiseSwap = ??

let list_pairwise_swap : list -> list |>
{ [] => []
| [0] => []
| [1] => []
| [0;1] => [1;0]
| [1;0] => [0;1]
| [1;0;1] => []
| [0;1;0;1] => [1;0;1;0]
} = ?
"""

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

let rec fold (l:list) (f:nat -> nat -> nat) (acc:nat) : nat =
  match l with
  | Nil -> acc
  | Cons (x, l) -> fold l f (f acc x)
;;

let rec add (n1:nat) (n2:nat) : nat =
  match n1 with
  | O -> n2
  | S (n1) -> S (add n1 n2)
;;

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

let list_tl : list -> list |>
  { [] => []
  | [0] => []
  | [0; 0] => [0] } = ?"""

nat_iseven =
  """type Nat
  = Z ()
  | S Nat

type Boolean
  = False
  | True

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
  = False
  | True

type Cmp
  = LT ()
  | EQ ()
  | GT ()

let rec compare (n1:nat) (n2:nat) : cmp =
  match n1 with
  | O ->
    (match n2 with
    | O -> EQ
    | S (m) -> LT)
  | S (m1) ->
    (match n2 with
    | O -> GT
    | S (m2) -> (compare m1 m2))
;;

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
  = False
  | True

let nat_pred : nat -> nat |>
  { O => O
  ; S (O) => O
  ; S (S (O)) => S (O) } = ?"""

nat_add =
  """type Nat
  = Z ()
  | S Nat

type Boolean
  = False
  | True

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

let rec comp_nat (n1:nat) (n2:nat) : cmp =
  match n1 with
  | O -> (match n2 with
          | O -> CEq
          | S (n2) -> CLt)
  | S (n1) -> (match n2 with
              | O -> CGt
              | S (n2) -> comp_nat n1 n2)
;;

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

type tree =
  | Leaf
  | Node of tree * bool * tree

type BooleanList
  = Nil ()
  | Cons (Boolean, BooleanList)

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil -> l2
  | Cons (x, l1) -> Cons (x, append l1 l2)
;;

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

type tree =
  | Leaf
  | Node of tree * bool * tree

type Nat
= Z ()
| S Nat

let rec sum (n1:nat) (n2:nat) : nat =
  match n1 with
  | O -> n2
  | S (n1) -> S (sum n1 n2)
;;

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

let rec sum (n1:nat) (n2:nat) : nat =
  match n1 with
  | O -> n2
  | S (n1) -> S (sum n1 n2)
;;

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

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil -> l2
  | Cons (x, l1) -> Cons (x, append l1 l2)
;;

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

let rec div2 (n:nat) : nat =
  match n with
  | O -> O
  | S (n1) -> match n1 with
    | O -> O
    | S (n2) -> S (div2 n2)
;;

let rec inc (n:nat) : nat =
  S( n )
;;


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

let rec sum (n1:nat) (n2:nat) : nat =
      match n1 with
      | O -> n2
      | S (n1p) -> S (sum n1p n2)
;;

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

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil -> l2
  | Cons (x, l1) -> Cons (x, append l1 l2)
;;

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

let rec append (l1:list) (l2:list) : list =
  match l1 with
  | Nil -> l2
  | Cons (x, l1) -> Cons (x, append l1 l2)
;;

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
    , ("list_compress", list_compress)
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

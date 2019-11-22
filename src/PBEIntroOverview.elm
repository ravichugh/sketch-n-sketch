module PBEIntroOverview exposing (programs)

import Dict exposing (Dict)

stutter_n =
   """type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

let
  replicate : Nat -> Nat -> NatList
  replicate n x =
    case n of
      Z _  -> ?? : NatList
      S n_ -> ?? : NatList
in
let
  append : NatList -> NatList -> NatList
  append xs ys =
    case xs of
      Nil _ -> ys
      Cons p -> Cons (get_2_1 p, append (get_2_2 p) ys)
in
let
  stutter_n : NatList -> Nat -> NatList
  stutter_n xs n =
    case xs of
      Nil _ -> Nil ()
      Cons p -> append (replicate n (get_2_1 p)) (stutter_n (get_2_2 p) n)
in

let _ = assert (stutter_n [1, 0] 1 == [1, 0]) in
let _ = assert (stutter_n [3]    2 == [3, 3]) in
  ()"""

plus =
   """type Nat
  = Z ()
  | S Nat

plus : Nat -> Nat -> Nat
plus m n =
  ??

let _ = assert (plus 0 1 == 1) in
let _ = assert (plus 2 0 == 2) in
let _ = assert (plus 1 2 == 3) in
  ()"""

max =
   """type Nat
  = Z ()
  | S Nat

max : Nat -> Nat -> Nat
max m n =
  case n of
    Z _ ->
      m

    S n_ ->
      case m of
        Z _ ->
          n

        S m_ ->
          ?? : Nat

specifyFunction2 max
  [ (1, 1, 1)
  , (1, 2, 2)
  , (3, 1, 3)
  ]"""

odd_unjust =
  """type Nat
  = Z ()
  | S Nat

type Boolean
  = F ()
  | T ()

type MaybeNat
  = NothingNat ()
  | JustNat Nat

let
  odd : Nat -> Boolean
  odd n =
    case n of
      Z _ ->
        F ()

      S n_ ->
        case n_ of
          Z _ ->
            T ()

          S n__ ->
            odd n__
in
let
  unJust : MaybeNat -> Nat
  unJust mx =
    case mx of
      NothingNat _ ->
        Z ()

      JustNat x ->
        x
in

assert (odd (unJust (?? : MaybeNat)) == T ())"""

minus =
  """type NatList
  = Nil ()
  | Cons (Nat, NatList)

type Nat
  = Z ()
  | S Nat

minus : Nat -> Nat -> Nat
minus a b =
  case a of
    S a_ ->
      case b of
        S b_ ->
          minus (?? : Nat) (?? : Nat)

        Z _ ->
          ??0 : Nat

    Z _ ->
      ??0 : Nat

specifyFunction2 minus
  [ (2, 0, 2)
  , (3, 2, 1)
  , (3, 1, 2)
  ]"""

mult =
  """type NatList
  = Nil ()
  | Cons (Nat, NatList)

type Nat
  = Z ()
  | S Nat

let
  plus : Nat -> Nat -> Nat
  plus m n =
    case m of
      Z _ ->
        n

      S m_ ->
        S (plus m_ n)
in
let
  mult : Nat -> Nat -> Nat
  mult p q =
    case p of
      Z _ ->
        Z ()

      S p_ ->
        plus ?? (mult ?? ??)
in

specifyFunction2 mult
  [ (2, 1, 2)
  , (3, 2, 6)
  ]"""

programs : List (String, String)
programs =
  [ ("stutter_n", stutter_n)
  , ("plus", plus)
  , ("max", max)
  , ("odd_unjust", odd_unjust)
  , ("minus", minus)
  , ("mult", mult)
  ]

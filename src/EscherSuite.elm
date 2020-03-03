module EscherSuite exposing (..)

import Dict exposing (Dict)

name = "EscherSuite"

prelude = """type Boolean
  = F ()
  | T ()

type Nat
  = Z ()
  | S Nat

type NatList
  = Nil ()
  | Cons (Nat, NatList)

type NatOpt
  = None ()
  | Some Nat

type NatTree
  = Leaf ()
  | Node (NatTree, Leaf (), NatTree)

let
  and : Boolean -> Boolean -> Boolean
  and x y =
    case x of
      F _ -> F ()
      T _ -> y
in

let
  or : Boolean -> Boolean -> Boolean
  or x y =
    case x of
      F _ -> y
      T _ -> T ()
in

let
  not : Boolean -> Boolean
  not x =
    case x of
      F _ -> T ()
      T _ -> F ()
in

let
  plus : Nat -> Nat -> Nat
  plus x y =
    case x of
      Z _  -> y
      S x_ -> S (plus x_ y)
in

let
  minus : Nat -> Nat -> Nat
  minus x y =
    case x of
      Z _  -> Z ()
      S x_ ->
        case y of
          Z _ -> x
          S y_ ->
            minus x_ y_
in

let
  div2 : Nat -> Nat
  div2 x =
    let
      helper : Nat -> Nat -> Nat
      helper single double =
        case single of
          Z _ ->
            Z ()

          S single_ ->
            case double of
              Z _ ->
                single

              S double_ ->
                case double_ of
                  Z _ ->
                    single_

                  S double__ ->
                    helper single_ double__
    in
      helper x x
in

let
  append : NatList -> NatList -> NatList
  append xs ys =
    case xs of
      Nil _ ->
        ys

      Cons p ->
        Cons (get_2_1 p, append (get_2_2 p) ys)
in

let
  isEmpty : NatList -> Boolean
  isEmpty xs =
    case xs of
      Nil _  -> T ()
      Cons _ -> F ()
in

let
  isLeaf : NatTree -> Boolean
  isLeaf t =
    case t of
      Leaf _ -> T ()
      Node _ -> F ()
in

"""

list_last = (,,,,)
  """let
  listLast : NatList -> NatOpt
  listLast xs =
    case xs of
      Nil _ ->
        None ()

      Cons p ->
        ?? : NatOpt
in

"""
  """specifyFunction listLast
  [ ([], None ())

  , ([9], Some 9)

  , ([1], Some 1)
  , ([1, 1], Some 1)

  , ([4], Some 4)
  , ([3, 4], Some 4)
  , ([2, 3, 4], Some 4)
  ]""" 7
  """specifyFunction listLast
  [ ([], None ())

  , ([9], Some 9)

  -- , ([1], Some 1)
  , ([1, 1], Some 1)

  -- , ([4], Some 4)
  -- , ([3, 4], Some 4)
  , ([2, 3, 4], Some 4)
  ]""" 4

list_reverse = (,,,,)
  """let
  listReverse : NatList -> NatList
  listReverse xs =
    ??
in

"""
  """specifyFunction listReverse
  [ ([], [])
  , ([4], [4])
  , ([3, 4], [4, 3])
  , ([2, 3, 4], [4, 3, 2])
  ]""" 4
  """specifyFunction listReverse
  [ ([], [])
  , ([4], [4])
  -- , ([3, 4], [4, 3])
  , ([2, 3, 4], [4, 3, 2])
  ]""" 3

list_length = (,,,,)
  """let
  listLength : NatList -> Nat
  listLength xs =
    ??
in

"""
  """specifyFunction listReverse
  [ ([], [])
  , ([4], [4])
  , ([3, 4], [4, 3])
  , ([2, 3, 4], [4, 3, 2])
  ]""" 4
  """specifyFunction listReverse
  [ ([], [])
  , ([4], [4])
  -- , ([3, 4], [4, 3])
  , ([2, 3, 4], [4, 3, 2])
  ]""" 3

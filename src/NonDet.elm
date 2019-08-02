--------------------------------------------------------------------------------
-- Simple wrapper around List monad for nondeterminism.
--------------------------------------------------------------------------------

module NonDet exposing
  ( NonDet

  , none, fromList, toList

  , map, pure, andThen, join
  , do, pureDo

  , isEmpty
  , union
  , oneOfEach
  , filter
  , dedup
  )

import Set exposing (Set)
import Dict exposing (Dict)
import Utils

--------------------------------------------------------------------------------
-- Declarations
--------------------------------------------------------------------------------

type NonDet a =
  N (List a)

--------------------------------------------------------------------------------
-- Creation
--------------------------------------------------------------------------------

none : NonDet a
none =
  fromList []

fromList : List a -> NonDet a
fromList =
  N

--------------------------------------------------------------------------------
-- Collection
--------------------------------------------------------------------------------

toList : NonDet a -> List a
toList (N xs) =
  xs

--------------------------------------------------------------------------------
-- Core Functions
--------------------------------------------------------------------------------

map : (a -> b) -> NonDet a -> NonDet b
map f =
  toList >> List.map f >> fromList

pure : a -> NonDet a
pure =
  List.singleton >> fromList

andThen : (a -> NonDet b) -> NonDet a -> NonDet b
andThen f =
  toList >> List.map f >> union

join : NonDet (NonDet a) -> NonDet a
join =
  toList >> union

--------------------------------------------------------------------------------
-- Generic Library Functions
--------------------------------------------------------------------------------

do : NonDet a -> (a -> NonDet b) -> NonDet b
do =
  flip andThen

pureDo : NonDet a -> (a -> b) -> NonDet b
pureDo =
  flip map

--------------------------------------------------------------------------------
-- Specific Library Functions
--------------------------------------------------------------------------------

isEmpty : NonDet a -> Bool
isEmpty =
  (==) none

-- "Sum"
union : List (NonDet a) -> NonDet a
union =
  List.map toList >> List.concat >> fromList

-- "Product"
oneOfEach : List (NonDet a) -> NonDet (List a)
oneOfEach =
  List.map toList >> Utils.oneOfEach >> fromList

filter : (a -> Bool) -> NonDet a -> NonDet a
filter predicate =
  toList >> List.filter predicate >> fromList

dedup : NonDet a -> NonDet a
dedup =
  toList >> Set.fromList >> Set.toList >> fromList

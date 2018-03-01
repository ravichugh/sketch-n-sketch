module LazyList exposing (..)
import Lazy exposing (Lazy, lazy, force)

import Maybe exposing ( Maybe(Just, Nothing) )

type LazyList v = Nil | Cons v (Lazy (LazyList v))

isEmpty: LazyList a -> Bool
isEmpty l = case l of
  Nil -> True
  _ -> False

-- Useful if the tail is already computed.
cons: v -> LazyList v -> LazyList v
cons head tail = Cons head <| lazy <| \() -> tail

map: (v -> w) -> LazyList v -> LazyList w
map f l =
  case l of
    Nil -> Nil
    Cons head tail -> Cons (f head) <| Lazy.map (map f) tail

append: LazyList a -> LazyList a -> LazyList a
append l1 l2 =
  case l1 of
    Nil -> l2
    Cons head tail -> Cons head (Lazy.map (\v -> append v l2) tail)

appendLazy: LazyList a -> Lazy (LazyList a) -> LazyList a
appendLazy l1 l2 =
  case l1 of
    Nil -> force l2
    Cons head tail -> Cons head (Lazy.map (\v -> appendLazy v l2) tail)

andThen: (v -> LazyList w) -> LazyList v -> LazyList w
andThen f l =
  case l of
    Nil -> Nil
    Cons head tail -> appendLazy (f head) (Lazy.map (\v -> andThen f v) tail)

filter: (v -> Bool) -> LazyList v -> LazyList v
filter isok l =
  case l of
    Nil -> Nil
    Cons head lazyTail ->
      if isok head then Cons head (Lazy.map (filter isok) lazyTail)
      else filter isok <| force lazyTail

flatten: LazyList (LazyList a) -> LazyList a
flatten l =
  case l of
    Nil -> Nil
    Cons head tail ->
      appendLazy head (Lazy.map flatten tail)

fromList: List a -> LazyList a
fromList l =
  case l of
    [] -> Nil
    head::tail -> Cons head (lazy (\() -> fromList tail))

toList : LazyList a -> List a
toList lazyList =
  case lazyList of
    Nil          -> []
    Cons x thunk -> x :: toList (Lazy.force thunk)

findFirst: (a -> Bool) -> LazyList a -> Maybe a
findFirst pred l =
  case l of
    Nil -> Nothing
    Cons head tail -> if pred head then Just head else findFirst pred (force tail)

takeWhile: (a -> Bool) -> LazyList a -> LazyList a
takeWhile pred l =
  case l of
    Nil -> Nil
    Cons head lazyTail -> if pred head then Cons head (Lazy.map (takeWhile pred) lazyTail) else Nil

cartesianProduct : LazyList a -> LazyList b -> LazyList (a, b)
cartesianProduct xs ys =
   flatten (map (\x -> map ((,) x) ys) xs)

maybeCons: Maybe a -> LazyList a -> LazyList a
maybeCons x l = case x of
  Nothing -> l
  Just v -> Cons v (Lazy.lazy <| \_ -> l)

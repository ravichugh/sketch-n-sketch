module Utils where

import List ((::))
import List
import String

maybeFind : a -> List (a,b) -> Maybe b
maybeFind k l = case l of
  []            -> Nothing
  (k0,v0) :: l' -> if | k == k0   -> Just v0
                      | otherwise -> maybeFind k l'

zip : List a -> List b -> List (a,b)
zip xs ys = case (xs, ys) of
  (x::xs', y::ys') -> (x,y) :: zip xs' ys'
  _                -> []

maybeZip : List a -> List b -> Maybe (List (a,b))
maybeZip xs ys = case (xs, ys) of
  (x::xs', y::ys') -> case maybeZip xs' ys' of
                        Nothing  -> Nothing
                        Just xys -> Just ((x,y) :: xys)
  ([], [])         -> Just []
  _                -> Nothing

mapi : ((Int, a) -> b) -> List a -> List b
mapi f xs =
  let n = List.length xs in
  List.map f (zip [1..n] xs)

split : Int -> List a -> (List a, List a)
split n xs = (List.take n xs, List.drop n xs)

delimit a b s = String.concat [a, s, b]

parens = delimit "(" ")"
bracks = delimit "[" "]"
braces = delimit "{" "}"

spaces = String.join " "
commas = String.join ", "
lines  = String.join "\n"

sum = List.foldl (+) 0

lift_2_2 f (a,b) (c,d) = (f a c, f b d)


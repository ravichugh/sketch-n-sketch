module Utils where

import List ((::))
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

delimit a b s = String.concat [a, s, b]

parens = delimit "(" ")"
bracks = delimit "[" "]"
braces = delimit "{" "}"


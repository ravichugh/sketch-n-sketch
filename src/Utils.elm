module Utils where

import String
import Debug
import Set
import Dict

maybeFind : a -> List (a,b) -> Maybe b
maybeFind k l = case l of
  []            -> Nothing
  (k0,v0) :: l' -> if | k == k0   -> Just v0
                      | otherwise -> maybeFind k l'

find err d k =
  case maybeFind k d of
    Just f  -> f
    Nothing -> Debug.crash <| "Utils.find: " ++ err

find_ = find ""

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

foldli : ((Int, a) -> b -> b) -> b -> List a -> b
foldli f init xs =
  let n = List.length xs in
  List.foldl f init (zip [1..n] xs)

split : Int -> List a -> (List a, List a)
split n xs = (List.take n xs, List.drop n xs)

oneOfEach : List (List a) -> List (List a)
oneOfEach xss = case xss of
  []       -> [[]]
  xs::xss' -> List.concatMap (\x -> List.map ((::) x) (oneOfEach xss')) xs

-- given [s1, ..., sn], compute s1' x ... x sn' where
--   s1'  =  s1 - s2 - s3 - ... - sn
--   s2'  =  s2 - s1 - s3 - ... - sn
--       ...
--   sn'  =  sn - s1 - s2 - ... - s(n-1)
--
cartProdWithDiff : List (Set.Set comparable) -> List (List comparable)
cartProdWithDiff = oneOfEach << List.map Set.toList << manySetDiffs

manySetDiffs : List (Set.Set comparable) -> List (Set.Set comparable)
manySetDiffs sets =
  mapi (\(i,locs_i) ->
    foldli (\(j,locs_j) acc ->
      if | i == j    -> acc
         | otherwise -> acc `Set.diff` locs_j
    ) locs_i sets
  ) sets

-- TODO combine findFirst and removeFirst

findFirst : (a -> Bool) -> List a -> Maybe a
findFirst p xs = case xs of
  []     -> Nothing
  x::xs' -> if | p x       -> Just x
               | otherwise -> findFirst p xs'

removeFirst : a -> List a -> List a
removeFirst x ys = case ys of
  []     -> []
  y::ys' -> if x == y then ys' else y :: removeFirst x ys'

delimit a b s = String.concat [a, s, b]

parens = delimit "(" ")"
bracks = delimit "[" "]"
braces = delimit "{" "}"

spaces = String.join " "
commas = String.join ", "
lines  = String.join "\n"

sum = List.foldl (+) 0

lift_2_2 f (a,b) (c,d) = (f a c, f b d)

assert s b = if b then () else Debug.crash ("assert error: " ++ s)

fromJust (Just x) = x

fromJust_ s mx = case mx of
  Just x  -> x
  Nothing -> Debug.crash <| "Utils.fromJust_: " ++ s

fromOk s mx = case mx of
  Ok x    -> x
  Err err -> Debug.crash <| "fromOk [" ++ s ++ "]: " ++ err

fromOk_ = fromOk ""

mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f mx = case mx of {Just x -> Just (f x); Nothing -> Nothing}

bindMaybe : (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f mx = case mx of {Just x -> f x; Nothing -> Nothing}

mapSnd : (b -> b') -> (a, b) -> (a, b')
mapSnd f (x,y) = (x, f y)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

setIsEmpty  = (==) [] << Set.toList
dictIsEmpty = (==) [] << Dict.toList


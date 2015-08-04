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

find_ d k = find ("[" ++ toString k ++ "]") d k

update : (comparable, v) -> List (comparable, v) -> List (comparable, v)
update (k1, v1) vals =
  case vals of
    [] -> []
    (k0, v0) :: vs ->
      if | k0 == k1  -> (k0, v1) :: vs
         | otherwise -> (k0, v0) :: update (k1, v1) vs

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

foldri f init xs = List.reverse (foldli f init xs)

reverse2 (xs,ys) = (List.reverse xs, List.reverse ys)

clamp i j n =
  if | n < i     -> i
     | j < n     -> j
     | otherwise -> n

singleton : a -> List a
singleton x = [x]

split : Int -> List a -> (List a, List a)
split n xs = (List.take n xs, List.drop n xs)

splitString : Int -> String -> (String, String)
splitString n s = (String.left n s, String.dropLeft n s)

munchString : String -> String -> Maybe String
munchString prefix s =
  let (pre,suf) = splitString (String.length prefix) s in
  if | pre == prefix -> Just suf
     | otherwise     -> Nothing

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

intersectMany : List (Set.Set comparable) -> Set.Set comparable
intersectMany (set::sets) = List.foldl Set.intersect set sets

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

maybeRemoveFirst : a -> List (a,b) -> Maybe (b, List (a,b))
maybeRemoveFirst x ys = case ys of
  []         -> Nothing
  (a,b)::ys' -> if
    | x == a    -> Just (b, ys')
    | otherwise -> case maybeRemoveFirst x ys' of
                     Nothing      -> Nothing
                     Just (b', l) -> Just (b', (a,b) :: l)

adjacentPairs : Bool -> List a -> List (a, a)
adjacentPairs includeLast (x0::xs) =
  let f xi (xPrev,acc) = (xi, (xPrev,xi) :: acc) in
  let (xn,pairs) = List.foldl f (x0,[]) xs in
  if | includeLast -> List.reverse ((xn,x0) :: pairs)
     | otherwise   -> List.reverse (pairs)

-- 1-based
geti : Int -> List a -> a
geti i = fromJust_ "Utils.geti" << List.head << List.drop (i-1)

delimit a b s = String.concat [a, s, b]

parens = delimit "(" ")"
bracks = delimit "[" "]"
ibracks = delimit "[|" "|]"
braces = delimit "{" "}"

spaces = String.join " "
commas = String.join ", "
lines  = String.join "\n"

sum = List.foldl (+) 0

lift_2_2 f (a,b) (c,d) = (f a c, f b d)

assert s b = if b then () else Debug.crash ("assert error: " ++ s)

fromJust m = case m of
  Just x -> x
  Nothing -> Debug.crash <| "Utils.fromJust: Nothing"

fromJust_ s mx = case mx of
  Just x  -> x
  Nothing -> Debug.crash <| "Utils.fromJust_: " ++ s

fromOk s mx = case mx of
  Ok x    -> x
  Err err -> Debug.crash <| "fromOk [" ++ s ++ "]: " ++ err

fromOk_ = fromOk ""

justGet k d = fromJust_ "Utils.justGet" <| Dict.get k d

justGet_ s k d = fromJust_ ("Utils.justGet " ++ s) <| Dict.get k d

head_ = fromJust_ "Utils.head_" << List.head
tail_ = fromJust_ "Utils.tail_" << List.tail

mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f mx = case mx of {Just x -> Just (f x); Nothing -> Nothing}

bindMaybe : (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f mx = case mx of {Just x -> f x; Nothing -> Nothing}

plusMaybe : Maybe a -> Maybe a -> Maybe a
plusMaybe mx my = case mx of {Just _ -> mx; Nothing -> my}

projJusts : List (Maybe a) -> Maybe (List a)
projJusts =
  List.foldr (\mx acc ->
    case (mx, acc) of
      (Just x, Just xs) -> Just (x::xs)
      _                 -> Nothing) (Just [])

mapSnd : (b -> b') -> (a, b) -> (a, b')
mapSnd f (x,y) = (x, f y)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

mapThd3 f (x,y,z) = (x, y, f z)

fourth4 (_,_,_,x) = x

setIsEmpty  = (==) [] << Set.toList
dictIsEmpty = (==) [] << Dict.toList
setCardinal = List.length << Set.toList

x `between` (a,b) = a <= x && x < b

-- n:number -> i:[0,n) -> RGB
numToColor n i =
  let j = round <| (i/n) * 500 in
  if | j `between` (  0, 360) -> numToColor_ j   -- color
     | j `between` (360, 380) -> (0, 0, 0)       -- black
     | j `between` (480, 500) -> (255, 255, 255) -- white
     | otherwise ->                              -- grayscale
         let x =
           round <| 255 * ((toFloat j - 380) / 100) in (x,x,x)

-- Maps a value in [0,360) to an RGB value that lies on the rainbow.
-- Note that you cannot get white or black from this.
-- Note that the choice of 0 to 360 implies degrees as a natural representation.
numToColor_ : number -> (Int, Int, Int)
numToColor_ val =
    let n    = toFloat <| val % 360
        i    = floor n // 60
        max  = 200
        min  = 55
        diff = max - min
    in if | i == 0 -> (max, round <| min + diff * (1 - (60 - n) / 60),  min)
          | i == 1 -> (round <| max - diff * (1 - (120 - n) / 60), max, min)
          | i == 2 -> (min, max, round <| min + diff * (1 - (180 - n) / 60))
          | i == 3 -> (min, round <| max - diff * (1 - (240 - n) / 60), max)
          | i == 4 -> (round <| min + diff * (1 - (300 - n) / 60), min, max)
          | i == 5 -> (max, min, round <| max - diff * (1 - (360 - n) / 60))

radiansToDegrees : Float -> Float
radiansToDegrees rad = (rad / pi) * 180

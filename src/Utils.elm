module Utils where

import String
import Debug
import Set exposing (Set)
import Dict exposing (Dict)
import Regex

flipTuple (a, b) = (b, a)

maybeFind : a -> List (a,b) -> Maybe b
maybeFind k l = case l of
  []            -> Nothing
  (k0,v0) :: l' -> if k == k0
                     then Just v0
                     else maybeFind k l'

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
      if k0 == k1
        then (k0, v1) :: vs
        else (k0, v0) :: update (k1, v1) vs

-- Extra elements left off if the lists are different lengths.
-- Resulting list length is minimum of (length xs, length ys)
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

maybeZipDicts : Dict comparable b -> Dict comparable c -> Maybe (Dict comparable (b,c))
maybeZipDicts d1 d2 =
  if Dict.keys d1 /= Dict.keys d2 then
    Nothing
  else
    d1 |> Dict.map (\k v1 -> (v1, justGet k d2)) |> Just

unzip3 : List (a, b, c) -> (List a, List b, List c)
unzip3 zipped =
  List.foldr
      (\(x, y, z) (xs, ys, zs) -> (x::xs, y::ys, z::zs))
      ([], [], [])
      zipped



listsEqualBy elementEqualityFunc xs ys =
  case (xs, ys) of
    ([], [])         -> True
    (x::xs', y::ys') -> (elementEqualityFunc x y) && (listsEqualBy elementEqualityFunc xs' ys')
    _                -> False -- Lists not the same length

zeroElements xs = case xs of
  [] -> True
  _  -> False

oneElement xs = case xs of
  [_] -> True
  _   -> False

maybeZipN : List (List a) -> Maybe (List (List a))
maybeZipN lists =
  if List.all zeroElements lists then
    Just []
  else if List.all oneElement lists then
    Just [List.concat lists]
  else
    let maybeHeads = projJusts (List.map List.head lists) in
    let maybeTails = projJusts (List.map List.tail lists) in
    case (maybeHeads, maybeTails) of
      (Just heads, Just tails) -> mapMaybe ((::) heads) (maybeZipN tails)
      _                        -> Nothing

mapi : ((Int, a) -> b) -> List a -> List b
mapi f xs =
  let n = List.length xs in
  List.map f (zip [1..n] xs)

foldli : ((Int, a) -> b -> b) -> b -> List a -> b
foldli f init xs =
  let n = List.length xs in
  List.foldl f init (zip [1..n] xs)

foldri f init xs = List.reverse (foldli f init xs)

-- three passes, oh well
filteri : ((Int, a) -> Bool) -> List a -> List a
filteri f xs =
  let n = List.length xs in
  List.map snd (List.filter f (zip [1..n] xs))

reverse2 (xs,ys) = (List.reverse xs, List.reverse ys)

-- In:  [1, 2, 3]
-- Out: [(1,2), (2,3), (3,1)]
selfZipCircConsecPairs : List a -> List (a, a)
selfZipCircConsecPairs list =
  let shiftList =
    case list of
      x::xs -> xs ++ [x]
      _     -> []
  in
  zip list shiftList

-- Preserves original list order
-- Dedups based on toString representation
dedup : List a -> List a
dedup xs = dedup_ (toString) xs

-- Preserves original list order
-- Dedups based on a provided function
dedup_ : (a -> comparable) -> List a -> List a
dedup_ f xs =
  let (deduped, _) =
    List.foldl (\x (dd, seen) ->
        if Set.member (f x) seen then (dd, seen) else (List.append dd [x], Set.insert (f x) seen)
      ) ([], Set.empty) xs
  in
    deduped

listDiffSet : List comparable -> Set.Set comparable -> List comparable
listDiffSet list setToRemove =
  List.filter (\element -> not <| Set.member element setToRemove) list

listDiff : List comparable -> List comparable -> List comparable
listDiff l1 l2 =
  listDiffSet l1 (Set.fromList l2)

groupBy : (a -> comparable) -> List a -> Dict.Dict comparable (List a)
groupBy f xs =
  List.foldl
      (\x dict ->
        let key = f x in
        let equivalents = getWithDefault key [] dict in
        Dict.insert key (equivalents ++ [x]) dict
      )
      Dict.empty
      xs

-- Is there a one-to-one mapping from the elements in l1 to the elements in l2?
--
-- e.g oneToOneMappingExists ["a", "b", "a"] ["z", "y", "z"] => True
--     oneToOneMappingExists ["a", "b", "b"] ["z", "y", "z"] => False
oneToOneMappingExists l1 l2 =
  let numericRepresentation list =
    List.foldl
        (\x (dict, numRep) ->
          case Dict.get x dict of
            Just n  ->
              (dict, numRep ++ [n])
            Nothing ->
              let n     = Dict.size dict in
              let dict' = Dict.insert x n dict in
              (dict', numRep ++ [n])
        )
        (Dict.empty, [])
        list
    |> snd
  in
  numericRepresentation l1 == numericRepresentation l2

clamp i j n =
  if n < i then      i
  else if j < n then j
  else               n

singleton : a -> List a
singleton x = [x]

split : Int -> List a -> (List a, List a)
split n xs = (List.take n xs, List.drop n xs)

splitString : Int -> String -> (String, String)
splitString n s = (String.left n s, String.dropLeft n s)

munchString : String -> String -> Maybe String
munchString prefix s =
  let (pre,suf) = splitString (String.length prefix) s in
  if pre == prefix
    then Just suf
    else Nothing

takeNLines : Int -> String -> String
takeNLines n s =
  String.lines s |> List.take n |> String.join "\n"

-- Replace all runs of whitespace with a single space
squish : String -> String
squish str =
  String.trim <|
    Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> " ") str

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
intersectMany list = case list of
  set::sets -> List.foldl Set.intersect set sets
  []        -> Debug.crash "intersectMany"

manySetDiffs : List (Set.Set comparable) -> List (Set.Set comparable)
manySetDiffs sets =
  mapi (\(i,locs_i) ->
    foldli (\(j,locs_j) acc ->
      if i == j
        then acc
        else acc `Set.diff` locs_j
    ) locs_i sets
  ) sets

unionAll : List (Set.Set comparable) -> Set.Set comparable
unionAll sets =
  List.foldl Set.union Set.empty sets

-- TODO combine findFirst and removeFirst

findFirst : (a -> Bool) -> List a -> Maybe a
findFirst p xs = case xs of
  []     -> Nothing
  x::xs' -> if p x
              then Just x
              else findFirst p xs'

removeFirst : a -> List a -> List a
removeFirst x ys = case ys of
  []     -> []
  y::ys' -> if x == y then ys' else y :: removeFirst x ys'

maybeRemoveFirst : a -> List (a,b) -> Maybe (b, List (a,b))
maybeRemoveFirst x ys = case ys of
  []         -> Nothing
  (a,b)::ys' ->
    if x == a
      then Just (b, ys')
      else case maybeRemoveFirst x ys' of
             Nothing      -> Nothing
             Just (b', l) -> Just (b', (a,b) :: l)

removeLastElement : List a -> List a
removeLastElement list =
  List.take ((List.length list)-1) list

-- Equivalent to Maybe.oneOf (List.map f list)
-- but maps the list lazily to return early
mapFirstSuccess : (a -> Maybe b) -> List a -> Maybe b
mapFirstSuccess f list =
  case list of
    []   -> Nothing
    x::xs ->
      case f x of
        Just result -> Just result
        Nothing     -> mapFirstSuccess f xs

orTry : Maybe a -> (() -> Maybe a) -> Maybe a
orTry maybe1 lazyMaybe2 =
  case maybe1 of
    Just x  -> maybe1
    Nothing -> lazyMaybe2 ()

adjacentPairs : Bool -> List a -> List (a, a)
adjacentPairs includeLast list = case list of
  [] -> []
  x0::xs ->
    let f xi (xPrev,acc) = (xi, (xPrev,xi) :: acc) in
    let (xn,pairs) = List.foldl f (x0,[]) xs in
    if includeLast
      then List.reverse ((xn,x0) :: pairs)
      else List.reverse (pairs)

-- 1-based
geti : Int -> List a -> a
geti i = fromJust_ "Utils.geti" << List.head << List.drop (i-1)

allSame list = case list of
  []    -> False
  v::vs -> List.filter ((/=) v) vs == []

removeDupes : List comparable -> List comparable
removeDupes = Set.toList << Set.fromList

maybeConsensus : List Bool -> Maybe Bool
maybeConsensus bools =
  if List.all ((==) True) bools then
    Just True
  else if List.all ((==) False) bools then
    Just False
  else
    Nothing

count : (a -> Bool) -> List a -> Int
count pred list =
  List.filter pred list |> List.length

delimit a b s = String.concat [a, s, b]

parens = delimit "(" ")"
bracks = delimit "[" "]"
ibracks = delimit "[|" "|]"
braces = delimit "{" "}"

spaces = String.join " "
commas = String.join ", "
lines  = String.join "\n"

sum = List.foldl (+) 0

avg : List Float -> Float
avg ns = List.sum ns / toFloat (List.length ns)

lift_2_2 f (a,b) (c,d) = (f a c, f b d)

assert s b = if b then () else Debug.crash ("assert error: " ++ s)

fromJust m = case m of
  Just x -> x
  Nothing -> Debug.crash <| "Utils.fromJust: Nothing"

fromJust_ s mx = case mx of
  Just x  -> x
  Nothing -> Debug.crash <| "Utils.fromJust_: " ++ s

fromOkay : String -> Result err a -> a
fromOkay s mx = case mx of
  Ok x  -> x
  Err _ -> Debug.crash <| "fromOkay [" ++ s ++ "]: "

-- TODO rename fromOkay and fromOk to fromOk and fromOkOrErrString

fromOk : String -> Result String a -> a
fromOk s mx = case mx of
  Ok x    -> x
  Err err -> Debug.crash <| "fromOk [" ++ s ++ "]: " ++ err

fromOk_ = fromOk ""

justGet k d = fromJust_ "Utils.justGet" <| Dict.get k d

justGet_ s k d = fromJust_ ("Utils.justGet " ++ s) <| Dict.get k d

getWithDefault key default dict =
  case Dict.get key dict of
    Just val -> val
    Nothing -> default

toggleSet : comparable -> Set comparable -> Set comparable
toggleSet x set =
  if Set.member x set then Set.remove x set else Set.insert x set

toggleDict : (comparable, v) -> Dict comparable v -> Dict comparable v
toggleDict (k,v) dict =
  if Dict.member k dict then Dict.remove k dict else Dict.insert k v dict

head msg = fromJust_ msg << List.head
last msg = fromJust_ msg << List.head << List.reverse
head_ = head "Utils.head_"
tail_ = fromJust_ "Utils.tail_" << List.tail
last_ = last "Utils.last_"

uncons xs = case xs of
  x::xs -> (x, xs)
  []    -> Debug.crash "uncons"

mapMaybe : (a -> b) -> Maybe a -> Maybe b
mapMaybe f mx = case mx of
  Just x  -> Just (f x)
  Nothing -> Nothing

bindMaybe : (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe f mx = case mx of
  Just x  -> f x
  Nothing -> Nothing

plusMaybe : Maybe a -> Maybe a -> Maybe a
plusMaybe mx my = case mx of
  Just _  -> mx
  Nothing -> my

elseMaybe : Maybe a -> a -> a
elseMaybe mx default = case mx of
  Just x  -> x
  Nothing -> default

projJusts : List (Maybe a) -> Maybe (List a)
projJusts =
  List.foldr (\mx acc ->
    case (mx, acc) of
      (Just x, Just xs) -> Just (x::xs)
      _                 -> Nothing) (Just [])

filterJusts : List (Maybe a) -> List a
filterJusts mxs = case mxs of
  []              -> []
  Just x  :: rest -> x :: filterJusts rest
  Nothing :: rest -> filterJusts rest

bindMaybe2 : (a -> b -> Maybe c) -> Maybe a -> Maybe b -> Maybe c
bindMaybe2 f mx my = bindMaybe (\x -> bindMaybe (f x) my) mx

bindMaybe3 : (a -> b -> c -> Maybe d) -> Maybe a -> Maybe b -> Maybe c -> Maybe d
bindMaybe3 f mx my mz = bindMaybe2 (\x y -> bindMaybe (f x y) mz) mx my

-- Returns Nothing if function ever returns Nothing
foldlMaybe : (a -> b -> Maybe b) -> Maybe b -> List a -> Maybe b
foldlMaybe f maybeAcc list =
  case (maybeAcc, list) of
    (Nothing, _)      -> Nothing
    (Just acc, x::xs) -> foldlMaybe f (f x acc) xs
    (_, [])           -> maybeAcc


projOk : List (Result a b) -> Result a (List b)
projOk list =
  List.foldr
      (\res out ->
        case (res, out) of
          (_, Err _)         -> out
          (Ok elem, Ok tail) -> Ok (elem::tail)
          (Err s, _)         -> Err s
      )
      (Ok [])
      list

mapFst : (a -> a') -> (a, b) -> (a', b)
mapFst f (a, b) = (f a, b)

mapSnd : (b -> b') -> (a, b) -> (a, b')
mapSnd f (a, b) = (a, f b)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

mapThd3 f (x,y,z) = (x, y, f z)

fourth4 (_,_,_,x) = x

bindResult : Result a b -> (b -> Result a b') -> Result a b'
bindResult res f =
  case res of
    Err a -> Err a
    Ok b  -> f b

setIsEmpty  = (==) [] << Set.toList
dictIsEmpty = (==) [] << Dict.toList
setCardinal = List.length << Set.toList

parseInt   = fromOk_ << String.toInt
parseFloat = fromOk_ << String.toFloat

-- Common elements shared at the beginning of each list
commonPrefix : List (List a) -> List a
commonPrefix lists =
  case lists of
    first::rest -> List.foldl commonPrefix2 first rest
    []          -> []

commonPrefix2 : List a -> List a -> List a
commonPrefix2 l1 l2 =
  case (l1, l2) of
    (x::xs, y::ys) -> if x == y then x::(commonPrefix2 xs ys) else []
    _              -> []

between x (a,b) = a <= x && x < b

distance (x1,y1) (x2,y2) = sqrt <| (x2-x1)^2 + (y2-y1)^2

distanceInt (x1,y1) (x2,y2) =
  distance (toFloat x1, toFloat y1) (toFloat x2, toFloat y2)

type alias MaybeOne a = List a
nothing               = []
just                  = singleton
maybeToMaybeOne mx    = case mx of
  Nothing -> nothing
  Just x  -> just x

-- n:number -> i:[0,n) -> RGB
numToColor n i =
  let j = round <| (i/n) * 500 in
  if j `between` (  0, 360) then numToColor_ j   else -- color
  if j `between` (360, 380) then (0, 0, 0)       else -- black
  if j `between` (480, 500) then (255, 255, 255)      -- white
  else                                                -- grayscale
    let x =
    round <| 255 * ((toFloat j - 380) / 100) in (x,x,x)

-- Maps a value in [0,360) to an RGB value that lies on the rainbow.
-- Note that you cannot get white or black from this.
-- Note that the choice of 0 to 360 implies degrees as a natural representation.
numToColor_ : Int -> (Int, Int, Int)
numToColor_ val =
    let n    = toFloat <| val % 360
        i    = floor n // 60
        max  = 200
        min  = 55
        diff = max - min
    in
     case i of
       0 -> (max, round <| min + diff * (1 - (60 - n) / 60),  min)
       1 -> (round <| max - diff * (1 - (120 - n) / 60), max, min)
       2 -> (min, max, round <| min + diff * (1 - (180 - n) / 60))
       3 -> (min, round <| max - diff * (1 - (240 - n) / 60), max)
       4 -> (round <| min + diff * (1 - (300 - n) / 60), min, max)
       5 -> (max, min, round <| max - diff * (1 - (360 - n) / 60))
       _ -> Debug.crash "numToColor"

radiansToDegrees : Float -> Float
radiansToDegrees rad = (rad / pi) * 180

--------------------------------------------------------------------------------
-- Unicode

-- http://unicode-table.com/en/sets/arrows-symbols/

uniLeft        = "←"
uniRight       = "→"
uniEnter       = "↵"
-- uniEnter    = "⏎"
uniSave        = "💾"
-- uniUndo     = "↶"
-- uniRedo     = "↷"
-- uniReload   = "⟲"
uniUndo        = "◀"
uniRedo        = "▶"
uniReload      = "⎋"
-- uniStar     = "✴"
uniCamera      = "📷"
uniLambda      = "λ"
uniDelta       = "Δ"
uniPlusMinus   = "±"


--------------------------------------------------------------------------------

unwrap1 xs = case xs of
  [x1] -> x1
  _ -> Debug.crash "unwrap1"

unwrap2 xs = case xs of
  [x1,x2] -> (x1, x2)
  _ -> Debug.crash "unwrap2"

unwrap3 xs = case xs of
  [x1,x2,x3] -> (x1, x2, x3)
  _ -> Debug.crash "unwrap3"

unwrap4 xs = case xs of
  [x1,x2,x3,x4] -> (x1, x2, x3, x4)
  _ -> Debug.crash "unwrap4"

unwrap5 xs = case xs of
  [x1,x2,x3,x4,x5] -> (x1, x2, x3, x4, x5)
  _ -> Debug.crash "unwrap5"

unwrap6 xs = case xs of
  [x1,x2,x3,x4,x5,x6] -> (x1, x2, x3, x4, x5, x6)
  _ -> Debug.crash "unwrap6"

unwrap7 xs = case xs of
  [x1,x2,x3,x4,x5,x6,x7] -> (x1, x2, x3, x4, x5, x6, x7)
  _ -> Debug.crash "unwrap7"

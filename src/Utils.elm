module Utils exposing (..)

import String
import Debug
import Set exposing (Set)
import Dict exposing (Dict)
import Regex

infinity = 1/0

-- Change e.g. 1.4999999999999 to 1.5.
correctFloatError : Float -> Float
correctFloatError x =
  if x == 0.0 then
    x
  else
    let tens = -(logBase 10 (abs x) |> round |> toFloat) in
    let multiplier = 2*2*2*3*3*5*5*7*11*13*17*19*23*(10.0^(tens + 1)) in
    let corrected = (x*multiplier |> round |> toFloat) / multiplier in
    if abs (corrected - x) /  x < 0.0000001
    then corrected
    else x


maybeFind : a -> List (a,b) -> Maybe b
maybeFind k l = case l of
  []            -> Nothing
  (k0,v0) :: l_ -> if k == k0
                     then Just v0
                     else maybeFind k l_

find err d k =
  case maybeFind k d of
    Just f  -> f
    Nothing -> Debug.crash <| "Utils.find: " ++ err

find_ d k = find ("[" ++ toString k ++ "]") d k

update : (k, v) -> List (k, v) -> List (k, v)
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
zip = zipWith (,)

zipWith f xs ys = case (xs, ys) of
  (x::xs_, y::ys_) -> f x y :: zipWith f xs_ ys_
  _                -> []

maybeZip : List a -> List b -> Maybe (List (a,b))
maybeZip xs ys = case (xs, ys) of
  (x::xs_, y::ys_) -> case maybeZip xs_ ys_ of
                        Nothing  -> Nothing
                        Just xys -> Just ((x,y) :: xys)
  ([], [])         -> Just []
  _                -> Nothing

zipAndLeftovers : List a -> List b -> (List (a,b), List a, List b)
zipAndLeftovers list1 list2 =
  case (list1, list2) of
    (x::xs, y::ys) -> mapFst3 ((::) (x, y)) (zipAndLeftovers xs ys)
    (xs,    [])    -> ([], xs, [])
    ([],    ys)    -> ([], [], ys)

maybeZipDicts : Dict a b -> Dict a c -> Maybe (Dict a (b,c))
maybeZipDicts d1 d2 =
  if Dict.keys d1 /= Dict.keys d2 then
    Nothing
  else
    d1 |> Dict.map (\k v1 -> (v1, justGet k d2)) |> Just

-- Extra elements left off if the lists are different lengths.
-- Resulting list length is minimum of (length xs, length ys, length zs)
zip3 : List a -> List b -> List c -> List (a,b,c)
zip3 xs ys zs = case (xs, ys, zs) of
  (x::xs_, y::ys_, z::zs_) -> (x,y,z) :: zip3 xs_ ys_ zs_
  _                        -> []

unzip3 : List (a, b, c) -> (List a, List b, List c)
unzip3 zipped =
  List.foldr
      (\(x, y, z) (xs, ys, zs) -> (x::xs, y::ys, z::zs))
      ([], [], [])
      zipped


-- Tranpose, essentially.
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
      (Just heads, Just tails) -> Maybe.map ((::) heads) (maybeZipN tails)
      _                        -> Nothing


zipi0 : List a -> List (Int, a)
zipi0 = zipi_ 0

zipi1 : List a -> List (Int, a)
zipi1 = zipi_ 1

zipi_ : Int -> List a -> List (Int, a)
zipi_ i list =
  case list of
    []    -> []
    x::xs -> (i, x) :: zipi_ (i+1) xs

mapi0 : ((Int, a) -> b) -> List a -> List b
mapi0 = mapi_ 0

mapi1 : ((Int, a) -> b) -> List a -> List b
mapi1 = mapi_ 1

mapi_ : Int -> ((Int, a) -> b) -> List a -> List b
mapi_ initI f xs =
  List.map f (zipi_ initI xs)

foldli0 : ((Int, a) -> b -> b) -> b -> List a -> b
foldli0 = foldli_ 0

foldli1 : ((Int, a) -> b -> b) -> b -> List a -> b
foldli1 = foldli_ 1

foldli_ : Int -> ((Int, a) -> b -> b) -> b -> List a -> b
foldli_ initI f init xs =
  List.foldl f init (zipi_ initI xs)

-- three passes, oh well
filteri1 : ((Int, a) -> Bool) -> List a -> List a
filteri1 f xs =
  List.map Tuple.second (List.filter f (zipi1 xs))

concatMapi1 f xs =
  List.concatMap f (zipi1 xs)

reverse2 (xs,ys) = (List.reverse xs, List.reverse ys)

-- If lists are different lengths, extra elements preserved changed.
filterMapTogetherPreservingLeftovers : (a -> b -> Maybe (a, b)) -> List a -> List b -> (List a, List b)
filterMapTogetherPreservingLeftovers f l1 l2 =
  case (l1, l2) of
    (x::xRest, y::yRest) ->
      case f x y of
        Just (xNew, yNew) ->
          let (newXRest, newYRest) = filterMapTogetherPreservingLeftovers f xRest yRest in
          (xNew::newXRest, yNew::newYRest)

        Nothing ->
          filterMapTogetherPreservingLeftovers f xRest yRest

    _ ->
      (l1, l2)


listsEqualBy elementEqualityFunc xs ys =
  case (xs, ys) of
    ([], [])         -> True
    (x::xs_, y::ys_) -> (elementEqualityFunc x y) && (listsEqualBy elementEqualityFunc xs_ ys_)
    _                -> False -- Lists not the same length


-- Preserves original list order
dedup : List a -> List a
dedup xs = dedupBy identity xs

-- Preserves original list order
-- Dedups based on a provided function (first seen element for each key is preserved)
dedupBy : (a -> b) -> List a -> List a
dedupBy f xs =
  let (deduped, _) =
    List.foldl (\x (dd, seen) ->
        let key = f x in
        if Set.member key seen then (dd, seen) else (dd ++ [x], Set.insert key seen)
      ) ([], Set.empty) xs
  in
  deduped

-- -- O(n^2). Elements do not need to be comparable.
-- Shouldn't need now that sets can hold anything.
-- dedup : List a -> List a
-- dedup xs =
--   List.foldr addAsSet [] xs

listDiffSet : List a -> Set a -> List a
listDiffSet list setToRemove =
  List.filter (\element -> not <| Set.member element setToRemove) list

listDiff : List a -> List a -> List a
listDiff l1 l2 =
  listDiffSet l1 (Set.fromList l2)

addAsSet : a -> List a -> List a
addAsSet x xs =
  if List.member x xs
  then xs
  else x::xs

addAllAsSet : List a -> List a -> List a
addAllAsSet xs ys =
  ys |> List.foldl addAsSet xs

removeAsSet : a -> List a -> List a
removeAsSet x xs =
  List.filter ((/=) x) xs

-- O(nm)
diffAsSet : List a -> List a -> List a
diffAsSet xs ys =
  ys |> List.foldl removeAsSet xs

-- O(nm)
intersectAsSet : List a -> List a -> List a
intersectAsSet xs ys =
  xs |> List.filter (\x -> List.member x ys)

intersectAllAsSet : List (List a) -> List a
intersectAllAsSet lists =
  case lists of
    first::rest -> List.foldl intersectAsSet first rest
    _           -> []

unionAllAsSet : List (List a) -> List a
unionAllAsSet lists =
  List.foldl addAllAsSet [] lists

removeAll = diffAsSet

-- O(nm)
isSublistAsSet : List a -> List a -> Bool
isSublistAsSet sub super =
  sub |> List.all (\x -> List.member x super)

-- O(n^2)
equalAsSets : List a -> List a -> Bool
equalAsSets a b =
  isSublistAsSet a b && isSublistAsSet b a

-- Assumes list already deduplicated.
combinationsAsSet : Int -> List a -> List (List a)
combinationsAsSet n list =
  case (n, list) of
    (0, _)     -> [[]]
    (_, [])    -> []
    (_, x::xs) -> List.map ((::) x) (combinationsAsSet (n-1) xs) ++ combinationsAsSet n xs -- combinations that include x ++ those that don't


groupBy : (a -> b) -> List a -> Dict.Dict b (List a)
groupBy f list =
  list
  |> List.map (\x -> (f x, x))
  |> pairsToDictOfLists

pairsToDictOfLists : List (a, b) -> Dict.Dict a (List b)
pairsToDictOfLists pairs =
  pairs
  |> List.foldr
      (\(key, val) dict ->
        let equivalents = getWithDefault key [] dict in
        Dict.insert key (val::equivalents) dict
      )
      Dict.empty

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
              let dict_ = Dict.insert x n dict in
              (dict_, numRep ++ [n])
        )
        (Dict.empty, [])
        list
    |> Tuple.second
  in
  numericRepresentation l1 == numericRepresentation l2


clamp i j n =
  if n < i then      i
  else if j < n then j
  else               n


zeroElements xs = case xs of
  [] -> True
  _  -> False

oneElement xs = case xs of
  [_] -> True
  _   -> False

maybeUnpackSingleton xs = case xs of
  [x] -> Just x
  _   -> Nothing

snoc : List a -> a -> List a
snoc xs x = xs ++ [x]

snocMaybe : List a -> Maybe a -> List a
snocMaybe xs mx = Maybe.withDefault xs (mapMaybe (snoc xs) mx)

split : Int -> List a -> (List a, List a)
split n xs = (List.take n xs, List.drop n xs)

-- Like String.split, but for lists.
--
-- Follows Ruby semantics for splitting, just because they never did me wrong.
--
-- splitBy [s] [s]       => []
-- splitBy [s] [s, b]    => [[], [b]]
-- splitBy [s] [a, s]    => [[a]]
-- splitBy [s] [a, s, s] => [[a]]
-- splitBy [s] [a, s, b] => [[a], [b]]
--
-- CORRECTION: RUBY HAS DONE ME WRONG.
--
-- This follows Python and does not ignore trailing separators:
--
-- splitBy [s] [s]       => [[], []]
-- splitBy [s] [s, b]    => [[], [b]]
-- splitBy [s] [a, s]    => [[a], []]
-- splitBy [s] [a, s, s] => [[a], [], []]
-- splitBy [s] [a, s, b] => [[a], [b]]
splitBy : List a -> List a -> List (List a)
splitBy splitElems list =
  case findSublistIndex splitElems list of
    Just i  -> (List.take i list) :: splitBy splitElems (List.drop (i + List.length splitElems) list)
    Nothing -> [list]

dropLast : Int -> List a -> List a
dropLast n list =
  list
  |> List.reverse
  |> List.drop n
  |> List.reverse

slice : Int -> Int -> List a -> List a
slice start end list =
  list
    |> List.drop start
    |> List.take (end - start)

cartProd : List a -> List b -> List (a, b)
cartProd xs ys =
   xs |> List.concatMap (\x -> List.map ((,) x) ys)

-- Cartesian product for arbitrary many lists of the same type
oneOfEach : List (List a) -> List (List a)
oneOfEach xss = case xss of
  []       -> [[]]
  xs::xss_ -> List.concatMap (\x -> List.map ((::) x) (oneOfEach xss_)) xs

-- given [s1, ..., sn], compute s1_ x ... x sn_ where
--   s1'  =  s1 - s2 - s3 - ... - sn
--   s2'  =  s2 - s1 - s3 - ... - sn
--       ...
--   sn'  =  sn - s1 - s2 - ... - s(n-1)
--
cartProdWithDiff : List (Set a) -> List (List a)
cartProdWithDiff = oneOfEach << List.map Set.toList << manySetDiffs

isSubset : Set a -> Set a -> Bool
isSubset sub sup =
  sub
  |> Set.toList
  |> List.all (\elem -> Set.member elem sup)

intersectMany : List (Set a) -> Set a
intersectMany list = case list of
  set::sets -> List.foldl Set.intersect set sets
  []        -> Debug.crash "intersectMany"

-- Leave behind the elements unique to each set.
manySetDiffs : List (Set a) -> List (Set a)
manySetDiffs sets =
  mapi1 (\(i,ithSet) ->
    foldli1 (\(j,jthSet) acc ->
      if i == j
        then acc
        else Set.diff acc jthSet
    ) ithSet sets
  ) sets

unionAll : List (Set a) -> Set a
unionAll sets =
  List.foldl Set.union Set.empty sets

-- Returns true if any two sets share an element.
-- Can help answer, "Is this a valid partition?"
-- Or if sets are disjoint
anyOverlap : List (Set a) -> Bool
anyOverlap sets =
  Set.size (unionAll sets) < List.sum (List.map Set.size sets)

-- More performant version, if you have a list and a set (fixes a major performance bug in assignUniqueNames)
anyOverlapListSet : List a -> Set a -> Bool
anyOverlapListSet items set =
  case items of
    []    -> False
    x::xs -> Set.member x set || anyOverlapListSet xs set

-- TODO combine findFirst and removeFirst

findFirst : (a -> Bool) -> List a -> Maybe a
findFirst p xs = case xs of
  []     -> Nothing
  x::xs_ -> if p x
              then Just x
              else findFirst p xs_

findLast : (a -> Bool) -> List a -> Maybe a
findLast p xs = findFirst p (List.reverse xs)

hasMatchingElement : (a -> Bool) -> List a -> Bool
hasMatchingElement p =
  maybeToBool << findFirst p

-- Search for a sublist in a list
findSublistIndex : List a -> List a -> Maybe Int
findSublistIndex targetList list =
  findSublistIndex_ 0 targetList list

findSublistIndex_ : Int -> List a -> List a -> Maybe Int
findSublistIndex_ i targetList list =
  if List.take (List.length targetList) list == targetList then
    Just i
  else
    case list of
      []    -> Nothing
      x::xs -> findSublistIndex_ (i+1) targetList xs

maybeFindAndRemoveFirst : (a -> Bool) -> List a -> Maybe (a, List a)
maybeFindAndRemoveFirst p xs =
  case xs of
    []     -> Nothing
    x::xs_ ->
      if p x then
        Just (x, xs_)
      else
        maybeFindAndRemoveFirst p xs_
        |> Maybe.map (\(removed, others) -> (removed, x::others))


removeFirst : a -> List a -> List a
removeFirst x ys = case ys of
  []     -> []
  y::ys_ -> if x == y then ys_ else y :: removeFirst x ys_

maybeRemoveFirst : a -> List (a,b) -> Maybe (b, List (a,b))
maybeRemoveFirst x ys = case ys of
  []         -> Nothing
  (a,b)::ys_ ->
    if x == a
      then Just (b, ys_)
      else case maybeRemoveFirst x ys_ of
             Nothing      -> Nothing
             Just (b_, l) -> Just (b_, (a,b) :: l)

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

firstOrLazySecond : Maybe a -> (() -> Maybe a) -> Maybe a
firstOrLazySecond maybe1 lazyMaybe2 =
  case maybe1 of
    Just x  -> maybe1
    Nothing -> lazyMaybe2 ()

-- In:  [1, 2, 3]
-- Out: [(1,2), (2,3), (3,1)]
circOverlappingAdjacentPairs : List a -> List (a, a)
circOverlappingAdjacentPairs list = overlappingAdjacentPairs_ True list

-- In:  [1, 2, 3]
-- Out: [(1,2), (2,3)]
overlappingAdjacentPairs : List a -> List (a, a)
overlappingAdjacentPairs list = overlappingAdjacentPairs_ False list

overlappingAdjacentPairs_ : Bool -> List a -> List (a, a)
overlappingAdjacentPairs_ includeLast list =
  let shiftList =
    case list of
      x::xs -> if includeLast then xs ++ [x] else xs
      _     -> []
  in
  zip list shiftList

-- 1-based
findi : (a -> Bool) -> List a -> Maybe Int
findi p xs = findi_ 1 p xs

findi_ : Int -> (a -> Bool) -> List a -> Maybe Int
findi_ i p xs = case xs of
  []     -> Nothing
  x::xs_ -> if p x then Just i else findi_ (i+1) p xs_

-- 1-based
geti : Int -> List a -> a
geti i = fromJust_ "Utils.geti" << List.head << List.drop (i-1)

-- 1-based
replacei : Int -> a -> List a -> List a
replacei i xi_ xs = List.take (i-1) xs ++ [xi_] ++ List.drop i xs

-- 1-based
removei : Int -> List a -> List a
removei i xs = List.take (i-1) xs ++ List.drop i xs

-- 1-based; inserts so element is at position i in resulting list
inserti : Int -> a -> List a -> List a
inserti i xi_ xs = List.take (i-1) xs ++ [xi_] ++ List.drop (i-1) xs

-- 0-based
maybeGeti0 : Int -> List a -> Maybe a
maybeGeti0 i list = if i >= 0 then list |> List.drop i |> List.head else Nothing

-- 1-based
maybeGeti1 : Int -> List a -> Maybe a
maybeGeti1 i list = if i >= 1 then list |> List.drop (i-1) |> List.head else Nothing

-- 0-based
getReplacei0 : Int -> (a -> a) -> List a -> List a
getReplacei0 i f list =
  case (List.take i list, List.drop i list) of
    (before, x::after) -> before ++ [f x] ++ after
    _                  -> list

-- 1-based
getReplacei1 : Int -> (a -> a) -> List a -> List a
getReplacei1 i f list =
  getReplacei0 (i-1) f list

mapHead : (a -> a) -> List a -> List a
mapHead f list =
  getReplacei0 0 f list

changeTail : (List a -> List a) -> List a -> List a
changeTail f list =
  case list of
    []    -> []
    x::xs -> x :: f xs

mapLast : (a -> a) -> List a -> List a
mapLast f list =
  getReplacei0 (List.length list - 1) f list

maybeMapLast : (a -> a) -> List a -> Maybe (List a)
maybeMapLast f list =
  maybeLast list
  |> Maybe.map (\last -> removeLastElement list ++ [f last])

-- O(n)
allSame list = case list of
  []    -> False
  v::vs -> List.all ((==) v) vs

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
angleBracks = delimit "<" ">"

spaces = String.join " "
commas = String.join ", "
lines  = String.join "\n"

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

commonPrefixString : List String -> String
commonPrefixString strings =
  strings |> List.map String.toList |> commonPrefix |> String.fromList

commonSuffixString : List String -> String
commonSuffixString strings =
  strings |> List.map String.reverse |> commonPrefixString |> String.reverse

-- Merge common prefix and common suffix; combine middle parts with toSentence.
--
-- [ "Insert Argument x into function"
-- , "Insert Argument y into function" ]
-- =>
-- "Insert Argument x and y into function"
mergeStrings : List String -> String
mergeStrings strings =
  let stringsWords  = strings |> List.map (squish >> String.split " ") in
  let prefixWords   = commonPrefix stringsWords in
  let noPrefixes    = removeCommonPrefix stringsWords in
  let suffixWords   = commonSuffix noPrefixes in
  let middleString =
    noPrefixes
    |> removeCommonSuffix
    |> List.map (String.join " ")
    |> List.filter (not << String.isEmpty)
    |> toSentence
  in
  prefixWords ++ [middleString] ++ suffixWords
  |> List.filter (not << String.isEmpty) -- Possibly elide middleString
  |> String.join " "

-- Replace all runs of whitespace with a single space
squish : String -> String
squish str =
  String.trim <|
    Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> " ") str

-- e.g. niceTruncateString 10 "..." "Really long text here" => "Really..."
niceTruncateString : Int -> String -> String -> String
niceTruncateString n toBeContinuedStr str =
  if String.length str > n then
    String.trimRight (String.left (n - String.length toBeContinuedStr) str) ++ toBeContinuedStr
  else
    str

perhapsPluralizeList : String -> List a -> String
perhapsPluralizeList str list =
  str ++ (if List.length list == 1 then "" else "s")

-- After Rails pluralize https://apidock.com/rails/ActionView/Helpers/TextHelper/pluralize
pluralize : Int -> String -> String
pluralize count singular =
  case count of
    1 -> toString count ++ " " ++ singular
    _ -> toString count ++ " " ++ singular ++ "s"

capitalize : String -> String
capitalize str =
  case String.split "" str of
    []          -> ""
    first::rest -> String.join "" (String.toUpper first :: rest)

-- https://stackoverflow.com/a/41124950
stringReplace : String -> String -> String -> String
stringReplace target replacement string =
  String.split target string |> String.join replacement

-- "WORD Word" -> "wordWord"
-- "word word" -> "wordWord"
naturalToCamelCase : String -> String
naturalToCamelCase natural =
  natural
  |> String.split " "
  |> List.map String.toLower
  |> mapi1 (\(i, word) -> if i == 1 then word else capitalize word)
  |> String.join ""

-- After ActiveSupport's to_sentence method
toSentence strings =
  case (dropLast 1 strings, maybeLast strings) of
    (  _, Nothing) -> ""
    ( [], Just z)  -> z
    ([y], Just z)  -> y ++ " and " ++ z
    ( ys, Just z)  -> String.join ", " ys ++ ", and " ++ z

sum = List.foldl (+) 0

avg : List Float -> Float
avg ns = List.sum ns / toFloat (List.length ns)

lift_2_2 f (a,b) (c,d) = (f a c, f b d)

assert s b = if b then () else Debug.crash ("assert error: " ++ s)

maybeToBool m = case m of
  Just _  -> True
  Nothing -> False

resultToBool r = case r of
  Ok _  -> True
  Err _ -> False

fromJust m = case m of
  Just x -> x
  Nothing -> Debug.crash <| "Utils.fromJust: Nothing"

fromJust_ s mx = case mx of
  Just x  -> x
  Nothing -> Debug.crash <| "Utils.fromJust_: " ++ s

-- Construct error lazily by calling f ()
fromJust__ f mx = case mx of
  Just x  -> x
  Nothing -> Debug.crash <| f ()

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

-- Useful with ImpureGoodies.crashToError on a thunk that also returns an error
unwrapNestedResult : Result e (Result e a) -> Result e a
unwrapNestedResult nestedResult =
  case nestedResult of
    Err e        -> Err e
    Ok (Err e)   -> Err e
    Ok (Ok item) -> Ok item

-- Don't construct error string on every dictionary lookup. (Serializing dictionary may be expensive)
justGetError s k d () =
  "Utils.justGet " ++ s ++ ": key " ++ toString k ++ " not found in dictionary " ++ toString d

justGet k d = fromJust__ (justGetError "" k d) <| Dict.get k d

justGet_ s k d = fromJust__ (justGetError s k d) <| Dict.get k d

getWithDefault key default dict =
  case Dict.get key dict of
    Just val -> val
    Nothing -> default

toggleSet : a -> Set a -> Set a
toggleSet x set =
  if Set.member x set then Set.remove x set else Set.insert x set

multiToggleSet : Set a -> Set a -> Set a
multiToggleSet insertSet set =
  Set.diff
    (Set.union insertSet set)
    (Set.intersect insertSet set)

toggleDict : (k, v) -> Dict k v -> Dict k v
toggleDict (k,v) dict =
  if Dict.member k dict then Dict.remove k dict else Dict.insert k v dict

flipDict : Dict a b -> Dict b a
flipDict dict =
  dict
  |> Dict.toList
  |> List.map flip
  |> Dict.fromList

multiKeySingleValue : List k -> v -> Dict k v
multiKeySingleValue keys value =
  List.foldl
      (\key dict -> Dict.insert key value dict)
      Dict.empty
      keys

dictAddToSet : k -> v -> Dict k (Set v) -> Dict k (Set v)
dictAddToSet k v dict =
  case Dict.get k dict of
    Just vs -> Dict.insert k (Set.insert v vs) dict
    Nothing -> Dict.insert k (Set.singleton v) dict

dictGetSet : k -> Dict k (Set v) -> Set v
dictGetSet k d =
  Maybe.withDefault Set.empty (Dict.get k d)

dictUnionSet : k -> (Set v) -> Dict k (Set v) -> Dict k (Set v)
dictUnionSet k more dict =
  Dict.insert k (Set.union more (dictGetSet k dict)) dict

insertAll : List (k, v) -> Dict k v -> Dict k v
insertAll pairs dict =
  pairs
  |> List.foldl
      (\(k, v) dict -> Dict.insert k v dict)
      dict

insertAllIntoSet : List a -> Set a -> Set a
insertAllIntoSet items set =
  items |> List.foldl Set.insert set

maybeLast list =
  case list of
    []    -> Nothing
    [x]   -> Just x
    _::xs -> maybeLast xs

head msg = fromJust_ msg << List.head
tail msg = fromJust_ msg << List.tail
last msg = fromJust_ msg << maybeLast
head_ = head "Utils.head_"
tail_ = fromJust_ "Utils.tail_" << List.tail
last_ = last "Utils.last_"

uncons xs = case xs of
  x::xs -> (x, xs)
  []    -> Debug.crash "uncons"

maybeUncons list = case list of
  []    -> Nothing
  x::xs -> Just (x, xs)

takeLast n list =
  list
  |> List.reverse
  |> List.take n
  |> List.reverse

takeWhile pred list =
  case list of
    []    -> []
    x::xs -> if pred x
             then x::(takeWhile pred xs)
             else []

dropWhile pred list =
  case list of
    []    -> []
    x::xs -> if pred x
             then dropWhile pred xs
             else list

-- Use Maybe.map
mapMaybe = Maybe.map

filterMaybe : (a -> Bool) -> Maybe a -> Maybe a
filterMaybe pred mx =
  case mx of
    Just x  -> if pred x then Just x else Nothing
    Nothing -> Nothing

bindMaybe : (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe = Maybe.andThen

plusMaybe : Maybe a -> Maybe a -> Maybe a
plusMaybe mx my = case mx of
  Just _  -> mx
  Nothing -> my

firstMaybe : List (Maybe a) -> Maybe a
firstMaybe list = List.foldr plusMaybe Nothing list

-- Use Maybe.withDefault (note: argument order is reversed)
elseMaybe : Maybe a -> a -> a
elseMaybe mx default = case mx of
  Just x  -> x
  Nothing -> default

-- Return Just [...] only if given list is all Justs
projJusts : List (Maybe a) -> Maybe (List a)
projJusts =
  List.foldr (\mx acc ->
    case (mx, acc) of
      (Just x, Just xs) -> Just (x::xs)
      _                 -> Nothing) (Just [])

bindMaybesToList : List (Maybe a) -> (List a -> List b) -> List b
bindMaybesToList list f =
  case projJusts list of
    Nothing -> []
    Just xs -> f xs

filterJusts : List (Maybe a) -> List a
filterJusts mxs = case mxs of
  []              -> []
  Just x  :: rest -> x :: filterJusts rest
  Nothing :: rest -> filterJusts rest

filterOks : List (Result e a) -> List a
filterOks mxs = case mxs of
  []            -> []
  Ok x  :: rest -> x :: filterOks rest
  Err _ :: rest -> filterOks rest

filterErrs : List (Result e a) -> List e
filterErrs mxs = case mxs of
  []            -> []
  Ok _  :: rest -> filterErrs rest
  Err x :: rest -> x :: filterErrs rest

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

-- Returns Nothing if function ever returns Nothing
foldrMaybe : (a -> b -> Maybe b) -> Maybe b -> List a -> Maybe b
foldrMaybe f maybeAcc list =
  case list of
    []    -> maybeAcc
    x::xs ->
      foldrMaybe f maybeAcc xs
      |> Maybe.andThen (\acc -> f x acc)

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

-- Returns Err if function ever returns Err
foldlResult : (a -> b -> Result err b) -> Result err b -> List a -> Result err b
foldlResult f resultAcc list =
  case (resultAcc, list) of
    (Err err, _)    -> resultAcc
    (Ok acc, x::xs) -> foldlResult f (f x acc) xs
    (_, [])         -> resultAcc


-- Use Tuple.mapFirst
-- mapFst : (a -> a_) -> (a, b) -> (a_, b)
-- mapFst f (a, b) = (f a, b)

-- Use Tuple.mapSecond
-- mapSnd : (b -> b_) -> (a, b) -> (a, b_)
-- mapSnd f (a, b) = (a, f b)

mapBoth : (a -> b) -> (a, a) -> (b, b)
mapBoth f (x, y) =
  (f x, f y)

fst3 (x,_,_) = x
snd3 (_,x,_) = x
thd3 (_,_,x) = x

mapFst3 f (x,y,z) = (f x, y, z)
mapSnd3 f (x,y,z) = (x, f y, z)
mapThd3 f (x,y,z) = (x, y, f z)

fourth4 (_,_,_,x) = x

flip (a, b) = (b, a)

bindResult : Result a b -> (b -> Result a b_) -> Result a b_
bindResult res f =
  case res of
    Err a -> Err a
    Ok b  -> f b

setIsEmpty  = (==) [] << Set.toList
dictIsEmpty = (==) [] << Dict.toList

parseInt   = fromOk_ << String.toInt
parseFloat = fromOk_ << String.toFloat

isPrefix : List a -> List a -> Bool
isPrefix prefix longer =
  prefix |> isPrefixOf longer

isPrefixOf : List a -> List a -> Bool
isPrefixOf longer prefix =
  case (longer, prefix) of
    (_, [])        -> True
    ([], _)        -> False
    (x::xs, y::ys) -> x == y && isPrefixOf xs ys

-- Common elements shared at the beginning of each list
commonPrefix : List (List a) -> List a
commonPrefix lists =
  case lists of
    first::rest -> List.foldl commonPrefixPair first rest
    []          -> []

commonSuffix : List (List a) -> List a
commonSuffix lists =
  lists
  |> List.map List.reverse
  |> commonPrefix
  |> List.reverse

commonPrefixPair : List a -> List a -> List a
commonPrefixPair l1 l2 =
  case (l1, l2) of
    (x::xs, y::ys) -> if x == y then x::(commonPrefixPair xs ys) else []
    _              -> []

removeCommonPrefix : List (List a) -> List (List a)
removeCommonPrefix lists =
  let prefixLength = List.length (commonPrefix lists) in
  lists |> List.map (List.drop prefixLength)

removeCommonSuffix : List (List a) -> List (List a)
removeCommonSuffix lists =
  lists
  |> List.map List.reverse
  |> removeCommonPrefix
  |> List.map List.reverse

sgn x =
  if x == 0 then 0
  else if x < 0 then -1
  else if x > 0 then 1
  else x -- NaN

between x (a,b) = a <= x && x < b

distance (x1,y1) (x2,y2) = sqrt <| (x2-x1)^2 + (y2-y1)^2

distanceInt (x1,y1) (x2,y2) =
  distance (toFloat x1, toFloat y1) (toFloat x2, toFloat y2)

midpoint (x1,y1) (x2,y2) = ((x1 + x2) / 2, (y1 + y2) / 2)

-- n:number -> i:[0,n) -> RGB
numToColor n i =
  let j = round <| (i/n) * 500 in
  if between j (  0, 360) then numToColor_ j   else -- color
  if between j (360, 380) then (0, 0, 0)       else -- black
  if between j (480, 500) then (255, 255, 255)      -- white
  else                                              -- grayscale
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

pairToList : (a, a) -> List a
pairToList (x1, x2) = [x1, x2]

unwrapSingletonSet : Set a -> a
unwrapSingletonSet set = case Set.toList set of
  [x] -> x
  _   -> Debug.crash "unwrapSingletonSet"

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

unwrap8 xs = case xs of
  [x1,x2,x3,x4,x5,x6,x7,x8] -> (x1, x2, x3, x4, x5, x6, x7, x8)
  _ -> Debug.crash "unwrap7"

--------------------------------------------------------------------------------

-- Composition is left-to-right
compose : List (a -> a) -> a -> a
compose =
  List.foldl (<<) identity

--------------------------------------------------------------------------------

and : List Bool -> Bool
and =
  List.foldl (&&) True

or : List Bool -> Bool
or =
  List.foldl (||) False

--------------------------------------------------------------------------------

-- Useful for ensuring that the () in "let _ = Debug.log s ()" is not forgotten.
log : String -> ()
log s =
  Debug.log s ()

--------------------------------------------------------------------------------

isEven : Int -> Bool
isEven n =
  n % 2 == 0

isOdd : Int -> Bool
isOdd n =
  n % 2 == 1

--------------------------------------------------------------------------------

fromResult : Result a a -> a
fromResult result =
  case result of
    Ok x ->
      x

    Err x ->
      x


-- prelude.little
--
-- This little library is accessible by every program.
-- This is not an example that generates an SVG canvas,
-- but we include it here for reference.

--; The identity function - given a value, returns exactly that value
-- id: (forall a (-> a a))
id x = x

--; A function that always returns the same value a, regardless of b
-- always: (forall (a b) (-> a b a))
always x _ = x

--; Composes two functions together
--compose: (forall (a b c) (-> (-> b c) (-> a b) (-> a c)))
compose f g = \x -> f (g x)

--flip: (forall (a b c) (-> (-> a b c) (-> b a c)))
flip f = \x y -> f y x
-- TODO other version:
-- (def flip (\(f x y) (f y x)))

--fst: (forall (a b) (-> [a b] a))
--snd: (forall (a b) (-> [a b] b))

fst [a, _] = a
snd [_, b] = b

--; Given a bool, returns the opposite boolean value
--not: (-> Bool Bool)
not b = if b then False else True

--; Given two bools, returns a bool regarding if the first argument is true, then the second argument is as well
--implies: (-> Bool Bool Bool)
implies p q = if p then q else True

--or:  (-> Bool Bool Bool)
--and: (-> Bool Bool Bool)

or p q = if p then True else q
and p q = if p then q else False

--lt: (-> Num Num Bool)
--eq: (-> Num Num Bool)
--le: (-> Num Num Bool)
--gt: (-> Num Num Bool)
--ge: (-> Num Num Bool)

lt x y = x < y
eq x y = x == y
le x y = or (lt x y) (eq x y)
gt = flip lt
ge x y = or (gt x y) (eq x y)

--; Returns the length of a given list
--len: (forall a (-> (List a) Num))
len xs = case xs of [] -> 0; (_ :: xs1) -> 1 + len xs1

--; Maps a function, f, over a list of values and returns the resulting list
--map: (forall (a b) (-> (-> a b) (List a) (List b)))
map f xs =
  case xs of [] -> []; hd::tl -> f hd :: map f tl

-- HEREHEREHERE

--; Combines two lists with a given function, extra elements are dropped
--map2: (forall (a b c) (-> (-> a b c) (List a) (List b) (List c)))
map2 f xs ys =
  case [xs, ys] of
    [x::xs1, y::ys1] -> f x y :: map2 f xs1 ys1
    _                -> []

--; Combines three lists with a given function, extra elements are dropped
--map3: (forall (a b c d) (-> (-> a b c d) (List a) (List b) (List c) (List d)))
map3 f xs ys zs =
  case [xs, ys, zs] of
    [x::xs1, y::ys1, z::zs1] -> f x y z :: map3 f xs1 ys1 zs1
    _                        -> []

--; Combines four lists with a given function, extra elements are dropped
--map4: (forall (a b c d e) (-> (-> a b c d e) (List a) (List b) (List c) (List d) (List e)))
map4 f ws xs ys zs =
  case [ws, xs, ys, zs]of
    [w::ws1, x::xs1, y::ys1, z::zs1] -> f w x y z :: map4 f ws1 xs1 ys1 zs1
    _                                -> []

--; Takes a function, an accumulator, and a list as input and reduces using the function from the left
--foldl: (forall (a b) (-> (-> a b b) b (List a) b))
foldl f acc xs =
  case xs of [] -> acc; x::xs1 -> foldl f (f x acc) xs1

--; Takes a function, an accumulator, and a list as input and reduces using the function from the right
--foldr: (forall (a b) (-> (-> a b b) b (List a) b))
foldr f acc xs =
  case xs of []-> acc; x::xs1 -> f x (foldr f acc xs1)

--; Given two lists, append the second list to the end of the first
--append: (forall a (-> (List a) (List a) (List a)))
append xs ys =
  case xs of [] -> ys; x::xs1 -> x :: append xs1 ys

--; concatenate a list of lists into a single list
--concat: (forall a (-> (List (List a)) (List a)))
concat xss = foldr append [] xss
-- TODO eta-reduced version:
-- (def concat (foldr append []))

--; Map a given function over a list and concatenate the resulting list of lists
--concatMap: (forall (a b) (-> (-> a (List b)) (List a) (List b)))
concatMap f xs = concat (map f xs)

--; Takes two lists and returns a list that is their cartesian product
--cartProd: (forall (a b) (-> (List a) (List b) (List [a b])))
cartProd xs ys =
  concatMap (\x -> map (\y -> [x, y]) ys) xs

--; Takes elements at the same position from two input lists and returns a list of pairs of these elements
--zip: (forall (a b) (-> (List a) (List b) (List [a b])))
zip xs ys = map2 (\x y -> [x, y]) xs ys
-- TODO eta-reduced version:
-- (def zip (map2 (\(x y) [x y])))

--; The empty list
--; (typ nil (forall a (List a)))
--nil: []
nil = []

--; attaches an element to the front of a list
--cons: (forall a (-> a (List a) (List a)))
cons x xs = x :: xs

--; attaches an element to the end of a list
--snoc: (forall a (-> a (List a) (List a)))
snoc x ys = append ys [x]

--; Returns the first element of a given list
--hd: (forall a (-> (List a) a))
--tl: (forall a (-> (List a) (List a)))
hd (x::xs) = x
tl (x::xs) = xs

--; Returns the last element of a given list
--last: (forall a (-> (List a) a))
last xs =
  case xs of
    [x]   -> x
    _::xs -> last xs

--; Given a list, reverse its order
--reverse: (forall a (-> (List a) (List a)))
reverse xs = foldl cons nil xs
-- TODO eta-reduced version:
-- (def reverse (foldl cons nil))

adjacentPairs xs = zip xs (tl xs)

--; Given two numbers, creates the list between them (inclusive)
--range: (-> Num Num (List Num))
range i j =
  if i < j + 1
    then cons i (range (i + 1) j)
    else nil

--; Given a number, create the list of 0 to that number inclusive (number must be > 0)
--list0N: (-> Num (List Num))
list0N n = range 0 n

--; Given a number, create the list of 1 to that number inclusive
--list1N: (-> Num (List Num))
list1N n = range 1 n

--zeroTo: (-> Num (List Num))
zeroTo n = range 0 (n - 1)

--; Given a number n and some value x, return a list with x repeated n times
--repeat: (forall a (-> Num a (List a)))
repeat n x = map (always x) (range 1 n)

--; Given two lists, return a single list that alternates between their values (first element is from first list)
--intermingle: (forall a (-> (List a) (List a) (List a)))
intermingle xs ys =
  case [xs, ys] of
    [x::xs1, y::ys1] -> cons x (cons y (intermingle xs1 ys1))
    [[], []]         -> nil
    _                -> append xs ys

intersperse sep xs =
  case xs of
    []    -> xs
    x::xs -> reverse (foldl (\y acc -> y :: sep :: acc) [x] xs)

--mapi: (forall (a b) (-> (-> [Num a] b) (List a) (List b)))
mapi f xs = map f (zip (range 0 (len xs - 1)) xs)

--nth: (forall a (-> (List a) Num (union Null a)))
nth xs n =
  if n < 0 then null
  else
    case [n, xs] of
      [_, []]     -> null
      [0, x::xs1] -> x
      [_, x::xs1] -> nth xs1 (n - 1)

-- (defrec nth (\(xs n)
--   (if (< n 0)   "ERROR: nth"
--     (case xs
--       ([]       "ERROR: nth")
--       ([x|xs1]  (if (= n 0) x (nth xs1 (- n 1))))))))

-- TODO change typ/def
-- (typ take (forall a (-> (List a) Num (union Null (List a)))))
--take: (forall a (-> (List a) Num (List (union Null a))))
take xs n =
  if n == 0 then []
  else
    case xs of
      []     -> [null]
      x::xs1 -> x :: take xs1 (n - 1)

-- (def take
--   (letrec take_ (\(n xs)
--     (case [n xs]
--       ([0 _]       [])
--       ([_ []]      [])
--       ([_ [x|xs1]] [x | (take_ (- n 1) xs1)])))
--   (compose take_ (max 0))))
--drop: (forall a (-> (List a) Num (union Null (List a))))
drop xs n =
  if le n 0 then xs
  else
    case xs of
      []     -> null
      x::xs1 -> drop xs1 (n - 1)

--; Drop n elements from the end of a list
-- dropEnd: (forall a (-> (List a) Num (union Null (List a))))
-- dropEnd xs n =
--   let tryDrop = drop (reverse xs) n in
--     {Error: typecase not yet implemented for Elm syntax}

--elem: (forall a (-> a (List a) Bool))
elem x ys =
  case ys of
    []     -> False
    y::ys1 -> or (x == y) (elem x ys1)

sortBy f xs =
  letrec ins x ys =   -- insert is a keyword...
    case ys of
      []    -> [x]
      y::ys -> if f x y then x :: y :: ys else y :: ins x ys
  in
  foldl ins [] xs

sortAscending = sortBy lt
sortDescending = sortBy gt


--; multiply two numbers and return the result
--mult: (-> Num Num Num)
mult m n =
  if m < 1 then 0 else n + mult (m + -1) n

--; Given two numbers, subtract the second from the first
--minus: (-> Num Num Num)
minus x y = x + mult y -1

--; Given two numbers, divide the first by the second
--div: (-> Num Num Num)
div m n =
  if m < n then 0 else
  if n < 2 then m else 1 + div (minus m n) n

--; Given a number, returns the negative of that number
--neg: (-> Num Num)
neg x = 0 - x

--; Absolute value
--abs: (-> Num Num)
abs x = if x < 0 then neg x else x

--; Sign function; -1, 0, or 1 based on sign of given number
--sgn: (-> Num Num)
sgn x = if 0 == x then 0 else x / abs x

--some: (forall a (-> (-> a Bool) (List a) Bool))
some p xs =
  case xs of
    []     -> False
    x::xs1 -> or (p x) (some p xs1)

--all: (forall a (-> (-> a Bool) (List a) Bool))
all p xs =
  case xs of
    []     -> True
    x::xs1 -> and (p x) (all p xs1)

--; Given an upper bound, lower bound, and a number, restricts that number between those bounds (inclusive)
--; Ex. clamp 1 5 4 = 4
--; Ex. clamp 1 5 6 = 5
--clamp: (-> Num Num Num Num)
clamp i j n = if n < i then i else if j < n then j else n

--between: (-> Num Num Num Bool)
between i j n = n == clamp i j n

--plus: (-> Num Num Num)
plus x y = x + y

--min: (-> Num Num Num)
min i j = if lt i j then i else j

--max: (-> Num Num Num)
max i j = if gt i j then i else j

--minimum: (-> (List Num) Num)
minimum (hd::tl) = foldl min hd tl

--maximum: (-> (List Num) Num)
maximum (hd::tl) = foldl max hd tl

--average: (-> (List Num) Num)
average nums =
  let sum = foldl plus 0 nums in
  let n = len nums in sum / n

--; Combine a list of strings with a given separator
--; Ex. joinStrings ", " ["hello" "world"] = "hello, world"
--joinStrings: (-> String (List String) String)
joinStrings sep ss =
  foldr (\str acc -> if acc == "" then str else str + sep + acc) "" ss

--; Concatenate a list of strings and return the resulting string
--concatStrings: (-> (List String) String)
concatStrings = joinStrings ""

--; Concatenates a list of strings, interspersing a single space in between each string
--spaces: (-> (List String) String)
spaces = joinStrings " "

--; First two arguments are appended at the front and then end of the third argument correspondingly
--; Ex. delimit "+" "+" "plus" = "+plus+"
--delimit: (-> String String String String)
delimit a b s = concatStrings [a, s, b]

--; delimit a string with parentheses
--parens: (-> String String)
parens = delimit "(" ")"


------------------- TODO

-- chopped everything starting from SVG Manipulating Functions
-- down to rectWithBorder

---------------------


--;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

-- (def columnsToRows (\columns
--   (let numColumns (len columns)
--   (let numRows ; maxColumnSize
--     (if (= numColumns 0) 0 (maximum (map len columns)))
--   (foldr
--     (\(col rows)
--       (let paddedCol (append col (repeat (- numRows (len col)) "."))
--       (map
--         (\[datum row] [ datum | row ])
--         (zip paddedCol rows))))
--     (repeat numRows [])
--     columns)
-- ))))
--
-- (def addColToRows (\(col rows)
--   (let width (maximum (map len rows))
--   (letrec foo (\(col rows)
--     (case [col rows]
--       ([ []     []     ] [                                          ])
--       ([ [x|xs] [r|rs] ] [ (snoc x r)                 | (foo xs rs) ])
--       ([ []     [r|rs] ] [ (snoc "" r)                | (foo [] rs) ])
--       ([ [x|xs] []     ] [ (snoc x (repeat width "")) | (foo xs []) ])
--     ))
--   (foo col rows)))))

-- (def border ["border" "1px solid black"])
-- (def padding ["padding" "5px"])
-- (def center ["align" "center"])
-- (def style (\list ["style" list]))
-- (def onlyStyle (\list [(style list)]))
--
-- (def td (\text
--   ["td" (onlyStyle [border padding])
--         [["TEXT" text]]]))
--
-- (def th (\text
--   ["th" (onlyStyle [border padding center])
--         [["TEXT" text]]]))
--
-- (def tr (\children
--   ["tr" (onlyStyle [border])
--         children]))
--
-- ; TODO div name is already taken...
--
-- (def div_ (\children ["div" [] children]))
-- (def h1 (\text ["h1" [] [["TEXT" text]]]))
-- (def h2 (\text ["h2" [] [["TEXT" text]]]))
-- (def h3 (\text ["h3" [] [["TEXT" text]]]))
--
-- (def table (\children
--   ["table" (onlyStyle [border padding]) children]))

-- (def table (\children
--   (let [x y] [100 100]
--   ["table" (onlyStyle [border padding
--                       ["position" "relative"]
--                       ["left" (toString x)]
--                       ["top" (toString y)]]) children])))

-- (def tableOfData (\data
--   (let letters (explode " ABCDEFGHIJKLMNOPQRSTUVWXYZ")
--   (let data (mapi (\[i row] [(+ i 1) | row]) data)
--   (let tableWidth (maximum (map len data))
--   (let headers
--     (tr (map (\letter (th letter)) (take letters tableWidth)))
--   (let rows
--     (map (\row (tr (map (\col (td (toString col))) row))) data)
--   (table
--     [ headers | rows ]
-- ))))))))


textNode text =
  ["TEXT", text]

textElementHelper tag styles attrs text =
  [ tag,  ["style", styles] :: attrs , [ textNode text ] ]

elementHelper tag styles attrs children =
  [ tag,  ["style", styles] :: attrs , children ]

p = textElementHelper "p"
th = textElementHelper "th"
td = textElementHelper "td"
h1 = textElementHelper "h1"
h2 = textElementHelper "h2"
h3 = textElementHelper "h3"

div_ = elementHelper "div"
tr = elementHelper "tr"
table = elementHelper "table"

-- absolutePositionStyles x y = let _ = [x, y] : Point in
--   [ ["position", "absolute"]
--   , ["left", toString x + "px"]
--   , ["top", toString y + "px"]
--   ]



setStyles newStyles [kind, attrs, children] =
  let attrs =
    -- TODO
    if styleAttr == null
      then ["style", []] :: attrs
      else attrs
  in
  let attrs =
    map \[key, val] ->
      case key of
        "style"->
          let otherStyles =
            concatMap \[k, v] ->
              case elem k (map fst newStyles) of
                True  ->  []
                False -> [[k, v]]
              val in
          ["style", append newStyles otherStyles]
        _->
          [key, val]
      attrs
  in
  [kind, attrs, children]

placeAt [x, y] node =
  let _ = [x, y] : Point in
  -- TODO px suffix should be added in LangSvg/Html translation
  setStyles
    [ ["position", "absolute"],
      ["left", toString x + "px"],
      ["top", toString y + "px"]
    ]
    node

placeAtFixed [x, y] node =
  let _ = [x, y] : Point in
  setStyles
    [["position", "fixed"], ["FIXED_LEFT", x], ["FIXED_TOP", y]]
    node

placeSvgAt [x, y] w h shapes =
  placeAt [x, y]
    ["svg", [["width", w], ["height", h]], shapes]

workspace minSize children =
  div_
    (cons
      (placeAt minSize (h3 "</workspace>"))
      children)

--;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

-- List --

indexedMap f xs = mapi (\[i,x] -> f i x) xs

-- Maybe --

nothing = ["Nothing"]
just x  = ["Just", x]

-- Tuple --

mapFirst f [x, y] = [f x, y]

mapSecond f [x, y] = [x, f y]

-- Custom Updates --

-- TODO

-- Editor --

freeze x = x



-- The type checker relies on the name of this definition.
let dummyPreludeMain = ["svg", [], []] in dummyPreludeMain

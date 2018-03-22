

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

-- TODO remove
freeze x = x

nil = []

cons x xs = x :: xs

zip xs ys =
  case [xs, ys] of
    [x::xsRest, y::ysRest] -> [x,y] :: zip xsRest ysRest
    _                      -> []

range i j =
  if i < j + 1
    then cons i (range (i + 1) j)
    else nil

reverse l =
  letrec r acc l = case l of [] -> acc; head::tail -> r (head::acc) tail in
  { apply = r [], update {output}= {values = [r [] output]}}.apply l


-- The current diff primitive is something like:
--
--   diff : List Value -> List Value -> List (DiffChunk (List Value))
--   diff ~= diffVals
--
-- The new diff primitive will be something like:
--
--   diff : Value -> Value -> Result String (Maybe VDiffs)
--   diff ~= defaultVDiffs
--
-- Once diff has been changed, implement library function like:
--
--   getSimpleListDiffOps : List Value -> List Value -> VDiffs -> List SimpleListDiffOp
--   getSimpleListDiffOps oldValues newValues vDiffs =
--     case vDiffs of
--       ["VListDiffs", values] ->
--         ..
--
-- where
--
--   type SimpleListDiffOp = Keep | Delete | Insert Value | Update Value


map1 f l =
  case l of
    []    -> []
    x::xs -> f x :: map1 f xs

-- Non-lens version of append
__update_append__ xs ys =
 case xs of
   [] -> ys
   x::xs1 -> x :: __update_append__ xs1 ys

-- Non-lens version of split
__update_split__ n l =
    letrec aux acc n l =
      if n == 0 then [reverse acc, l] else
      case l of
        [] -> [reverse acc, l]
        head::tail -> aux (head::acc) (n - 1) tail
    in aux [] n l

-- type Results err ok = { values: List ok } | { error: err }

-- every onFunction should either return a {values = ...} or an {error =... }
-- start    : a
-- onUpdate : a -> {oldOutput: b, newOutput: b, index: Int, diffs: VDiffs} -> Results String a
-- onInsert : a -> {newOutput: b, index: Int, diffs: VDiffs}  -> Results String a
-- onRemove : a -> {oldOutput: b, index: Int, diffs! VDoffs}  -> Results String a
-- onSkip   : a -> {count: Int, index: Int, oldOutputs: List b, newOutputs: List b}  -> Results String a
-- onFinish : a -> Results String c
-- onGather : c -> ({value: d, diff: Maybe VDiffs } | { value: d })
-- oldOutput: List b
-- newOutput: List b
-- diffs    : VListDiffs
-- Returns  : {error: String} | {values: List d} | {values: List d, diffs: List (Maybe VDiffs)}
foldDiff =
  letrec append xs ys =
    case xs of
      [] -> ys
      x::xs1 -> x :: append xs1 ys
  in
  let Results =
    letrec keepOks l =
      case l of
        [] -> []
        { error }::tail -> keepOks tail
        { values =  ll }::tail -> ll ++ map1 keepOks tail
    in
    letrec projOks l =
      case l of
        [] -> {values = []}
        {values = []}::tail -> projOks tail
        {values = vhead::vtail}::tail -> {values = vhead::(vtail ++ keepOks tail)}
        {error = msg}::tail ->
          case projOks tail of
            {error = msgTail} -> { error = msg }
            { values = []}-> {error = msg}
            result -> result
    in
    let andThen callback results =
      --andThen : (a -> Results x b) -> Results x a -> Results x b
      case results of
        {values = ll} -> ll |> map1 callback |> projOks
        {error = msg} -> results
    in
    {
      keepOks = keepOks
      projOks = projOks
      andThen = andThen
    }
  in
  \{start, onSkip, onUpdate, onRemove, onInsert, onFinish, onGather} oldOutput newOutput diffs ->
  let listDiffs = case diffs of
    ["VListDiffs", l] -> l
    _ -> error <| "Expected VListDiffs, got " + toString diffs
  in
  -- Returns either {error} or {values=list of values}
  --     fold: Int -> List b -> List b -> List (Int, VListElemDiff) -> a -> Results String c
  letrec fold  j      oldOutput  newOutput  listDiffs                    acc =
      let next i      oldOutput_ newOutput_ d newAcc =
        newAcc |> Results.andThen (\accCase ->
          fold i oldOutput_ newOutput_ d accCase
        )
      in
      case listDiffs of
      [] ->
        let count = len newOutput in
        if count == 0 then
          onFinish acc
        else
         onSkip acc {count = count, index = j, oldOutputs = oldOutput, newOutputs = newOutput}
         |> next (j + count) [] [] listDiffs

      [i, diff]::dtail  ->
        if i > j then
          let count = i - j in
          let [previous, remainingOld] = __update_split__ count oldOutput in
          let [current,  remainingNew] = __update_split__ count newOutput in
          onSkip acc {count = count, index = j, oldOutputs = previous, newOutputs = current}
          |> next i remainingOld remainingNew listDiffs
        else case diff of
          ["VListElemUpdate", d]->
            let previous::remainingOld = oldOutput in
            let current::remainingNew = newOutput in
            onUpdate acc {oldOutput = previous, index = i, output = current, newOutput = current, diffs = d}
            |> next (i + 1) remainingOld remainingNew dtail
          ["VListElemInsert", count] ->
            if count >= 1 then
              let current::remainingNew = newOutput in
              onInsert acc {newOutput = current, index = i}
              |> next i oldOutput remainingNew (if count == 1 then dtail else [i, ["VListElemInsert", count - 1]]::dtail)
            else error <| "insertion count should be >= 1, got " + toString count
          ["VListElemDelete", count] ->
            if count >= 1 then
              let dropped::remainingOld = oldOutput in
              onRemove acc {oldOutput =dropped, index = i} |>
              next (i + count) remainingOld newOutput (if count == 1 then dtail else [i + 1, ["VListElemDelete", count - 1]]::dtail)
            else error <| "deletion count should be >= 1, got " ++ toString count
      _ -> error <| "Expected a list of diffs, got " + toString diffs
  in
  case fold 0 oldOutput newOutput listDiffs start of
    { error = msg } -> {error = msg}
    { values = values } -> -- values might be a pair of value and diffs. We use onGather to do the split.
      letrec aux accValues accDiffs values = case values of
        [] -> case accDiffs of
          ["Nothing"] -> {values = accValues}
          ["Just", diffs] -> {values = accValues, diffs = diffs}
        head::tail -> case onGather head of
          {value, diff} -> case accDiffs of
            ["Nothing"] -> if len accValues > 0 then { error = "Diffs not specified for all values, e.g." + toString value } else
              aux [value] ["Just", [diff]] tail
            ["Just", diffs] ->
              aux (accValues ++ [value]) ["Just", diffs ++ [diff]] tail
          {value} -> case accDiffs of
            ["Nothing"] -> aux [value] accDiffs tail
            ["Just", diffs] -> { error = "Diffs not specified until " + toString value }
      in aux [] ["Nothing"] values

append aas bs = {
    apply [aas, bs] = freeze <| __update_append__ aas bs
    update {input = [aas, bs], outputNew, outputOld, diffs} =
      let asLength = len aas in
      foldDiff {
        start = [[], [], [], [], len aas, len bs]
        onSkip [nas, nbs, diffas, diffbs, numA, numB] {count = n, newOutputs = outs} =
          if n <= numA then
            {values = [[nas ++ outs, nbs, diffas, diffbs, numA - n, numB]]}
          else
            let [forA, forB] = __update_split__ numA outs in
            {values = [[nas ++ forA, nbs ++ forB, diffas, diffbs, 0, numB - (n - numA)]]}
        onUpdate [nas, nbs, diffas, diffbs, numA, numB] {newOutput = out, diffs, index} =
          { values = [if numA >= 1
           then [nas ++ [out],                                      nbs,
                 diffas ++ [[index, ["VListElemUpdate", diffs]]], diffbs,
                 numA - 1,                                          numB]
           else [nas,    nbs ++ [out],
                 diffas, diffbs ++ [[index - asLength, ["VListElemUpdate", diffs]]],
                 0,      numB - 1]] }
        onRemove  [nas, nbs, diffas, diffbs, numA, numB] {oldOutput, index} =
          if 1 <= numA then
            {values = [[nas, nbs, diffas ++ [[index, ["VListElemDelete", 1]]], diffbs, numA - 1, numB]] }
          else
            {values = [[nas, nbs, diffas, diffbs ++ [[index - asLength, ["VListElemDelete", 1]]], numA, numB - 1]] }
        onInsert [nas, nbs, diffas, diffbs, numA, numB] {newOutput, index} =
          {values =
            (if numA > 0 || len nbs == 0 then
              [[nas ++ [newOutput], nbs,
                diffas ++ [[index, ["VListElemInsert", 1]]], diffbs,
                numA, numB]]
            else []) ++
              (if len nbs > 0 || numA == 0 then
                [[nas,    nbs ++ [newOutput],
                  diffas, diffbs ++ [[index - asLength, ["VListElemInsert", 1]]],
                  numA, numB]]
              else [])
            }

        onFinish [nas, nbs, diffas, diffbs, _, _] = {
           values = [[[nas, nbs], (if len diffas == 0 then [] else
             [[0, ["VListElemUpdate", ["VListDiffs", diffas]]]]) ++
                   (if len diffbs == 0 then [] else
             [[1, ["VListElemUpdate", ["VListDiffs", diffbs]]]])]]
          }
        onGather [[nas, nbs], diffs] = {value = [nas, nbs],
          diff = if len diffs == 0 then ["Nothing"] else ["Just", ["VListDiffs", diffs]]}
      } outputOld outputNew diffs
    }.apply [aas, bs]

--; Maps a function, f, over a list of values and returns the resulting list
--map: (forall (a b) (-> (-> a b) (List a) (List b)))
map f l =
  {
  apply [f, l] = freeze (map1 f l)
  update {input=[f, l], oldOutput, outputNew, diffs} =
    foldDiff {
      start =
        --Start: the collected functions, the collected inputs, the inputs yet to process.
        [[], [], l]


      onSkip [fs, insA, insB] {count} =
        --'outs' was the same in oldOutput and outputNew
        let [skipped, remaining] = __update_split__ count insB in
        {values = [[fs, insA ++ skipped, remaining]]}

      onUpdate [fs, insA, insB] {oldOutput, newOutput, diffs} =
        let input::remaining = insB in
        case updateApp {fun [f,x] = f x, input = [f, input], output = newOutput, oldOutput = oldOutput, diffs = diffs} of
          { error = msg } -> {error = msg}
          { values = v } -> {values = v |>
              map1 (\[newF, newA] -> [newF :: fs, insA ++ [newA], remaining])}

      onRemove [fs, insA, insB] {oldOutput} =
        let _::remaining = insB in
        { values = [[fs, insA, remaining]] }

      onInsert [fs, insA, insB] {newOutput} =
        let input = case insB of h::_ -> h; _ -> case insA of h::_ -> h; _ -> error "Empty list for map, cannot insert" in
        case updateApp {fun [f,x] = f x, input = [f, input], output = newOutput} of
          { error = msg } -> {error = msg }
          { values = v} -> {values = v |>
              map (\[newF, newA] -> [newF::fs, insA++[newA], insB])}

      onFinish [newFs, newIns, _] =
       --after we finish, we need to return the new function
       --as a merge of original functions with all other modifications
       -- and the collected new inputs
       {values = [[merge f newFs, newIns]] }

      onGather result =
        -- TODO: Later, include the , diff= here.
        { value = result }
    } oldOutput outputNew diffs
  }.apply [f, l]

zipWithIndex xs =
  { apply x = zip (range 0 (len xs - 1)) xs
    update {output} = {values = [map (\[i, x] -> x) output]}  }.apply xs


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
-- append xs ys =
--   case xs of [] -> ys; x::xs1 -> x :: append xs1 ys

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
-- zip xs ys = map2 (\x y -> [x, y]) xs ys
-- TODO eta-reduced version:
-- (def zip (map2 (\(x y) [x y])))

--; The empty list
--; (typ nil (forall a (List a)))
--nil: []
-- nil = []

--; attaches an element to the front of a list
--cons: (forall a (-> a (List a) (List a)))
-- cons x xs = x :: xs

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
-- range i j =
--   if i < j + 1
--     then cons i (range (i + 1) j)
--     else nil

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
mapi f xs = map f (zipWithIndex xs)
 
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



-- absolutePositionStyles x y = let _ = [x, y] : Point in
--   [ ["position", "absolute"]
--   , ["left", toString x + "px"]
--   , ["top", toString y + "px"]
--   ]


-- Returns a list of HTML nodes parsed from a string. It uses the API for loosely parsing HTML
-- Example: html "Hello<b>world</b>" returns [["TEXT","Hello"],["b",[], [["TEXT", "world"]]]]
html string =
  let take =
    letrec aux n l = if n == 0 then [] else
      case l of
        [] -> []
        head::tail -> head :: (aux (n - 1) tail)
    in aux in
  let drop =
    letrec aux n l = if n == 0 then l else
      case l of
        [] -> []
        head::tail -> aux (n - 1) tail
    in aux in {
  apply trees = 
    freeze (letrec domap tree = case tree of
      ["HTMLInner", v] -> ["TEXT", replaceAllIn "&amp;|&lt;|&gt;|</[^>]*>" (\{match} -> case match of "&amp;" -> "&"; "&lt;" -> "<"; "&gt;" -> ">"; _ -> "") v]
      ["HTMLElement", tagName, attrs, ws1, endOp, children, closing] ->
        [ tagName
        , map (case of
          ["HTMLAttribute", ws0, name, value] -> case value of
            ["HTMLAttributeUnquoted", _, _, content ] -> [name, content]
            ["HTMLAttributeString", _, _, _, content ] -> [name, content]
            ["HTMLAttributeNoValue"] -> [name, ""]) attrs
        , map domap children]
      ["HTMLComment", _, content] -> ["comment", [["display", "none"]], [["TEXT", content]]]
    in map domap trees)

  update {input, oldOutput, newOutput} =
    let toHTMLAttribute [name, value] = ["HTMLAttribute", " ", name, ["HTMLAttributeString", "", "", "\"", value]] in
    let toHTMLInner text = ["HTMLInner", replaceAllIn "<|>|&" (\{match} -> case match of "&" -> "&amp;"; "<" -> "&lt;"; ">" -> "&gt;"; _ -> "") text] in
    letrec mergeAttrs acc ins d = case d of
      [] -> acc
      {kept}::dt -> mergeAttrs (append acc (take (len kept) ins)) (drop (len kept) ins) dt
      {deleted=[deleted]}::{inserted=[inserted]}::dt ->
        let newIn = case [take 1 ins, inserted] of
          [ [["HTMLAttribute", sp0, name, value]], [name2, value2 ]] ->
            case value of
              ["HTMLAttributeUnquoted", sp1, sp2, v] ->
                case extractFirstIn "\\s" v of
                  ["Nothing"] ->
                    ["HTMLAttribute", sp0, name2, ["HTMLAttributeUnquoted", sp1, sp2, value2]]
                  _ ->
                    ["HTMLAttribute", sp0, name2, ["HTMLAttributeString", sp1, sp2, "\"", value2]]
              ["HTMLAttributeString", sp1, sp2, delim, v] ->
                    ["HTMLAttribute", sp0, name2, ["HTMLAttributeString", sp1, sp2, delim, value2]]
              ["HTMLAttributeNoValue"] -> 
                 if value2 == "" then ["HTMLAttribute", sp0, name2, ["HTMLAttributeNoValue"]]
                 else toHTMLAttribute [name2, value2]
              _ -> "Error, expected HTMLAttributeUnquoted, HTMLAttributeString, HTMLAttributeNoValue" + 1
        in mergeAttrs (append acc [newIn]) (drop 1 ins) dt
      {deleted}::dt ->
        mergeAttrs acc (drop (len deleted) ins) dt
      {inserted}::dt ->
        let newIns = map toHTMLAttribute inserted in
        mergeAttrs (append acc newIns) ins dt
    in
    letrec toHTMLNode e = case e of
      ["TEXT",v2] -> toHTMLInner v2
      [tag, attrs, children] -> ["HTMLElement", tag, map toHTMLAttribute attrs, "",
           ["RegularEndOpening"], map toHTMLNode children, ["RegularClosing", ""]]
    in
    letrec mergeNodes acc ins d = case d of
      [] -> acc
      {kept}::dt -> mergeNodes (append acc (take (len kept) ins)) (drop (len kept) ins) dt
      {deleted=[deleted]}::{inserted=[inserted]}::dt ->
        let newElement = case [take 1 ins, deleted, inserted] of
          [ [["HTMLInner", v]], _, ["TEXT",v2]] -> toHTMLInner v2
          [ [["HTMLElement", tagName, attrs, ws1, endOp, children, closing]],
            [tag1, attrs1, children1], [tag2, attrs2, children2] ] ->
             if tag2 == tagName then
               ["HTMLElement", tag2, mergeAttrs [] attrs (diff attrs1 attrs2), ws1, endOp,
                  mergeNodes [] children (diff children1 children2), closing]
             else toHTMLNode inserted
          _ -> toHTMLNode inserted
        in
        mergeNodes (append acc [newElement]) (drop 1 ins) dt
      {deleted}::dt ->
        mergeNodes acc (drop (len deleted) ins) dt
      {inserted}::dt ->
        mergeNodes (append acc (map toHTMLNode inserted)) ins dt
    in
    {values = [mergeNodes [] input (diff oldOutput newOutput)]}
}.apply (parseHTML string)

matchIn r x = case extractFirstIn r x of
  ["Nothing"] -> False
  _ -> True


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

List =
  letrec simpleMap f l =
    case l of
      []    -> []
      x::xs -> f x :: simpleMap f xs
  in
  { simpleMap = simpleMap
    map =
      -- TODO lensed version
      simpleMap
    length = -- TODO move all definitions here
      len
    nth = nth
    indexedMap f xs = mapi (\[i,x] -> f i x) xs
  }

-- Maybe --

nothing = ["Nothing"]
just x  = ["Just", x]

-- Tuple --

Tuple =
  { mapFirst f [x, y] = [f x, y]
    mapSecond f [x, y] = [x, f y]
  }

-- Editor --

Editor = {}

-- Update --

Update =
  { freeze = freeze
    applyLens lens x = lens.apply x
  }

-- TODO remove this; add as imports as needed in examples
{freeze, applyLens} = Update

-- Lens: Constant Input

SoftFreeze =
  let
    constantInputLens = { apply x = x, update {input} = { values = [input] } }
  in
  { wrap = Update.applyLens constantInputLens }

-- Custom Update: List Map, List Append, ...

-- TODO

-- HTML

Html =
  let textNode text =
    ["TEXT", text]
  in
  let textElementHelper tag styles attrs text =
    [ tag,  ["style", styles] :: attrs , [ textNode text ] ]
  in
  let elementHelper tag styles attrs children =
    [ tag,  ["style", styles] :: attrs , children ]
  in
  { textNode = textNode
    p = textElementHelper "p"
    th = textElementHelper "th"
    td = textElementHelper "td"
    h1 = textElementHelper "h1"
    h2 = textElementHelper "h2"
    h3 = textElementHelper "h3"
    div_ = elementHelper "div"
    tr = elementHelper "tr"
    table = elementHelper "table"
  }

-- TODO remove this; add as imports as needed in examples
{textNode, p, th, td, h1, h2, h3, div_, tr, table} = Html

-- Lens: Table Library

  -- freeze and constantInputLens aren't actually needed below,
  -- because these definitions are now impicitly frozen in Prelude
  -- But for performance it's better

  -- TODO in wrapData, use update. calculate length of rows to determine empties.

TableWithButtons = {

  wrapData =
    Update.applyLens
      { apply rows   = rows |> map (\row -> [freeze False, row])
      , unapply rows = rows |> concatMap (\[flag,row] ->
                                 if flag == True
                                   then [ row, ["","",""] ]
                                   else [ row ]
                               )
                            |> just
      }

  mapData f =
    map (Tuple.mapSecond f)

  tr flag styles attrs children =
    let [hasBeenClicked, nope, yep] =
      ["has-been-clicked", SoftFreeze.wrap "gray", SoftFreeze.wrap "coral"]
    in
    let onclick =
      """
      var hasBeenClicked = document.createAttribute("@hasBeenClicked");
      var buttonStyle = document.createAttribute("style");

      if (this.parentNode.getAttribute("@hasBeenClicked") == "False") {
        hasBeenClicked.value = "True";
        buttonStyle.value = "color: @yep;";
      } else {
        hasBeenClicked.value = "False";
        buttonStyle.value = "color: @nope;";
      }

      this.parentNode.setAttributeNode(hasBeenClicked);
      this.setAttributeNode(buttonStyle);
      """
    in
    let button = -- text-button.enabled is an SnS class
      [ "span"
      , [ ["class", "text-button.enabled"]
        , ["onclick", onclick]
        , ["style", [["color", nope]]]
        ]
      , [textNode "+"]
      ]
    in
    Html.tr styles
      ([hasBeenClicked, toString flag] :: attrs)
      (snoc button children)

}


-- The type checker relies on the name of this definition.
let dummyPreludeMain = ["svg", [], []] in dummyPreludeMain

module MicroTests where

import Lang exposing (strVal)
import LangParser exposing (parseV, parseE, freshen)
import Eval

_ `ignore` _ = ()

--------------------------------------------------------------------------------

-- right now, these always get run

testParser = ()

  `ignore` parseV "1"
  `ignore` parseV "[1]"
  `ignore` parseV " []"
  `ignore` parseV " [1  2 3]   "

  `ignore` parseE "(\\x 1)"
  `ignore` parseE "(\\(x y z) 1)"
  `ignore` parseE "(let f (\\x (\\y [(+ x 0) (+ x y)])) ((f 3) 5))"
  `ignore` parseE "(let f (\\x (\\y [(+ x 0) (+ x y)])) (f 3 5))"
  `ignore` parseE "(let f (\\(x y) [(+ x 0) (+ x y)]) (f 3 5))"
  `ignore` parseE "(let f (\\(x y) [(+ x 0) (+ x y)]) ((f 3) 5))"

  `ignore` parseE "true"
  `ignore` parseE "(< 1 2)"
  `ignore` parseE "(if true 2 [3])"
  `ignore` parseE "(if (< 1 2) [3] [])"

  `ignore` parseE "[1|2]"
  `ignore` parseE "[1 | 2]"
  `ignore` parseE "[1 2 | 3]"
  `ignore` parseE "  [1 | [2 | [3]]]"

  `ignore` parseE "((\\[x] x) [3])"
  `ignore` parseE "((\\  [x] x) [3])"
  `ignore` parseE "((\\[x y z] (+ x (+ y z))) [1 2 3])"

  `ignore` parseE "(let _ [] [])"
  `ignore` parseE "(case [] ([] true) ([_|_] false))"


--------------------------------------------------------------------------------

makeTest se sv' =
  let e  = freshen (parseE se)
      v  = Eval.run e
      v' = parseV sv'
  in
  {e=e, v=v, vnew=v'}

test0 () =
  makeTest
    "(let f (\\(x y) [(+ x 0) (+ x y)]) (f 3 5))"
    "[3 9]"

test1 () =
  makeTest
    "(if (< 1 2) (+ 2 4) (+ 3 3))"
    "10"

test2 () =
  makeTest
    "(letrec sum (\\n (if (< n 0) 0 (+ n (sum (- n 1))))) (sum 3))"
    "[]"

test3 () =
  makeTest
    "(letrec fact (\\n (if (< n 1) 1 (* n (fact (- n 1))))) (fact 5))"
    "[]"

test4 () =
  makeTest
    "(letrec foo (\\n (if (< n 1) [] [n (foo (- n 1))]))
     (letrec bar (\\n (if (< n 1) [] [n | (bar (- n 1))]))
       [(foo 5) (bar 5)]))"
    "[]"

test5 () =
  makeTest
    "[1 | [2 | [3]]]"
    "[1 2 3]"

test6 () =
  makeTest
    "(let sum3 (\\[x y z] (+ x (+ y z))) (sum3 [1 2 3]))"
    "[1 2 3]"


test7 () =
  makeTest
    "(let hd (\\[hd | tl] hd) (hd [1 2 3]))"
    "[1 2 3]"

test8 () =
  makeTest
    "(let tl (\\[hd | tl] tl) (tl [1 2 3]))"
    "[1 2 3]"

test9 () =
  makeTest
    "(let [x y z] [1 2 3] (+ x (+ y z)))"
    "[1 2 3]"

test10 () =
  makeTest
    "(let isNil (\\xs (case xs ([] true) ([_|_] false)))
       [(isNil []) (isNil [1])])"
    "[1 2 3]"

test11 () =
  makeTest
    "(let plus1 (\\x (+ x 1))
     (letrec map (\\f (\\xs (case xs ([] []) ([hd|tl] [(f hd)|(map f tl)]))))
       (map plus1 [1 2 3])))"
    "[1 2 3]"

test12 () =
  makeTest
    "(let plus1 (\\x (+ x 1))
     (letrec map (\\(f xs) (case xs ([] []) ([hd|tl] [(f hd)|(map f tl)])))
       (map plus1 [1 2 3])))"
    "[1 2 3]"

test13 () =
  makeTest
    "(letrec mult (\\(m n) (if (< m 1) 0 (+ n (mult (- m 1) n))))
       [(mult 0 10) (mult 2 4) (mult 10 9)])"
    "[1 2 3]"

test14 () =
  makeTest
    "(letrec map (\\(f xs) (case xs ([] []) ([hd|tl] [(f hd)|(map f tl)])))
     (letrec mult (\\(m n) (if (< m 1) 0 (+ n (mult (- m 1) n))))
     (let [x0 y0 sep] [10 8 30]
       (map (\\i [(+ x0 (mult i sep)) y0]) [0 1 2]))))"
    "[[10 8] [40 8] [100 8]]"

test15 () =
  makeTest
    "(let [x0 y0 sep] [10 28 30]
       (map (\\i (circle_ (+ x0 (mult i sep)) y0 10)) [0 1 2]))"
    (strVal (Eval.run (parseE
      "[(circle_ 10 28 10) (circle_ 40 28 10) (circle_ 100 28 10)]")))

test16 () =
  makeTest
    "(let [x0 y0 sep] [10 28 30]
       (map (\\i (circle_ (+ x0 (mult i sep)) y0 10)) [0 1 2]))"
    (strVal (Eval.run (parseE
      "[(circle_ 150 28 10) (circle_ 40 28 10) (circle_ 70 28 10)]")))

test17 () =
  makeTest
    "(let [x0 y0 sep] [10 28 30]
       (map (\\i (circle_ (+ x0 (mult i sep)) y0 10)) [0 1 2]))"
    (strVal (Eval.run (parseE
      "[(circle_ 10 28 10) (circle_ 150 28 10) (circle_ 70 28 10)]")))

test18 () =
  makeTest
    "(let [x0 y0 sep] [10 28 30]
       (map (\\i (square_ (+ x0 (mult i sep)) y0 20)) [0 1 2]))"
    (strVal (Eval.run (parseE
      "[(square_ 150 28 20) (square_ 40 28 20) (square_ 70 28 20)]")))

test19 () =
  makeTest
    "(let [x0 y0 sep] [10 28 30]
       (map (\\i (rect_ (+ x0 (mult i sep)) y0 20 30)) [0 1 2]))"
    "[]"

test20 () =
  makeTest
    "(let i 200 [(line_ 10 20 i 40) (line_ 10 70 i 40)])"
    (strVal (Eval.run (parseE
      "[(line_ 10 20 300 40) (line_ 10 70 200 40)]")))

test21 () =
  makeTest
    "[(polygon_ [[10 10] [200 10] [100 50]])]"
    "[]"

-- approximation of elm logo:
-- https://github.com/evancz/elm-svg/blob/1.0.2/examples/Logo.elm
test22 () =
  makeTest
    "(let foo (\\(color pts) (polygon color 'black' 0 pts))
     [ (foo '#F0AD00' [[161 152] [231 82] [91 82]])
       (foo '#7FD13B' [[8 0] [79 70] [232 70] [161 0]])
       (rect '#7FD13B' 192 107 107 108)
       (foo '#60B5CC' [[323 143] [323 0] [179 0]])
       (foo '#5A6378' [[152 161] [0 8] [0 314]])
       (foo '#F0AD00' [[255 246] [323 314] [323 178]])
       (foo '#60B5CC' [[161 170] [8 323] [314 323]])
     ])"
    "[]"


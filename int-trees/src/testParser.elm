module TestParser where

import Lang
import LangParser (parseV, parseE, freshen)

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

  -- `ignore` parseE "(let _ [] [])"
  -- `ignore` parseE "(case [] ([] true) ([_|_] false))"


--------------------------------------------------------------------------------

makeTest se sv' =
  let e  = se |> parseE |> freshen 1 |> fst
      v  = Lang.run e
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


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

--------------------------------------------------------------------------------

makeSyncTest se sv' =
  let e  = se |> parseE |> freshen 1 |> fst
      v  = Lang.run e
      v' = parseV sv'
  in
  {e=e, v=v, vnew=v'}

test0 () =
  makeSyncTest
    "(let f (\\(x y) [(+ x 0) (+ x y)]) (f 3 5))"
    "[3 9]"

test1 () =
  makeSyncTest
    "(if (< 1 2) (+ 2 4) (+ 3 3))"
    "10"

test2 () =
  makeSyncTest
    "(letrec sum (\\n (if (< n 0) 0 (+ n (sum (- n 1))))) (sum 3))"
    "[]"

test3 () =
  makeSyncTest
    "(letrec fact (\\n (if (< n 1) 1 (* n (fact (- n 1))))) (fact 5))"
    "[]"


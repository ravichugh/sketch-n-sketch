module MicroTests where

import Lang exposing (strVal)
import LangParser exposing (parseV, parseE)
import Eval

_ `ignore` _ = ()

--------------------------------------------------------------------------------

-- right now, these always get run

testParser = ()

  `ignore` parseV "1"
  `ignore` parseV "[1]"
  `ignore` parseV " []"
  `ignore` parseV " [1  2 3]   "
  `ignore` parseV " 1.0 "
  `ignore` parseV " -1.0 "
  `ignore` parseV " -1 "

  `ignore` parseE "(\\x 1)"
  `ignore` parseE "(\\(x y z) 1)"
  `ignore` parseE "(let f (\\x (\\y [(+ x 0) (+ x y)])) ((f 3) 5))"
  `ignore` parseE "(let f (\\x (\\y [(+ x 0) (+ x y)])) (f 3 5))"
  `ignore` parseE "(let f (\\(x y) [(+ x 0) (+ x y)]) (f 3 5))"
  `ignore` parseE "(let f (\\(x y) [(+ x 0) (+ x y)]) ((f 3) 5))"
  `ignore` parseE " (- -1 0) "
  `ignore` parseE " (--1 0) "

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
  let e  = parseE se
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
--
-- https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform
-- http://tutorials.jenkov.com/svg/svg-viewport-view-box.html
--
test22 () =
  makeTest
    "(let foo (\\(color pts) (polygon color 'black' 0 pts))
     [ ['svgAttrs' [['x' '0'] ['y' '0'] ['viewBox' '0 0 323.141 322.95']]]
       (foo '#F0AD00' [[161 152] [231 82] [91 82]])
       (foo '#7FD13B' [[8 0] [79 70] [232 70] [161 0]])
       (addAttr
          (rect '#7FD13B' 192 107 107 108)
          ['transform' 'matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)'])
       (foo '#60B5CC' [[323 143] [323 0] [179 0]])
       (foo '#5A6378' [[152 161] [0 8] [0 314]])
       (foo '#F0AD00' [[255 246] [323 314] [323 178]])
       (foo '#60B5CC' [[161 170] [8 323] [314 323]])
     ])"
    "[]"

test23 () =
  makeTest
    "(let [x0 y0 sep] [10 28 30]
       (map2 (\\(i j) (square_ (+ x0 (mult i sep)) (+ y0 (mult j sep)) 20))
             [0 1 2] [0 1 2]))"
    (strVal (Eval.run (parseE
      "[(square_ 150 28 20) (square_ 40 58 20) (square_ 70 88 20)]")))

test24 () =
  makeTest
    "(let [x0 y0 sep] [10 28 30]
       (map2 (\\(i j) (square_ (+ x0 (mult i sep)) (+ y0 (mult j sep)) 20))
             [0 1 2] [0 1 2]))"
    (strVal (Eval.run (parseE
      "[(square_ 10 28 20) (square_ 40 58 20) (square_ 100 88 20)]")))

-- two equations that constrain the same variable, but both have same solution
test25 () =
  makeTest
    "(let [x0 y0 sep] [10 28 30]
       (map2 (\\(i j) (square_ (+ x0 (mult i sep)) (+ y0 (mult j sep)) 20))
             [0 1 2] [0 1 2]))"
    (strVal (Eval.run (parseE
      "[(square_ 10 28 20) (square_ 40 58 20) (square_ 100 118 20)]")))

test26 () =
  makeTest
    "(let [x0 y0 sep] [10 28 30]
       (map (\\[i j] (square_ (+ x0 (mult i sep)) (+ y0 (mult j sep)) 20))
            (cartProd [0 1 2] [0 1])))"
    "[['rect' ['x' 10] ['y' 28] ['width' 20] ['height' 20] ['fill' '#999999']]
      ['rect' ['x' 10] ['y' 58] ['width' 20] ['height' 20] ['fill' '#999999']]
      ['rect' ['x' 40] ['y' 28] ['width' 20] ['height' 20] ['fill' '#999999']]
      ['rect' ['x' 40] ['y' 99] ['width' 20] ['height' 20] ['fill' '#999999']]
      ['rect' ['x' 70] ['y' 28] ['width' 20] ['height' 20] ['fill' '#999999']]
      ['rect' ['x' 70] ['y' 58] ['width' 20] ['height' 20] ['fill' '#999999']]]"

-- changing two leaves, each of which leads to two disjoint solutions
test27 () =
  makeTest
    "(let [x0 y0 xsep ysep] [10 28 30 30]
       (map (\\[i j] (square_ (+ x0 (mult i xsep)) (+ y0 (mult j ysep)) 20))
            (cartProd [0 1 2] [0 1])))"
    "[['rect' ['x' 10] ['y' 28] ['width' 20] ['height' 20] ['fill' '#999999']]
      ['rect' ['x' 10] ['y' 58] ['width' 20] ['height' 20] ['fill' '#999999']]
      ['rect' ['x' 40] ['y' 28] ['width' 20] ['height' 20] ['fill' '#999999']]
      ['rect' ['x' 60] ['y' 78] ['width' 20] ['height' 20] ['fill' '#999999']]
      ['rect' ['x' 70] ['y' 28] ['width' 20] ['height' 20] ['fill' '#999999']]
      ['rect' ['x' 70] ['y' 58] ['width' 20] ['height' 20] ['fill' '#999999']]]"

-- rudimentary olympic rings
test28 () =
  makeTest
    "(let [x0 y0 w r dx dy] [30 30 7 20 32 20]
     (let dxHalf (div dx 2)
     (let row1
       (map (\\[i c] (ring c w (+ x0 (mult i dx)) y0 r))
            (zip [0 1 2] ['blue' 'black' 'red']))
     (let row2
       (map (\\[i c] (ring c w (+ (+ x0 dxHalf) (mult i dx)) (+ y0 dy) r))
            (zip [0 1] ['yellow' 'green']))
       (append row1 row2)))))"
    "[]"

-- similar to test15, but one solution requires floating point division
-- instead of integer division
test29 () =
  makeTest
    "(let [x0 y0 sep] [10 28 30]
       (map (\\i (circle_ (+ x0 (mult i sep)) y0 10)) [0 1 2]))"
    (strVal (Eval.run (parseE
      "[(circle_ 10 28 10) (circle_ 40 28 10) (circle_ 101 28 10)]")))

test30 () =
  makeTest
    "(let [x0 y0 sep rx ry] [10 28 60 15 10]
       (map (\\i (ellipse_ (+ x0 (mult i sep)) y0 rx ry)) [0 1 2]))"
    (strVal (Eval.run (parseE
      "[(ellipse_ 10 28 15 10) (ellipse_ 70 28 25 40) (ellipse_ 130 28 15 10)]")))

test31 () =
  makeTest
    "(let [x0 y0 w h delta] [50 50 200 200 10]
     [ (rect 'white' x0 y0 w h)
       (polygon 'black' 'DUMMY' 0
         [[(+ x0 delta) y0]
          [(+ x0 w) y0]
          [(+ x0 w) (- (+ y0 h) delta)]])
       (polygon 'black' 'DUMMY' 0
         [[x0 (+ y0 delta)]
          [x0 (- (+ y0 h) delta)]
          [(- (+ x0 (div w 2)) delta) (+ y0 (div h 2))]])
       (polygon 'black' 'DUMMY' 0
         [[(+ x0 delta) (+ y0 h)]
          [(- (+ x0 w) delta) (+ y0 h)]
          [(+ x0 (div w 2)) (+ (+ y0 (div h 2)) delta)]])
     ])"
    "[]"

test32 () =
  makeTest
    "(let [x0 y0 w h delta] [50 50 200 200 10]
     [ (rect 'white' x0 y0 w h)
       (polyline 'none' 'black' 1
         [[(+ x0 delta) y0]
          [(+ x0 w) y0]
          [(+ x0 w) (- (+ y0 h) delta)]])
       (polyline 'none' 'black' 1
         [[x0 (+ y0 delta)]
          [x0 (- (+ y0 h) delta)]
          [(- (+ x0 (div w 2)) delta) (+ y0 (div h 2))]])
       (polyline 'none' 'black' 1
         [[(+ x0 delta) (+ y0 h)]
          [(- (+ x0 w) delta) (+ y0 h)]
          [(+ x0 (div w 2)) (+ (+ y0 (div h 2)) delta)]])
     ])"
    "[]"

test33 () =
  makeTest
    "[ (path_ ['M' 20 20 'L' 60 20 'L' 60 80 'Z'])
       (path_ ['M' 10 10 'H' 90 'V' 90 'H' 10 'L' 10 10 'Z'])
       (path_ ['M' 150 0 'L' 75 200 'L' 225 200 'Z'])
     ]"
    "[]"

test34 () =
  makeTest
    "[ (path_ ['M' 10 10   'C' 20 20 40 20 50 10])
       (path_ ['M' 70 10   'C' 70 20 120 20 120 10])
       (path_ ['M' 130 10  'C' 120 20 180 20 170 10])
       (path_ ['M' 10 60   'C' 20 80 40 80 50 60])
       (path_ ['M' 70 60   'C' 70 80 110 80 110 60])
       (path_ ['M' 130 60  'C' 120 80 180 80 170 60])
       (path_ ['M' 10 110  'C' 20 140 40 140 50 110])
       (path_ ['M' 70 110  'C' 70 140 110 140 110 110])
       (path_ ['M' 130 110 'C' 120 140 180 140 170 110])
     ]"
    "[]"

test35 () =
  makeTest
    "[ (path_ ['M' 10 80 'C' 40 10 65 10 95 80 'S' 150 150 180 80])
       (path_ ['M' 10 80 'Q' 95 10 180 80])
       (path_ ['M' 10 80 'Q' 52.5 10 95 80 'T' 180 80])
     ]"
    "[]"

test36 () =
  makeTest
    "[ (addAttr
         (path 'green' 'black' 2
           ['M' 10 315
            'L' 110 215
            'A' 30 50 0 0 1 162.55 162.45
            'L' 172.55 152.45
            'A' 30 50 -45 0 1 215.1 109.9
            'L' 315 10])
         ['opacity' 0.5]) ]"
    "[]"

test37 () =
  makeTest
    "[ (path 'green' 'black' 2
         ['M' 80 80 'A' 45 45 0 0 0 125 125 'L' 125 80 'Z'])
       (path 'red' 'black' 2
         ['M' 230 80 'A' 45 45 0 1 0 275 125 'L' 275 80 'Z'])
       (path 'purple' 'black' 2
         ['M' 80 230 'A' 45 45 0 0 1 125 275 'L' 125 230 'Z'])
       (path 'blue' 'black' 2
         ['M' 230 230 'A' 45 45 0 1 1 275 275 'L' 275 230 'Z'])
     ]"
    "[]"

test38 () =
  makeTest
    "[ ['text'
         [['x' 10] ['y' 20] ['style' 'fill:red']]
         [['TEXT' 'Several lines:']
          ['tspan' [['x' 10] ['y' 45]] [['TEXT' 'First line.']]]
          ['tspan' [['x' 10] ['y' 70]] [['TEXT' 'Second line.']]] ]]
     ]"
    "[]"


module Examples where

import Lang
import LangParser
import Eval
import MicroTests
import Utils

makeExample name s =
  let thunk () =
    let e = Utils.fromOk_ (LangParser.parseE s) in
    let v = Eval.run e in
    {e=e, v=v}
  in
  (name, thunk)

logo = "
  ; sketch-n-sketch logo
  ;
  (let [x0 y0 w h delta] [50 50 200 200 10]
  (let [xw yh w2 h2] [(+ x0 w) (+ y0 h) (div w 2) (div h 2)]
  (let poly (\\pts (polygon 'black' 'none' 0 pts))
  (svg [
    (rect 'white' x0 y0 w h)
    (poly
      [[(+ x0 delta) y0]
       [xw y0]
       [xw (- yh delta)]])
    (poly
      [[x0 (+ y0 delta)]
       [x0 (- yh delta)]
       [(- (+ x0 w2) delta) (+ y0 h2)]])
    (poly
      [[(+ x0 delta) yh]
       [(- xw delta) yh]
       [(+ x0 w2) (+ (+ y0 h2) delta)]])
  ]))))
"

threeBoxes = "
  (let [x0 y0 sep] [40 28 110]
  (svg (map (\\i (rect 'lightblue' (+ x0 (mult i sep)) y0 60 130)) [0 1 2])))
"

sixBoxesA = "
  (let [x0 y0 sep] [10 28 60]
  (svg
    (map (\\[i j] (square_ (+ x0 (mult i sep)) (+ y0 (mult j sep)) 50))
         (cartProd [0 1 2] [0 1]))))
"

sixBoxesB = "
  (let [x0 y0 xsep ysep] [10 28 60 60]
  (svg
    (map (\\[i j] (square_ (+ x0 (mult i xsep)) (+ y0 (mult j ysep)) 50))
         (cartProd [0 1 2] [0 1]))))
"

blank = "
  ;
  ; Write a little program below.
  ; Or choose an example from the list.
  ;
  (svg [(rect 'maroon' 100 15 200 50)])
"

elmLogo = "
  ; Elm logo, based on:
  ; https://github.com/evancz/elm-svg/blob/1.0.2/examples/Logo.elm
  ;
  ; Notice how the 'viewBox' attribute puts the canvas in
  ; \"full screen\" mode. Also, although we don't currently handle
  ; rotations (i.e. 'transform's) specially, the resulting zone
  ; is still useful; toggle the Zones option to see.
  ;
  (let foo (\\(color pts) (polygon color 'black' 0 pts))
  [ 'svg' [['x' '0'] ['y' '0'] ['viewBox' '0 0 323.141 322.95']]
    [
    (foo '#F0AD00' [[161 152] [231 82] [91 82]])
    (foo '#7FD13B' [[8 0] [79 70] [232 70] [161 0]])
    (addAttr
       (rect '#7FD13B' 192 107 107 108)
       ['transform' 'matrix(0.7071 0.7071 -0.7071 0.7071 186.4727 -127.2386)'])
    (foo '#60B5CC' [[323 143] [323 0] [179 0]])
    (foo '#5A6378' [[152 161] [0 8] [0 314]])
    (foo '#F0AD00' [[255 246] [323 314] [323 178]])
    (foo '#60B5CC' [[161 170] [8 323] [314 323]])
    ]
  ])
"

examples =
  [ makeExample "Scratch" blank
  , makeExample "3 Boxes" threeBoxes
  , makeExample "6 Boxes A" sixBoxesA
  , makeExample "6 Boxes B" sixBoxesB
  , makeExample "Logo" logo
  , makeExample "Elm Logo" elmLogo
  ]

list = examples ++ MicroTests.sampleTests


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

examples =
  [ makeExample "3 Boxes" threeBoxes
  , makeExample "6 Boxes A" sixBoxesA
  , makeExample "6 Boxes B" sixBoxesB
  , makeExample "Logo" logo
  ]

list = examples ++ MicroTests.sampleTests


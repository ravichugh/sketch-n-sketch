module Examples where

import Lang
import LangParser
import Eval
import MainSvg

makeExample name s =
  let thunk () =
    let e = LangParser.parseE s in
    let v = Eval.run e in
    {e=e, v=v}
  in
  (name, thunk)

logo = "
  (let [x0 y0 w h delta] [50 50 200 200 10]
  (svg [
    (rect 'white' x0 y0 w h)
    (polygon 'black' 'DUMMY' 0
      [[(+ x0 delta) y0]
       [(+ x0 w) y0]
       [(+ x0 w) (minus (+ y0 h) delta)]])
    (polygon 'black' 'DUMMY' 0
      [[x0 (+ y0 delta)]
       [x0 (minus (+ y0 h) delta)]
       [(minus (+ x0 (div w 2)) delta) (+ y0 (div h 2))]])
    (polygon 'black' 'DUMMY' 0
      [[(+ x0 delta) (+ y0 h)]
       [(minus (+ x0 w) delta) (+ y0 h)]
       [(+ x0 (div w 2)) (+ (+ y0 (div h 2)) delta)]])
  ]))
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

list = examples ++ MainSvg.sampleTests


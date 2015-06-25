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

rings = "
  (let [x0 y0 w r dx dy] [30 30 7 20 32 20]
  (let dxHalf (div dx 2)
  (let row1
    (map (\\[i c] (ring c w (+ x0 (mult i dx)) y0 r))
         (zip [0 1 2] ['blue' 'black' 'red']))
  (let row2
    (map (\\[i c] (ring c w (+ (+ x0 dxHalf) (mult i dx)) (+ y0 dy) r))
         (zip [0 1] ['yellow' 'green']))
    (svg (append row1 row2))))))
"

polygons = "
  ; TODO polygons, based on tests 45,46,47
  (svg [])
"

stars = "
  ; TODO stars, based on test48
  (svg [])
"

sliders = "
  ;
  ; The ni constants get adjusted by the sliders,
  ; and then clamped to fit within the [min, max] range.
  ; Also try changing the min and max constants below.
  ;
  (let [min max] [0! 10!]
  (let [n1 n2 n3 n4] [5 5 5 5]
  (let [m1 m2 m3 m4] (map (clamp min max) [n1 n2 n3 n4])
  ;
  ; Both the horizontal and vertical slider abstractions
  ; below take a dropBall parameter:
  ;  - if true, the ball can slide off the rail;
  ;  - if false, the ball disappears when off the rail.
  ;
  (let hSlider (\\(dropBall xStart xEnd y minVal maxVal curVal)
    (let [rPoint wLine rBall] [4! 3! 10!]
    (let [xDiff valDiff] [(- xEnd xStart) (- maxVal minVal)]
    (let xBall (+ xStart (* xDiff (/ (- curVal minVal) valDiff)))
    (let xBall_ (clamp xStart xEnd xBall)
    (let rBall_ (if dropBall (if (= xBall_ xBall) rBall 0) rBall)
      [ (circle 'black' xStart y rPoint)
        (circle 'black' xEnd y rPoint)
        (line 'black' wLine xStart y xEnd y)
        (circle 'black' xBall y rBall_)
      ]))))))
  ;
  (let vSlider (\\(dropBall yStart yEnd x minVal maxVal curVal)
    (let [rPoint wLine rBall] [4! 3! 10!]
    (let [yDiff valDiff] [(- yEnd yStart) (- maxVal minVal)]
    (let yBall (+ yStart (* yDiff (/ (- curVal minVal) valDiff)))
    (let yBall_ (clamp yStart yEnd yBall)
    (let rBall_ (if dropBall (if (= yBall_ yBall) rBall 0) rBall)
      [ (circle 'black' x yStart rPoint)
        (circle 'black' x yEnd rPoint)
        (line 'black' wLine x yStart x yEnd)
        (circle 'black' x yBall rBall_)
      ]))))))
  ;
  ; TODO display m values on canvas
  ; TODO y (resp x) needs to be thawed in s1/s2 (resp s3/s4)
  ;      until change to loc-mappings...
  ;
  (let sliders
    (let s1 (hSlider false 30! 230! 30 min max n1)
    (let s2 (hSlider true 30! 230! 60 min max n2)
    (let s3 (vSlider false 100! 300 80 min max n3)
    (let s4 (vSlider true 100! 300 180 min max n4)
      (foldl append nil [s1 s2 s3 s4])))))
  ;
  (let displays
    (let t1 (text 300 100 (+ 'm1=' (toString m1)))
    (let t2 (text 300 120 (+ 'm2=' (toString m2)))
    (let t3 (text 300 140 (+ 'm3=' (toString m3)))
    (let t4 (text 300 160 (+ 'm4=' (toString m4)))
      [t1 t2 t3 t4]))))
  ;
    (svg (append sliders displays)))))))))
"

todo = "
  ; TODO
  (svg [])
"

examples =
  [ makeExample "Scratch" blank
  , makeExample "3 Boxes" threeBoxes
  , makeExample "6 Boxes A" sixBoxesA
  , makeExample "6 Boxes B" sixBoxesB
  , makeExample "Logo" logo
  , makeExample "Elm Logo" elmLogo
  , makeExample "Rings" rings
  , makeExample "Polygons" polygons
  , makeExample "Stars" stars
  , makeExample "Sliders" sliders
  , makeExample "US-13 Flag" todo
  , makeExample "US-50 Flag" todo
  , makeExample "French Sudan Flag" todo
  , makeExample "Ferris Wheel" todo
  ]

list = examples ++ MicroTests.sampleTests


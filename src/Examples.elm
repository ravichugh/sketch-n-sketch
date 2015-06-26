module Examples (list, scratchName, scratch) where

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

--------------------------------------------------------------------------------

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

scratch = "
  ;
  ; Write a little program below.
  ; Or choose an example from the list.
  ;
  ; Changes made to this *Scratch* example will be saved and
  ; restored when navigating to and from other examples.
  ; For the remaining named examples, changes will be discarded
  ; when choosing a different example.
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
  ;
  (let nStar (\\(fill stroke w n len1 len2 rot cx cy)
    (let pti (\\[i len]
      (let anglei (+ (- (/ (* i (pi)) n) rot) halfPi)
      (let xi (+ cx (* len (cos anglei)))
      (let yi (+ cy (neg (* len (sin anglei))))
        [xi yi]))))
    (let lengths
      (map (\\b (if b len1 len2))
           (concat (repeat n [true false])))
    (let indices (list0N (- (* 2! n) 1!))
      (polygon fill stroke w (map pti (zip indices lengths)))))))
  ;
  (let [x0 y0 sep ni nj] [100 100 100 3! 7!]
  (let [outerLen innerLen] [50 20]
  (let iStar (\\i
     (let off (mult (- i ni) sep)
     (let [xi yi] [(+ x0 off) (+ y0 off)]
     (nStar 'goldenrod' 'black' 3 i outerLen innerLen 0! xi yi))))
  ;
  (svg (map iStar (range ni nj)))))))
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
  (let horizSlider (\\(dropBall xStart xEnd y minVal maxVal curVal)
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
  (let vertSlider (\\(dropBall yStart yEnd x minVal maxVal curVal)
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
  (let sliders
    (let s1 (horizSlider false 30! 230! 30! min max n1)
    (let s2 (horizSlider true 30! 230! 70! min max n2)
    (let s3 (vertSlider false 110! 300! 110! min max n3)
    (let s4 (vertSlider true 110! 300! 150! min max n4)
      (foldl append nil [s1 s2 s3 s4])))))
  ;
  (let displays
    (let t1 (text 300 100 (+ 'm1 = ' (toString m1)))
    (let t2 (text 300 120 (+ 'm2 = ' (toString m2)))
    (let t3 (text 300 140 (+ 'm3 = ' (toString m3)))
    (let t4 (text 300 160 (+ 'm4 = ' (toString m4)))
      [t1 t2 t3 t4]))))
  ;
    (svg (append sliders displays)))))))))
"

clique = "
  (let node (\\[x y] (circle 'lightblue' x y 20))
  (let edge (\\[[x y] [i j]] (line 'lightgreen' 5 x y i j))
  (letrec genpairs
     (\\xs
       (case xs
         ([x y | xx] [[x y] | (append (genpairs (cons x xx)) (genpairs (cons y xx)))])
         ([x] [])
         ([] [])))
  (let pts [[200 50] [400 50] [100 223] [200 389] [400 391] [500 223]]
  (let nodes (map node pts)
  (let pairs (genpairs  pts)
  (let edges (map edge pairs)
    (svg (append edges nodes)))))))))
"

frenchSudan = "
    (let [x0 y0 w h] [50 30 150 300]
    (let xoff (+ x0 w)
    (let yoff (+ y0 (/ h 4))
    (let stripe (\\[color x] (rect color x y0 w h))
    (let minrad
      (if (< (/ w 7.5!) (/ h 15!))
        (/ w 7.5!)
        (/ h 15!))
    (let figline (\\[[a b] [c d]] (line 'black' (/ minrad 2) a b c d))
    (let [x1 x2 x3] (map (\\n (+ x0 (* w n))) [1.2 1.5 1.8])
    (let [y1 y2 y3 y4] (map (\\n (+ y0 (/ h n))) [4.3 2.8 1.9 1.4])
      (svg 
        (append
          (map stripe [['blue' x0] ['white' (+ x0 w)] ['red' (+ x0 (* 2 w))]])
          (snoc
            (ellipse 'black' x2 y1 (/ w 7.5!) (/ h 15!))
            (map
              figline
              [[[x1 y1] [x1 y2]]
               [[x1 y2] [x3 y2]]
               [[x3 y1] [x3 y2]]
               [[x1 y4] [x1 y3]]
               [[x1 y3] [x3 y3]]
               [[x3 y3] [x3 y4]]
               [[x2 y1] [x2 y3]]]))))))))))))
"

usFlag50 = "
    (let [x0 y0 ni nj pts w h rad] [108 20 0! 12! 5! 500 20 6]
    (let block (rect '#09096d' x0 y0 (* w (/ 2! 5!)) (* 7! h))
    (let stripes
      (map
        (\\i (rect 'red' x0 (+ y0 (* i h)) w h))
        [0! 2! 4! 6! 8! 10! 12!])
    (let base (append stripes [block])
      (svg 
        (append
          base
          (map
            (\\[i j]
              (let xsep (/ w 15!)
              (let ysep (* h 1.3)
                (circle
                  'white'
                  (+ x0 (* i xsep))
                  (+ y0 (* j ysep))
                  rad))))
          (append (cartProd (range 0.5 5.5) (range 0.75 4.75)) (cartProd (range 1 5) (range 1.2 4.2))))))))))
"

usFlag13 ="
    (let nstar
    (\\(n cx cy len1 len2 rot)
      (let pti
        (\\[i len]
          (let anglei (+ rot (/ (* i (pi)) n))
          (let xi (+ cx (* len (cos anglei)))
          (let yi (+ cy (* len (sin anglei)))
            [xi yi]))))
      (let lengths
        (map
          (\\b
            (if b
              len1
              len2))
          (concat  (repeat n [true false])))
      (let indices (list0N  (- (* 2! n) 1!))
        (polygon 'white' 'DUMMY' 0 (map pti (zip indices lengths)))))))
    (let rotate (\\a (/ (* (+ 9! a) (pi)) 6!))
    (let [x0 y0 ni nj pts w h] [108 20 0! 12! 5! 500 20]
    (let [blockw blockh] [(/ w 3!) (* 7! h)]
    (let min
      (if (< blockw blockh)
        (* 0.4! blockw)
        (* 0.4! blockh))
    (let [outerLen innerLen] [10 4]
    (let block (rect '#09096d' x0 y0 blockw blockh)
    (let stripes
      (map
        (\\i (rect 'red' x0 (+ y0 (* i h)) w h))
        [0! 2! 4! 6! 8! 10! 12!])
    (let base (append stripes [block])
      (svg 
        (append
          base
          (map
            (\\i
                (nstar
                  pts
                  (+ (+ x0 (/ w 6!)) (* min (cos (rotate  i))))
                  (+ (+ y0 (* h 3.5!)) (* min (sin (rotate  i))))
                  outerLen
                  innerLen
                  (rotate  i)))
          (range ni nj)))))))))))))
"

flw1 = "
    (let [x0 y0 w h max] [69 55 53.2 74.4 10!]
    (let xoff (\\n (+ x0 (* w n)))
    (let yoff (\\n (+ y0 (* h n)))
    (let blkline (\\[[a b] [c d]] (line 'black' 3 a b c d))
    (let redpoly
      (\\[a b]
        (polygon
          'red'
          'black'
          3
          [[(xoff  a) (yoff  a)]
           [(xoff  a) (yoff  b)]
           [(xoff  b) (yoff  b)]
           [(xoff  b) (yoff  a)]]))
    (let dimension
      [0! 1 2 2.9 2.4 1.5 9.1 7.9 8.2 8.7 10!]
    (let verticals
      (zip
        (map (\\n [(xoff  n) y0]) dimension)
        (map (\\n [(xoff  n) (+ y0 (* h max))]) dimension))
    (let horizontals
      (zip
        (map (\\n [x0 (yoff  n)]) dimension)
        (map (\\n [(+ x0 (* w max)) (yoff  n)]) dimension))
      (svg 
        (append
          (map blkline (append verticals horizontals))
          (append
            (append
              (let [p0 p1 p2 p3 p4] [0 1 2 2.9 5]
                (map redpoly [[p0 p1] [p1 p2] [p2 p3] [p3 p4]]))
              (map (\\[x y] (ellipse 'blue' x y (* w 4) h)) [[(xoff  5) (yoff  9)]]))
            (map
              (\\[x y r] (circle 'yellow' x y r))
              [[(xoff  6) (yoff  1.75) (+ w h)]
               [(xoff  6) (yoff  7) (/ (+ w h) 4)]
               [(xoff  6) (yoff  5) (/ (+ w h) 2)]]))))))))))))
"

ferris = "
  ;
  ; Take this ferris wheel for a spin!
  ;
  (let [numSpokes_ spokeLen_ rotAngle_] [5 80 0]
  ;
  (let [numSpokes s1] (hSlider true 20! 420! 20! 3! 10! numSpokes_)
  (let [spokeLen s2] (hSlider true 20! 420! 50! 40! 200! spokeLen_)
  (let [rotAngle s3] (hSlider false 20! 420! 80! (neg twoPi) twoPi rotAngle_)
  ;
  (let sliders (concat [s1 s2 s3])
  (let wheel
    (let [cx cy] [220! 300!]
    (let rim [(ring 'darkgray' 8! cx cy spokeLen)]
    (let center [(circle 'black' cx cy 20!)]
    (let frame [(nStar 'goldenrod' 'darkgray' 3! numSpokes spokeLen 0! rotAngle cx cy)]
    (let spokePts (nPointsOnCircle numSpokes rotAngle cx cy spokeLen)
    (let caps (map (\\[x y] (circle 'black' x y 7!)) spokePts)
    (let cars
      (let wCar 30!
      (let wHalfCar (/ wCar 2!)
      (map (\\[x y] (squareCenter 'lightgray' x y wCar)) spokePts)))
    (concat [rim cars center frame caps]))))))))
  ;
  (svg (append sliders wheel))))))))
"

--------------------------------------------------------------------------------

todo = "
  ; TODO
  (svg [])
"

scratchName = "*Scratch*"

examples =
  [ makeExample scratchName scratch
  , makeExample "3 Boxes" threeBoxes
  , makeExample "6 Boxes A" sixBoxesA
  , makeExample "6 Boxes B" sixBoxesB
  , makeExample "Logo" logo
  , makeExample "Elm Logo" elmLogo
  , makeExample "Rings" rings
  , makeExample "Polygons" polygons
  , makeExample "Stars" stars
  , makeExample "Sliders" sliders
  , makeExample "US-13 Flag" usFlag13
  , makeExample "US-50 Flag" usFlag50
  , makeExample "French Sudan Flag" frenchSudan
  , makeExample "Frank Lloyd Wright" flw1
  , makeExample "Ferris Wheel" ferris
  , makeExample "Clique" clique
  ]

list = examples ++ MicroTests.sampleTests


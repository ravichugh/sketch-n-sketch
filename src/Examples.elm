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

frenchsudan = "
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

currentusflag = "
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

colonialflag ="
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

franklloydwright1 = "
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

examples =
  [ makeExample "3 Boxes" threeBoxes
  , makeExample "6 Boxes A" sixBoxesA
  , makeExample "6 Boxes B" sixBoxesB
  , makeExample "Logo" logo
  , makeExample "French Sudan Flag" frenchsudan
  , makeExample "U.S. Flag" currentusflag
  , makeExample "U.S. Colonial Flag" colonialflag
  , makeExample "Frank Lloyd Wright Design" franklloydwright1
  ]

list = examples ++ MicroTests.sampleTests


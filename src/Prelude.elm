module Prelude (src) where

src = "

(let id (\\x x)

(let always (\\(x _) x)

(let compose (\\(f g) (\\x (f (g x))))

(let fst (\\[x|_] x)

(letrec len (\\xs (case xs ([] 0) ([_ | xs1] (+ 1 (len xs1)))))

(letrec map (\\(f xs)
  (case xs ([] []) ([hd|tl] [(f hd)|(map f tl)])))

(letrec map2 (\\(f xs ys)
  (case [xs ys]
    ([[x|xs1] [y|ys1]] [ (f x y) | (map2 f xs1 ys1) ])
    (_                 [])))

(letrec foldl (\\(f acc xs)
  (case xs ([] acc) ([x|xs1] (foldl f (f x acc) xs1))))

(letrec foldr (\\(f acc xs)
  (case xs ([] acc) ([x|xs1] (f x (foldr f acc xs1)))))

(letrec append (\\(xs ys)
  (case xs ([] ys) ([x|xs1] [ x | (append xs1 ys)])))

(let concat (foldr append [])

(let concatMap (\\(f xs) (concat (map f xs)))

(let cartProd (\\(xs ys)
  (concatMap (\\x (map (\\y [x y]) ys)) xs))

(let zip (map2 (\\(x y) [x y]))

(let nil  []
(let cons (\\(x xs) [x | xs])
(let snoc (\\(x ys) (append ys [x]))
(let hd   (\\[x|xs] x)
(let tl   (\\[x|xs] xs)

(let reverse (foldl cons nil)

(letrec range (\\(i j)
  (if (< i (+ j 1))
      (cons i (range (+ i 1) j))
      nil))

(let list0N
  (letrec foo (\\i (if (< i 0) nil (cons i (foo (- i 1)))))
  (compose reverse foo))

(let list1N (\\n (range 1 n))

(let repeat (\\(n x) (map (always x) (range 1 n)))

(letrec intermingle (\\(xs ys)
  (case [xs ys]
    ([[x|xs1] [y|ys1]] (cons x (cons y (intermingle xs1 ys1))))
    ([[]      []]      nil)
    (_                 (append xs ys))))

(letrec mult (\\(m n)
  (if (< m 1) 0 (+ n (mult (+ m -1) n))))

(let minus (\\(x y) (+ x (mult y -1)))

(letrec div (\\(m n)
  (if (< m n) 0
  (if (< n 2) m
    (+ 1 (div (minus m n) n)))))

(let neg (\\x (- 0 x))

(let not (\\b (if b false true))

(let clamp (\\(i j n) (if (< n i) i (if (< j n) j n)))

(let joinStrings (\\(sep ss)
  (foldr (\\(str acc) (if (= acc '') str (+ str (+ sep acc)))) '' ss))

;
; SVG Manipulating Functions
;

(let circle (\\(fill x y r)
  ['circle'
     [['cx' x] ['cy' y] ['r' r] ['fill' fill]]
     []])

(let ring (\\(c w x y r)
  ['circle'
     [ ['cx' x] ['cy' y] ['r' r] ['fill' 'none'] ['stroke' c] ['stroke-width' w] ]
     []])

(let ellipse (\\(fill x y rx ry)
  ['ellipse'
     [ ['cx' x] ['cy' y] ['rx' rx] ['ry' ry] ['fill' fill] ]
     []])

(let rect (\\(fill x y w h)
  ['rect'
     [ ['x' x] ['y' y] ['width' w] ['height' h] ['fill' fill] ]
     []])

(let square (\\(fill x y side) (rect fill x y side side))

(let line (\\(fill w x1 y1 x2 y2)
  ['line'
     [ ['x1' x1] ['y1' y1] ['x2' x2] ['y2' y2] ['stroke' fill] ['stroke-width' w] ]
     []])

(let polygon (\\(fill stroke w pts)
  ['polygon'
     [ ['fill' fill] ['points' pts] ['stroke' stroke] ['stroke-width' w] ]
     []])

(let polyline (\\(fill stroke w pts)
  ['polyline'
     [ ['fill' fill] ['points' pts] ['stroke' stroke] ['stroke-width' w] ]
     []])

(let path (\\(fill stroke w d)
  ['path'
     [ ['fill' fill] ['stroke' stroke] ['stroke-width' w] ['d' d] ]
     []])

(let text (\\(x y s)
   ['text' [['x' x] ['y' y] ['style' 'fill:black']]
           [['TEXT' s]]])

(let addAttr (\\([shapeKind oldAttrs children] newAttr)
  [shapeKind (snoc newAttr oldAttrs) children])

(let svg (\\shapes ['svg' [] shapes])
(let svgViewBox (\\(xMax yMax shapes)
  (let [sx sy] [(toString xMax) (toString yMax)]
  ['svg'
    [['x' '0'] ['y' '0'] ['viewBox' (joinStrings ' ' ['0' '0' sx sy])]]
    shapes]))

(let rectCenter (\\(fill cx cy w h)
  (rect fill (- cx (/ w 2)) (- cy (/ h 2)) w h))

(let square (\\(fill x y w) (rect fill x y w w))
(let squareCenter (\\(fill cx cy w) (rectCenter fill cx cy w w))

(let circle_    (circle 'red')
(let ellipse_   (ellipse 'orange')
(let rect_      (rect '#999999')
(let square_    (square '#999999')
(let line_      (line 'blue' 2)
(let polygon_   (polygon 'green' 'purple' 3)
(let path_      (path 'transparent' 'goldenrod' 5)

; TODO add constant literals to patterns, and match 'svg'
;
; accDiff pre-condition: indices in increasing order
;   (so can't just use foldr instead of reverse . foldl)
;
(let updateCanvas (\\([_ svgAttrs oldShapes] diff)
  (let oldShapesI (zip (list1N (len oldShapes)) oldShapes)
  (let initAcc [[] diff]
  (let f (\\([i oldShape] [accShapes accDiff])
    (case accDiff
      ([]
        [(cons oldShape accShapes) accDiff])
      ([[j newShape] | accDiffRest]
        (if (= i j)
          [(cons newShape accShapes) accDiffRest]
          [(cons oldShape accShapes) accDiff]))))
  (let newShapes (reverse (fst (foldl f initAcc oldShapesI)))
    ['svg' svgAttrs newShapes])))))

; \"constant folding\"
(let twoPi (* 2 (pi))
(let halfPi (/ (pi) 2)

; TODO explain coordinate system for these functions

(let nPointsOnUnitCircle (\\(n rot)
  (let off (- halfPi rot)
  (let foo (\\i
    (let ang (+ off (* (/ i n) twoPi))
    [(cos ang) (neg (sin ang))]))
  (map foo (list0N (- n 1))))))

(let nPointsOnCircle (\\(n rot cx cy r)
  (let pts (nPointsOnUnitCircle n rot)
  (map (\\[x y] [(+ cx (* x r)) (+ cy (* y r))]) pts)))

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

(let hSlider_ (\\(dropBall roundInt xStart xEnd y minVal maxVal curVal)
  (let [rPoint wLine rBall] [4! 3! 10!]
  (let [xDiff valDiff] [(- xEnd xStart) (- maxVal minVal)]
  (let xBall (+ xStart (* xDiff (/ (- curVal minVal) valDiff)))
  (let xBall_ (clamp xStart xEnd xBall)
  (let rBall_ (if dropBall (if (= xBall_ xBall) rBall 0) rBall)
  (let val
    (let val_ (clamp minVal maxVal curVal)
    (if roundInt (round val_) val_))
  (let shapes
    [ (line 'black' wLine xStart y xEnd y)
      (circle 'black' xStart y rPoint)
      (circle 'black' xEnd y rPoint)
      (circle 'black' xBall y rBall_)
      (text (+ xEnd 10) (+ y 5) (toString val)) ]
  [val shapes]))))))))

(let hSlider (hSlider_ true)

0)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

"

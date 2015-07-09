module PreludeGenerated (src) where

prelude = "

(def id (\\x x))

(def always (\\(x _) x))

(def compose (\\(f g) (\\x (f (g x)))))

(def fst (\\[x|_] x))

(defrec len (\\xs (case xs ([] 0) ([_ | xs1] (+ 1 (len xs1))))))

(defrec map (\\(f xs)
  (case xs ([] []) ([hd|tl] [(f hd)|(map f tl)]))))

(defrec map2 (\\(f xs ys)
  (case [xs ys]
    ([[x|xs1] [y|ys1]] [ (f x y) | (map2 f xs1 ys1) ])
    (_                 []))))

(defrec foldl (\\(f acc xs)
  (case xs ([] acc) ([x|xs1] (foldl f (f x acc) xs1)))))

(defrec foldr (\\(f acc xs)
  (case xs ([] acc) ([x|xs1] (f x (foldr f acc xs1))))))

(defrec append (\\(xs ys)
  (case xs ([] ys) ([x|xs1] [ x | (append xs1 ys)]))))

(def concat (foldr append []))

(def concatMap (\\(f xs) (concat (map f xs))))

(def cartProd (\\(xs ys)
  (concatMap (\\x (map (\\y [x y]) ys)) xs)))

(def zip (map2 (\\(x y) [x y])))

(def nil  [])
(def cons (\\(x xs) [x | xs]))
(def snoc (\\(x ys) (append ys [x])))
(def hd   (\\[x|xs] x))
(def tl   (\\[x|xs] xs))

(def reverse (foldl cons nil))

(defrec range (\\(i j)
  (if (< i (+ j 1))
      (cons i (range (+ i 1) j))
      nil)))

(def list0N
  (letrec foo (\\i (if (< i 0) nil (cons i (foo (- i 1)))))
  (compose reverse foo)))

(def list1N (\\n (range 1 n)))

(def repeat (\\(n x) (map (always x) (range 1 n))))

(defrec intermingle (\\(xs ys)
  (case [xs ys]
    ([[x|xs1] [y|ys1]] (cons x (cons y (intermingle xs1 ys1))))
    ([[]      []]      nil)
    (_                 (append xs ys)))))

(defrec mult (\\(m n)
  (if (< m 1) 0 (+ n (mult (+ m -1) n)))))

(def minus (\\(x y) (+ x (mult y -1))))

(defrec div (\\(m n)
  (if (< m n) 0
  (if (< n 2) m
    (+ 1 (div (minus m n) n))))))

(def neg (\\x (- 0 x)))

(def not (\\b (if b false true)))

(def implies (\\(p q) (if p q true)))

(def clamp (\\(i j n) (if (< n i) i (if (< j n) j n))))

(def joinStrings (\\(sep ss)
  (foldr (\\(str acc) (if (= acc '') str (+ str (+ sep acc)))) '' ss)))

;
; SVG Manipulating Functions
;

(def circle (\\(fill x y r)
  ['circle'
     [['cx' x] ['cy' y] ['r' r] ['fill' fill]]
     []]))

(def ring (\\(c w x y r)
  ['circle'
     [ ['cx' x] ['cy' y] ['r' r] ['fill' 'none'] ['stroke' c] ['stroke-width' w] ]
     []]))

(def ellipse (\\(fill x y rx ry)
  ['ellipse'
     [ ['cx' x] ['cy' y] ['rx' rx] ['ry' ry] ['fill' fill] ]
     []]))

(def rect (\\(fill x y w h)
  ['rect'
     [ ['x' x] ['y' y] ['width' w] ['height' h] ['fill' fill] ]
     []]))

(def square (\\(fill x y side) (rect fill x y side side)))

(def line (\\(fill w x1 y1 x2 y2)
  ['line'
     [ ['x1' x1] ['y1' y1] ['x2' x2] ['y2' y2] ['stroke' fill] ['stroke-width' w] ]
     []]))

(def polygon (\\(fill stroke w pts)
  ['polygon'
     [ ['fill' fill] ['points' pts] ['stroke' stroke] ['stroke-width' w] ]
     []]))

(def polyline (\\(fill stroke w pts)
  ['polyline'
     [ ['fill' fill] ['points' pts] ['stroke' stroke] ['stroke-width' w] ]
     []]))

(def path (\\(fill stroke w d)
  ['path'
     [ ['fill' fill] ['stroke' stroke] ['stroke-width' w] ['d' d] ]
     []]))

(def text (\\(x y s)
   ['text' [['x' x] ['y' y] ['style' 'fill:black']
            ['font-family' 'Tahoma, sans-serif']]
           [['TEXT' s]]]))

(def addAttr (\\([shapeKind oldAttrs children] newAttr)
  [shapeKind (snoc newAttr oldAttrs) children]))

(def svg (\\shapes ['svg' [] shapes]))
(def svgViewBox (\\(xMax yMax shapes)
  (let [sx sy] [(toString xMax) (toString yMax)]
  ['svg'
    [['x' '0'] ['y' '0'] ['viewBox' (joinStrings ' ' ['0' '0' sx sy])]]
    shapes])))

(def rectCenter (\\(fill cx cy w h)
  (rect fill (- cx (/ w 2)) (- cy (/ h 2)) w h)))

(def square (\\(fill x y w) (rect fill x y w w)))
(def squareCenter (\\(fill cx cy w) (rectCenter fill cx cy w w)))

(def circle_    (circle 'red'))
(def ellipse_   (ellipse 'orange'))
(def rect_      (rect '#999999'))
(def square_    (square '#999999'))
(def line_      (line 'blue' 2))
(def polygon_   (polygon 'green' 'purple' 3))
(def path_      (path 'transparent' 'goldenrod' 5))

; TODO add constant literals to patterns, and match 'svg'
;
; accDiff pre-condition: indices in increasing order
;   (so can't just use foldr instead of reverse . foldl)
;
(def updateCanvas (\\([_ svgAttrs oldShapes] diff)
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
    ['svg' svgAttrs newShapes]))))))

; \"constant folding\"
(def twoPi (* 2 (pi)))
(def halfPi (/ (pi) 2))

; TODO explain coordinate system for these functions

(def nPointsOnUnitCircle (\\(n rot)
  (let off (- halfPi rot)
  (let foo (\\i
    (let ang (+ off (* (/ i n) twoPi))
    [(cos ang) (neg (sin ang))]))
  (map foo (list0N (- n 1)))))))

(def nPointsOnCircle (\\(n rot cx cy r)
  (let pts (nPointsOnUnitCircle n rot)
  (map (\\[x y] [(+ cx (* x r)) (+ cy (* y r))]) pts))))

(def nStar (\\(fill stroke w n len1 len2 rot cx cy)
  (let pti (\\[i len]
    (let anglei (+ (- (/ (* i (pi)) n) rot) halfPi)
    (let xi (+ cx (* len (cos anglei)))
    (let yi (+ cy (neg (* len (sin anglei))))
      [xi yi]))))
  (let lengths
    (map (\\b (if b len1 len2))
         (concat (repeat n [true false])))
  (let indices (list0N (- (* 2! n) 1!))
    (polygon fill stroke w (map pti (zip indices lengths))))))))

; TODO refactor as in paper
(def hSlider_ (\\(dropBall roundInt xStart xEnd y minVal maxVal caption curVal)
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
      (text (+ xEnd 10) (+ y 5) (+ caption (toString val))) ]
  [val shapes])))))))))

(def hSlider (hSlider_ true))

(def button_ (\\(dropBall xStart y caption xCur)
  (let [rPoint wLine rBall wSlider] [4! 3! 10! 70!]
  (let xEnd (+ xStart wSlider)
  (let xBall (+ xStart (* xCur wSlider))
  (let xBall_ (clamp xStart xEnd xBall)
  (let rBall_ (if dropBall (if (= xBall_ xBall) rBall 0) rBall)
  (let val (< xCur 0.5)
  (let shapes
    [ (circle 'black' xStart y rPoint)
      (circle 'black' xEnd y rPoint)
      (line 'black' wLine xStart y xEnd y)
      (circle (if val 'darkgreen' 'darkred') xBall y rBall_)
      (text (+ xEnd 10) (+ y 5) (+ caption (toString val))) ]
  [val shapes])))))))))

(def button (button_ true))

0

"


src = prelude


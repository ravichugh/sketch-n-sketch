module Prelude (src) where

src = "

(let id (\\x x)

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

(letrec list0N (\\i (if (< i 0) nil (cons i (list0N (- i 1)))))

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

(let circle (\\(fill x y r)
  ['circle'
     [['cx' x] ['cy' y] ['r' r] ['fill' fill]]
     []])

(let ring (\\(c w x y r)
  ['circle'
     [ ['cx' x] ['cy' y] ['r' r] ['fill' 'none'] ['stroke' c] ['strokeWidth' w] ]
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
     [ ['x1' x1] ['y1' y1] ['x2' x2] ['y2' y2] ['stroke' fill] ['strokeWidth' w] ]
     []])

(let polygon (\\(fill stroke w pts)
  ['polygon'
     [ ['fill' fill] ['points' pts] ['stroke' stroke] ['strokeWidth' w] ]
     []])

(let polyline (\\(fill stroke w pts)
  ['polyline'
     [ ['fill' fill] ['points' pts] ['stroke' stroke] ['strokeWidth' w] ]
     []])

(let path (\\(fill stroke w d)
  ['path'
     [ ['fill' fill] ['stroke' stroke] ['strokeWidth' w] ['d' d] ]
     []])

(let addAttr (\\([shapeKind oldAttrs children] newAttr)
  [shapeKind (snoc newAttr oldAttrs) children])

(let svg (\\shapes ['svg' [] shapes])

(let circle_    (circle 'red')
(let ellipse_   (ellipse 'orange')
(let rect_      (rect '#999999')
(let square_    (square '#999999')
(let line_      (line 'blue' 2)
(let polygon_   (polygon 'green' 'purple' 3)
(let path_      (path 'transparent' 'brown' 5)


0)))))))))))))))))))))))))))))))))))))))

"

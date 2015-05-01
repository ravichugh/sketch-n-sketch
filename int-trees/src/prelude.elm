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

(let cons (\\(x xs) [x | xs])
(let hd   (\\[x|xs] x)
(let tl   (\\[x|xs] xs)

(let plus  (\\(x y) (+ x y))
(let minus (\\(x y) (- x y))
(let neg   (\\x     (- 0 x))

(letrec mult (\\(m n)
  (if (< m 1) 0 (+ n (mult (- m 1) n))))

(letrec div (\\(m n)
  (if (< m n) 0
  (if (< n 2) m
    (+ 1 (div (- m n) n)))))

(let circle (\\(fill x y r)
  ['circle' ['cx' x] ['cy' y] ['r' r] ['fill' fill]])

(let ring (\\(c w x y r)
  ['circle' ['cx' x] ['cy' y] ['r' r] ['fill' 'none']
            ['stroke' c] ['strokeWidth' w]])

(let rect (\\(fill x y w h)
  ['rect' ['x' x] ['y' y] ['width' w] ['height' h] ['fill' fill]])

(let square (\\(fill x y side) (rect fill x y side side))

(let line (\\(fill w x1 y1 x2 y2)
  ['line' ['x1' x1] ['y1' y1] ['x2' x2] ['y2' y2]
          ['stroke' fill] ['strokeWidth' w]])

(let polygon (\\(fill stroke w pts)
  ['polygon' ['fill' fill] ['points' pts]
             ['stroke' stroke] ['strokeWidth' w]])

(let circle_    (circle 'red')
(let rect_      (rect '#999999')
(let square_    (square '#999999')
(let line_      (line 'blue' 2)
(let polygon_   (polygon 'green' 'purple' 3)

0)))))))))))))))))))))))))))))

"

-- prelude : Exp -> Exp
-- prelude body =
--   let foo e = case e of
--     ELet b p e1 e2 -> ELet b p e1 (foo e2)
--     EConst 0 _     -> body
--   in
--   foo exp


module Prelude (src) where

src = "

(let id (\\x x)

(letrec map (\\(f xs)
  (case xs ([] []) ([hd|tl] [(f hd)|(map f tl)])))

(letrec mult (\\(m n)
  (if (< m 1) 0 (+ n (mult (- m 1) n))))

(let circle (\\(fill x y r)
  ['circle' ['cx' x] ['cy' y] ['r' r] ['fill' fill]])

(let rect (\\(fill x y w h)
  ['rect' ['x' x] ['y' y] ['width' w] ['height' h] ['fill' fill]])

(let square (\\(fill x y side) (rect fill x y side side))

(let line (\\(fill w x1 y1 x2 y2)
  ['line' ['x1' x1] ['y1' y1] ['x2' x2] ['y2' y2] ['stroke' fill] ['strokeWidth' w]])

(let polygon (\\(fill stroke w pts)
  ['polygon' ['fill' fill] ['points' pts] ['stroke' stroke] ['strokeWidth' w]])

(let circle_    (circle 'red')
(let rect_      (rect '#999999')
(let square_    (square '#999999')
(let line_      (line 'blue' 2)
(let polygon_   (polygon 'green' 'purple' 3)

0)))))))))))))

"

-- prelude : Exp -> Exp
-- prelude body =
--   let foo e = case e of
--     ELet b p e1 e2 -> ELet b p e1 (foo e2)
--     EConst 0 _     -> body
--   in
--   foo exp


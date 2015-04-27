module Prelude (src) where

src = "

(let id (\\x x)

(letrec map (\\(f xs)
  (case xs ([] []) ([hd|tl] [(f hd)|(map f tl)])))

(letrec mult (\\(m n)
   (if (< m 1) 0 (+ n (mult (- m 1) n))))

(let circle (\\(x y r)
   ['circle' ['cx' x] ['cy' y] ['r' r]])

0))))

"

-- prelude : Exp -> Exp
-- prelude body =
--   let foo e = case e of
--     ELet b p e1 e2 -> ELet b p e1 (foo e2)
--     EConst 0 _     -> body
--   in
--   foo exp


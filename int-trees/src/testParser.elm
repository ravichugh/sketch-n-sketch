module TestParser where

import Lang
import LangParser

test0 () =
  let
    se = "
      (let f (fn x (fn y [(+ x 0) (+ x y)]))
        ((f 3) 5))
    "
    
    e   = se |> LangParser.parseE |> LangParser.freshen 1 |> fst
    v   = Lang.run e
    sv' = "[3 9]"
    v'  = LangParser.parseV sv'
  in
  {e=e, v=v, vnew=v'}


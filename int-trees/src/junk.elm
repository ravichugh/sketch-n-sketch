module Junk where

import Lang (..)
import LangParser

doit =
  let (x,y) = (EVar "x", EVar "y") in
  let e =
    eApp (EFun "x" (EFun "y" (EList [ePlus x (eConst 0), ePlus x y]))) [eConst 3, eConst 5]
  in
  let (e,_) = LangParser.freshen 1 e in
  strExp e ++ " ->* " ++ strVal (eval [] e)
    -- EApp (EFun "x" (ePlus (EVar "x") (eConst 1))) (eConst 10)
    -- EConst 1 dummyLoc

strDiff v1 v2 =
  let mmx = diff v1 v2 in
  case mmx of
    Nothing -> "incompatible" ++ "\n  " ++ strVal v1 ++ "\n  " ++ strVal v2
    Just Nothing -> "no diff " ++ "\n  " ++ strVal v1 ++ "\n  " ++ strVal v2
    Just (Just (vc,w1,w2)) ->
      "context = " ++ strVal vc ++
        "\n  " ++ strVal w1 ++ "\n  " ++ strVal w2


module Junk where

import Text
import Dict
import String
import List

import Lang (..)
import LangParser
import Sync

print = Text.leftAligned << Text.monospace << Text.fromString

br s1 s2 = s1 ++ "\n" ++ s2

main =
  let (x,y) = (EVar "x", EVar "y") in
  let e =
    eApp (EFun "x" (EFun "y" (EList [ePlus x (eConst 0), ePlus x y])))
         [eConst 3, eConst 5]
  in
  let (e,_) = LangParser.freshen 1 e in
  let v = eval [] e in
  let (VList [v1, VConst 8 tr2]) = v in
  let subst0 = LangParser.substOf e in
  let substs = Sync.inferSubsts subst0 (VConst 9 tr2) in
  print <|
    strExpLocs e ++ " ->* " ++ strValLocs v `br`
    toString (Dict.toList subst0) `br`
    toString substs `br`
    String.join "\n" (List.map (\s ->
      let e' = applySubst s e in
      strExp e' ++ " ->* " ++ strVal (run e')) substs)

strDiff v1 v2 =
  let mmx = diff v1 v2 in
  case mmx of
    Nothing -> "incompatible" ++ "\n  " ++ strVal v1 ++ "\n  " ++ strVal v2
    Just Nothing -> "no diff " ++ "\n  " ++ strVal v1 ++ "\n  " ++ strVal v2
    Just (Just (vc,w1,w2)) ->
      "context = " ++ strVal vc ++
        "\n  " ++ strVal w1 ++ "\n  " ++ strVal w2


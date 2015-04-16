module LangParser where

import List ((::))
import List

import Lang (..)

-- this will be done while parsing eventually...

freshen : Int -> Exp -> (Exp, Int)
freshen k e = case e of
  EConst i _ -> (EConst i k, k + 1)
  EVar x     -> (EVar x, k)
  EFun x e   -> let (e',k') = freshen k e in (EFun x e', k')
  EApp e1 e2 -> let ([e1',e2'],k') = freshenExps k [e1,e2] in (EApp e1' e2', k')
  EOp op es  -> let (es',k') = freshenExps k es in (EOp op es', k')
  EList es   -> let (es',k') = freshenExps k es in (EList es', k')

freshenExps k es =
  List.foldr (\e (es',k') ->
    let (e1,k1) = freshen k' e in
    (e1::es', k1)) ([],k) es


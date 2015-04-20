module Main where

import Text
import Dict
import String
import List
import Debug

import Lang (..)
import LangParser
import Sync
import Utils

print = Text.leftAligned << Text.monospace << Text.fromString

------------------------------------------------------------------------------

e0 =
  let (x,y) = (EVar "x", EVar "y") in
  fst <| LangParser.freshen 1 <|
    ELet "f" (eFun ["x","y"] (EList [ePlus x (eConst 0), ePlus x y])) <|
      eApp (EVar "f")
           [eConst 3, eConst 5]

v0  = run e0
v0' = VList [vConst 3, vConst 9]
-- v0' = VList [vConst 2, vConst 8]

------------------------------------------------------------------------------

doExample e v v' =
  let s0 =
    Utils.lines [
        sExpLocsK 1 e ++ " ->* " ++ strValLocs v
      , ""
      , "Initial Program"
      , sExpK 1 e
      , ""
      , "Initial Result"
      , strVal v
      , ""
      , "Updated Result"
      , strVal v'
      ]
  in
  case diff v v' of
    Nothing       -> print <| Utils.lines [s0, "bad change"]
    Just (Same _) -> print <| Utils.lines [s0, "no change"]
    Just (Diff vc w w') ->
      let subst0 = LangParser.substOf e in
      let substs = Sync.inferSubsts subst0 w' in
      let l =
        List.sortBy snd <|
          List.map (\s ->
            let e1 = applySubst s e in
            let v1 = run e1 in
            let n  = Sync.compareVals (v, v1) in
            ((e1, v1), n)
          ) substs
      in
      -- let s1 = Utils.spaces [ strVal vc, strVal w, strVal w' ] in
      print <|
        Utils.lines [
          s0
        , ""
        , String.join "\n\n"
            (Utils.mapi (\(i,((ei,vi),vdiff)) ->
               "Option " ++ toString i ++ " "
               ++ Utils.parens ("vdiff = " ++ toString vdiff) ++ "\n"
               ++ sExpK 1 ei ++ " ->* " ++ strVal vi) l)
        ]

main =
  doExample e0 v0 v0'


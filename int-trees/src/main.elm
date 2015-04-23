module Main where

import Text
import Dict
import String
import List
import Debug

import Lang (..)
import LangParser
import Sync
import TestParser
import Utils

print = Text.leftAligned << Text.monospace << Text.fromString

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
  let {e,v,vnew} = TestParser.test0 () in
  doExample e v vnew


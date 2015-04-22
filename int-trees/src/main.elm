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
-- Sample Tests

se0 = "
  (let f (fn x (fn y [(+ x 0) (+ x y)]))
    ((f 3) 5))
"

e0  = se0 |> LangParser.parseE |> LangParser.freshen 1 |> fst
v0  = run e0
sv0 = "[3 9]"
v0' = LangParser.parseV sv0

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


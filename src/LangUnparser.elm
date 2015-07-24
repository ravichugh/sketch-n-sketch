module LangUnparser (unparseE) where

import Lang exposing (..)
import OurParser2 as P
import Utils

import String
import Debug


------------------------------------------------------------------------------

-- NOTES:
--  - Lang/LangParser2 is set up such that trailing whitespace on each line
--    is not reinstated

bumpLine n pos = { line = n + pos.line, col = 1 }
bumpCol  n pos = { pos | col <- n + pos.col }

incLine = bumpLine 1
incCol  = bumpCol 1
decCol  = bumpCol (-1)

lines i j = if
  | i > j     -> Debug.crash <| "Unparser.lines: " ++ toString (i,j)
  | otherwise -> String.repeat (j-i) "\n"

cols i j = if
  | i > j     -> Debug.crash <| "Unparser.cols: " ++ toString (i,j)
  | otherwise -> String.repeat (j-i) " "

whitespace : P.Pos -> P.Pos -> String
whitespace endPrev startNext =
  Debug.log ("whiteSpace " ++ toString (endPrev, startNext)) <|
  if endPrev.line == startNext.line
  then cols endPrev.col startNext.col
  else lines endPrev.line startNext.line ++ cols 1 startNext.col

parens startOutside startInside endInside endOutside s =
  Utils.parens
    <| whitespace (incCol startOutside) startInside
    ++ s
    ++ whitespace endInside (decCol endOutside)

space = whitespace

unparse : Exp -> String
unparse e = case e.val of
  EBase v -> strBaseVal v
  EConst i l -> let (_,b,_) = l in toString i ++ b
  EVar x -> x
  EComment s e1 ->
    let white = whitespace (incLine e.start) e1.start in
    ";" ++ s ++ "\n" ++ white ++ unparse e1
  EApp e1 [e2] ->
    parens e.start e1.start e2.end e.end <|
      unparse e1 ++ space e1.end e2.start ++ unparse e2
  _ ->
    let _ = Debug.log "unparse NYI" () in
    sExp e

unparseE : Exp -> String
unparseE e = whitespace P.startPos e.start ++ unparse e


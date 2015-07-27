module LangUnparser (unparseE) where

import Lang exposing (..)
import OurParser2 exposing (Pos, WithPos, WithInfo, startPos)
import Utils
import Config

import String
import Debug

------------------------------------------------------------------------------

parserDebug s x =
  if | Config.debugParser -> Debug.log s x
     | otherwise          -> x

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

whitespace : Pos -> Pos -> String
whitespace endPrev startNext =
  parserDebug ("whiteSpace " ++ toString (endPrev, startNext)) <|
    if endPrev.line == startNext.line
    then cols endPrev.col startNext.col
    else lines endPrev.line startNext.line ++ cols 1 startNext.col

delimit : String -> String -> Pos -> Pos -> Pos -> Pos -> String -> String
delimit open close startOutside startInside endInside endOutside s =
  Utils.delimit open close
    <| whitespace (incCol startOutside) startInside
    ++ s
    ++ whitespace endInside (decCol endOutside)

parens = delimit "(" ")"

space = whitespace   -- at least one " "

-- NOTE: delimit/space functions that build on this are at the end

------------------------------------------------------------------------------

unparsePat : Pat -> String
unparsePat p = case p.val of
  PVar x -> x
  PList ps Nothing ->
    bracksAndSpaces p.start p.end (List.map UPat ps)
  PList ps (Just pRest) ->
    let _ = Debug.log "TODO: unparsePat" () in
    strPat p

-- TODO: compute whitespace once per AST-skeleton

unparse : Exp -> String
unparse e = case e.val of
  EBase v -> strBaseVal v
  EConst i l -> let (_,b,_) = l in toString i ++ b
  EVar x -> x
  EFun [p] e1 ->
    -- haven't recorded pos for "\", so always "(\"
    let lambda = WithInfo "\\" (bumpCol 1 e.start) (bumpCol 2 e.start) in
    parens e.start lambda.start e1.end e.end
      <| lambda.val ++ space lambda.end p.start
      ++ unparsePat p ++ space p.end e1.start
      ++ unparse e1
  EFun ps e1 ->
    let lambda = WithInfo "\\" (bumpCol 1 e.start) (bumpCol 2 e.start) in
    let (p1::_) = ps in
    let (sPats, pLastEnd) = spaces (List.map UPat ps) in
    parens e.start lambda.start e1.end e.end
      <| lambda.val ++ space lambda.end (decCol p1.start)
      ++ parens (decCol p1.start) p1.start pLastEnd (incCol pLastEnd) sPats
      ++ space pLastEnd e1.start
      ++ unparse e1
  EApp e1 es -> parensAndSpaces e.start e.end (List.map UExp (e1::es))
  EList es Nothing -> bracksAndSpaces e.start e.end (List.map UExp es)
  EList es (Just eRest) ->
    -- haven't recorded pos for "|"
    let _ = Debug.log "TODO: test this" () in
    let ((e1::_),(en::_)) = (es, List.reverse es) in
    let s1 = delimitAndSpaces "[" "|" e.start (bumpCol 2 en.end) (List.map UExp es) in
       s1 ++ space (bumpCol 2 en.end) eRest.start
    ++ unparse eRest ++ space eRest.end e.end
    ++ "]"
  EOp op es ->
    let sOp = { op | val <- strOp op.val } in
    parensAndSpaces e.start e.end (UStr sOp :: List.map UExp es)
  -- EIf e1 e2 e3 -> parensAndSpaces e.start e.end [e1,e2,e3] unparse
  EIf e1 e2 e3 ->
    let _ = Debug.log "TODO EIf" in
    sExp e
  ELet Let b p e1 e2 ->
    -- haven't recorded pos for "let"
    let sLet = if b then "letrec" else "let" in
    parens e.start (incCol e.start) e2.end e.end
      <| sLet ++ space (bumpCol (1 + String.length sLet) e.start) p.start
      ++ unparsePat p ++ space p.end e1.start
      ++ unparse e1 ++ space e1.end e2.start
      ++ unparse e2
  ELet Def b p e1 e2 ->
    -- haven't recorded pos for "def"
    let sDef = if b then "defrec" else "def" in
    let s1 =
      parens e.start (incCol e.start) e1.end e.end
        <| sDef ++ space (bumpCol (1 + String.length sDef) e.start) p.start
        ++ unparsePat p ++ space p.end e1.start
        ++ unparse e1
    in
    s1 ++ space e.end e2.start ++ unparse e2
  ECase e1 l ->
    let sCase = "case" in
    let nCase = 1 + String.length sCase in
    let (sBranches, lastBranchEnd) = spaces (List.map UBra l) in
    let (firstBranch::_) = l in
    parens e.start (incCol e.start) lastBranchEnd e.end
      <| sCase ++ space (bumpCol nCase e.start) e1.start
      ++ unparse e1 ++ space e1.end firstBranch.start
      ++ sBranches
  EComment s e1 ->
    let white = whitespace (incLine e.start) e1.start in
    ";" ++ s ++ "\n" ++ white ++ unparse e1

unparseE : Exp -> String
unparseE e = whitespace startPos e.start ++ unparse e

------------------------------------------------------------------------------

{-

NOTE:
  Defining Unparsable at the end, since otherwise there's the following
  undefined error (probably due to the mutual recursion):

  Cannot read property 'arity' of undefined

-}

type Unparsable
  = UExp (WithInfo Exp_)    -- = Exp
  | UPat (WithInfo Pat_)    -- = Pat
  | UBra (WithInfo Branch_) -- = Branch
  | UStr (WithInfo String)

strU thing = case thing of
  UExp e -> unparse e
  UPat p -> unparsePat p
  UStr s -> identity s.val
  UBra b ->
    let (p,e) = b.val in
    let s = unparsePat p ++ space p.end e.start ++ unparse e in
    parens b.start p.start e.end b.end s

startU thing = case thing of
  UExp x -> x.start
  UPat x -> x.start
  UStr x -> x.start
  UBra x -> x.start

endU thing = case thing of
  UExp x -> x.end
  UPat x -> x.end
  UStr x -> x.end
  UBra x -> x.end

spaces : List Unparsable -> (String, Pos)
spaces things =
  let (hd::tl) = things in
  let foo cur (acc, endPrev) =
    let acc'     = strU cur :: space endPrev (startU cur) :: acc in
    let endPrev' = endU cur in
    (acc', endPrev')
  in
  let (l, endLast) = List.foldl foo ([strU hd], endU hd) tl in
  (String.join "" (List.reverse l), endLast)

delimitAndSpaces : String -> String -> Pos -> Pos -> List Unparsable -> String
delimitAndSpaces open close start end things =
  case things of
    [] ->
      open ++ whitespace (incCol start) (decCol end) ++ close
    hd :: _ ->
      let startFirst   = startU hd in
      let (s, endLast) = spaces things in
      delimit open close start startFirst endLast end s

parensAndSpaces = delimitAndSpaces "(" ")"
bracksAndSpaces = delimitAndSpaces "[" "]"


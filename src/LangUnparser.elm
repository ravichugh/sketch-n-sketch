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

-- TODO:
--  make spaces/delimitAndSpaces more general, to take different
--  types of (f,thing) pairs (i.e. Exp,Pat,String)

spaces : List (WithInfo a) -> (WithInfo a -> String) -> (String, Pos)
spaces things f =
  let (hd::tl) = things in
  let foo cur (acc, endPrev) =
    let acc'     = f cur :: space endPrev cur.start :: acc in
    let endPrev' = cur.end in
    (acc', endPrev')
  in
  let (l, endLast) = List.foldl foo ([f hd], hd.end) tl in
  (String.join "" (List.reverse l), endLast)

delimitAndSpaces
   : String -> String -> Pos -> Pos -> List (WithInfo a)
  -> (WithInfo a -> String) -> String
delimitAndSpaces open close start end things f =
  case things of
    [] ->
      open ++ whitespace (incCol start) (decCol end) ++ close
    hd :: _ ->
      let startFirst   = hd.start in
      let (s, endLast) = spaces things f in
      delimit open close start startFirst endLast end s

parensAndSpaces = delimitAndSpaces "(" ")"
bracksAndSpaces = delimitAndSpaces "[" "]"

unparsePat : Pat -> String
unparsePat p = case p.val of
  PVar x -> x
  PList ps Nothing ->
    bracksAndSpaces p.start p.end ps unparsePat
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
    let (sPats, pLastEnd) = spaces ps unparsePat in
    parens e.start lambda.start e1.end e.end
      <| lambda.val ++ space lambda.end (decCol p1.start)
      ++ parens (decCol p1.start) p1.start pLastEnd (incCol pLastEnd) sPats
      ++ space pLastEnd e1.start
      ++ unparse e1
  EApp e1 es -> parensAndSpaces e.start e.end (e1::es) unparse
  EList es Nothing -> bracksAndSpaces e.start e.end es unparse
  EList es (Just eRest) ->
    -- haven't recorded pos for "|"
    let _ = Debug.log "TODO: test this" () in
    let ((e1::_),(en::_)) = (es, List.reverse es) in
    let s1 = delimitAndSpaces "[" "|" e.start (bumpCol 2 en.end) es unparse in
       s1 ++ space (bumpCol 2 en.end) eRest.start
    ++ unparse eRest ++ space eRest.end e.end
    ++ "]"
  EOp op es ->
    let varOp = { op | val <- EVar (strOp op.val) } in
    parensAndSpaces e.start e.end (varOp::es) unparse
      -- HACK: varOp so that parensAndSpaces can be used
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
    let (sBranches, lastBranchEnd) =
      let f branch =
        let (p,e) = branch.val in
        parens branch.start p.start e.end branch.end
          <| unparsePat p ++ space p.end e.start ++ unparse e
      in
      spaces l f in
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


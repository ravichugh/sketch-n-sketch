module LangUnparser (unparseE) where

import Lang exposing (..)
import OurParser2 exposing (Pos, WithPos, WithInfo, startPos)
import Utils
import Config

import String
import Debug

import Benchmark

------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugParser

------------------------------------------------------------------------------

-- NOTES:
--  - Lang/LangParser2 is set up such that trailing whitespace on each line
--    is not reinstated

bumpLine n pos = { line = n + pos.line, col = 1 }
bumpCol  n pos = { pos | col = n + pos.col }

incLine = bumpLine 1
incCol  = bumpCol 1
decCol  = bumpCol (-1)

lines i j =
  if i > j
    then Debug.crash <| "Unparser.lines: " ++ toString (i,j)
    else String.repeat (j-i) "\n"

cols i j =
  if i > j
    then Debug.crash <| "Unparser.cols: " ++ toString (i,j)
    else String.repeat (j-i) " "

whitespace : Pos -> Pos -> String
whitespace endPrev startNext =
  debugLog ("whiteSpace " ++ toString (endPrev, startNext)) <|
    if endPrev.line == startNext.line
    then cols endPrev.col startNext.col
    else lines endPrev.line startNext.line ++ cols 1 startNext.col

delimit : String -> String -> Pos -> Pos -> Pos -> Pos -> String -> String
delimit open close startOutside startInside endInside endOutside s =
  let olen = String.length open
      clen = String.length close in
  Utils.delimit open close
    <| whitespace (bumpCol olen startOutside) startInside
    ++ s
    ++ whitespace endInside (bumpCol (-1 * clen) endOutside)

parens = delimit "(" ")"

space = whitespace   -- at least one " "

-- NOTE: delimit/space functions that build on this are at the end

------------------------------------------------------------------------------

unparsePat : Pat -> String
unparsePat p = case p.val of
  PVar x wd -> case wd.val of
    NoWidgetDecl        -> x
    IntSlider a tok b _ -> x ++ bracesAndSpaces wd.start wd.end [UInt a, UStr tok, UInt b]
    NumSlider a tok b _ -> x ++ bracesAndSpaces wd.start wd.end [UNum a, UStr tok, UNum b]
  PList ps Nothing ->
    bracksAndSpaces p.start p.end (List.map UPat ps)
  PList ps (Just pRest) ->
    -- let _ = Debug.log "TODO: unparsePat" () in
    strPat p
  PConst n -> strNum n
  PBase bv -> strBaseVal bv

-- NOTE:
--   haven't recorded pos for "\", "let", "case", "if", etc.
--   so makeToken picks a canonical position

makeToken start s =
  let n = String.length s in
  WithInfo s start (bumpCol n start)

-- TODO: compute whitespace once per AST-skeleton

unparse : Exp -> String
unparse e = case e.val of
  EBase v -> strBaseVal v
  EConst i l wd ->
    let s = let (_,b,_) = l in toString i ++ b in
    case wd.val of
      NoWidgetDecl        -> s
      IntSlider a tok b _ -> s ++ bracesAndSpaces wd.start wd.end [UInt a, UStr tok, UInt b]
      NumSlider a tok b _ -> s ++ bracesAndSpaces wd.start wd.end [UNum a, UStr tok, UNum b]
    -- TODO: parse/unparse are not inverses for floats (e.g. 1.0)
  EVar x -> x
  EFun [p] e1 ->
    let tok = makeToken (incCol e.start) "\\" in
    parensAndSpaces e.start e.end [UStr tok, UPat p, UExp e1]
  EFun ps e1 ->
    let tok = makeToken (incCol e.start) "\\" in
    parensAndSpaces e.start e.end [UStr tok, UParens (List.map UPat ps), UExp e1]
  EApp e1 es -> parensAndSpaces e.start e.end (List.map UExp (e1::es))
  EList es Nothing -> bracksAndSpaces e.start e.end (List.map UExp es)
  EList es (Just eRest) ->
    let en = Utils.head_ <| List.reverse es in
    let tok1 = makeToken e.start   "[" in
    let tok2 = makeToken en.end    "|" in
    let tok3 = makeToken eRest.end "]" in
       delimitAndSpaces tok1.val tok2.val tok1.start tok2.end (List.map UExp es)
    ++ space tok2.end eRest.start
    ++ unparse eRest ++ space eRest.end tok3.start
    ++ tok3.val
  EIndList rs ->
    ibracksAndSpaces e.start e.end (List.concat (List.map unparseRange rs))
  EOp op es ->
    let sOp = { op | val = strOp op.val } in
    parensAndSpaces e.start e.end (UStr sOp :: List.map UExp es)
  EIf e1 e2 e3 ->
    let tok = makeToken (incCol e.start) "if" in
    parensAndSpaces e.start e.end (UStr tok :: List.map UExp [e1,e2,e3])
  ELet Let b p e1 e2 ->
    let tok = makeToken (incCol e.start) (if b then "letrec" else "let") in
    parensAndSpaces e.start e.end (UStr tok :: UPat p :: List.map UExp [e1,e2])
  ELet Def b p e1 e2 ->
    -- TODO don't used nested defs until this is re-worked
    let tok = makeToken (incCol e.start) (if b then "defrec" else "def") in
    let s1 = parensAndSpaces e.start e.end [UStr tok, UPat p, UExp e1] in
    s1 ++ space e.end e2.start ++ unparse e2
  ECase e1 l ->
    let tok = makeToken (incCol e.start) "case" in
    parensAndSpaces e.start e.end (UStr tok :: UExp e1 :: List.map UBra l)
  EComment s e1 ->
    let white = whitespace (incLine e.start) e1.start in
    ";" ++ s ++ "\n" ++ white ++ unparse e1
  EOption s1 s2 e1 ->
    let tok1 = makeToken e.start "#" in
    let tok2 = makeToken s1.end ":" in
    let s = fst (spaces (List.map UStr [tok1, s1, tok2, s2])) in
    let white = whitespace (incLine e.start) e1.start in
    s ++ "\n" ++ white ++ unparse e1

unparseE : Exp -> String
unparseE e =
  Benchmark.logDuration "unparseE" <|
    \() ->
      whitespace startPos e.start ++ unparse e

-- Currently only remembers whitespace after ".."
unparseRange : ERange -> List Unparsable
unparseRange r = case r.val of (l,u) -> case (l.val,u.val) of
    (EConst lv lt _, EConst uv ut _) ->
        if lv == uv
          then [ UExp l ]
          else [ UExp l
               , UStr <| makeToken l.end ".."
               , UExp u
               ]
    _ -> Debug.crash "unparseRange"

-- NOTE: use this to go back to original unparser
-- unparseE = sExp

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
  | UInt (WithInfo Int)
  | UNum (WithInfo Num)
  | UParens (List Unparsable)
      -- no start/pos info, so using first/last elements as
      -- canonical positions

strU thing = case thing of
  UExp e -> unparse e
  UPat p -> unparsePat p
  UStr s -> identity s.val
  UInt i -> toString i.val
  UNum i -> strNum i.val
  UBra b ->
    let (p,e) = b.val in
    let s = unparsePat p ++ space p.end e.start ++ unparse e in
    parens b.start p.start e.end b.end s
  UParens l ->
    let (start, end) = (startU thing, endU thing) in
    parens start (incCol start) (decCol end) end (fst (spaces l))

startU thing = case thing of
  UExp x -> x.start
  UPat x -> x.start
  UStr x -> x.start
  UInt x -> x.start
  UNum x -> x.start
  UBra x -> x.start

  UParens l -> let first = Utils.head_ l in decCol (startU first)

endU thing = case thing of
  UExp x -> x.end
  UPat x -> x.end
  UStr x -> x.end
  UInt x -> x.end
  UNum x -> x.end
  UBra x -> x.end

  UParens l -> let last = Utils.head_ (List.reverse l) in incCol (endU last)

spaces : List Unparsable -> (String, Pos)
spaces things =
  let (hd,tl) = Utils.uncons things in
  let foo cur (acc, endPrev) =
    let acc'     = strU cur :: space endPrev (startU cur) :: acc in
    let endPrev' = endU cur in
    (acc', endPrev')
  in
  let (l, endLast) = List.foldl foo ([strU hd], endU hd) tl in
  (String.join "" (List.reverse l), endLast)

delimitAndSpaces : String -> String -> Pos -> Pos -> List Unparsable -> String
delimitAndSpaces open close start end things =
  let olen = String.length open
      clen = String.length close in
  case things of
    [] ->
      open ++ whitespace (bumpCol olen start) (bumpCol (-1 * clen) end) ++ close
    hd :: _ ->
      let startFirst   = startU hd in
      let (s, endLast) = spaces things in
      delimit open close start startFirst endLast end s

parensAndSpaces = delimitAndSpaces "(" ")"
bracksAndSpaces = delimitAndSpaces "[" "]"
bracesAndSpaces = delimitAndSpaces "{" "}"
ibracksAndSpaces = delimitAndSpaces "[|" "|]"

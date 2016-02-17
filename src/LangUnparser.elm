module LangUnparser
  (unparse, equationToLittle, bumpCol, incCol, preceedingWhitespace,
    preceedingWhitespaceExp__, addPreceedingWhitespace,
    replacePreceedingWhitespace, replacePreceedingWhitespacePat) where

import Lang exposing (..)
import OurParser2 exposing (Pos, WithPos, WithInfo, startPos)
import Utils
import Config

import String
import Dict
import Debug

------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugParser

------------------------------------------------------------------------------

-- TODO: I think these can go away...

bumpCol n pos = { pos | col = n + pos.col }
incCol = bumpCol 1

------------------------------------------------------------------------------


preceedingWhitespace : Exp -> String
preceedingWhitespace exp =
  preceedingWhitespaceExp__ exp.val.e__


preceedingWhitespacePat : Pat -> String
preceedingWhitespacePat pat =
  case pat.val of
    PVar   ws ident wd         -> ws
    PConst ws n                -> ws
    PBase  ws v                -> ws
    PList  ws1 es ws2 rest ws3 -> ws1


preceedingWhitespaceExp__ : Exp__ -> String
preceedingWhitespaceExp__ e__ =
  case e__ of
    EBase    ws v                     -> ws
    EConst   ws n l wd                -> ws
    EVar     ws x                     -> ws
    EFun     ws1 ps e1 ws2            -> ws1
    EApp     ws1 e1 es ws2            -> ws1
    EList    ws1 es ws2 rest ws3      -> ws1
    EIndList ws1 rs ws2               -> ws1
    EOp      ws1 op es ws2            -> ws1
    EIf      ws1 e1 e2 e3 ws2         -> ws1
    ELet     ws1 kind rec p e1 e2 ws2 -> ws1
    ECase    ws1 e1 bs ws2            -> ws1
    EComment ws s e1                  -> ws
    EOption  ws1 s1 ws2 s2 e1         -> ws1


addPreceedingWhitespace : String -> Exp -> Exp
addPreceedingWhitespace newWs exp =
  mapPreceedingWhitespace (\oldWs -> oldWs ++ newWs) exp


replacePreceedingWhitespace : String -> Exp -> Exp
replacePreceedingWhitespace newWs exp =
  mapPreceedingWhitespace (\oldWs -> newWs) exp


replacePreceedingWhitespacePat : String -> Pat -> Pat
replacePreceedingWhitespacePat newWs pat =
  mapPreceedingWhitespacePat (\oldWs -> newWs) pat


mapPreceedingWhitespace : (String -> String) -> Exp -> Exp
mapPreceedingWhitespace mapWs exp =
  let e__' =
    case exp.val.e__ of
      EBase    ws v                     -> EBase    (mapWs ws) v
      EConst   ws n l wd                -> EConst   (mapWs ws) n l wd
      EVar     ws x                     -> EVar     (mapWs ws) x
      EFun     ws1 ps e1 ws2            -> EFun     (mapWs ws1) ps e1 ws2
      EApp     ws1 e1 es ws2            -> EApp     (mapWs ws1) e1 es ws2
      EList    ws1 es ws2 rest ws3      -> EList    (mapWs ws1) es ws2 rest ws3
      EIndList ws1 rs ws2               -> EIndList (mapWs ws1) rs ws2
      EOp      ws1 op es ws2            -> EOp      (mapWs ws1) op es ws2
      EIf      ws1 e1 e2 e3 ws2         -> EIf      (mapWs ws1) e1 e2 e3 ws2
      ELet     ws1 kind rec p e1 e2 ws2 -> ELet     (mapWs ws1) kind rec p e1 e2 ws2
      ECase    ws1 e1 bs ws2            -> ECase    (mapWs ws1) e1 bs ws2
      EComment ws s e1                  -> EComment (mapWs ws) s e1
      EOption  ws1 s1 ws2 s2 e1         -> EOption  (mapWs ws1) s1 ws2 s2 e1
  in
  let val = exp.val in
  { exp | val = { val | e__ = e__' } }


mapPreceedingWhitespacePat : (String -> String) -> Pat -> Pat
mapPreceedingWhitespacePat mapWs pat =
  let pat_' =
    case pat.val of
      PVar   ws ident wd         -> PVar   (mapWs ws) ident wd
      PConst ws n                -> PConst (mapWs ws) n
      PBase  ws v                -> PBase  (mapWs ws) v
      PList  ws1 es ws2 rest ws3 -> PList  (mapWs ws1) es ws2 rest ws3
  in
  { pat | val = pat_' }


unparseWD : WidgetDecl -> String
unparseWD wd =
  case wd.val of
    NoWidgetDecl        -> ""
    IntSlider a tok b _ -> "{" ++ toString a.val ++ tok.val ++ toString b.val ++ "}"
    NumSlider a tok b _ -> "{" ++ toString a.val ++ tok.val ++ toString b.val ++ "}"

unparsePat : Pat -> String
unparsePat p = case p.val of
  PVar ws x wd ->
    ws ++ x ++ unparseWD wd
  PList ws1 ps ws2 Nothing ws3 ->
    ws1 ++ "[" ++ (String.concat (List.map unparsePat ps)) ++ ws3 ++ "]"
  PList ws1 ps ws2 (Just pRest) ws3 ->
    ws1 ++ "[" ++ (String.concat (List.map unparsePat ps)) ++ ws2 ++ "|" ++ unparsePat pRest ++ ws3 ++ "]"
  PConst ws n -> ws ++ strNum n
  PBase ws bv -> ws ++ strBaseVal bv

unparse : Exp -> String
unparse e = case e.val.e__ of
  EBase ws v -> ws ++ strBaseVal v
  EConst ws n l wd ->
    let (_,b,_) = l in
    ws ++ toString n ++ b ++ unparseWD wd
    -- TODO: parse/unparse are not inverses for floats (e.g. 1.0)
  EVar ws x -> ws ++ x
  EFun ws1 [p] e1 ws2 ->
    ws1 ++ "(\\" ++ unparsePat p ++ unparse e1 ++ ws2 ++ ")"
  EFun ws1 ps e1 ws2 ->
    ws1 ++ "(\\(" ++ (String.concat (List.map unparsePat ps)) ++ ")" ++ unparse e1 ++ ws2 ++ ")"
  EApp ws1 e1 es ws2 ->
    ws1 ++ "(" ++ unparse e1 ++ (String.concat (List.map unparse es)) ++ ws2 ++ ")"
  EList ws1 es ws2 Nothing ws3 ->
    ws1 ++ "[" ++ (String.concat (List.map unparse es)) ++ ws3 ++ "]"
  EList ws1 es ws2 (Just eRest) ws3 ->
    ws1 ++ "[" ++ (String.concat (List.map unparse es)) ++ ws2 ++ "|" ++ unparse eRest ++ ws3 ++ "]"
  EIndList ws1 rs ws2 ->
    ws1 ++ "[|" ++ (String.concat (List.map unparseRange rs)) ++ ws2 ++ "|]"
  EOp ws1 op es ws2 ->
    ws1 ++ "(" ++ strOp op.val ++ (String.concat (List.map unparse es)) ++ ws2 ++ ")"
  EIf ws1 e1 e2 e3 ws2 ->
    ws1 ++ "(if" ++ unparse e1 ++ unparse e2 ++ unparse e3 ++ ws2 ++ ")"
  ELet ws1 Let b p e1 e2 ws2 ->
    let tok = if b then "letrec" else "let" in
    ws1 ++ "(" ++ tok ++ unparsePat p ++ unparse e1 ++ unparse e2 ++ ws2 ++ ")"
  ELet ws1 Def b p e1 e2 ws2 ->
    -- TODO don't used nested defs until this is re-worked
    let tok = if b then "defrec" else "def" in
    ws1 ++ "(" ++ tok ++ unparsePat p ++ unparse e1 ++ ws2 ++ ")" ++ unparse e2
  ECase ws1 e1 bs ws2 ->
    let branchesStr =
      String.concat
        <| List.map (\(Branch_ bws1 pat exp bws2) -> bws1 ++ "(" ++ unparsePat pat ++ unparse exp ++ bws2 ++ ")")
        <| List.map (.val) bs
    in
    ws1 ++ "(case" ++ unparse e1 ++ branchesStr ++ ws2 ++ ")"
  EComment ws s e1 ->
    ws ++ ";" ++ s ++ "\n" ++ unparse e1
  EOption ws1 s1 ws2 s2 e1 ->
    ws1 ++ "# " ++ s1.val ++ ":" ++ ws2 ++ s2.val ++ "\n" ++ unparse e1

unparseRange : Range -> String
unparseRange r = case r.val of
  Point e           -> unparse e
  Interval e1 ws e2 -> unparse e1 ++ ws ++ ".." ++ unparse e2

traceToLittle : SubstStr -> Trace -> String
traceToLittle substStr trace =
  case trace of
    TrLoc (locId, _, _) ->
      case Dict.get locId substStr of
        Just str -> str
        Nothing  -> "?"
    TrOp op childTraces ->
      let childLittleStrs = List.map (traceToLittle substStr) childTraces in
      "(" ++ strOp op ++ " " ++ String.join " " childLittleStrs ++ ")"

equationToLittle : SubstStr -> FeatureEquation -> String
equationToLittle substStr eqn =
  case eqn of
    EqnVal val ->
      case val.v_ of
        VConst (n, trace) ->
          let littlizedTrace = traceToLittle substStr trace in
          if littlizedTrace /= "?" then
            littlizedTrace
          else
            -- Constants introduced by the equation (e.g. /2 for midpoint) won't
            -- have a value in subst.
            toString n

        _ ->
          "?"

    EqnOp op childEqns ->
      let childLittleStrs = List.map (equationToLittle substStr) childEqns in
      "(" ++ strOp op ++ " " ++ String.join " " childLittleStrs ++ ")"

-- NOTE: use this to go back to original unparser
-- unparse = sExp

module LangUnparser
  (unparse, unparsePat, unparseWD, unparseType,
    traceToLittle, bumpCol, incCol, precedingWhitespace,
    precedingWhitespaceExp__, addPrecedingWhitespace,
    replacePrecedingWhitespace, replacePrecedingWhitespacePat,
    indent
  ) where

import Lang exposing (..)
import OurParser2 exposing (Pos, WithPos, WithInfo, startPos)
import Utils
import Config

import String
import Dict
import Debug
import Regex

------------------------------------------------------------------------------

debugLog = Config.debugLog Config.debugParser

------------------------------------------------------------------------------

-- TODO: I think these can go away...

bumpCol n pos = { pos | col = n + pos.col }
incCol = bumpCol 1

------------------------------------------------------------------------------


precedingWhitespace : Exp -> String
precedingWhitespace exp =
  precedingWhitespaceExp__ exp.val.e__


precedingWhitespacePat : Pat -> String
precedingWhitespacePat pat =
  case pat.val of
    PVar   ws ident wd         -> ws
    PConst ws n                -> ws
    PBase  ws v                -> ws
    PList  ws1 es ws2 rest ws3 -> ws1
    PAs    ws1 ident ws2 p     -> ws1


precedingWhitespaceExp__ : Exp__ -> String
precedingWhitespaceExp__ e__ =
  case e__ of
    EBase      ws v                     -> ws
    EConst     ws n l wd                -> ws
    EVar       ws x                     -> ws
    EFun       ws1 ps e1 ws2            -> ws1
    EApp       ws1 e1 es ws2            -> ws1
    EList      ws1 es ws2 rest ws3      -> ws1
    EIndList   ws1 rs ws2               -> ws1
    EOp        ws1 op es ws2            -> ws1
    EIf        ws1 e1 e2 e3 ws2         -> ws1
    ELet       ws1 kind rec p e1 e2 ws2 -> ws1
    ECase      ws1 e1 bs ws2            -> ws1
    ETypeCase  ws1 pat bs ws2           -> ws1
    EComment   ws s e1                  -> ws
    EOption    ws1 s1 ws2 s2 e1         -> ws1
    ETyp       ws1 pat tipe e ws2       -> ws1
    EColonType ws1 e ws2 tipe ws3       -> ws1
    ETypeAlias ws1 pat tipe e ws2       -> ws1


addPrecedingWhitespace : String -> Exp -> Exp
addPrecedingWhitespace newWs exp =
  mapPrecedingWhitespace (\oldWs -> oldWs ++ newWs) exp


replacePrecedingWhitespace : String -> Exp -> Exp
replacePrecedingWhitespace newWs exp =
  mapPrecedingWhitespace (\oldWs -> newWs) exp


replacePrecedingWhitespacePat : String -> Pat -> Pat
replacePrecedingWhitespacePat newWs pat =
  mapPrecedingWhitespacePat (\oldWs -> newWs) pat


mapPrecedingWhitespace : (String -> String) -> Exp -> Exp
mapPrecedingWhitespace mapWs exp =
  let e__' =
    case exp.val.e__ of
      EBase      ws v                     -> EBase      (mapWs ws) v
      EConst     ws n l wd                -> EConst     (mapWs ws) n l wd
      EVar       ws x                     -> EVar       (mapWs ws) x
      EFun       ws1 ps e1 ws2            -> EFun       (mapWs ws1) ps e1 ws2
      EApp       ws1 e1 es ws2            -> EApp       (mapWs ws1) e1 es ws2
      EList      ws1 es ws2 rest ws3      -> EList      (mapWs ws1) es ws2 rest ws3
      EIndList   ws1 rs ws2               -> EIndList   (mapWs ws1) rs ws2
      EOp        ws1 op es ws2            -> EOp        (mapWs ws1) op es ws2
      EIf        ws1 e1 e2 e3 ws2         -> EIf        (mapWs ws1) e1 e2 e3 ws2
      ELet       ws1 kind rec p e1 e2 ws2 -> ELet       (mapWs ws1) kind rec p e1 e2 ws2
      ECase      ws1 e1 bs ws2            -> ECase      (mapWs ws1) e1 bs ws2
      ETypeCase  ws1 pat bs ws2           -> ETypeCase  (mapWs ws1) pat bs ws2
      EComment   ws s e1                  -> EComment   (mapWs ws) s e1
      EOption    ws1 s1 ws2 s2 e1         -> EOption    (mapWs ws1) s1 ws2 s2 e1
      ETyp       ws1 pat tipe e ws2       -> ETyp       (mapWs ws1) pat tipe e ws2
      EColonType ws1 e ws2 tipe ws3       -> EColonType (mapWs ws1) e ws2 tipe ws3
      ETypeAlias ws1 pat tipe e ws2       -> ETypeAlias (mapWs ws1) pat tipe e ws2
  in
  let val = exp.val in
  { exp | val = { val | e__ = e__' } }

{- TODO:
     add a flag to mapPrecedingWhitespace that specifies whether
       or not to recurse into Exp children.
     then, re-define indent as follows:

indent : String -> Exp -> Exp
indent spaces =
  mapPrecedingWhitespace True <| \s ->
    s |> String.reverse
      |> Regex.replace (Regex.AtMost 1) (Regex.regex "\n") (\_ -> spaces ++ "\n")
      |> String.reverse
-}

indent : String -> Exp -> Exp
indent spaces e =
  let recurse = indent spaces in
  let wrap e__ = WithInfo (Exp_ e__ e.val.eid) e.start e.end in
  let processWS ws =
    ws |> String.reverse
       |> Regex.replace (Regex.AtMost 1) (Regex.regex "\n") (\_ -> spaces ++ "\n")
       |> String.reverse
  in
  case e.val.e__ of
    EConst _ _ _ _         -> e
    EBase _ _              -> e
    EVar _ _               -> e
    EFun ws1 ps e' ws2     -> wrap (EFun (processWS ws1) ps (recurse e') ws2)
    EApp ws1 e1 es ws2     -> wrap (EApp (processWS ws1) (recurse e1) (List.map recurse es) ws2)
    EOp ws1 op es ws2      -> wrap (EOp (processWS ws1) op (List.map recurse es) ws2)
    EList ws1 es ws2 m ws3 -> wrap (EList (processWS ws1) (List.map recurse es) ws2 (Utils.mapMaybe recurse m) ws3)
    EIndList ws1 rs ws2    ->
      let rangeRecurse r_ = case r_ of
        Interval e1 ws e2 -> Interval (recurse e1) ws (recurse e2)
        Point e1          -> Point (recurse e1)
      in
      wrap (EIndList (processWS ws1) (List.map (mapValField rangeRecurse) rs) ws2)
    EIf ws1 e1 e2 e3 ws2     -> wrap (EIf (processWS ws1) (recurse e1) (recurse e2) (recurse e3) ws2)
    ECase ws1 e1 branches ws2 ->
      let newE1 = recurse e1 in
      let newBranches =
        List.map
            (mapValField (\(Branch_ bws1 p ei bws2) -> Branch_ bws1 p (recurse ei) bws2))
            branches
      in
      wrap (ECase (processWS ws1) newE1 newBranches ws2)
    ETypeCase ws1 pat tbranches ws2 ->
      let newBranches =
        List.map
            (mapValField (\(TBranch_ bws1 tipe ei bws2) -> TBranch_ bws1 tipe (recurse ei) bws2))
            tbranches
      in
      wrap (ETypeCase (processWS ws1) pat newBranches ws2)
    EComment ws s e1              -> wrap (EComment (processWS ws) s (recurse e1))
    EOption ws1 s1 ws2 s2 e1      -> wrap (EOption (processWS ws1) s1 ws2 s2 (recurse e1))
    ELet ws1 k b p e1 e2 ws2      -> wrap (ELet (processWS ws1) k b p (recurse e1) (recurse e2) ws2)
    ETyp ws1 pat tipe e ws2       -> wrap (ETyp (processWS ws1) pat tipe (recurse e) ws2)
    EColonType ws1 e ws2 tipe ws3 -> wrap (EColonType (processWS ws1) (recurse e) ws2 tipe ws3)
    ETypeAlias ws1 pat tipe e ws2 -> wrap (ETypeAlias (processWS ws1) pat tipe (recurse e) ws2)


mapPrecedingWhitespacePat : (String -> String) -> Pat -> Pat
mapPrecedingWhitespacePat mapWs pat =
  let pat_' =
    case pat.val of
      PVar   ws ident wd         -> PVar   (mapWs ws) ident wd
      PConst ws n                -> PConst (mapWs ws) n
      PBase  ws v                -> PBase  (mapWs ws) v
      PList  ws1 es ws2 rest ws3 -> PList  (mapWs ws1) es ws2 rest ws3
      PAs    ws1 ident ws2 p     -> PAs    (mapWs ws1) ident ws2 p
  in
  { pat | val = pat_' }


unparseWD : WidgetDecl -> String
unparseWD wd =
  case wd.val of
    NoWidgetDecl        -> ""
    IntSlider a tok b _ -> "{" ++ toString a.val ++ tok.val ++ toString b.val ++ "}"
    NumSlider a tok b _ -> "{" ++ toString a.val ++ tok.val ++ toString b.val ++ "}"

unparsePat : Pat -> String
unparsePat pat = case pat.val of
  PVar ws x wd ->
    ws ++ x ++ unparseWD wd
  PList ws1 ps ws2 Nothing ws3 ->
    ws1 ++ "[" ++ (String.concat (List.map unparsePat ps)) ++ ws3 ++ "]"
  PList ws1 ps ws2 (Just pRest) ws3 ->
    ws1 ++ "[" ++ (String.concat (List.map unparsePat ps)) ++ ws2 ++ "|" ++ unparsePat pRest ++ ws3 ++ "]"
  PConst ws n -> ws ++ strNum n
  PBase ws bv -> ws ++ strBaseVal bv
  PAs ws1 ident ws2 p -> ws1 ++ ident ++ ws2 ++ "@" ++ (unparsePat p)

unparseType : Type -> String
unparseType tipe =
  case tipe.val of
    TNum ws                   -> ws ++ "Num"
    TBool ws                  -> ws ++ "Bool"
    TString ws                -> ws ++ "String"
    TNull ws                  -> ws ++ "Null"
    TList ws1 tipe ws2        -> ws1 ++ "(List" ++ (unparseType tipe) ++ ws2 ++ ")"
    TDict ws1 tipe1 tipe2 ws2 -> ws1 ++ "(Dict" ++ (unparseType tipe1) ++ (unparseType tipe2) ++ ws2 ++ ")"
    TTuple ws1 typeList ws2 maybeRestType ws3 ->
      case maybeRestType of
        Just restType -> ws1 ++ "[" ++ (String.concat (List.map unparseType typeList)) ++ ws2 ++ "|" ++ (unparseType restType) ++ ws3 ++ "]"
        Nothing       -> ws1 ++ "[" ++ (String.concat (List.map unparseType typeList)) ++ ws3 ++ "]"
    TArrow ws1 typeList ws2 -> ws1 ++ "(->" ++ (String.concat (List.map unparseType typeList)) ++ ws2 ++ ")"
    TUnion ws1 typeList ws2 -> ws1 ++ "(union" ++ (String.concat (List.map unparseType typeList)) ++ ws2 ++ ")"
    TNamed ws1 ident        -> ws1 ++ ident
    TVar ws1 ident          -> ws1 ++ ident
    TWildcard ws            -> ws ++ "_"
    TForall ws1 typeVars tipe1 ws2 ->
      ws1 ++ Utils.parens (String.concat (List.map (\(ws,x) -> ws ++ x) typeVars) ++ unparseType tipe1 ++ ws2)

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
  ETypeCase ws1 pat tbranches ws2 ->
    let tbranchesStr =
      String.concat
        <| List.map (\(TBranch_ bws1 tipe exp bws2) -> bws1 ++ "(" ++ unparseType tipe ++ unparse exp ++ bws2 ++ ")")
        <| List.map (.val) tbranches
    in
    ws1 ++ "(typecase" ++ unparsePat pat ++ tbranchesStr ++ ws2 ++ ")"
  EComment ws s e1 ->
    ws ++ ";" ++ s ++ "\n" ++ unparse e1
  EOption ws1 s1 ws2 s2 e1 ->
    ws1 ++ "# " ++ s1.val ++ ":" ++ ws2 ++ s2.val ++ "\n" ++ unparse e1
  ETyp ws1 pat tipe e ws2 ->
    ws1 ++ "(typ" ++ (unparsePat pat) ++ (unparseType tipe) ++ ws2 ++ ")" ++ unparse e
  EColonType ws1 e ws2 tipe ws3 ->
    ws1 ++ "(" ++ (unparse e) ++ ws2 ++ ":" ++ (unparseType tipe) ++ ws3 ++ ")"
  ETypeAlias ws1 pat tipe e ws2 ->
    ws1 ++ "(def" ++ (unparsePat pat) ++ (unparseType tipe) ++ ws2 ++ ")" ++ unparse e

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

-- NOTE: use this to go back to original unparser
-- unparse = sExp

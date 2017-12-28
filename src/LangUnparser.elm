module LangUnparser exposing
  ( expsEquivalent, patsEquivalent
  , unparse, unparsePat, unparseWD, unparseType
  , unparseWithIds
  , unparseWithUniformWhitespace, unparsePatWithUniformWhitespace
  , bumpCol, incCol
  )

import Lang exposing (..)
import ValUnparser exposing (..)
import Utils
import Config
import ImpureGoodies

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

expsEquivalent : Exp -> Exp -> Bool
expsEquivalent exp1 exp2 =
  unparseWithUniformWhitespace True True exp1 == unparseWithUniformWhitespace True True exp2

patsEquivalent : Pat -> Pat -> Bool
patsEquivalent pat1 pat2 =
  unparsePatWithUniformWhitespace True pat1 == unparsePatWithUniformWhitespace True pat2

------------------------------------------------------------------------------

escapeQuotes quoteChar string =
  string
  |> Regex.replace Regex.All (Regex.regex <| Regex.escape "\\") (\_ -> "\\\\")
  |> Regex.replace Regex.All (Regex.regex quoteChar)            (\_ -> "\\" ++ quoteChar)

unparseBaseVal bv =
  case bv of
    EBool True   -> "true"
    EBool False  -> "false"
    EString qc s -> qc ++ (escapeQuotes qc s) ++ qc
    ENull        -> "null"

unparseBaseValWithUniformWhitespace = unparseBaseVal  -- BaseVals don't have any whitespace yet.

unparseWD : WidgetDecl -> String
unparseWD wd =
  let strHidden bool = if bool then ",\"hidden\"" else "" in
  case wd.val of
    NoWidgetDecl        -> ""
    IntSlider a tok b _ hidden ->
      "{" ++ toString a.val ++ tok.val ++ toString b.val ++ strHidden hidden ++ "}"
    NumSlider a tok b _ hidden ->
      "{" ++ toString a.val ++ tok.val ++ toString b.val ++ strHidden hidden ++ "}"

unparseWDWithUniformWhitespace = unparseWD  -- WidgetDecls don't have any whitespace yet.

unparsePat : Pat -> String
unparsePat pat = case pat.val.p__ of
  PVar ws x wd ->
    ws.val ++ x ++ unparseWD wd
  PList ws1 ps ws2 Nothing ws3 ->
    ws1.val ++ "[" ++ (String.concat (List.map unparsePat ps)) ++ ws3.val ++ "]"
  PList ws1 ps ws2 (Just pRest) ws3 ->
    ws1.val ++ "[" ++ (String.concat (List.map unparsePat ps)) ++ ws2.val ++ "|" ++ unparsePat pRest ++ ws3.val ++ "]"
  PConst ws n -> ws.val ++ strNum n
  PBase ws bv -> ws.val ++ unparseBaseVal bv
  PAs ws1 ident ws2 p -> ws1.val ++ ident ++ ws2.val ++ "@" ++ (unparsePat p)

unparsePatWithIds : Pat -> String
unparsePatWithIds pat =
  let pidTag = "«" ++ toString pat.val.pid ++ "»" in
  case pat.val.p__ of
    PVar ws x wd ->
      ws.val ++ x ++ unparseWD wd ++ pidTag
    PList ws1 ps ws2 Nothing ws3 ->
      ws1.val ++ "[" ++ (String.concat (List.map unparsePatWithIds ps)) ++ ws3.val ++ "]" ++ pidTag
    PList ws1 ps ws2 (Just pRest) ws3 ->
      ws1.val ++ "[" ++ (String.concat (List.map unparsePatWithIds ps)) ++ ws2.val ++ "|" ++ unparsePatWithIds pRest ++ ws3.val ++ "]" ++ pidTag
    PConst ws n -> ws.val ++ strNum n ++ pidTag
    PBase ws bv -> ws.val ++ unparseBaseVal bv ++ pidTag
    PAs ws1 ident ws2 p -> ws1.val ++ ident ++ pidTag ++ ws2.val ++ "@" ++ (unparsePatWithIds p)

unparsePatWithUniformWhitespace includeWidgetDecls pat =
  let recurse p = unparsePatWithUniformWhitespace includeWidgetDecls p in
  case pat.val.p__ of
    PVar _ x wd ->
      " " ++ x ++ (if includeWidgetDecls then unparseWDWithUniformWhitespace wd else "")
    PList _ ps _ Nothing _ ->
      " " ++ "[" ++ (String.concat (List.map recurse ps)) ++ " " ++ "]"
    PList _ ps _ (Just pRest) _ ->
      " " ++ "[" ++ (String.concat (List.map recurse ps)) ++ " " ++ "|" ++ recurse pRest ++ " " ++ "]"
    PConst _ n -> " " ++ strNum n
    PBase _ bv -> " " ++ unparseBaseValWithUniformWhitespace bv
    PAs _ ident _ p -> " " ++ ident ++ " " ++ "@" ++ recurse p

unparseType : Type -> String
unparseType tipe =
  case tipe.val of
    TNum ws                   -> ws.val ++ "Num"
    TBool ws                  -> ws.val ++ "Bool"
    TString ws                -> ws.val ++ "String"
    TNull ws                  -> ws.val ++ "Null"
    TList ws1 tipe ws2        -> ws1.val ++ "(List" ++ (unparseType tipe) ++ ws2.val ++ ")"
    TDict ws1 tipe1 tipe2 ws2 -> ws1.val ++ "(Dict" ++ (unparseType tipe1) ++ (unparseType tipe2) ++ ws2.val ++ ")"
    TTuple ws1 typeList ws2 maybeRestType ws3 ->
      case maybeRestType of
        Just restType -> ws1.val ++ "[" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ "|" ++ (unparseType restType) ++ ws3.val ++ "]"
        Nothing       -> ws1.val ++ "[" ++ (String.concat (List.map unparseType typeList)) ++ ws3.val ++ "]"
    TArrow ws1 typeList ws2 -> ws1.val ++ "(->" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ ")"
    TUnion ws1 typeList ws2 -> ws1.val ++ "(union" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ ")"
    TNamed ws1 "Num"        -> ws1.val ++ "Bad_NUM"
    TNamed ws1 "Bool"       -> ws1.val ++ "Bad_BOOL"
    TNamed ws1 "String"     -> ws1.val ++ "Bad_STRING"
    TNamed ws1 "Null"       -> ws1.val ++ "Bad_NULL"
    TNamed ws1 ident        -> ws1.val ++ ident
    TVar ws1 ident          -> ws1.val ++ ident
    TWildcard ws            -> ws.val ++ "_"
    TForall ws1 typeVars tipe1 ws2 ->
      let strVar (ws,x) = ws.val ++ x in
      let sVars =
        case typeVars of
          One var             -> strVar var
          Many ws1_ vars ws2_ -> ws1_.val ++ Utils.parens (String.concat (List.map strVar vars) ++ ws2_.val)
      in
      ws1.val ++ Utils.parens ("forall" ++ sVars ++ unparseType tipe1 ++ ws2.val)

unparseTypeWithUniformWhitespace : Type -> String
unparseTypeWithUniformWhitespace tipe =
  let recurse t = unparseTypeWithUniformWhitespace t in
  case tipe.val of
    TNum _                -> " " ++ "Num"
    TBool _               -> " " ++ "Bool"
    TString _             -> " " ++ "String"
    TNull _               -> " " ++ "Null"
    TList _ tipe _        -> " " ++ "(List" ++ (recurse tipe) ++ " " ++ ")"
    TDict _ tipe1 tipe2 _ -> " " ++ "(Dict" ++ (recurse tipe1) ++ (recurse tipe2) ++ " " ++ ")"
    TTuple _ typeList _ maybeRestType _ ->
      case maybeRestType of
        Just restType -> " " ++ "[" ++ (String.concat (List.map recurse typeList)) ++ " " ++ "|" ++ (recurse restType) ++ " " ++ "]"
        Nothing       -> " " ++ "[" ++ (String.concat (List.map recurse typeList)) ++ " " ++ "]"
    TArrow _ typeList _ -> " " ++ "(->" ++ (String.concat (List.map recurse typeList)) ++ " " ++ ")"
    TUnion _ typeList _ -> " " ++ "(union" ++ (String.concat (List.map recurse typeList)) ++ " " ++ ")"
    TNamed _ "Num"      -> " " ++ "Bad_NUM"
    TNamed _ "Bool"     -> " " ++ "Bad_BOOL"
    TNamed _ "String"   -> " " ++ "Bad_STRING"
    TNamed _ "Null"     -> " " ++ "Bad_NULL"
    TNamed _ ident      -> " " ++ ident
    TVar _ ident        -> " " ++ ident
    TWildcard _          -> " " ++ "_"
    TForall _ typeVars tipe1 _ ->
      let strVar (ws,x) = " " ++ x in
      let sVars =
        case typeVars of
          One var             -> strVar var
          Many _ vars _ -> " " ++ Utils.parens (String.concat (List.map strVar vars) ++ " ")
      in
      " " ++ Utils.parens ("forall" ++ sVars ++ recurse tipe1 ++ " ")

unparse : Exp -> String
unparse e =
  unparse_ e
  -- ImpureGoodies.logTimedRun "LangUnparser.unparse" (\() ->
  --   unparse_ e
  -- )

unparse_ : Exp -> String
unparse_ e = case e.val.e__ of
  EBase ws v -> ws.val ++ unparseBaseVal v
  EConst ws n l wd ->
    let (_,b,_) = l in
    ws.val ++ toString n ++ b ++ unparseWD wd
    -- TODO: parse/unparse are not inverses for floats (e.g. 1.0)
  EVar ws x -> ws.val ++ x
  EFun ws1 [p] e1 ws2 ->
    ws1.val ++ "(\\" ++ unparsePat p ++ unparse_ e1 ++ ws2.val ++ ")"
  EFun ws1 ps e1 ws2 ->
    ws1.val ++ "(\\(" ++ (String.concat (List.map unparsePat ps)) ++ ")" ++ unparse_ e1 ++ ws2.val ++ ")"
  EApp ws1 e1 es ws2 ->
    ws1.val ++ "(" ++ unparse_ e1 ++ (String.concat (List.map unparse_ es)) ++ ws2.val ++ ")"
  EList ws1 es ws2 Nothing ws3 ->
    ws1.val ++ "[" ++ (String.concat (List.map unparse_ es)) ++ ws3.val ++ "]"
  EList ws1 es ws2 (Just eRest) ws3 ->
    ws1.val ++ "[" ++ (String.concat (List.map unparse_ es)) ++ ws2.val ++ "|" ++ unparse_ eRest ++ ws3.val ++ "]"
  EOp ws1 op es ws2 ->
    ws1.val ++ "(" ++ strOp op.val ++ (String.concat (List.map unparse_ es)) ++ ws2.val ++ ")"
  EIf ws1 e1 e2 e3 ws2 ->
    ws1.val ++ "(if" ++ unparse_ e1 ++ unparse_ e2 ++ unparse_ e3 ++ ws2.val ++ ")"
  ELet ws1 Let b p e1 e2 ws2 ->
    case p.val.p__ of
      PVar _ "_IMPLICIT_MAIN" _ ->
        ""
      _ ->
        let tok = if b then "letrec" else "let" in
        ws1.val ++ "(" ++ tok ++ unparsePat p ++ unparse_ e1 ++ unparse_ e2 ++ ws2.val ++ ")"
  ELet ws1 Def b p e1 e2 ws2 ->
    -- TODO don't used nested defs until this is re-worked
    let tok = if b then "defrec" else "def" in
    ws1.val ++ "(" ++ tok ++ unparsePat p ++ unparse_ e1 ++ ws2.val ++ ")" ++ unparse_ e2
  ECase ws1 e1 bs ws2 ->
    let branchesStr =
      String.concat
        <| List.map (\(Branch_ bws1 pat exp bws2) -> bws1.val ++ "(" ++ unparsePat pat ++ unparse_ exp ++ bws2.val ++ ")")
        <| List.map (.val) bs
    in
    ws1.val ++ "(case" ++ unparse_ e1 ++ branchesStr ++ ws2.val ++ ")"
  ETypeCase ws1 e1 tbranches ws2 ->
    let tbranchesStr =
      String.concat
        <| List.map (\(TBranch_ bws1 tipe exp bws2) -> bws1.val ++ "(" ++ unparseType tipe ++ unparse_ exp ++ bws2.val ++ ")")
        <| List.map (.val) tbranches
    in
    ws1.val ++ "(typecase" ++ unparse_ e1 ++ tbranchesStr ++ ws2.val ++ ")"
  EComment ws s e1 ->
    ws.val ++ ";" ++ s ++ "\n" ++ unparse_ e1
  EOption ws1 s1 ws2 s2 e1 ->
    ws1.val ++ "# " ++ s1.val ++ ":" ++ ws2.val ++ s2.val ++ "\n" ++ unparse_ e1
  ETyp ws1 pat tipe e ws2 ->
    ws1.val ++ "(typ" ++ (unparsePat pat) ++ (unparseType tipe) ++ ws2.val ++ ")" ++ unparse_ e
  EColonType ws1 e ws2 tipe ws3 ->
    ws1.val ++ "(" ++ (unparse_ e) ++ ws2.val ++ ":" ++ (unparseType tipe) ++ ws3.val ++ ")"
  ETypeAlias ws1 pat tipe e ws2 ->
    ws1.val ++ "(def" ++ (unparsePat pat) ++ (unparseType tipe) ++ ws2.val ++ ")" ++ unparse_ e
  EParens ws1 e ws2 ->
    unparse_ e


unparseWithIds : Exp -> String
unparseWithIds e =
  let eidTag = "<" ++ toString e.val.eid ++ ">" in
  case e.val.e__ of
    EBase ws v -> ws.val ++ unparseBaseVal v ++ eidTag
    EConst ws n l wd ->
      let (locId,b,_) = l in
      ws.val ++ toString n ++ b ++ unparseWD wd ++ "{" ++ toString locId ++ "}" ++ eidTag
      -- TODO: parse/unparseWithIds are not inverses for floats (e.g. 1.0)
    EVar ws x -> ws.val ++ x ++ eidTag
    EFun ws1 [p] e1 ws2 ->
      ws1.val ++ "(" ++ eidTag ++ "\\" ++ unparsePatWithIds p ++ unparseWithIds e1 ++ ws2.val ++ ")"
    EFun ws1 ps e1 ws2 ->
      ws1.val ++ "(" ++ eidTag ++ "\\(" ++ (String.concat (List.map unparsePatWithIds ps)) ++ ")" ++ unparseWithIds e1 ++ ws2.val ++ ")"
    EApp ws1 e1 es ws2 ->
      ws1.val ++ "(" ++ unparseWithIds e1 ++ (String.concat (List.map unparseWithIds es)) ++ ws2.val ++ ")" ++ eidTag
    EList ws1 es ws2 Nothing ws3 ->
      ws1.val ++ "[" ++ (String.concat (List.map unparseWithIds es)) ++ ws3.val ++ "]" ++ eidTag
    EList ws1 es ws2 (Just eRest) ws3 ->
      ws1.val ++ "[" ++ (String.concat (List.map unparseWithIds es)) ++ ws2.val ++ "|" ++ unparseWithIds eRest ++ ws3.val ++ "]" ++ eidTag
    EOp ws1 op es ws2 ->
      ws1.val ++ "(" ++ eidTag ++ strOp op.val ++ (String.concat (List.map unparseWithIds es)) ++ ws2.val ++ ")"
    EIf ws1 e1 e2 e3 ws2 ->
      ws1.val ++ "(" ++ eidTag ++ "if" ++ unparseWithIds e1 ++ unparseWithIds e2 ++ unparseWithIds e3 ++ ws2.val ++ ")"
    ELet ws1 Let b p e1 e2 ws2 ->
      let tok = if b then "letrec" else "let" in
      ws1.val ++ "(" ++ eidTag ++ tok ++ unparsePatWithIds p ++ unparseWithIds e1 ++ unparseWithIds e2 ++ ws2.val ++ ")"
    ELet ws1 Def b p e1 e2 ws2 ->
      -- TODO don't used nested defs until this is re-worked
      let tok = if b then "defrec" else "def" in
      ws1.val ++ "(" ++ eidTag ++ tok ++ unparsePatWithIds p ++ unparseWithIds e1 ++ ws2.val ++ ")" ++ unparseWithIds e2
    ECase ws1 e1 bs ws2 ->
      let branchesStr =
        String.concat
          <| List.map (\(Branch_ bws1 pat exp bws2) -> bws1.val ++ "(" ++ unparsePatWithIds pat ++ unparseWithIds exp ++ bws2.val ++ ")")
          <| List.map (.val) bs
      in
      ws1.val ++ "(" ++ eidTag ++ "case" ++ unparseWithIds e1 ++ branchesStr ++ ws2.val ++ ")"
    ETypeCase ws1 e1 tbranches ws2 ->
      let tbranchesStr =
        String.concat
          <| List.map (\(TBranch_ bws1 tipe exp bws2) -> bws1.val ++ "(" ++ unparseType tipe ++ unparseWithIds exp ++ bws2.val ++ ")")
          <| List.map (.val) tbranches
      in
      ws1.val ++ "(" ++ eidTag ++ "typecase" ++ unparseWithIds e1 ++ tbranchesStr ++ ws2.val ++ ")"
    EComment ws s e1 ->
      ws.val ++ ";" ++ eidTag ++ s ++ "\n" ++ unparseWithIds e1
    EOption ws1 s1 ws2 s2 e1 ->
      ws1.val ++ "#" ++ eidTag ++ " " ++ s1.val ++ ":" ++ ws2.val ++ s2.val ++ "\n" ++ unparseWithIds e1
    ETyp ws1 pat tipe e ws2 ->
      ws1.val ++ "(" ++ eidTag ++ "typ" ++ (unparsePatWithIds pat) ++ (unparseType tipe) ++ ws2.val ++ ")" ++ unparseWithIds e
    EColonType ws1 e ws2 tipe ws3 ->
      ws1.val ++ "(" ++ (unparseWithIds e) ++ ws2.val ++ ":" ++ eidTag ++ (unparseType tipe) ++ ws3.val ++ ")"
    ETypeAlias ws1 pat tipe e ws2 ->
      ws1.val ++ "(" ++ eidTag ++ "def" ++ (unparsePatWithIds pat) ++ (unparseType tipe) ++ ws2.val ++ ")" ++ unparseWithIds e
    EParens ws1 e ws2 ->
      ws1.val ++ "(" ++ eidTag ++ unparseWithIds e ++ ws2.val ++ ")"


-- Ignores given whitespace.
--
-- Useful as a key for equality comparison and deduplication.
unparseWithUniformWhitespace : Bool -> Bool -> Exp -> String
unparseWithUniformWhitespace includeWidgetDecls includeConstAnnotations exp =
  let recurse e = unparseWithUniformWhitespace includeWidgetDecls includeConstAnnotations e in
  let recursePat e = unparsePatWithUniformWhitespace includeWidgetDecls e in
  case exp.val.e__ of
    EBase _ v -> " " ++ unparseBaseValWithUniformWhitespace v
    EConst _ n l wd ->
      let (_,b,_) = l in
      " " ++ toString n ++ (if includeConstAnnotations then b else "") ++ (if includeWidgetDecls then unparseWDWithUniformWhitespace wd else "")
      -- TODO: parse/recurse are not inverses for floats (e.g. 1.0)
    EVar _ x -> " " ++ x
    EFun _ [p] e1 _ ->
      " " ++ "(\\" ++ recursePat p ++ recurse e1 ++ " " ++ ")"
    EFun _ ps e1 _ ->
      " " ++ "(\\(" ++ (String.concat (List.map recursePat ps)) ++ ")" ++ recurse e1 ++ " " ++ ")"
    EApp _ e1 es _ ->
      " " ++ "(" ++ recurse e1 ++ (String.concat (List.map recurse es)) ++ " " ++ ")"
    EList _ es _ Nothing _ ->
      " " ++ "[" ++ (String.concat (List.map recurse es)) ++ " " ++ "]"
    EList _ es _ (Just eRest) _ ->
      " " ++ "[" ++ (String.concat (List.map recurse es)) ++ " " ++ "|" ++ recurse eRest ++ " " ++ "]"
    EOp _ op es _ ->
      " " ++ "(" ++ strOp op.val ++ (String.concat (List.map recurse es)) ++ " " ++ ")"
    EIf _ e1 e2 e3 _ ->
      " " ++ "(if" ++ recurse e1 ++ recurse e2 ++ recurse e3 ++ " " ++ ")"
    ELet _ Let b p e1 e2 _ ->
      let tok = if b then "letrec" else "let" in
      " " ++ "(" ++ tok ++ recursePat p ++ recurse e1 ++ recurse e2 ++ " " ++ ")"
    ELet _ Def b p e1 e2 _ ->
      -- TODO don't used nested defs until this is re-worked
      let tok = if b then "defrec" else "def" in
      " " ++ "(" ++ tok ++ recursePat p ++ recurse e1 ++ " " ++ ")" ++ recurse e2
    ECase _ e1 bs _ ->
      let branchesStr =
        String.concat
          <| List.map (\(Branch_ _ pat exp _) -> " " ++ "(" ++ recursePat pat ++ recurse exp ++ " " ++ ")")
          <| List.map (.val) bs
      in
      " " ++ "(case" ++ recurse e1 ++ branchesStr ++ " " ++ ")"
    ETypeCase _ e1 tbranches _ ->
      let tbranchesStr =
        String.concat
          <| List.map (\(TBranch_ _ tipe exp _) -> " " ++ "(" ++ unparseTypeWithUniformWhitespace tipe ++ recurse exp ++ " " ++ ")")
          <| List.map (.val) tbranches
      in
      " " ++ "(typecase" ++ recurse e1 ++ tbranchesStr ++ " " ++ ")"
    EComment _ s e1 ->
      " " ++ ";" ++ s ++ "\n" ++ recurse e1
    EOption _ s1 _ s2 e1 ->
      " " ++ "# " ++ s1.val ++ ":" ++ " " ++ s2.val ++ "\n" ++ recurse e1
    ETyp _ pat tipe e _ ->
      " " ++ "(typ" ++ (recursePat pat) ++ (unparseTypeWithUniformWhitespace tipe) ++ " " ++ ")" ++ recurse e
    EColonType _ e _ tipe _ ->
      " " ++ "(" ++ (recurse e) ++ " " ++ ":" ++ (unparseTypeWithUniformWhitespace tipe) ++ " " ++ ")"
    ETypeAlias _ pat tipe e _ ->
      " " ++ "(def" ++ (recursePat pat) ++ (unparseTypeWithUniformWhitespace tipe) ++ " " ++ ")" ++ recurse e
    EParens _ e _ ->
      recurse e

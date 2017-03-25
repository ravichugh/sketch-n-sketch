module LangUnparser exposing
  ( unparse, unparsePat, unparseWD, unparseType
  , unparseWithIds
  , unparseWithUniformWhitespace, unparsePatWithUniformWhitespace
  , bumpCol, incCol
  )

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
    ws ++ x ++ unparseWD wd
  PList ws1 ps ws2 Nothing ws3 ->
    ws1 ++ "[" ++ (String.concat (List.map unparsePat ps)) ++ ws3 ++ "]"
  PList ws1 ps ws2 (Just pRest) ws3 ->
    ws1 ++ "[" ++ (String.concat (List.map unparsePat ps)) ++ ws2 ++ "|" ++ unparsePat pRest ++ ws3 ++ "]"
  PConst ws n -> ws ++ strNum n
  PBase ws bv -> ws ++ unparseBaseVal bv
  PAs ws1 ident ws2 p -> ws1 ++ ident ++ ws2 ++ "@" ++ (unparsePat p)

unparsePatWithIds : Pat -> String
unparsePatWithIds pat =
  let pidTag = "«" ++ toString pat.val.pid ++ "»" in
  case pat.val.p__ of
    PVar ws x wd ->
      ws ++ x ++ unparseWD wd ++ pidTag
    PList ws1 ps ws2 Nothing ws3 ->
      ws1 ++ "[" ++ (String.concat (List.map unparsePatWithIds ps)) ++ ws3 ++ "]" ++ pidTag
    PList ws1 ps ws2 (Just pRest) ws3 ->
      ws1 ++ "[" ++ (String.concat (List.map unparsePatWithIds ps)) ++ ws2 ++ "|" ++ unparsePatWithIds pRest ++ ws3 ++ "]" ++ pidTag
    PConst ws n -> ws ++ strNum n ++ pidTag
    PBase ws bv -> ws ++ unparseBaseVal bv ++ pidTag
    PAs ws1 ident ws2 p -> ws1 ++ ident ++ pidTag ++ ws2 ++ "@" ++ (unparsePatWithIds p)

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
    TNamed ws1 "Num"        -> ws1 ++ "Bad_NUM"
    TNamed ws1 "Bool"       -> ws1 ++ "Bad_BOOL"
    TNamed ws1 "String"     -> ws1 ++ "Bad_STRING"
    TNamed ws1 "Null"       -> ws1 ++ "Bad_NULL"
    TNamed ws1 ident        -> ws1 ++ ident
    TVar ws1 ident          -> ws1 ++ ident
    TWildcard ws            -> ws ++ "_"
    TForall ws1 typeVars tipe1 ws2 ->
      let strVar (ws,x) = ws ++ x in
      let sVars =
        case typeVars of
          One var             -> strVar var
          Many ws1_ vars ws2_ -> ws1_ ++ Utils.parens (String.concat (List.map strVar vars) ++ ws2_)
      in
      ws1 ++ Utils.parens ("forall" ++ sVars ++ unparseType tipe1 ++ ws2)

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
unparse e = case e.val.e__ of
  EBase ws v -> ws ++ unparseBaseVal v
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


unparseWithIds : Exp -> String
unparseWithIds e =
  let eidTag = "<" ++ toString e.val.eid ++ ">" in
  case e.val.e__ of
    EBase ws v -> ws ++ unparseBaseVal v ++ eidTag
    EConst ws n l wd ->
      let (locId,b,_) = l in
      ws ++ toString n ++ b ++ unparseWD wd ++ "{" ++ toString locId ++ "}" ++ eidTag
      -- TODO: parse/unparseWithIds are not inverses for floats (e.g. 1.0)
    EVar ws x -> ws ++ x ++ eidTag
    EFun ws1 [p] e1 ws2 ->
      ws1 ++ "(\\" ++ unparsePatWithIds p ++ unparseWithIds e1 ++ ws2 ++ ")" ++ eidTag
    EFun ws1 ps e1 ws2 ->
      ws1 ++ "(\\(" ++ (String.concat (List.map unparsePatWithIds ps)) ++ ")" ++ unparseWithIds e1 ++ ws2 ++ ")" ++ eidTag
    EApp ws1 e1 es ws2 ->
      ws1 ++ "(" ++ unparseWithIds e1 ++ (String.concat (List.map unparseWithIds es)) ++ ws2 ++ ")" ++ eidTag
    EList ws1 es ws2 Nothing ws3 ->
      ws1 ++ "[" ++ (String.concat (List.map unparseWithIds es)) ++ ws3 ++ "]" ++ eidTag
    EList ws1 es ws2 (Just eRest) ws3 ->
      ws1 ++ "[" ++ (String.concat (List.map unparseWithIds es)) ++ ws2 ++ "|" ++ unparseWithIds eRest ++ ws3 ++ "]" ++ eidTag
    EOp ws1 op es ws2 ->
      ws1 ++ "(" ++ strOp op.val ++ (String.concat (List.map unparseWithIds es)) ++ ws2 ++ ")" ++ eidTag
    EIf ws1 e1 e2 e3 ws2 ->
      ws1 ++ "(if" ++ unparseWithIds e1 ++ unparseWithIds e2 ++ unparseWithIds e3 ++ ws2 ++ ")" ++ eidTag
    ELet ws1 Let b p e1 e2 ws2 ->
      let tok = if b then "letrec" else "let" in
      ws1 ++ "(" ++ tok ++ unparsePatWithIds p ++ unparseWithIds e1 ++ unparseWithIds e2 ++ ws2 ++ ")" ++ eidTag
    ELet ws1 Def b p e1 e2 ws2 ->
      -- TODO don't used nested defs until this is re-worked
      let tok = if b then "defrec" else "def" in
      ws1 ++ "(" ++ tok ++ unparsePatWithIds p ++ unparseWithIds e1 ++ ws2 ++ ")" ++ eidTag ++ unparseWithIds e2
    ECase ws1 e1 bs ws2 ->
      let branchesStr =
        String.concat
          <| List.map (\(Branch_ bws1 pat exp bws2) -> bws1 ++ "(" ++ unparsePatWithIds pat ++ unparseWithIds exp ++ bws2 ++ ")")
          <| List.map (.val) bs
      in
      ws1 ++ "(case" ++ unparseWithIds e1 ++ branchesStr ++ ws2 ++ ")" ++ eidTag
    ETypeCase ws1 pat tbranches ws2 ->
      let tbranchesStr =
        String.concat
          <| List.map (\(TBranch_ bws1 tipe exp bws2) -> bws1 ++ "(" ++ unparseType tipe ++ unparseWithIds exp ++ bws2 ++ ")")
          <| List.map (.val) tbranches
      in
      ws1 ++ "(typecase" ++ unparsePatWithIds pat ++ tbranchesStr ++ ws2 ++ ")" ++ eidTag
    EComment ws s e1 ->
      ws ++ ";" ++ s ++ eidTag ++ "\n" ++ unparseWithIds e1
    EOption ws1 s1 ws2 s2 e1 ->
      ws1 ++ "# " ++ s1.val ++ ":" ++ ws2 ++ s2.val ++ eidTag ++ "\n" ++ unparseWithIds e1
    ETyp ws1 pat tipe e ws2 ->
      ws1 ++ "(typ" ++ (unparsePatWithIds pat) ++ (unparseType tipe) ++ ws2 ++ ")" ++ eidTag ++ unparseWithIds e
    EColonType ws1 e ws2 tipe ws3 ->
      ws1 ++ "(" ++ (unparseWithIds e) ++ ws2 ++ ":" ++ (unparseType tipe) ++ ws3 ++ ")" ++ eidTag
    ETypeAlias ws1 pat tipe e ws2 ->
      ws1 ++ "(def" ++ (unparsePatWithIds pat) ++ (unparseType tipe) ++ ws2 ++ ")" ++ eidTag ++ unparseWithIds e


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
    ETypeCase _ pat tbranches _ ->
      let tbranchesStr =
        String.concat
          <| List.map (\(TBranch_ _ tipe exp _) -> " " ++ "(" ++ unparseTypeWithUniformWhitespace tipe ++ recurse exp ++ " " ++ ")")
          <| List.map (.val) tbranches
      in
      " " ++ "(typecase" ++ recursePat pat ++ tbranchesStr ++ " " ++ ")"
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

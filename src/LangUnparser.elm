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
  PWildcard ws ->
    ws.val ++ "_"
  PVar ws x wd ->
    ws.val ++ x ++ unparseWD wd
  PList ws1 ps ws2 Nothing ws3 ->
    ws1.val ++ "[" ++ (String.concat (List.map unparsePat ps)) ++ ws3.val ++ "]"
  PList ws1 ps ws2 (Just pRest) ws3 ->
    ws1.val ++ "[" ++ (String.concat (List.map unparsePat ps)) ++ ws2.val ++ "|" ++ unparsePat pRest ++ ws3.val ++ "]"
  -- PRecord _ _ _ -> Debug.crash "internal error, cannot unparse pattern in LangUnparser"
  PRecord _ _ _ -> "internal error, cannot unparse pattern in LangUnparser"
  PConst ws n -> ws.val ++ strNum n
  PBase ws bv -> ws.val ++ unparseBaseVal bv
  PAs ws1 p1 ws2 p2 -> ws1.val ++ (unparsePat p1) ++ ws2.val ++ "@" ++ (unparsePat p2)
  PParens ws1 p ws2 -> ws1.val ++ "(" ++ unparsePat p ++ ws2.val ++ ")"
  PColonType _ _ _ _ -> "internal error, cannot unparse pcolontype in LangUnparser"

unparsePatWithIds : Pat -> String
unparsePatWithIds pat =
  let pidTag = "«" ++ toString pat.val.pid ++ "»" in
  case pat.val.p__ of
    PWildcard ws ->
      ws.val ++ "_"
    PVar ws x wd ->
      ws.val ++ x ++ unparseWD wd ++ pidTag
    PList ws1 ps ws2 Nothing ws3 ->
      ws1.val ++ "[" ++ (String.concat (List.map unparsePatWithIds ps)) ++ ws3.val ++ "]" ++ pidTag
    PList ws1 ps ws2 (Just pRest) ws3 ->
      ws1.val ++ "[" ++ (String.concat (List.map unparsePatWithIds ps)) ++ ws2.val ++ "|" ++ unparsePatWithIds pRest ++ ws3.val ++ "]" ++ pidTag
    PConst ws n -> ws.val ++ strNum n ++ pidTag
    -- PRecord _ _ _ -> Debug.crash "internal error, cannot unparse pattern with ids in LangUnparser"
    PRecord _ _ _ -> "internal error, cannot unparse pattern with ids in LangUnparser"
    PBase ws bv -> ws.val ++ unparseBaseVal bv ++ pidTag
    PAs ws1 p1 ws2 p2 -> ws1.val ++ (unparsePatWithIds p1) ++ pidTag ++ ws2.val ++ "@" ++ (unparsePatWithIds p2)
    PParens ws1 p ws2 -> ws1.val ++ "(" ++ pidTag ++ unparsePatWithIds p ++ ws2.val ++ ")"
    PColonType _ _ _ _ -> "internal error, cannot unparse pcolontype in LangUnparser"

unparsePatWithUniformWhitespace includeWidgetDecls pat =
  let recurse p = unparsePatWithUniformWhitespace includeWidgetDecls p in
  case pat.val.p__ of
    PWildcard ws ->
      " " ++ "_"
    PVar _ x wd ->
      " " ++ x ++ (if includeWidgetDecls then unparseWDWithUniformWhitespace wd else "")
    PList _ ps _ Nothing _ ->
      " " ++ "[" ++ (String.concat (List.map recurse ps)) ++ " " ++ "]"
    PList _ ps _ (Just pRest) _ ->
      " " ++ "[" ++ (String.concat (List.map recurse ps)) ++ " " ++ "|" ++ recurse pRest ++ " " ++ "]"
    -- PRecord _ _ _ -> Debug.crash "internal error, cannot unparse pattern in LangUnparser"
    PRecord _ _ _ -> "internal error, cannot unparse pattern in LangUnparser"
    PConst _ n -> " " ++ strNum n
    PBase _ bv -> " " ++ unparseBaseValWithUniformWhitespace bv
    PAs _ p1 _ p2 -> " " ++ recurse p1 ++ " " ++ "@" ++ recurse p2
    PParens _ p _ -> " (" ++ recurse p ++ " )"
    PColonType _ _ _ _ -> "internal error, cannot unparse pcolontype in LangUnparser"

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
    -- TRecord _ _ _ _ -> Debug.crash "internal error: cannot unparse TRecord in LangUnparser"
    TRecord _ _ _ _ -> "internal error: cannot unparse TRecord in LangUnparser"
    TArrow ws1 typeList ws2 -> ws1.val ++ "(->" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ ")"
    TUnion ws1 typeList ws2 -> ws1.val ++ "(union" ++ (String.concat (List.map unparseType typeList)) ++ ws2.val ++ ")"
    {-TApp ws1 "Num" _ _        -> ws1.val ++ "Bad_NUM"
    TApp ws1 "Bool" _ _       -> ws1.val ++ "Bad_BOOL"
    TApp ws1 "String" _ _     -> ws1.val ++ "Bad_STRING"
    TApp ws1 "Null" _ _       -> ws1.val ++ "Bad_NULL"-}
    TApp ws1 ident ts _       -> ws1.val ++ unparseType ident ++ String.concat (List.map unparseType ts)
    TVar ws1 ident          -> ws1.val ++ ident
    TWildcard ws            -> ws.val ++ "_"
    TForall ws1 typeVars tipe1 ws2 ->
      let sVars =
        case typeVars of
           [var]             -> unparseTPat var
           vars -> ws1.val ++ Utils.parens (String.concat (List.map unparseTPat vars) ++ ws2.val)
      in
      ws1.val ++ Utils.parens ("forall" ++ sVars ++ unparseType tipe1 ++ ws2.val)
    TParens ws1 e ws2 ->
      ws1.val ++ "(" ++ unparseType e ++ ws2.val ++ ")"

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
    -- TRecord _ _ _ _ ->  Debug.crash "[internal error] Cannot unparse record type in Langunparser"
    TRecord _ _ _ _ ->  "[internal error] Cannot unparse record type in Langunparser"
    TArrow _ typeList _ -> " " ++ "(->" ++ (String.concat (List.map recurse typeList)) ++ " " ++ ")"
    TUnion _ typeList _ -> " " ++ "(union" ++ (String.concat (List.map recurse typeList)) ++ " " ++ ")"
    --TApp _ "Num" _ _      -> " " ++ "Bad_NUM"
    --TApp _ "Bool" _ _     -> " " ++ "Bad_BOOL"
    --TApp _ "String" _ _   -> " " ++ "Bad_STRING"
    --TApp _ "Null" _ _     -> " " ++ "Bad_NULL"
    TApp _ t ts _     -> " " ++ unparseTypeWithUniformWhitespace t ++ String.concat (List.map unparseTypeWithUniformWhitespace ts)
    TVar _ ident        -> " " ++ ident
    TWildcard _          -> " " ++ "_"
    TForall _ typeVars tipe1 _ ->
      let sVars =
        case typeVars of
           [var]             -> unparseTPat var
           vars -> " " ++ Utils.parens (String.concat (List.map unparseTPat vars) ++ " ")
      in
      " " ++ Utils.parens ("forall" ++ sVars ++ recurse tipe1 ++ " ")
    TParens _ e _ ->
      " (" ++ unparseTypeWithUniformWhitespace e ++ ")"

unparseTPat: TPat -> String
unparseTPat pat = case pat.val of
  TPatVar ws ident -> ws.val ++ ident

unparse : Exp -> String
unparse e =
  unparse_ e
  -- ImpureGoodies.logTimedRun "LangUnparser.unparse" (\() ->
  --   unparse_ e
  -- )


unparseDecls withIds (Declarations _ types anns exps) =
    let patu = if withIds then unparsePatWithIds else unparsePat in
    let typu = if withIds then unparseType else unparseType in
    let expu = if withIds then unparseWithIds else unparse_ in
    (foldLeftGroup "" types <| (\acc group isRec ->
       acc ++ (group |> List.map (\(LetType _ wsBefore _ pat _ ws2 tipe) ->
        wsBefore.val ++ "(def" ++ (patu pat) ++ (typu tipe) ++ ws2.val ++ ")"
       ) |> String.join "")
    )) ++
    (Utils.foldLeft "" anns <| (\acc (LetAnnotation _ wsBefore pat _ ws2 tipe) ->
       acc ++ wsBefore.val ++ "(typ" ++ (patu pat) ++ (typu tipe) ++ ws2.val ++ ")"
    )) ++
    (foldLeftGroup "" exps <| (\acc group isRec ->
       let tok = if isRec then "defrec" else "def" in -- TODO: This doesn't work for mutually recursive, only recursive.
       acc ++ (group |> List.map (\(LetExp _ wsBefore pat _ ws2 e1) ->
         wsBefore.val ++ "(" ++ tok ++ patu pat ++ expu e1 ++ ws2.val ++ ")"
       ) |> String.join "")
    ))

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
  EApp ws1 e1 es apptype ws2 ->
    ws1.val ++ "(" ++ unparse_ e1 ++ (String.concat (List.map unparse_ es)) ++ ws2.val ++ ")"
  EList ws1 es ws2 Nothing ws3 ->
    ws1.val ++ "[" ++ (String.concat (List.map (unparse_ << Tuple.second) es)) ++ ws3.val ++ "]"
  EList ws1 es ws2 (Just eRest) ws3 ->
    ws1.val ++ "[" ++ (String.concat (List.map (unparse_ << Tuple.second) es)) ++ ws2.val ++ "|" ++ unparse_ eRest ++ ws3.val ++ "]"
  -- ERecord _ _ _ _ -> Debug.crash "internal error, cannot unparse record in LangUnparser"
  -- ESelect _ _ _ _ _ -> Debug.crash "internal error, cannot unparse recordselect in LangUnparser"
  ERecord _ _ _ _ -> "internal error, cannot unparse record in LangUnparser"
  ESelect _ _ _ _ _ -> "internal error, cannot unparse recordselect in LangUnparser"
  EOp ws1 _ op es ws2 ->
    ws1.val ++ "(" ++ strOp op.val ++ (String.concat (List.map unparse_ es)) ++ ws2.val ++ ")"
  EIf ws1 e1 _ e2 _ e3 ws2 ->
    ws1.val ++ "(if" ++ unparse_ e1 ++ unparse_ e2 ++ unparse_ e3 ++ ws2.val ++ ")"
  ELet ws1 Let (Declarations _ [] [] [(isRec, [LetExp _ wsBefore p _ wse1 e1])]) ws2 e2 ->
    case p.val.p__ of
      PVar _ "_IMPLICIT_MAIN" _ ->
        ""
      _ ->
        let tok = if isRec then "letrec" else  "let" in
        ws1.val ++ "(" ++ tok ++ wsBefore.val ++ unparsePat p ++ unparse_ e1 ++ unparse_ e2 ++ ws2.val ++ ")"
  ELet ws1 Let _ _ _ ->
    "internal error, cannot unparse ELet/Let in LangUnparser if more than one exp declaration"
  ELet ws1 Def decls ws2 e2 ->
    unparseDecls False decls ++ unparse_ e2
  ECase ws1 e1 bs ws2 ->
    let branchesStr =
      String.concat
        <| List.map (\(Branch_ bws1 pat exp bws2) -> bws1.val ++ "(" ++ unparsePat pat ++ unparse_ exp ++ bws2.val ++ ")")
        <| List.map (.val) bs
    in
    ws1.val ++ "(case" ++ unparse_ e1 ++ branchesStr ++ ws2.val ++ ")"
  EColonType ws1 e ws2 tipe ws3 ->
    ws1.val ++ "(" ++ (unparse_ e) ++ ws2.val ++ ":" ++ (unparseType tipe) ++ ws3.val ++ ")"
  EParens ws1 e pStyle ws2 ->
    unparse_ e
  EHole ws h ->
    ws.val ++ "??"


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
    EApp ws1 e1 es apptype ws2 ->
      ws1.val ++ "(" ++ unparseWithIds e1 ++ (String.concat (List.map unparseWithIds es)) ++ ws2.val ++ ")" ++ eidTag
    EList ws1 es ws2 Nothing ws3 ->
      ws1.val ++ "[" ++ (String.concat (List.map (unparseWithIds << Tuple.second) es)) ++ ws3.val ++ "]" ++ eidTag
    EList ws1 es ws2 (Just eRest) ws3 ->
      ws1.val ++ "[" ++ (String.concat (List.map (unparseWithIds << Tuple.second) es)) ++ ws2.val ++ "|" ++ unparseWithIds eRest ++ ws3.val ++ "]" ++ eidTag
    -- ERecord _ _ _ _ -> Debug.crash "internal error, cannot unparse record in LangUnparser"
    -- ESelect _ _ _ _ _ -> Debug.crash "internal error, cannot unparse select in LangUnparser"
    ERecord _ _ _ _ -> "internal error, cannot unparse record in LangUnparser"
    ESelect _ _ _ _ _ -> "internal error, cannot unparse select in LangUnparser"
    EOp ws1 wso op es ws2 ->
      ws1.val ++ "(" ++ eidTag ++ strOp op.val ++ (String.concat (List.map unparseWithIds es)) ++ ws2.val ++ ")"
    EIf ws1 e1 _ e2 _ e3 ws2 ->
      ws1.val ++ "(" ++ eidTag ++ "if" ++ unparseWithIds e1 ++ unparseWithIds e2 ++ unparseWithIds e3 ++ ws2.val ++ ")"
    ELet ws1 Let (Declarations _ [] [] [(isRec, [LetExp _ wsBefore p _ wse1 e1])]) ws2 e2 ->
      let tok = if isRec then "letrec" else "let" in
      ws1.val ++ "(" ++ eidTag ++ tok ++ unparsePatWithIds p ++ unparseWithIds e1 ++ unparseWithIds e2 ++ ws2.val ++ ")"
    ELet ws1 Let _ _ _ ->
      eidTag ++ "internal error, cannot unparse ELet/Let in LangUnparser if more than one exp declaration"
    ELet ws1 Def decls ws2 e2 ->
      eidTag ++ unparseDecls True decls ++ unparseWithIds e2
    ECase ws1 e1 bs ws2 ->
      let branchesStr =
        String.concat
          <| List.map (\(Branch_ bws1 pat exp bws2) -> bws1.val ++ "(" ++ unparsePatWithIds pat ++ unparseWithIds exp ++ bws2.val ++ ")")
          <| List.map (.val) bs
      in
      ws1.val ++ "(" ++ eidTag ++ "case" ++ unparseWithIds e1 ++ branchesStr ++ ws2.val ++ ")"
    EColonType ws1 e ws2 tipe ws3 ->
      ws1.val ++ "(" ++ (unparseWithIds e) ++ ws2.val ++ ":" ++ eidTag ++ (unparseType tipe) ++ ws3.val ++ ")"
    EParens ws1 e pStyle ws2 ->
      ws1.val ++ "(" ++ eidTag ++ unparseWithIds e ++ ws2.val ++ ")"
    EHole ws h ->
      ws.val ++ "??" ++ eidTag


-- Ignores given whitespace.
--
-- Useful as a key for equality comparison and deduplication.
unparseWithUniformWhitespace : Bool -> Bool -> Exp -> String
unparseWithUniformWhitespace includeWidgetDecls includeConstAnnotations exp =
  let recurse e = unparseWithUniformWhitespace includeWidgetDecls includeConstAnnotations e in
  let recursePat e = unparsePatWithUniformWhitespace includeWidgetDecls e in
  let recurseDecls (Declarations _ types anns exps) =
    (foldLeftGroup "" types <| (\acc group isRec ->
       acc ++ (group |> List.map (\(LetType _ _ _ pat _ _ tipe) ->
        " " ++ "(def" ++ (recursePat pat) ++ (unparseTypeWithUniformWhitespace tipe) ++ " " ++ ")"
       ) |> String.join "")
    )) ++
    (Utils.foldLeft "" anns <| (\acc (LetAnnotation _ _ pat _ _ tipe) ->
       acc ++ " " ++ "(typ" ++ (recursePat pat) ++ (unparseTypeWithUniformWhitespace tipe) ++ " " ++ ")"
    )) ++
    (foldLeftGroup "" exps <| (\acc group isRec ->
       let tok = if isRec then "defrec" else "def" in -- TODO: This doesn't work for mutually recursive, only recursive.
       acc ++ (group |> List.map (\(LetExp _ _ pat _ _ e1) ->
         " " ++ "(" ++ tok ++ recursePat pat ++ recurse e1 ++ " " ++ ")"
       ) |> String.join "")
    ))
  in
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
    EApp _ e1 es apptype _ ->
      " " ++ "(" ++ recurse e1 ++ (String.concat (List.map recurse es)) ++ " " ++ ")"
    EList _ es _ Nothing _ ->
      " " ++ "[" ++ (String.concat (List.map (recurse << Tuple.second) es)) ++ " " ++ "]"
    EList _ es _ (Just eRest) _ ->
      " " ++ "[" ++ (String.concat (List.map (recurse << Tuple.second) es)) ++ " " ++ "|" ++ recurse eRest ++ " " ++ "]"
    -- ERecord _ _ _ _ -> -- Don't need to reinvent the wheel.
    --   Debug.crash "[Internal error] Cannot unparse records in FastParse"
    -- ESelect _ _ _ _ _ -> -- Don't need to reinvent the wheel.
    --   Debug.crash "[Internal error] Cannot unparse records in FastParse"
    ERecord _ _ _ _ -> -- Don't need to reinvent the wheel.
      "[Internal error] Cannot unparse records in FastParse"
    ESelect _ _ _ _ _ -> -- Don't need to reinvent the wheel.
      "[Internal error] Cannot unparse records in FastParse"
    EOp _ _ op es _ ->
      " " ++ "(" ++ strOp op.val ++ (String.concat (List.map recurse es)) ++ " " ++ ")"
    EIf _ e1 _ e2 _ e3 _ ->
      " " ++ "(if" ++ recurse e1 ++ recurse e2 ++ recurse e3 ++ " " ++ ")"
    ELet _ Let (Declarations _ [] [] [(isRec, [LetExp _ _ p _ _ e1])]) _ e2 ->
      let tok = if isRec then "letrec" else "let" in
      " " ++ "(" ++ tok ++ recursePat p ++ recurse e1 ++ recurse e2 ++ " " ++ ")"
    ELet _ Let _ _ _ ->
      "[Internal error] do not support more than 1 definition in ELet/Let in LangUnparser"
    ELet _ Def decls _ e2 ->
      recurseDecls decls ++ recurse e2
    ECase _ e1 bs _ ->
      let branchesStr =
        String.concat
          <| List.map (\(Branch_ _ pat exp _) -> " " ++ "(" ++ recursePat pat ++ recurse exp ++ " " ++ ")")
          <| List.map (.val) bs
      in
      " " ++ "(case" ++ recurse e1 ++ branchesStr ++ " " ++ ")"
    EColonType _ e _ tipe _ ->
      " " ++ "(" ++ (recurse e) ++ " " ++ ":" ++ (unparseTypeWithUniformWhitespace tipe) ++ " " ++ ")"
    EParens _ e _ _ ->
      recurse e
    EHole _ _ ->
      " ??"

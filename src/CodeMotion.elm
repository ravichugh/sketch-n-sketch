module CodeMotion exposing
  ( moveDefinitionPat, moveDefinitionBeforeEId
  )

import Lang exposing (..)
import LangTools exposing (..)
import LangUnparser exposing (unparse, unparseWithIds, unparsePat)
-- import DependenceGraph exposing
  -- (ScopeGraph, ScopeOrder(..), parentScopeOf, childScopesOf)
import InterfaceModel exposing (SynthesisResult(..))
import Utils

import Dict


type alias VarEquation = (Ident, Exp)
type alias RebuildLetExp = Exp -> Exp

-- returns the binding (x, e1) to be removed from let-expression at scopeId,
-- the body of the let at the scopeId
-- and a function foo that rewrites this let-expression by wrapping the
--   residual pattern p_ and equation e1_ (that remain after plucking)
--   around a rewritten body e2_ (provided as an argument)
pluck : PatternId -> Exp -> Maybe (VarEquation, RebuildLetExp)
pluck ((scopeEId, scopeBranchI), path) program =
  findExpByEId program scopeEId
  |> Maybe.andThen (\scope -> pluck_ scope path program)

-- TODO: Update for ECase/EFun
-- TODO: Update for nested patterns
pluck_ : Exp -> List Int -> Exp -> Maybe (VarEquation, RebuildLetExp)
pluck_ scopeExp path program =
  case scopeExp.val.e__ of
    ELet ws1 letKind False p e1 e2 ws2 ->
      case (p.val, e1.val.e__, path) of

        (PVar _ x _, _, []) ->
          Just ((x, e1), \e2_ -> e2_)

        ( PList pws1 ps pws2 Nothing pws3
        , EList ews1 es ews2 Nothing ews3
        , [i]
        ) ->
          case (List.length ps == List.length es, (Utils.geti i ps).val) of
            (False, _) ->
              Debug.log "pluck different lengths" Nothing

            (True, PVar _ xi _) ->
              let ps_ = Utils.removei i ps in
              let es_ = Utils.removei i es in
              let ei = Utils.geti i es in
              if List.length ps_ == 0 then
                Just ((xi, ei), \e2_ -> e2_)
              else
                let p_ = replaceP_  p (PList pws1 ps_ pws2 Nothing pws3) in
                let e1_ = replaceE__ e1 (EList ews1 es_ ews2 Nothing ews3) in
                let let_ e2_ = replaceE__ scopeExp (ELet ws1 letKind False p_ e1_ e2_ ws2) in
                Just ((xi, ei), let_)

            (True, pi) ->
              let _ = Debug.log "trying to pluck" pi in
              Nothing

        _ ->
          let _ = Debug.log ("pluck_: bad pattern " ++ unparsePat p) path in
          Nothing

    _ ->
      Debug.crash <| "pluck_: bad Exp__ " ++ unparseWithIds scopeExp


------------------------------------------------------------------------------

ensureWhitespace : String -> String
ensureWhitespace s = if s == "" then " " else s

ensureWhitespaceExp : Exp -> Exp
ensureWhitespaceExp exp = mapPrecedingWhitespace ensureWhitespace exp

ensureWhitespaceExps : List Exp -> List Exp
ensureWhitespaceExps exps =
  case exps of
    []         -> []
    hd :: rest -> ensureWhitespaceExp hd :: rest

ensureWhitespacePats : List Pat -> List Pat
ensureWhitespacePats pats =
  case pats of
    []         -> []
    hd :: rest -> replacePrecedingWhitespacePat
                    (ensureWhitespace (precedingWhitespacePat hd)) hd :: rest

strUnsafeBool isUnsafe = if isUnsafe then "[UNSAFE] " else ""


------------------------------------------------------------------------------


-- TODO: update for EFun/ECase
--
-- Moving a definition is safe if all identifiers resolve to the same bindings.
--
-- More specifically:
--   - All free variables in the moved assignment still resolve to the same bindings
--   - All previous references to the moved identifier still resolve to that identifer
--   - All other variables uses of the same name do not resolve to the moved identifier
--
-- Returns (list of safe results, list of unsafe results)
moveDefinitionBeforeEId : PatternId -> EId -> Exp -> (List SynthesisResult, List SynthesisResult)
moveDefinitionBeforeEId sourcePatId targetEId program =
  let ((sourceScopeEId, _), _) = sourcePatId in
  case pluck sourcePatId program of
    Nothing -> Debug.crash <| "could not pluck pat " ++ toString sourcePatId ++ " from " ++ unparseWithIds program
    Just ((ident, boundExp), wrapWithResidualLet) ->
      let newProgram =
        flip mapExp program <| \e ->
          if e.val.eid == sourceScopeEId then
            wrapWithResidualLet (expToLetBody e)
          else if e.val.eid == targetEId then
            let letOrDef = if isTopLevel e program then Def else Let in
            withDummyPos <|
              ELet (precedingWhitespace e) letOrDef False
                (pVar ident) (ensureWhitespaceExp boundExp)
                e ""
          else
            e
      in
      -- let _ = Debug.log ("old: \n" ++ unparseWithIds program) () in
      -- let _ = Debug.log ("new: \n" ++ unparseWithIds newProgram) () in
      let oldScopeBody   = justFindExpByEId program sourceScopeEId |> expToLetBody in
      let movedScopeBody = justFindExpByEId newProgram targetEId in
      let isSafe =
        let identUsesSafe =
          -- Effectively comparing EIds of all uses of the identifier.
          -- Presumably these will be exactly equal rather than equal as sets since we shouldn't be moving around the relative position of the variable usages...still, I think a set is the natural strucuture here
          Utils.equalAsSets (identifierUses ident oldScopeBody) (identifierUses ident movedScopeBody)
        in
        let boundExpVarsSafe =
          freeVars boundExp
          |> List.all (\e -> bindingScopeIdFor e program == bindingScopeIdFor e newProgram)
        in
        identUsesSafe && boundExpVarsSafe
      in
      let caption =
        strUnsafeBool (not isSafe) ++ " Move " ++ ident ++ " before " ++ (unparse >> Utils.squish >> Utils.niceTruncateString 20 "...") movedScopeBody
      in
      let result =
        SynthesisResult { description = caption, exp = newProgram, sortKey = [], children = Nothing }
      in
      if isSafe
      then ([result], [])
      else ([], [result])


------------------------------------------------------------------------------

-- TODO: update for EFun/ECase
--
-- Returns (list of safe results, list of unsafe results)
moveDefinitionPat : PatternId -> PatternId -> Exp -> (List SynthesisResult, List SynthesisResult)
moveDefinitionPat sourcePatId targetPatId program =
  let ((sourceScopeEId, _), _) = sourcePatId in
  let ((targetEId, _), _) = targetPatId in
  case pluck sourcePatId program of
    Nothing -> Debug.crash <| "could not pluck pat " ++ toString sourcePatId ++ " from " ++ unparseWithIds program
    Just ((ident, boundExp), wrapWithResidualLet) ->
      let newProgram =
        flip mapExp program <| \e ->
          if e.val.eid == sourceScopeEId then
            let sourceBody = expToLetBody e in
            if sourceScopeEId /= targetEId
              then wrapWithResidualLet sourceBody
              else insertPat_ (ident, boundExp) targetPatId (wrapWithResidualLet sourceBody)
          else if e.val.eid == targetEId then
            insertPat_ (ident, boundExp) targetPatId e
          else
            e
      in
      -- let _ = Debug.log ("old: \n" ++ unparseWithIds program) () in
      -- let _ = Debug.log ("new: \n" ++ unparseWithIds newProgram) () in
      let oldScopeBody = justFindExpByEId program sourceScopeEId |> expToLetBody in
      let newScope     = justFindExpByEId newProgram targetEId in
      let newScopeBody = expToLetBody newScope in
      let isSafe =
        let identUsesSafe =
          -- Effectively comparing EIds of all uses of the identifier.
          -- Presumably these will be exactly equal rather than equal as sets since we shouldn't be moving around the relative position of the variable usages...still, I think a set is the natural strucuture here
          Utils.equalAsSets (identifierUses ident oldScopeBody) (identifierUses ident newScopeBody)
        in
        let boundExpVarsSafe =
          freeVars boundExp
          |> List.all (\e -> bindingScopeIdFor e program == bindingScopeIdFor e newProgram)
        in
        let noDuplicateNamesInPat =
          let namesDefinedAtNewScope = identifiersListInPat (expToLetPat newScope) in
          namesDefinedAtNewScope == Utils.dedup namesDefinedAtNewScope
        in
        identUsesSafe && boundExpVarsSafe && noDuplicateNamesInPat
      in
      let caption =
        strUnsafeBool (not isSafe) ++ " Move " ++ ident ++ " to make " ++ (unparsePat >> Utils.squish >> Utils.niceTruncateString 20 "...") (expToLetPat newScope)
      in
      let result =
        SynthesisResult { description = caption, exp = newProgram, sortKey = [], children = Nothing }
      in
      if isSafe
      then ([result], [])
      else ([], [result])


-- TODO: Update for ECase/EFun
-- TODO: Update for nested patterns
insertPat_ (ident, boundExp) ((_, targetBranchI), targetPath) exp =
  case exp.val.e__ of
    ELet ws1 letKind rec p e1 e2 ws2 ->
      let maybeNewP_E__Pair =
        case (p.val, e1.val.e__, targetPath) of
          (PVar pws1 x _, _, [i]) ->
            Just ( PList pws1                      (Utils.inserti i (pVar ident) [p] |> cleanupPatListWhitespace " ") "" Nothing ""
                 , EList (precedingWhitespace e1)  (Utils.inserti i (ensureWhitespaceExp boundExp) [e1]) "" Nothing "" )

          ( PList pws1 ps pws2 Nothing pws3
          , EList ews1 es ews2 Nothing ews3
          , [i]
          ) ->
            Just ( PList pws1 (Utils.inserti i (pVar ident) ps |> ensureWhitespacePats) pws2 Nothing pws3
                 , EList ews1 (Utils.inserti i boundExp es     |> ensureWhitespaceExps) ews2 Nothing ews3 )

          _ ->
            Nothing
      in
      case maybeNewP_E__Pair of
        Just (newP_, newE__) ->
            replaceE__ exp (ELet ws1 letKind rec (replaceP_ p newP_) (replaceE__ e1 newE__) e2 ws2)

        Nothing ->
          let _ = Debug.log "insertPat_: pattern, path " (p.val, targetPath) in
          exp

    _ ->
      let _ = Debug.log "insertPat_: not ELet" exp.val.e__ in
      exp


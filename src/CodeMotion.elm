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

-- TODO: Update for ECase
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
  let ((sourceScopeEId, sourceScopeBranchI), _) = sourcePatId in
  case pluck sourcePatId program of
    Nothing -> Debug.crash <| "could not pluck pat " ++ toString sourcePatId ++ " from " ++ unparseWithIds program
    Just ((ident, boundExp), wrapWithResidualLet) ->
      -- let expToWrapWithMovedIdent = justFindExpByEId program targetEId in
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

      --           ELet (precedingWhitespace e) (expToLetKind e) False
      --             (pVar xPlucked) (ensureWhitespaceExp ePlucked)
      --             e ""

  -- let sourceScope = Tuple.first sourcePat in
  -- let ((xPlucked, ePlucked), residualLetExp) =
  --   Utils.fromJust_ "moveDefUpBeforeLet" (pluck sourcePat exp) in
  --
  -- let move upDown prefix =
  --   let caption =
  --     let
  --       x = DependenceGraph.lookupIdent sourcePat scopeGraph
  --       y = DependenceGraph.lookupIdent (targetScope, []) scopeGraph
  --     in
  --     prefix ++ Utils.spaces ["move", x, upDown, "before", y]
  --   in
  --   let newExp =
  --     flip mapExp exp <| \e ->   -- this used to be called insertLet
  --       if e.val.eid == sourceScope then
  --         let e2_ = expToLetBody e in
  --         residualLetExp e2_
  --       else if e.val.eid == targetScope then
  --         withDummyPos <|        -- this used to be called insertLet_
  --           ELet (precedingWhitespace e) (expToLetKind e) False
  --             (pVar xPlucked) (ensureWhitespaceExp ePlucked)
  --             e ""
  --       else
  --         e
  --   in
  --   [ SynthesisResult { description = caption, exp = newExp, sortKey = [], children = Nothing } ]
  -- in
  --
  -- let shadowing =
  --   DependenceGraph.checkVisible scopeGraph xPlucked sourcePat targetScope in
  --
  -- let moveUp () =
  --   let unsafe =
  --     strUnsafeBool <|
  --       DependenceGraph.patternTransitivelyDependsOnScope scopeGraph
  --         sourcePat targetScope
  --   in
  --   move "up" (shadowing ++ unsafe)
  -- in
  --
  -- case DependenceGraph.scopeOrder scopeGraph sourceScope targetScope of
  --
  --   SameScope ->
  --     []
  --
  --   ChildScope ->
  --     moveUp ()
  --
  --   NearestCommonAncestor commonScope ->
  --     let parentOfTargetScope = parentScopeOf targetScope scopeGraph in
  --     case DependenceGraph.scopeOrder scopeGraph commonScope parentOfTargetScope of
  --       SameScope  -> moveUp ()
  --       _          -> []
  --         -- ChildScope case is handled by the outer ChildScope case
  --
  --   ParentScope ->
  --     let unsafe =
  --       strUnsafeBool <|
  --         DependenceGraph.usedOnTheWayDownTo scopeGraph
  --           sourcePat targetScope False
  --     in
  --     move "down" (shadowing ++ unsafe)


------------------------------------------------------------------------------

moveDefinitionPat : PatternId -> PatTargetPosition -> Exp -> List SynthesisResult
moveDefinitionPat sourcePat targetPos program =
  []
--   let sourceScope = Tuple.first sourcePat in
--   let targetScope = Tuple.first (Tuple.second targetPos) in
--   let pluckedStuff = Utils.fromJust_ "moveDefPat_" <|
--     pluck sourcePat program
--   in
--
--   let shadowing =
--     -- TODO checkVisible for PLists
--     let targetId = Tuple.first (Tuple.second targetPos) in
--     DependenceGraph.checkVisible scopeGraph
--        (Tuple.first (Tuple.first pluckedStuff)) sourcePat targetId
--   in
--
--   case DependenceGraph.scopeOrder scopeGraph sourceScope targetScope of
--
--     ChildScope ->
--       let unsafe =
--         strUnsafeBool <|
--           DependenceGraph.patternTransitivelyDependsOnScope scopeGraph
--             sourcePat targetScope
--       in
--       moveDefinitionPat_ scopeGraph sourcePat targetPos program
--          pluckedStuff "up" (shadowing ++ unsafe)
--
--     ParentScope ->
--       let unsafe =
--         strUnsafeBool <|
--           DependenceGraph.usedOnTheWayDownTo scopeGraph
--             sourcePat targetScope True
--       in
--       moveDefinitionPat_ scopeGraph sourcePat targetPos program
--          pluckedStuff "down" (shadowing ++ unsafe)
--
--     SameScope ->
--       let unsafe = strUnsafeBool False in
--       moveDefinitionPat_ scopeGraph sourcePat targetPos program
--          pluckedStuff "over" (shadowing ++ unsafe)
--
--     NearestCommonAncestor _ ->
--       []
--
-- moveDefinitionPat_ scopeGraph sourcePat targetPos program pluckedStuff upDownOver prefix =
--   let caption =
--     captionMoveDefinitionBeforeAfterPat scopeGraph sourcePat targetPos upDownOver prefix
--   in
--   let newProgram =
--     insertPat (Tuple.first sourcePat) pluckedStuff targetPos program
--   in
--   [ SynthesisResult { description = caption, exp = newProgram, sortKey = [], children = Nothing } ]

-- TODO: update for EFun/ECase
insertPat : ScopeId -> (VarEquation, RebuildLetExp) -> PatTargetPosition -> Exp -> Exp
insertPat sourceId (pluckedEquation, wrapWithResidualLet) patTargetPosition =
  let (beforeAfter, ((targetEId, targetBranchI), targetPath)) = patTargetPosition in
  let (sourceEId, sourceBranchI) = sourceId in
  mapExp <| \e ->
    if e.val.eid == sourceEId then
      let e2_ = expToLetBody e in
      if sourceEId /= targetEId
        then wrapWithResidualLet e2_
        else insertPat_ pluckedEquation patTargetPosition (wrapWithResidualLet e2_)
    else if e.val.eid == targetEId then
      insertPat_ pluckedEquation patTargetPosition e
    else
      e

insertPat_ (xMove, eMove) (beforeAfter, ((targetEId, targetBranchI), targetPath)) e =
  case e.val.e__ of
    ELet ws1 letKind rec p e1 e2 ws2 ->
      case (p.val, e1.val.e__, targetPath) of
        (PVar _ x _, _, []) ->
          let j = if beforeAfter == 0 then 0 else 1 in
          let ps = List.take j [p]
                     ++ [pVar xMove]
                     ++ List.drop j [p] in
          let es = List.take j [e1]
                     ++ [ensureWhitespaceExp eMove]
                     ++ List.drop j [e1] in
          let p_ = replaceP_ p (PList " " ps "" Nothing "") in
          let e1_ = replaceE__ e1 (EList " " es "" Nothing "") in
          replaceE__ e (ELet ws1 letKind rec p_ e1_ e2 ws2)

        ( PList pws1 ps pws2 Nothing pws3
        , EList ews1 es ews2 Nothing ews3
        , [i]
        ) ->
          let j = if beforeAfter == 0 then i - 1 else i in
          let ps_ =
            List.take j ps
              ++ [pVar xMove]
              ++ ensureWhitespacePats (List.drop j ps) in
          let es_ =
            List.take j es
              ++ [ensureWhitespaceExp eMove]
              ++ ensureWhitespaceExps (List.drop j es) in
          let p_  = replaceP_ p (PList pws1 ps_ pws2 Nothing pws3) in
          let e1_ = replaceE__ e1 (EList ews1 es_ ews2 Nothing ews3) in
          replaceE__ e (ELet ws1 letKind rec p_ e1_ e2 ws2)

        _ ->
          let _ = Debug.log "insertPat_: pattern" p.val in
          e

    _ ->
      let _ = Debug.log "insertPat_: not ELet" e.val.e__ in
      e

-- captionMoveDefinitionBeforeAfterPat scopeGraph sourcePat targetPos upDownOver prefix =
--   let caption =
--     let
--       x = DependenceGraph.lookupIdent sourcePat scopeGraph
--       y = DependenceGraph.lookupIdent (Tuple.second targetPos) scopeGraph
--       beforeAfter = if Tuple.first targetPos == 0 then "before" else "after"
--     in
--     Utils.spaces ["move", x, upDownOver, beforeAfter, y]
--   in
--   prefix ++ caption

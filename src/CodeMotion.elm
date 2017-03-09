module CodeMotion exposing
  ( moveDefinitionPat, moveDefinitionBeforeEId
  , makeEListReorderTool
  )

import Lang exposing (..)
import LangTools exposing (..)
import LangUnparser exposing (unparse, unparseWithIds, unparsePat)
import LangParser2
-- import DependenceGraph exposing
  -- (ScopeGraph, ScopeOrder(..), parentScopeOf, childScopesOf)
import InterfaceModel exposing (Model, SynthesisResult(..), NamedDeuceTool)
import Utils exposing (MaybeOne)

import Dict


type alias PatBoundExp = (Pat, Exp)
type alias RebuildLetExp = Exp -> Exp


-- returns the bindings (pi, e1i) to be removed from let-expression at scopeId
-- and a function foo that rewrites this let-expression by wrapping the
-- residual pattern and residual bound expression (that remain after plucking)
-- around a rewritten body (provided as an argument)
pluck : ScopeId -> List (List Int) -> Exp -> (List PatBoundExp, RebuildLetExp)
pluck (scopeEId, scopeBranchI) paths program =
  case findExpByEId program scopeEId of
    Just scopeExp -> pluck_ scopeExp paths
    Nothing       -> ([], (toString >> Debug.crash))


pluck_ : Exp -> List (List Int) -> (List PatBoundExp, RebuildLetExp)
pluck_ scopeExp paths =
  let ((pluckedPatBoundExps, maybeNewPatAndBoundExp), ws1, letKind, isRec, ws2) =
    case scopeExp.val.e__ of
      ELet ws1 letKind False p e1 e2 ws2 -> (pluck__ p e1 paths, ws1, letKind, False, ws2)
      _                                  -> Debug.crash <| "pluck_: bad Exp__ (note: letrec not supported) " ++ unparseWithIds scopeExp
  in
  let wrapWithResidualLet =
    case maybeNewPatAndBoundExp of
      Nothing ->
        identity

      Just (newPat, newBoundExp) ->
        (\newBody -> replaceE__ scopeExp (ELet ws1 letKind isRec newPat newBoundExp newBody ws2))
  in
  (pluckedPatBoundExps, wrapWithResidualLet)


-- A little messy because we want to safely pluck out multiple elements
-- (removing them in a safe order so that we pluck out the right elements)
pluck__ : Pat -> Exp -> List (List Int) -> (List PatBoundExp, Maybe (Pat, Exp))
pluck__ p e1 paths =
  let endRecursionUnchanged = ([], Just (p, e1)) in
  if paths == [] then
    -- Fast terminate recursion
    endRecursionUnchanged
  else if List.any ((==) []) paths then
    ([(p, e1)], Nothing)
  else
    case (p.val, e1.val.e__) of
      (PAs _ _ _ childPat, _) ->
        -- TODO: allow but mark unsafe if as-pattern is used
        let _ = Debug.log "can't pluck out of as-pattern yet (unsafe)" in
        endRecursionUnchanged

      ( PList pws1 ps pws2 maybePTail pws3
      , EList ews1 es ews2 maybeETail ews3
      ) ->
        let (maxI, maybeTailI) =
          if List.length ps == List.length es && Utils.maybeToBool maybePTail && Utils.maybeToBool maybeETail then
            (List.length ps + 1, Just (List.length ps + 1))
          else
            (min (List.length ps) (List.length es), Nothing)
        in
        let (pluckedPatBoundExps, newPs, newEs, newMaybeETail, newMaybePTail) =
          -- find/remove right to left (so removal is safe)
          (List.range 1 maxI)
          |> List.foldr
              (\i (patBoundExps, ps, es, maybePTail, maybeETail) ->
                let nextPaths =
                  paths
                  |> List.filter (\path -> Utils.head "pluck__ path should not be empty here" path == i)
                  |> List.map (Utils.tail "pluck__ path should not be empty here")
                in
                if maybeTailI == Just i then
                  -- Recursing into the tail binding
                  let pi = Utils.fromJust maybePTail in
                  let ei = Utils.fromJust maybeETail in
                  let (plucked, maybeNewPatAndBoundExp) = pluck__ pi ei nextPaths in
                  case maybeNewPatAndBoundExp of
                    Just (newTailPat, newTailBoundExp) -> (plucked ++ patBoundExps, ps, es, Just newTailPat, Just newTailBoundExp)
                    Nothing                            -> (plucked ++ patBoundExps, ps, es, Nothing, Nothing)
                else
                  let pi = Utils.geti i ps in
                  let ei = Utils.geti i es in
                  let (plucked, maybeNewPatAndBoundExp) = pluck__ pi ei nextPaths in
                  case maybeNewPatAndBoundExp of
                    Just (newPat, newBoundExp) -> (plucked ++ patBoundExps, Utils.replacei i newPat ps, Utils.replacei i newBoundExp es, maybePTail, maybeETail)
                    Nothing                    -> (plucked ++ patBoundExps, Utils.removei i ps, Utils.removei i es, maybePTail, maybeETail)
              )
              ([], ps, es, maybePTail, maybeETail)
        in
        case (newPs, newEs, newMaybeETail, newMaybePTail) of
          ([], [], Nothing, Nothing) ->
            ( pluckedPatBoundExps
            , Nothing
            )

          ([newP], [newE], Nothing, Nothing) ->
            -- Delistify singleton lists
            ( pluckedPatBoundExps
            , Just (copyPrecedingWhitespacePat p newP,
                    copyPrecedingWhitespace e1 newE)
            )

          _ ->
            ( pluckedPatBoundExps
            , Just (replaceP_ p   (PList pws1 (newPs |> imitatePatListWhitespace ps) pws2 maybePTail pws3),
                    replaceE__ e1 (EList ews1 (newEs |> imitateExpListWhitespace es) ews2 maybeETail ews3))
            )

        -- if List.length ps >= i && List.length es >= i then
        --   let (pi, ei) = (Utils.geti i ps, Utils.geti i es) in
        --   pluck__ pi ei is
        --   |> Maybe.map
        --       (\(plucked, maybeNewPatAndBoundExp) ->
        --         let (newPs, newEs) =
        --           case maybeNewPatAndBoundExp of
        --             Just (newPat, newBoundExp) ->
        --               ( Utils.replacei i newPat ps      |> imitatePatListWhitespace ps
        --               , Utils.replacei i newBoundExp es |> imitateExpListWhitespace es
        --               )
        --
        --             Nothing ->
        --               ( Utils.removei i ps |> imitatePatListWhitespace ps
        --               , Utils.removei i es |> imitateExpListWhitespace es
        --               )
        --         in
        --         case (newPs, newEs, maybePTail, maybeETail) of
        --           ([], [], Nothing, Nothing) ->
        --             ( plucked
        --             , Nothing
        --             )
        --
        --           ([newP], [newE], Nothing, Nothing) ->
        --             -- Delistify singleton lists
        --             ( plucked
        --             , Just (copyPrecedingWhitespacePat p newP,
        --                     copyPrecedingWhitespace e1 newE)
        --             )
        --
        --           _ ->
        --             ( plucked
        --             , Just (replaceP_ p   (PList pws1 newPs pws2 maybePTail pws3),
        --                     replaceE__ e1 (EList ews1 newEs ews2 maybeETail ews3))
        --             )
        --
        --       )
        -- else if List.length ps == List.length es && i == 1 + List.length ps && Utils.maybeToBool maybePTail && Utils.maybeToBool maybeETail then
        --   -- Recursing into the tail binding
        --   let pi = Utils.fromJust maybePTail in
        --   let ei = Utils.fromJust maybeETail in
        --   pluck__ pi ei is
        --   |> Maybe.map
        --       (\(plucked, maybeNewPatAndBoundExp) ->
        --         case maybeNewPatAndBoundExp of
        --           Just (newTailPat, newTailBoundExp) ->
        --             ( plucked
        --             , Just (replaceP_ p   (PList pws1 ps pws2 (Just newTailPat) pws3),
        --                     replaceE__ e1 (EList ews1 es ews2 (Just newTailBoundExp) ews3))
        --             )
        --
        --           Nothing ->
        --             ( plucked
        --             , Just (replaceP_ p   (PList pws1 ps pws2 Nothing pws3),
        --                     replaceE__ e1 (EList ews1 es ews2 Nothing ews3))
        --             )
        --       )
        -- else
        --   Debug.log "pluck index longer than head list of PList or EList" Nothing

      _ ->
        let _ = Debug.log ("pluck_: bad pattern " ++ unparsePat p) paths in
        endRecursionUnchanged


------------------------------------------------------------------------------


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
  let ((sourceScopeEId, sourceScopeBranchI), sourcePath) = sourcePatId in
  case pluck (sourceScopeEId, sourceScopeBranchI) [sourcePath] program of
    ([(pluckedPat, boundExp)], wrapWithResidualLet) ->
      let insertedLetEId = LangParser2.maxId program + 1 in
      let newProgram =
        flip mapExp program <| \e ->
          -- Written oddly, but this correctly handles the cases
          -- where sourceScopeEId == targetEId; both when a subpattern is extracted
          -- right before the scope, and when you try to move an entire scope
          -- right before itself
          let newE =
            if e.val.eid == sourceScopeEId
            then wrapWithResidualLet (expToLetBody e)
            else e
          in
          if e.val.eid == targetEId then
            let letOrDef = if isTopLevelEId targetEId program then Def else Let in
            withDummyPosEId insertedLetEId <|
              ELet (precedingWhitespace newE) letOrDef False
                (ensureWhitespacePat pluckedPat) (ensureWhitespaceExp boundExp)
                (ensureWhitespaceExp newE) ""
          else
            newE
      in
      -- let _ = Debug.log ("old: \n" ++ unparseWithIds program) () in
      -- let _ = Debug.log ("new: \n" ++ unparseWithIds newProgram) () in
      let oldScopeBody   = justFindExpByEId program sourceScopeEId |> expToLetBody in
      let movedScopeBody = justFindExpByEId newProgram insertedLetEId |> expToLetBody in
      let isSafe =
        let identUsesSafe =
          -- Effectively comparing EIds of all uses of the identifiers.
          -- Presumably these will be exactly equal rather than equal as sets since we shouldn't be moving around the relative position of the variable usages...still, I think a set is the natural strucuture here
          let identSafe ident =
            Utils.equalAsSets (identifierUses ident oldScopeBody) (identifierUses ident movedScopeBody)
          in
          List.all identSafe (identifiersListInPat pluckedPat)
        in
        let boundExpVarsSafe =
          freeVars boundExp
          |> List.all (\e -> bindingScopeIdFor e program == bindingScopeIdFor e newProgram)
        in
        identUsesSafe && boundExpVarsSafe
      in
      let caption =
        strUnsafeBool (not isSafe) ++ "Move " ++ (unparsePat >> Utils.squish) pluckedPat ++ " before " ++ (unparse >> Utils.squish >> Utils.niceTruncateString 20 "...") movedScopeBody
      in
      let result =
        SynthesisResult { description = caption, exp = newProgram, sortKey = [], children = Nothing }
      in
      if isSafe
      then ([result], [])
      else ([], [result])

    _ ->
      Debug.crash <| "could not pluck pat " ++ toString sourcePatId ++ " from " ++ unparseWithIds program


------------------------------------------------------------------------------


-- Returns (list of safe results, list of unsafe results)
moveDefinitionPat : PatternId -> PatternId -> Exp -> (List SynthesisResult, List SynthesisResult)
moveDefinitionPat sourcePatId targetPatId program =
  let ((sourceScopeEId, sourceScopeBranchI), sourcePath) = sourcePatId in
  let ((targetEId, _), _) = targetPatId in
  case pluck (sourceScopeEId, sourceScopeBranchI) [sourcePath] program of
    ([(pluckedPat, boundExp)], wrapWithResidualLet) ->
      let newProgram =
        flip mapExp program <| \e ->
          if e.val.eid == sourceScopeEId then
            let sourceBody = expToLetBody e in
            if sourceScopeEId /= targetEId
              then wrapWithResidualLet sourceBody
              else insertPat_ (pluckedPat, boundExp) targetPatId (wrapWithResidualLet sourceBody)
          else if e.val.eid == targetEId then
            insertPat_ (pluckedPat, boundExp) targetPatId e
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
          let identSafe ident =
            Utils.equalAsSets (identifierUses ident oldScopeBody) (identifierUses ident newScopeBody)
          in
          List.all identSafe (identifiersListInPat pluckedPat)
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
        strUnsafeBool (not isSafe) ++ "Move " ++ (unparsePat >> Utils.squish) pluckedPat ++ " to make " ++ (unparsePat >> Utils.squish >> Utils.niceTruncateString 20 "...") (expToLetPat newScope)
      in
      let result =
        SynthesisResult { description = caption, exp = newProgram, sortKey = [], children = Nothing }
      in
      if isSafe
      then ([result], [])
      else ([], [result])

    _ ->
      Debug.crash <| "could not pluck pat " ++ toString sourcePatId ++ " from " ++ unparseWithIds program


insertPat_ : PatBoundExp -> PatternId -> Exp -> Exp
insertPat_ (patToInsert, boundExp) ((_, targetBranchI), targetPath) exp =
  case exp.val.e__ of
    ELet ws1 letKind rec p e1 e2 ws2 ->
      case insertPat__ (patToInsert, boundExp) p e1 targetPath of
        Just (newPat, newBoundExp) ->
          replaceE__ exp (ELet ws1 letKind rec newPat newBoundExp e2 ws2)

        Nothing ->
          let _ = Debug.log "insertPat_: pattern, path " (p.val, targetPath) in
          exp

    _ ->
      let _ = Debug.log "insertPat_: not ELet" exp.val.e__ in
      exp


insertPat__ : PatBoundExp -> Pat -> Exp -> List Int -> Maybe (Pat, Exp)
insertPat__ (patToInsert, boundExp) p e1 path =
  let maybeNewP_E__Pair =
    case (p.val, e1.val.e__, path) of
      (PVar pws1 _ _, _, [i]) ->
        Just ( PList pws1                      (Utils.inserti i patToInsert [p] |> setPatListWhitespace "" " ") "" Nothing ""
             , EList (precedingWhitespace e1)  (Utils.inserti i boundExp [e1]   |> setExpListWhitespace "" " ") "" Nothing "" )

      (PAs pws1 _ _ _, _, [i]) ->
        Just ( PList pws1                      (Utils.inserti i patToInsert [p] |> setPatListWhitespace "" " ") "" Nothing ""
             , EList (precedingWhitespace e1)  (Utils.inserti i boundExp [e1]   |> setExpListWhitespace "" " ") "" Nothing "" )

      (PAs pws1 _ _ _, _, i::is) ->
        -- TODO: allow but mark unsafe if as-pattern is used
        let _ = Debug.log "can't insert into as-pattern yet (unsafe)" in
        Nothing

      ( PList pws1 ps pws2 maybePTail pws3
      , EList ews1 es ews2 maybeETail ews3
      , [i]
      ) ->
        if List.length ps + 1 >= i && List.length es + 1 >= i then
          Just ( PList pws1 (Utils.inserti i patToInsert ps |> imitatePatListWhitespace ps) pws2 Nothing pws3
               , EList ews1 (Utils.inserti i boundExp es    |> imitateExpListWhitespace es) ews2 Nothing ews3 )
        else
          let _ = Debug.log "can't insert into this list (note: cannot insert on list tail)" (unparsePat p, unparse e1, path) in
          Nothing

      ( PList pws1 ps pws2 maybePTail pws3
      , EList ews1 es ews2 maybeETail ews3
      , i::is
      ) ->
        if List.length ps >= i && List.length es >= i then
          let (pi, ei) = (Utils.geti i ps, Utils.geti i es) in
          insertPat__ (patToInsert, boundExp) pi ei is
          |> Maybe.map
              (\(newPat, newBoundExp) ->
                let (newPs, newEs) =
                  ( Utils.replacei i newPat ps      |> imitatePatListWhitespace ps
                  , Utils.replacei i newBoundExp es |> imitateExpListWhitespace es
                  )
                in
                (PList pws1 newPs pws2 maybePTail pws3,
                 EList ews1 newEs ews2 maybeETail ews3)

              )
        else if List.length ps == List.length es && i == 1 + List.length ps && Utils.maybeToBool maybePTail && Utils.maybeToBool maybeETail then
          -- Recursing into the tail binding
          let pi = Utils.fromJust maybePTail in
          let ei = Utils.fromJust maybeETail in
          insertPat__ (patToInsert, boundExp) pi ei is
          |> Maybe.map
              (\(newPat, newBoundExp) ->
                (PList pws1 ps pws2 (Just newPat) pws3,
                 EList ews1 es ews2 (Just newBoundExp) ews3)
              )
        else
          let _ = Debug.log "can't insert into this list (note: cannot insert on list tail)" (unparsePat p, unparse e1, path) in
          Nothing

      _ ->
        let _ = Debug.log "insertPat__: pattern, path " (p.val, path) in
        Nothing
  in
  case maybeNewP_E__Pair of
    Just (newP_, newE__) ->
      Just (replaceP_ p newP_, replaceE__ e1 newE__)

    Nothing ->
      Nothing


------------------------------------------------------------------------------

makeEListReorderTool
    : Model -> List EId -> ExpTargetPosition
   -> MaybeOne NamedDeuceTool
makeEListReorderTool m expIds expTarget =
  let maybeParentELists =
    List.map (findParentEList m.inputExp) (Tuple.second expTarget :: expIds)
  in
  case Utils.dedupByEquality maybeParentELists of

    -- check that a single EList is the parent of all selected expressions
    -- and of the selected expression target positions
    [(Just (eListId, (ws1, listExps, ws2, Nothing, ws3)), True)] ->

      let (plucked, prefix, maybeSuffix) =
        List.foldl (\listExp_i (plucked, prefix, maybeSuffix) ->

          if listExp_i.val.eid == Tuple.second expTarget then
            case (maybeSuffix, Tuple.first expTarget) of
              (Nothing, Before) ->
                if List.member listExp_i.val.eid expIds
                  then (plucked ++ [listExp_i], prefix, Just [])
                  else (plucked, prefix, Just [listExp_i])
              (Nothing, After) ->
                if List.member listExp_i.val.eid expIds
                  then (plucked ++ [listExp_i], prefix, Just [])
                  else (plucked, prefix ++ [listExp_i], Just [])
              (Just _, _) ->
                Debug.crash "reorder fail ..."

          else if List.member listExp_i.val.eid expIds then
            (plucked ++ [listExp_i], prefix, maybeSuffix)

          else
            case maybeSuffix of
              Nothing     -> (plucked, prefix ++ [listExp_i], maybeSuffix)
              Just suffix -> (plucked, prefix, Just (suffix ++ [listExp_i]))

          ) ([], [], Nothing) listExps
      in
      let reorderedListExps =
        case maybeSuffix of
          Nothing     -> Debug.crash "reorder fail ..."
          Just suffix -> prefix ++ plucked ++ suffix
      in
      let reorderedEList =
        withDummyPos <|
          EList ws1 (imitateExpListWhitespace listExps reorderedListExps)
                ws2 Nothing ws3
      in
      let newExp =
        replaceExpNode eListId reorderedEList m.inputExp
      in
      [ ("Reorder List", \() -> InterfaceModel.oneSafeResult newExp) ]

    _ -> []

findParentEList exp eid =
  let foo e (mostRecentEList, foundExp) =
    if foundExp then (mostRecentEList, True)
    else if e.val.eid == eid then (mostRecentEList, True)
    else
      case e.val.e__ of
        EList ws1 listExps ws2 listRest ws3 ->
          (Just (e.val.eid, (ws1, listExps, ws2, listRest, ws3)), False)
        _ ->
          (mostRecentEList, foundExp)
  in
  foldExp foo (Nothing, False) exp

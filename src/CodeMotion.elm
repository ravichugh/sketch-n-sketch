module CodeMotion exposing
  ( moveDefinitionPat, moveDefinitionsBeforeEId
  , makeEListReorderTool
  )

import Lang exposing (..)
import LangTools exposing (..)
import LangTransform
import LangUnparser exposing (unparse, unparseWithIds, unparsePat)
import LangParser2
-- import DependenceGraph exposing
  -- (ScopeGraph, ScopeOrder(..), parentScopeOf, childScopesOf)
import InterfaceModel exposing (Model, SynthesisResult(..), synthesisResult, setResultSafe, NamedDeuceTool)
import Utils exposing (MaybeOne)

import Dict


type alias PatBoundExp = (Pat, Exp)

-- Removes the binding (p, e1) from the program.
--
-- If this would remove a whole pattern, the pattern is left as [] matched to [] for a later clean up step.
--
-- This technique preserves EIds and pattern paths for later insertion.
pluck : PatternId -> Exp -> Maybe (PatBoundExp, Exp)
pluck ((scopeEId, scopeBranchI), path) program =
  findExpByEId program scopeEId
  |> Maybe.andThen (\scope -> pluck_ scope path program)


pluck_ : Exp -> List Int -> Exp -> Maybe (PatBoundExp, Exp)
pluck_ scopeExp path program =
  let (maybePluckedAndNewPatAndBoundExp, ws1, letKind, isRec, e2, ws2) =
    case scopeExp.val.e__ of
      ELet ws1 letKind False p e1 e2 ws2 -> (pluck__ p e1 path, ws1, letKind, False, e2, ws2)
      _                                  -> Debug.crash <| "pluck_: bad Exp__ (note: letrec not supported) " ++ unparseWithIds scopeExp
  in
  case maybePluckedAndNewPatAndBoundExp of
    Nothing ->
      Nothing

    Just (pluckedPatBoundExp, newPat, newBoundExp) ->
      Just <|
        ( pluckedPatBoundExp
        , replaceExpNodeE__ scopeExp (ELet ws1 letKind isRec newPat newBoundExp e2 ws2) program
        )


pluck__ : Pat -> Exp -> List Int -> Maybe (PatBoundExp, Pat, Exp)
pluck__ p e1 path =
  case (p.val, e1.val.e__, path) of
    (_, _, []) ->
      Just <|
        ( (p, e1)
        , replaceP_ p   <| PList (precedingWhitespacePat p) [] "" Nothing ""
        , replaceE__ e1 <| EList (precedingWhitespace e1)   [] "" Nothing ""
        )

    (PAs _ _ _ childPat, _, i::is) ->
      -- TODO: allow but mark unsafe if as-pattern is used
      let _ = Debug.log "can't pluck out of as-pattern yet (unsafe)" in
      Nothing

    ( PList pws1 ps pws2 maybePTail pws3
    , EList ews1 es ews2 maybeETail ews3
    , i::is
    ) ->
      if List.length ps >= i && List.length es >= i then
        let pi = Utils.geti i ps in
        let ei = Utils.geti i es in
        pluck__ pi ei is
        |> Maybe.map
            (\(plucked, newPat, newBoundExp) ->
              let (newPs, newEs) =
                ( Utils.replacei i newPat ps
                , Utils.replacei i newBoundExp es
                )
              in
              ( plucked
              , replaceP_ p   <| PList pws1 newPs pws2 maybePTail pws3
              , replaceE__ e1 <| EList ews1 newEs ews2 maybeETail ews3
              )
            )
      else if List.length ps == List.length es && i == 1 + List.length ps && Utils.maybeToBool maybePTail && Utils.maybeToBool maybeETail then
        -- Recursing into the tail binding
        let pi = Utils.fromJust maybePTail in
        let ei = Utils.fromJust maybeETail in
        pluck__ pi ei is
        |> Maybe.map
            (\(plucked, newTailPat, newTailBoundExp) ->
              ( plucked
              , replaceP_ p   <| PList pws1 ps pws2 (Just newTailPat) pws3
              , replaceE__ e1 <| EList ews1 es ews2 (Just newTailBoundExp) ews3
              )
            )
      else
        Debug.log "pluck index longer than head list of PList or EList" Nothing

    _ ->
      let _ = Debug.log ("pluck_: bad pattern " ++ unparsePat p) path in
      Nothing


------------------------------------------------------------------------------


-- Moving a definition is safe if all identifiers resolve to the same bindings.
--
-- More specifically:
--   - All free variables in the moved assignment still resolve to the same bindings
--   - All previous references to the moved identifier still resolve to that identifer
--   - All other variables uses of the same name do not resolve to the moved identifier
--
-- Returns (list of safe results, list of unsafe results)
moveDefinitionsBeforeEId : List PatternId -> EId -> Exp -> List SynthesisResult
moveDefinitionsBeforeEId sourcePatIds targetEId program =
  let (pluckedPatAndBoundExpAndOldScopeBodies, programWithoutPlucked) =
    sourcePatIds
    |> List.sortBy
        (\((scopeEId, branchI), path) ->
          let scope = justFindExpByEId program scopeEId in
          (scope.start.line, scope.start.col, branchI, path)
        )
    |> List.foldr
        (\sourcePatId (pluckedPatAndBoundExpAndOldScopeBodies, programBeingPlucked) ->
          case pluck sourcePatId programBeingPlucked of
            Just ((pluckedPat, pluckedBoundExp), programWithoutPlucked) ->
              let ((sourceScopeEId, _), _) = sourcePatId in
              let oldScopeBody = justFindExpByEId program sourceScopeEId |> expToLetBody in -- search in original program b/c old body is for safety check
              ((pluckedPat, pluckedBoundExp, oldScopeBody)::pluckedPatAndBoundExpAndOldScopeBodies, programWithoutPlucked)
            Nothing ->
              (pluckedPatAndBoundExpAndOldScopeBodies, programBeingPlucked)
        )
        ([], program)
  in
  if pluckedPatAndBoundExpAndOldScopeBodies == [] then
    Debug.log "could not pluck anything" []
  else
    let (pluckedPats, pluckedBoundExps, _) = Utils.unzip3 pluckedPatAndBoundExpAndOldScopeBodies in
    let (newPat, newBoundExp) =
      case (pluckedPats, pluckedBoundExps) of
        ([pluckedPat], [boundExp]) ->
          (pluckedPat, boundExp)

        _ ->
          ( withDummyRange <| PList " " (pluckedPats      |> setPatListWhitespace "" " ") "" Nothing ""
          , withDummyPos   <| EList " " (pluckedBoundExps |> setExpListWhitespace "" " ") "" Nothing "" -- May want to be smarter about whitespace here to avoid long lines.
          )
    in
    let insertedLetEId = LangParser2.maxId program + 1 in
    let newProgram =
      programWithoutPlucked
      |> mapExpNode
          targetEId
          (\expToWrap ->
            let letOrDef = if isTopLevelEId targetEId programWithoutPlucked then Def else Let in
            withDummyPosEId insertedLetEId <|
              ELet (precedingWhitespace expToWrap) letOrDef False
                (ensureWhitespacePat newPat) (ensureWhitespaceExp newBoundExp)
                (ensureWhitespaceExp expToWrap) ""
          )
      |> LangTransform.simplifyAssignments
    in
    -- let _ = Debug.log ("old: \n" ++ unparseWithIds program) () in
    -- let _ = Debug.log ("new: \n" ++ unparseWithIds newProgram) () in
    let newScopeBody = justFindExpByEId newProgram insertedLetEId |> expToLetBody in
    let isSafe =
      let identUsesSafe =
        -- Effectively comparing EIds of all uses of the identifiers.
        -- Presumably these will be exactly equal rather than equal as sets since we shouldn't be moving around the relative position of the variable usages...still, I think a set is the natural strucuture here
        let identSafe oldScopeBody ident =
          Utils.equalAsSets (identifierUses ident oldScopeBody) (identifierUses ident newScopeBody)
        in
        pluckedPatAndBoundExpAndOldScopeBodies
        |> List.all
            (\(pluckedPat, _, oldScopeBody) ->
              identifiersListInPat pluckedPat |> List.all (identSafe oldScopeBody)
            )
      in
      let boundExpVarsSafe =
        freeVars newBoundExp
        |> List.all (\e -> bindingScopeIdFor e program == bindingScopeIdFor e newProgram)
      in
      let noDuplicateNamesInPat =
        let namesDefinedAtNewScope = identifiersListInPat newPat in
        namesDefinedAtNewScope == Utils.dedup namesDefinedAtNewScope
      in
      identUsesSafe && boundExpVarsSafe && noDuplicateNamesInPat
    in
    let caption =
      "Move " ++ (List.map (unparsePat >> Utils.squish) pluckedPats |> Utils.toSentence) ++ " before " ++ (unparse >> Utils.squish >> Utils.niceTruncateString 20 "...") newScopeBody
    in
    let result =
      synthesisResult caption newProgram |> setResultSafe isSafe
    in
    [result]


------------------------------------------------------------------------------


-- Returns (list of safe results, list of unsafe results)
moveDefinitionPat : List PatternId -> PatternId -> Exp -> List SynthesisResult
moveDefinitionPat sourcePatIds targetPatId program =
  let (pluckedPatAndBoundExpAndOldScopeBodies, programWithoutPlucked) =
    sourcePatIds
    |> List.sortBy
        (\((scopeEId, branchI), path) ->
          let scope = justFindExpByEId program scopeEId in
          (scope.start.line, scope.start.col, branchI, path)
        )
    |> List.foldr
        (\sourcePatId (pluckedPatAndBoundExpAndOldScopeBodies, programBeingPlucked) ->
          case pluck sourcePatId programBeingPlucked of
            Just ((pluckedPat, pluckedBoundExp), programWithoutPlucked) ->
              let ((sourceScopeEId, _), _) = sourcePatId in
              let oldScopeBody = justFindExpByEId program sourceScopeEId |> expToLetBody in -- search in original program b/c old body is for safety check
              ((pluckedPat, pluckedBoundExp, oldScopeBody)::pluckedPatAndBoundExpAndOldScopeBodies, programWithoutPlucked)
            Nothing ->
              (pluckedPatAndBoundExpAndOldScopeBodies, programBeingPlucked)
        )
        ([], program)
  in
  if pluckedPatAndBoundExpAndOldScopeBodies == [] then
    Debug.log "could not pluck anything" []
  else
    let ((targetEId, _), targetPath) = targetPatId in
    let newProgram =
      programWithoutPlucked
      |> mapExpNode
          targetEId
          (\newScopeExp ->
            pluckedPatAndBoundExpAndOldScopeBodies
            |> List.foldr
                (\(pluckedPat, pluckedBoundExp, _) newScopeExp ->
                  insertPat_ (pluckedPat, pluckedBoundExp) targetPath newScopeExp
                )
                newScopeExp
          )
      |> LangTransform.simplifyAssignments
    in
    -- let _ = Debug.log ("old: \n" ++ unparseWithIds program) () in
    -- let _ = Debug.log ("new: \n" ++ unparseWithIds newProgram) () in
    let newScope       = justFindExpByEId newProgram targetEId in
    let newScopeLetPat = expToLetPat newScope in
    let newScopeBody   = expToLetBody newScope in
    let isSafe =
      let identUsesSafe =
        -- Effectively comparing EIds of all uses of the identifiers.
        -- Presumably these will be exactly equal rather than equal as sets since we shouldn't be moving around the relative position of the variable usages...still, I think a set is the natural strucuture here
        let identSafe oldScopeBody ident =
          Utils.equalAsSets (identifierUses ident oldScopeBody) (identifierUses ident newScopeBody)
        in
        pluckedPatAndBoundExpAndOldScopeBodies
        |> List.all
            (\(pluckedPat, _, oldScopeBody) ->
              identifiersListInPat pluckedPat |> List.all (identSafe oldScopeBody)
            )
      in
      let boundExpVarsSafe =
        pluckedPatAndBoundExpAndOldScopeBodies
        |> List.all
            (\(_, pluckedBoundExp, _) ->
              freeVars pluckedBoundExp
              |> List.all (\e -> bindingScopeIdFor e program == bindingScopeIdFor e newProgram)
            )
      in
      let noDuplicateNamesInPat =
        let namesDefinedAtNewScope = identifiersListInPat newScopeLetPat in
        namesDefinedAtNewScope == Utils.dedup namesDefinedAtNewScope
      in
      identUsesSafe && boundExpVarsSafe && noDuplicateNamesInPat
    in
    let caption =
      let (pluckedPats, _, _) = Utils.unzip3 pluckedPatAndBoundExpAndOldScopeBodies in
      "Move " ++ (List.map (unparsePat >> Utils.squish) pluckedPats |> Utils.toSentence) ++ " to make " ++ (unparsePat >> Utils.squish >> Utils.niceTruncateString 20 "...") newScopeLetPat
    in
    let result =
      synthesisResult caption newProgram |> setResultSafe isSafe
    in
    [result]


insertPat_ : PatBoundExp -> List Int -> Exp -> Exp
insertPat_ (patToInsert, boundExp) targetPath exp =
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
      Just (replaceP_ p newP_, replaceE__ e1 newE__) -- Hmm this will produce duplicate EIds in the boundExp when PVar or PAs are expanded into PList

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
      [ ("Reorder List", \() -> [synthesisResult "Reorder List" newExp]) ]

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

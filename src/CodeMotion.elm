module CodeMotion exposing
  ( moveDefinitionPat, moveDefinitionsBeforeEId
  , makeEListReorderTool
  , makeIntroduceVarTool
  , makeMakeEqualTool
  )

import Lang exposing (..)
import LangTools exposing (..)
import LangTransform
import LangUnparser exposing (unparse, unparseWithIds, unparseWithUniformWhitespace, unparsePat)
import LangParser2
-- import DependenceGraph exposing
  -- (ScopeGraph, ScopeOrder(..), parentScopeOf, childScopesOf)
import InterfaceModel exposing
  ( Model, SynthesisResult(..), NamedDeuceTool
  , synthesisResult, setResultSafe, oneSafeResult
  )
import Utils exposing (MaybeOne)

import Dict
import Set


getELet program eid =
  let exp = justFindExpByEId program eid in
  case exp.val.e__ of
    ELet ws1 letKind rec p1 e1 e2 ws2 -> (ws1, letKind, rec, p1, e1, e2, ws2)
    _                                 -> Debug.crash "getELet: shouldn't happen"


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
moveDefinitions_ : (List PatBoundExp -> Exp -> (Exp, EId)) -> List PatternId -> Exp -> List SynthesisResult
moveDefinitions_ makeNewProgram sourcePatIds program =
  let (programUniqueNames, uniqueNameToOldName) = assignUniqueNames program in
  let sortedSourcePatIds =
    sourcePatIds
    |> List.sortBy
        (\((scopeEId, branchI), path) ->
          let scope = justFindExpByEId programUniqueNames scopeEId in
          (scope.start.line, scope.start.col, branchI, path)
        )
  in
  let (pluckedPatAndBoundExpAndOldScopeEIds, programWithoutPlucked) =
    sortedSourcePatIds
    |> List.foldr
        (\sourcePatId (pluckedPatAndBoundExpAndOldScopeBodies, programBeingPlucked) ->
          case pluck sourcePatId programBeingPlucked of
            Just ((pluckedPat, pluckedBoundExp), programWithoutPlucked) ->
              let ((sourceScopeEId, _), _) = sourcePatId in
              ((pluckedPat, pluckedBoundExp, sourceScopeEId)::pluckedPatAndBoundExpAndOldScopeBodies, programWithoutPlucked)
            Nothing ->
              (pluckedPatAndBoundExpAndOldScopeBodies, programBeingPlucked)
        )
        ([], programUniqueNames)
  in
  if pluckedPatAndBoundExpAndOldScopeEIds == [] then
    Debug.log "could not pluck anything" []
  else
    let (pluckedPats, pluckedBoundExps, _)   = Utils.unzip3 pluckedPatAndBoundExpAndOldScopeEIds in
    let pluckedPatIdentifiersUnique          = Utils.unionAll <| List.map identifiersSetInPat pluckedPats in
    let pluckedBoundExpFreeIdentifiersUnique = Utils.unionAll <| List.map freeIdentifiers pluckedBoundExps in
    let (newProgramUniqueNames, newScopeEId) =
      makeNewProgram (Utils.zip pluckedPats pluckedBoundExps) programWithoutPlucked
    in
    let newPatUniqueNames = justFindExpByEId newProgramUniqueNames newScopeEId |> expToLetPat in
    let makeResult renamings newProgram =
      let newScopeExp  = justFindExpByEId newProgram newScopeEId in
      let newScopeBody = newScopeExp |> expToLetBody in
      let newPat       = newScopeExp |> expToLetPat in
      let uniqueNameToOldNameUsed =
        Dict.diff uniqueNameToOldName (List.map Utils.flip renamings |> Dict.fromList)
      in
      let isSafe =
        let identUsesSafe =
          -- Presumably these will be exactly equal rather than equal as sets since we shouldn't be moving around the relative position of the variable usages...still, I think a set is the natural strucuture here
          let identSafe oldScopeEId identUniqueName =
            let identInNewProgram = Utils.getWithDefault identUniqueName identUniqueName uniqueNameToOldNameUsed in
            let oldScopeBody = justFindExpByEId programUniqueNames oldScopeEId |> expToLetBody in
            Utils.equalAsSets (identifierUsageEIds identUniqueName oldScopeBody) (identifierUsageEIds identInNewProgram newScopeBody)
          in
          pluckedPatAndBoundExpAndOldScopeEIds
          |> List.all
              (\(pluckedPat, _, oldScopeEId) ->
                identifiersListInPat pluckedPat |> List.all (identSafe oldScopeEId)
              )
        in
        let boundExpVarsSafe =
          pluckedPatAndBoundExpAndOldScopeEIds
          |> List.all
              (\(_, pluckedBoundExp, _) ->
                let newPluckedBoundExp = renameIdentifiers uniqueNameToOldNameUsed pluckedBoundExp in
                Utils.zip (freeVars pluckedBoundExp) (freeVars newPluckedBoundExp)
                |> List.all (\(varUnique, varNew) -> bindingScopeIdFor varUnique programUniqueNames == bindingScopeIdFor varNew newProgram)
              )
        in
        let noDuplicateNamesInPat =
          let namesDefinedAtNewScope = identifiersListInPat newPat in
          namesDefinedAtNewScope == Utils.dedup namesDefinedAtNewScope
        in
        identUsesSafe && boundExpVarsSafe && noDuplicateNamesInPat
      in
      let caption =
        let renamingsStr =
          if not <| List.isEmpty renamings
          then " renaming " ++ (renamings |> List.map (\(oldName, newName) -> oldName ++ " to " ++ newName) |> Utils.toSentence)
          else ""
        in
        "Move "
        ++ (List.map (renameIdentifiersInPat uniqueNameToOldName >> unparsePat >> Utils.squish) pluckedPats |> Utils.toSentence)
        ++ renamingsStr
      in
      let result =
        synthesisResult caption newProgram |> setResultSafe isSafe
      in
      result
    in
    let newProgramOriginalNamesResult =
      let newProgramOriginalNames = renameIdentifiers uniqueNameToOldName newProgramUniqueNames in
      [ makeResult [] newProgramOriginalNames ]
    in
    let newProgramMaybeRenamedResults =
      if InterfaceModel.isResultSafe (Utils.head "CodeMotion.moveDefinitionsBeforeEId" newProgramOriginalNamesResult) then
        []
      else
        let (uniqueNameToOldNameMoved, uniqueNameToOldNameUnmoved) =
          let namesMoved = Set.union pluckedPatIdentifiersUnique pluckedBoundExpFreeIdentifiersUnique in
          uniqueNameToOldName
          |> Dict.toList
          |> List.partition (\(uniqueName, oldName) -> Set.member uniqueName namesMoved)
        in
        let resultForOriginalNamesPriority uniqueNameToOldNamePrioritized =
          let (newProgramPartiallyOriginalNames, _, renamingsPreserved) =
            -- Try revert back to original names one by one, as safe.
            uniqueNameToOldNamePrioritized
            |> List.foldl
                (\(uniqueName, oldName) (newProgramPartiallyOriginalNames, newPatPartiallyOriginalNames, renamingsPreserved) ->
                  let intendedUses = varsWithName uniqueName programUniqueNames |> List.map (.val >> .eid) in
                  let usesInNewProgram = identifierUsesAfterDefiningPat uniqueName newProgramPartiallyOriginalNames |> List.map (.val >> .eid) in
                  let identifiersInNewPat = identifiersListInPat newPatPartiallyOriginalNames in
                  -- If this name is part of the new pattern and renaming it would created a duplicate name, don't rename.
                  if List.member uniqueName identifiersInNewPat && List.member oldName identifiersInNewPat then
                    (newProgramPartiallyOriginalNames, newPatPartiallyOriginalNames, renamingsPreserved ++ [(oldName, uniqueName)])
                  else if not <| Utils.equalAsSets intendedUses usesInNewProgram then
                    -- Definition of this variable was moved in such a way that renaming can't make the program work.
                    -- Might as well use the old name and let the programmer fix the mess they made.
                    ( renameIdentifier uniqueName oldName newProgramPartiallyOriginalNames
                    , renameIdentifierInPat uniqueName oldName newPatPartiallyOriginalNames
                    , renamingsPreserved
                    )
                  else
                    let usesIfRenamed =
                      let identScopeAreas = findScopeAreasByIdent uniqueName newProgramPartiallyOriginalNames in
                      identScopeAreas
                      |> List.map (renameIdentifier uniqueName oldName)
                      |> List.concatMap (identifierUsageEIds oldName)
                    in
                    if Utils.equalAsSets intendedUses usesIfRenamed then
                      -- Safe to rename.
                      ( renameIdentifier uniqueName oldName newProgramPartiallyOriginalNames
                      , renameIdentifierInPat uniqueName oldName newPatPartiallyOriginalNames
                      , renamingsPreserved
                      )
                    else
                      (newProgramPartiallyOriginalNames, newPatPartiallyOriginalNames, renamingsPreserved ++ [(oldName, uniqueName)])
                )
                (newProgramUniqueNames, newPatUniqueNames, [])
          in
          makeResult renamingsPreserved newProgramPartiallyOriginalNames
        in
        [ resultForOriginalNamesPriority (uniqueNameToOldNameUnmoved ++ uniqueNameToOldNameMoved)
        , resultForOriginalNamesPriority (uniqueNameToOldNameMoved ++ uniqueNameToOldNameUnmoved)
        ]
    in
    newProgramOriginalNamesResult ++ newProgramMaybeRenamedResults
    |> Utils.dedupBy (\(SynthesisResult {exp}) -> unparseWithUniformWhitespace False False exp)


moveDefinitionsBeforeEId : List PatternId -> EId -> Exp -> List SynthesisResult
moveDefinitionsBeforeEId sourcePatIds targetEId program =
  let makeNewProgram pluckedPatAndBoundExps programWithoutPluckedUniqueNames =
    let (pluckedPats, pluckedBoundExps) = List.unzip pluckedPatAndBoundExps in
    let (newPatUniqueNames, newBoundExpUniqueNames) =
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
      programWithoutPluckedUniqueNames
      |> mapExpNode
          targetEId
          (\expToWrap ->
            let letOrDef = if isTopLevelEId targetEId programWithoutPluckedUniqueNames then Def else Let in
            withDummyPosEId insertedLetEId <|
              ELet (precedingWhitespace expToWrap) letOrDef False
                (ensureWhitespacePat newPatUniqueNames) (ensureWhitespaceExp newBoundExpUniqueNames)
                (ensureWhitespaceExp expToWrap) ""
          )
      |> LangTransform.simplifyAssignments
    in
    (newProgram, insertedLetEId)
  in
  moveDefinitions_ makeNewProgram sourcePatIds program


moveDefinitionPat : List PatternId -> PatternId -> Exp -> List SynthesisResult
moveDefinitionPat sourcePatIds targetPatId program =
  let makeNewProgram pluckedPatAndBoundExps programWithoutPluckedUniqueNames =
    let ((targetEId, _), targetPath) = targetPatId in
    let newProgram =
      programWithoutPluckedUniqueNames
      |> mapExpNode
          targetEId
          (\newScopeExp ->
            pluckedPatAndBoundExps
            |> List.foldr
                (\(pluckedPat, pluckedBoundExp) newScopeExp ->
                  insertPat_ (pluckedPat, pluckedBoundExp) targetPath newScopeExp
                )
                newScopeExp
          )
      |> LangTransform.simplifyAssignments
    in
    (newProgram, targetEId)
  in
  moveDefinitions_ makeNewProgram sourcePatIds program


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


------------------------------------------------------------------------------

addNewEquationsAround program targetId newEquations e =
  copyPrecedingWhitespace e <|
    eLetOrDef
      (if isTopLevelEId targetId program then Def else Let)
      newEquations e

addNewEquationsInside targetPath newEquations e =
  insertPat_ (patBoundExpOf newEquations) targetPath e

------------------------------------------------------------------------------

makeIntroduceVarTool m expIds targetPos =
  case targetPos of

    ExpTargetPosition (After, expTargetId) ->
      []

    ExpTargetPosition (Before, expTargetId) ->
      makeIntroduceVarTool_ m expIds expTargetId
        (addNewEquationsAround m.inputExp expTargetId)

    PatTargetPosition patTarget ->
      case patTargetPositionToTargetPatId patTarget of
        ((targetId, 1), targetPath) ->
          makeIntroduceVarTool_ m expIds targetId
            (addNewEquationsInside targetPath)

        _ ->
          []

makeIntroduceVarTool_ m expIds addNewVarsAtThisId addNewEquationsAt =
  let toolName =
    "Introduce Var" ++ (if List.length expIds == 1 then "" else "s")
  in
  let (_,_,_,_,_,targetBody,_) = getELet m.inputExp addNewVarsAtThisId in
  let targetBodyEId = targetBody.val.eid in
  let visibleAtTargetBody =
    visibleIdentifiersAtEIds m.inputExp (Set.singleton targetBodyEId)
  in
  [ (toolName, \() ->
      let (newEquations, expWithNewVarsUsed) =
         List.foldl
           (\eId (acc1, acc2) ->
             -- TODO version of scopeNamesLiftedThrough for EId instead of Loc?
             -- let scopes = scopeNamesLocLiftedThrough m.inputExp loc in
             -- let newVar = String.join "_" (scopes ++ [name]) in
             let name = expNameForEId m.inputExp eId in
             let namesToAvoid =
               Set.union visibleAtTargetBody
                   (Set.fromList (List.map Tuple.first acc1))
             in
             let newVar = nonCollidingName name 1 namesToAvoid in
             let expAtEId = justFindExpByEId m.inputExp eId in
             let expWithNewVarUsed =
               replaceExpNodePreservingPrecedingWhitespace eId (eVar newVar) acc2
             in
             ((newVar, expAtEId) :: acc1, expWithNewVarUsed)
           )
           ([], m.inputExp)
           expIds
      in
      let newExp =
        mapExp (\e -> if e.val.eid == addNewVarsAtThisId
                      then addNewEquationsAt newEquations e
                      else e
               ) expWithNewVarsUsed
      in
      synthesisResult toolName newExp
        |> setResultSafe (freeVars newExp == freeVars m.inputExp)
        |> Utils.just
    ) ]


------------------------------------------------------------------------------

makeMakeEqualTool m nums =
  let expIds = List.map (\(eid,_,_,_,_) -> eid) nums in
  let (firstNumExpId,_,firstNum,_,_) = Utils.head_ nums in
  let targetLet =
    justInsideDeepestCommonScope m.inputExp (\e -> List.member e.val.eid expIds)
  in
  let targetId = targetLet.val.eid in
  -- TODO: commonName sometimes returns a name like "1"
  -- let name = commonNameForEIds m.inputExp expIds in
  -- let name = commonNameForEIdsWithDefault "newVar" m.inputExp expIds in
  let name = expNameForEId m.inputExp firstNumExpId in
  let namesToAvoid =
    visibleIdentifiersAtEIds m.inputExp (Set.singleton targetId)
  in
  let newVar = nonCollidingName name 1 namesToAvoid in
  let newEquation = (newVar, eConst firstNum dummyLoc) in
  let eSubst =
    List.foldl (\(eid,ws,_,_,_) -> Dict.insert eid (EVar ws newVar)) Dict.empty nums
  in
  let expWithNewVarUsed = applyESubst eSubst m.inputExp in
  let newExp =
    expWithNewVarUsed |> mapExp (\e ->
      if e.val.eid == targetId
      then addNewEquationsAround m.inputExp targetId [newEquation] e
      else e
    )
  in
  [ ("Make Equal", \() -> oneSafeResult newExp) ]

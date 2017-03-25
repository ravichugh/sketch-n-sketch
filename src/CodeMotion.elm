module CodeMotion exposing
  ( moveDefinitionsPat, moveDefinitionsBeforeEId
  , moveEquationsBeforeEId
  , duplicateDefinitionsPat, duplicateDefinitionsBeforeEId
  , abstractPVar, abstractExp, shouldBeParameterIsConstant, shouldBeParameterIsNamedUnfrozenConstant
  , removeArg, removeArgs, addArg, addArgFromPat, addArgs, addArgsFromPats, reorderFunctionArgs
  , makeEListReorderTool
  , makeIntroduceVarTool
  , makeMakeEqualTool
  , addToolCompareSubExpressions
  )

import Lang exposing (..)
import LangTools exposing (..)
import LangSimplify
import LangUnparser exposing (unparse, unparseWithIds, unparseWithUniformWhitespace, unparsePat)
import LangParser2
-- import DependenceGraph exposing
  -- (ScopeGraph, ScopeOrder(..), parentScopeOf, childScopesOf)
import InterfaceModel exposing
  ( Model, SynthesisResult(..), NamedDeuceTool
  , synthesisResult, setResultSafe, mapResultSafe, oneSafeResult
  )
import Utils exposing (MaybeOne)
import Either exposing (..)

import Dict
import Regex
import Set


getELet program eid =
  let exp = justFindExpByEId program eid in
  case exp.val.e__ of
    ELet ws1 letKind rec p1 e1 e2 ws2 -> (ws1, letKind, rec, p1, e1, e2, ws2)
    _                                 -> Debug.crash "getELet: shouldn't happen"


type alias PatBoundExp = (Pat, Exp)


pluckAll : List PathedPatternId -> Exp -> (List (Pat, Exp, EId), Exp)
pluckAll sourcePathedPatIds program =
    let sortedSourcePathedPatIds =
      sourcePathedPatIds
      |> List.sortBy
          (\((scopeEId, branchI), path) ->
            (locationInProgram program scopeEId, branchI, path)
          )
    in
  let (pluckedPatAndBoundExpAndOldScopeEIds, programWithoutPlucked) =
    sortedSourcePathedPatIds
    |> List.foldr
        (\sourcePathedPatId (pluckedPatAndBoundExpAndOldScopeBodies, programBeingPlucked) ->
          case pluck sourcePathedPatId programBeingPlucked of
            Just ((pluckedPat, pluckedBoundExp), programWithoutPlucked) ->
              let ((sourceScopeEId, _), _) = sourcePathedPatId in
              ((pluckedPat, pluckedBoundExp, sourceScopeEId)::pluckedPatAndBoundExpAndOldScopeBodies, programWithoutPlucked)
            Nothing ->
              (pluckedPatAndBoundExpAndOldScopeBodies, programBeingPlucked)
        )
        ([], program)
  in
  (pluckedPatAndBoundExpAndOldScopeEIds, programWithoutPlucked)


-- Removes the binding (p, e1) from the program, returns it and the program without with binding.
--
-- If this would remove a whole pattern, the pattern is left as [] matched to [] for a later clean up step.
--
-- This technique preserves EIds and pattern paths for later insertion.
pluck : PathedPatternId -> Exp -> Maybe (PatBoundExp, Exp)
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
  case (p.val.p__, e1.val.e__, path) of
    (_, _, []) ->
      Just <|
        ( (p, e1)
        , replaceP__ p   <| PList (precedingWhitespacePat p) [] "" Nothing ""
        , replaceE__ e1 <| EList (precedingWhitespace e1)   [] "" Nothing ""
        )

    (PAs _ _ _ childPat, _, i::is) ->
      -- TODO: allow but mark unsafe if as-pattern is used
      let _ = Debug.log "can't pluck out of as-pattern yet (unsafe)" () in
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
              , replaceP__ p   <| PList pws1 newPs pws2 maybePTail pws3
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
              , replaceP__ p   <| PList pws1 ps pws2 (Just newTailPat) pws3
              , replaceE__ e1 <| EList ews1 es ews2 (Just newTailBoundExp) ews3
              )
            )
      else
        Debug.log "pluck index longer than head list of PList or EList" Nothing

    _ ->
      let _ = Debug.log ("pluck_: bad pattern " ++ unparsePat p) path in
      Nothing


-- Find all paths to empty lists.
-- Pats are presumed not to have constants/base vals. (i.e. pats are function arguments)
-- Returned dead paths are sorted left-to-right
deadPathsInPats : List Pat -> List (List Int)
deadPathsInPats pats =
  pats
  |> Utils.zipi1
  |> List.concatMap
      (\(i, pat) ->
        deadPathsInPat pat
        |> List.map ((::) i)
      )


-- Find all paths to empty lists.
-- Pat is presumed not to have constants/base vals. (i.e. pat is a function argument)
-- Returned dead paths are sorted left-to-right
deadPathsInPat : Pat -> List (List Int)
deadPathsInPat pat =
  if identifiersListInPat pat == [] then
    [[]]
  else
    case pat.val.p__ of
      PVar   _ _ _ -> []
      PConst _ _   -> Debug.log "why do you put constants in your function arguments?!" []
      PBase  _ _   -> Debug.log "why do you put base vals in your function arguments?!"[]

      PAs _ _ _ _ ->
        -- plucking out of as-pattern is generally unsafe (not allowed yet)
        -- so we shouldn't be creating dead paths inside as-patterns
        []

      PList ws1 ps ws2 maybeTail ws3 ->
        let deadPathsInTail =
          maybeTail
          |> Maybe.map
              (\tailPat ->
                let tailI = List.length ps + 1 in
                deadPathsInPat tailPat |> List.map ((::) tailI)
              )
          |> Maybe.withDefault []
        in
        deadPathsInPats ps ++ deadPathsInTail


-- Returns Maybe (pluckedPat, patsWithoutPlucked)
pluckPatFromPats : List Int -> List Pat -> Maybe (Pat, List Pat)
pluckPatFromPats path pats =
  case path of
    i::is ->
      Utils.maybeGet1 i pats
      |> Maybe.andThen (pluckPat is)
      |> Maybe.map
          (\(pluckedPat, maybeRemainingPat) ->
            case maybeRemainingPat of
              Just remainingPat -> (pluckedPat, Utils.replacei i remainingPat pats |> imitatePatListWhitespace pats)
              Nothing           -> (pluckedPat, Utils.removei i pats               |> imitatePatListWhitespace pats)
          )

    [] ->
      Nothing


justRemovePatFromPats : String -> List Int -> List Pat -> List Pat
justRemovePatFromPats failureMessage path pats =
  pluckPatFromPats path pats
  |> Utils.fromJust_ failureMessage
  |> Tuple.second


-- Returns Maybe (pluckedPat, Maybe residualPatWithoutPlucked)
pluckPat : List Int -> Pat -> Maybe (Pat, Maybe Pat)
pluckPat path pat =
  case (pat.val.p__, path) of
    (_, []) ->
      Just (pat, Nothing)

    (PAs ws1 ident ws2 p, 1::is) ->
      let _ = Debug.log "plucking out of as-pattern is generally unsafe (not allowed yet)" () in
      Nothing

    -- (PAs ws1 ident ws2 p, 1::is) ->
    --   let result = pluckPat is p in
    --   case result of
    --     Just (pluckedPat, Just remainingPat) ->
    --       Just (pluckedPat, Just <| replaceP__ pat (PAs ws1 ident ws2 remainingPat))
    --
    --     _ ->
    --       result

    (PList ws1 ps ws2 maybeTail ws3, i::is) ->
      if i <= List.length ps then
        pluckPatFromPats (i::is) ps
        |> Maybe.map
            (\(pluckedPat, remainingPats) ->
              (pluckedPat, Just <| replaceP__ pat (PList ws1 remainingPats ws2 maybeTail ws3))
            )
      else if i == List.length ps + 1 then
        maybeTail
        |> Maybe.andThen (pluckPat is)
        |> Maybe.map
            (\(pluckedPat, maybeRemainingTail) ->
              (pluckedPat, Just <| replaceP__ pat (PList ws1 ps ws2 maybeRemainingTail ws3))
            )
      else
        Nothing

    _ ->
      Nothing


-- Returns Maybe (pluckedExp, expsWithoutPlucked)
pluckExpFromExpsByPath : List Int -> List Exp -> Maybe (Exp, List Exp)
pluckExpFromExpsByPath path exps =
  case path of
    i::is ->
      Utils.maybeGet1 i exps
      |> Maybe.andThen (pluckExpByPath is)
      |> Maybe.map
          (\(pluckedExp, maybeRemainingExp) ->
            case maybeRemainingExp of
              Just remainingExp -> (pluckedExp, Utils.replacei i remainingExp exps)
              Nothing           -> (pluckedExp, Utils.removei i exps)
          )

    [] ->
      Nothing


-- Returns Maybe (pluckedExp, Maybe residualExpWithoutPlucked)
pluckExpByPath : List Int -> Exp -> Maybe (Exp, Maybe Exp)
pluckExpByPath path exp =
  case (exp.val.e__, path) of
    (_, []) ->
      Just (exp, Nothing)

    (EList ws1 es ws2 maybeTail ws3, i::is) ->
      if i <= List.length es then
        pluckExpFromExpsByPath (i::is) es
        |> Maybe.map
            (\(pluckedExp, remainingExps) ->
              (pluckedExp, Just <| replaceE__ exp (EList ws1 remainingExps ws2 maybeTail ws3))
            )
      else if i == List.length es + 1 then
        maybeTail
        |> Maybe.andThen (pluckExpByPath is)
        |> Maybe.map
            (\(pluckedExp, maybeRemainingTail) ->
              (pluckedExp, Just <| replaceE__ exp (EList ws1 es ws2 maybeRemainingTail ws3))
            )
      else
        Nothing

    _ ->
      Nothing


------------------------------------------------------------------------------

-- Precondition: program has been run through assignUniqueNames
-- Also returns a dictionary of movedIdent to new scopeId
-- Does not (and should not) run LangSimply.simplifyAssignments
-- When to run simplifyAssignments depends on how many transformations are being composed.
liftDependenciesBasedOnUniqueNames : Exp -> (Exp, Dict.Dict Ident ScopeId)
liftDependenciesBasedOnUniqueNames program =
  let needToLift =
    Set.diff (freeIdentifiers program) preludeIdentifiers
    |> Set.toList
  in
  let bringIdentIntoScope identToLift =
    let maybeOriginalDefiningScope =
      program
      |> findFirstNode (expToMaybeLetPat >> Maybe.map (identifiersListInPat >> List.member identToLift) >> Maybe.withDefault False)
    in
    case maybeOriginalDefiningScope of
      Nothing -> Nothing
      Just originalDefiningScope ->
        case pathForIdentInPat identToLift (expToLetPat originalDefiningScope) of
          Nothing -> Nothing
          Just path ->
            case pluck ((originalDefiningScope.val.eid, 1), path) program of
              Nothing -> Nothing
              Just ((pluckedPat, pluckedBoundExp), programWithoutPlucked) ->
                let eidToWrap = deepestCommonScope program (expToMaybeIdent >> (==) (Just identToLift)) |> .val |> .eid in
                let insertedLetEId = LangParser2.maxId program + 1 in
                let newProgram =
                  programWithoutPlucked
                  |> mapExpNode
                      eidToWrap
                      (\expToWrap ->
                        let letOrDef = if isTopLevelEId eidToWrap programWithoutPlucked then Def else Let in
                        withDummyExpInfoEId insertedLetEId <|
                          ELet (precedingWhitespace expToWrap) letOrDef False
                            (ensureWhitespacePat pluckedPat) (ensureWhitespaceExp pluckedBoundExp)
                            (ensureWhitespaceExp expToWrap) ""
                      )
                in
                Just (newProgram, identToLift, insertedLetEId)
  in
  -- Look for an identifier we can lift
  let maybeNewProgramIdentAndNewLetEId =
    needToLift |> Utils.mapFirstSuccess bringIdentIntoScope
  in
  case maybeNewProgramIdentAndNewLetEId of
    Nothing ->
      -- no more dependencies we can lift
      ( program
      , Dict.empty
      )

    Just (newProgram, movedIdent, insertedLetEId) ->
      let (finalProgram, movedIdentToScopeId) =
        liftDependenciesBasedOnUniqueNames newProgram
      in
      ( finalProgram
      , Dict.insert movedIdent (insertedLetEId, 1) movedIdentToScopeId
      )


-- Moving a definition is safe if all identifiers resolve to the same bindings.
--
-- More specifically:
--   - All free variables in the moved assignment still resolve to the same bindings
--   - All previous references to the moved identifier still resolve to that identifer
--   - All other variables uses of the same name do not resolve to the moved identifier
--
moveDefinitions_ : Bool -> (List PatBoundExp -> Exp -> (Exp, EId)) -> List PathedPatternId -> Exp -> List SynthesisResult
moveDefinitions_ doCleanUp makeNewProgram sourcePathedPatIds program =
  let maybeClean =
    -- When composing transformations, can't clean up until the end (don't want to remove EIds).
    if doCleanUp
    then LangSimplify.simplifyAssignments
    else identity
  in
  let (programUniqueNames, uniqueNameToOldName) = assignUniqueNames program in
  let (pluckedPatAndBoundExpAndOldScopeEIds, programWithoutPlucked) =
    pluckAll sourcePathedPatIds programUniqueNames
  in
  if pluckedPatAndBoundExpAndOldScopeEIds == [] then
    Debug.log "could not pluck anything" []
  else
    let (pluckedPats, pluckedBoundExps, _)   = Utils.unzip3 pluckedPatAndBoundExpAndOldScopeEIds in
    let pluckedPathedPatIdentifiersUnique          = Utils.unionAll <| List.map identifiersSetInPat pluckedPats in
    let pluckedBoundExpFreeIdentifiersUnique = Utils.unionAll <| List.map freeIdentifiers pluckedBoundExps in
    let (newProgramUniqueNames, newScopeEId) =
      makeNewProgram (Utils.zip pluckedPats pluckedBoundExps) programWithoutPlucked
    in
    let newPatUniqueNames = justFindExpByEId newProgramUniqueNames newScopeEId |> expToLetPat in
    let makeResult renamings uniqueIdentToMovedScopeId newProgram =
      let newScopeExp  = justFindExpByEId newProgram newScopeEId in
      let newScopeBody = newScopeExp |> expToLetBody in
      let newPat       = newScopeExp |> expToLetPat in
      let uniqueNameToOldNameUsed =
        Dict.diff uniqueNameToOldName (List.map Utils.flip renamings |> Dict.fromList)
      in
      let isSafe =
        let identUsesSafe =
          -- Presumably these will be exactly equal rather than equal as sets since we shouldn't be moving around the relative position of the variable usages...still, I think a set is the natural strucuture here.
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
                |> List.all
                    (\(varUnique, varNew) ->
                      let expectedScopeId =
                        Dict.get (expToIdent varUnique) uniqueIdentToMovedScopeId
                        |> Maybe.map Just -- If found, "Just" means not free in program.
                        |> Maybe.withDefault (bindingScopeIdFor varUnique programUniqueNames)
                        -- "Nothing" here means free in program.
                      in
                      expectedScopeId == bindingScopeIdFor varNew newProgram
                    )
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
        let movedThingStrings =
          pluckedPats ++ (Dict.keys uniqueIdentToMovedScopeId |> List.map pVar0)
          |> List.map
              (renameIdentifiersInPat uniqueNameToOldName >> unparsePat >> Utils.squish)
        in
        "Move "
        ++ Utils.toSentence movedThingStrings
        ++ renamingsStr
      in
      let result =
        synthesisResult caption (maybeClean newProgram) |> setResultSafe isSafe
      in
      result
    in
    let newProgramOriginalNamesResult =
      let newProgramOriginalNames = renameIdentifiers uniqueNameToOldName newProgramUniqueNames in
      [ makeResult [] Dict.empty newProgramOriginalNames ]
    in
    let newProgramMaybeRenamedResults =
      if InterfaceModel.isResultSafe (Utils.head "CodeMotion.moveDefinitionsBeforeEId" newProgramOriginalNamesResult) then
        []
      else
        let (uniqueNameToOldNameMoved, uniqueNameToOldNameUnmoved) =
          let namesMoved = Set.union pluckedPathedPatIdentifiersUnique pluckedBoundExpFreeIdentifiersUnique in
          uniqueNameToOldName
          |> Dict.toList
          |> List.partition (\(uniqueName, oldName) -> Set.member uniqueName namesMoved)
        in
        let (newProgramUniqueNamesDependenciesLifted, uniqueIdentToMovedScopeId) =
          liftDependenciesBasedOnUniqueNames newProgramUniqueNames
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
                (newProgramUniqueNamesDependenciesLifted, newPatUniqueNames, [])
          in
          makeResult renamingsPreserved uniqueIdentToMovedScopeId newProgramPartiallyOriginalNames
        in
        [ resultForOriginalNamesPriority (uniqueNameToOldNameUnmoved ++ uniqueNameToOldNameMoved)
        , resultForOriginalNamesPriority (uniqueNameToOldNameMoved ++ uniqueNameToOldNameUnmoved)
        ]
    in
    newProgramOriginalNamesResult ++ newProgramMaybeRenamedResults
    |> Utils.dedupBy (\(SynthesisResult {exp}) -> unparseWithUniformWhitespace False False exp)


moveDefinitionsBeforeEId : List PathedPatternId -> EId -> Exp -> List SynthesisResult
moveDefinitionsBeforeEId sourcePathedPatIds targetEId program =
  moveDefinitionsBeforeEId_ True sourcePathedPatIds targetEId program


moveDefinitionsBeforeEId_ : Bool -> List PathedPatternId -> EId -> Exp -> List SynthesisResult
moveDefinitionsBeforeEId_ doCleanUp sourcePathedPatIds targetEId program =
  -- let _ = Debug.log ("moving " ++ toString sourcePathedPatIds ++ " before " ++ toString targetEId ++ " in " ++ unparseWithIds program) () in
  let makeNewProgram pluckedPatAndBoundExps programWithoutPluckedUniqueNames =
    let (pluckedPats, pluckedBoundExps) = List.unzip pluckedPatAndBoundExps in
    let (newPatUniqueNames, newBoundExpUniqueNames) =
      case (pluckedPats, pluckedBoundExps) of
        ([pluckedPat], [boundExp]) ->
          (pluckedPat, boundExp)

        _ ->
          ( withDummyPatInfo <| PList " " (pluckedPats      |> setPatListWhitespace "" " ") "" Nothing ""
          , withDummyExpInfo <| EList " " (pluckedBoundExps |> setExpListWhitespace "" " ") "" Nothing "" -- May want to be smarter about whitespace here to avoid long lines.
          )
    in
    let insertedLetEId = LangParser2.maxId program + 1 in
    let newProgram =
      programWithoutPluckedUniqueNames
      |> mapExpNode
          targetEId
          (\expToWrap ->
            let letOrDef = if isTopLevelEId targetEId programWithoutPluckedUniqueNames then Def else Let in
            withDummyExpInfoEId insertedLetEId <|
              ELet (precedingWhitespace expToWrap) letOrDef False
                (ensureWhitespacePat newPatUniqueNames) (ensureWhitespaceExp newBoundExpUniqueNames)
                (ensureWhitespaceExp expToWrap) ""
          )
    in
    (newProgram, insertedLetEId)
  in
  moveDefinitions_ doCleanUp makeNewProgram sourcePathedPatIds program


moveDefinitionsPat : List PathedPatternId -> PathedPatternId -> Exp -> List SynthesisResult
moveDefinitionsPat sourcePathedPatIds targetPathedPatId program =
  let makeNewProgram pluckedPatAndBoundExps programWithoutPluckedUniqueNames =
    let ((targetEId, _), targetPath) = targetPathedPatId in
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
    in
    (newProgram, targetEId)
  in
  moveDefinitions_ True makeNewProgram sourcePathedPatIds program


makeDuplicateResults_ newScopeEId pluckedPatAndBoundExpAndOldScopeEIds newProgram originalProgram =
  let (pluckedPats, pluckedBoundExps, _) = Utils.unzip3 pluckedPatAndBoundExpAndOldScopeEIds in
  let newScopeExp = justFindExpByEId newProgram newScopeEId in
  let newScopePat      = newScopeExp |> expToLetPat in
  let newScopeBoundExp = newScopeExp |> expToLetBoundExp in
  let newScopeBody     = newScopeExp |> expToLetBody in
  let isSafe =
    let identUsesSafe =
      0 == Set.size (Set.intersect (identifiersSetInPats pluckedPats) (freeIdentifiers newScopeBody))
    in
    let boundExpVarsSafe =
      let oldBoundExpFreeIdentBindingScopeIds =
        pluckedBoundExps
        |> List.concatMap freeVars
        |> List.map
            (\var ->
              ( expToIdent var
              , bindingScopeIdFor var originalProgram |> Maybe.withDefault (-1, -1)) -- (-1, -1) if free in originalProgram
            )
        |> Set.fromList
      in
      let newBoundExpFreeIdentBindingScopeIds =
        freeVars newScopeBoundExp
        |> List.map
            (\var ->
              ( expToIdent var
              , bindingScopeIdFor var newProgram |> Maybe.withDefault (-1, -1)) -- (-1, -1) if free in newProgram
            )
        |> Set.fromList
      in
      Utils.isSubset oldBoundExpFreeIdentBindingScopeIds newBoundExpFreeIdentBindingScopeIds
    in
    let noDuplicateNamesInPat =
      let namesDefinedAtNewScope = identifiersListInPat newScopePat in
      namesDefinedAtNewScope == Utils.dedup namesDefinedAtNewScope
    in
    identUsesSafe && boundExpVarsSafe && noDuplicateNamesInPat
  in
  let caption =
    let patStrs = List.map (unparsePat >> Utils.squish) pluckedPats in
    "Duplicate "
    ++ (if List.length patStrs == 1 then "Definition" else "Definitions")
    ++ " of "
    ++ Utils.toSentence patStrs
  in
  let result =
    synthesisResult caption newProgram |> setResultSafe isSafe
  in
  [ result ]


duplicateDefinitionsBeforeEId : List PathedPatternId -> EId -> Exp -> List SynthesisResult
duplicateDefinitionsBeforeEId sourcePathedPatIds targetEId originalProgram =
  let (pluckedPatAndBoundExpAndOldScopeEIds, _) =
    pluckAll sourcePathedPatIds originalProgram
  in
  let (pluckedPats, pluckedBoundExps, _) = Utils.unzip3 pluckedPatAndBoundExpAndOldScopeEIds in
  let insertedLetEId = LangParser2.maxId originalProgram + 1 in
  let newProgram =
    originalProgram
    |> mapExpNode
        targetEId
        (\expToWrap ->
          let letOrDef = if isTopLevelEId targetEId originalProgram then Def else Let in
          let (newPat, newBoundExp) =
            case (pluckedPats, pluckedBoundExps) of
              ([pluckedPat], [boundExp]) ->
                (pluckedPat, boundExp)

              _ ->
                ( withDummyPatInfo <| PList " " (pluckedPats      |> setPatListWhitespace "" " ") "" Nothing ""
                , withDummyExpInfo <| EList " " (pluckedBoundExps |> setExpListWhitespace "" " ") "" Nothing "" -- May want to be smarter about whitespace here to avoid long lines.
                )
          in
          withDummyExpInfoEId insertedLetEId <|
            ELet (precedingWhitespace expToWrap) letOrDef False
              (ensureWhitespacePat newPat) (ensureWhitespaceExp newBoundExp)
              (ensureWhitespaceExp expToWrap) ""
        )
    |> LangParser2.freshen -- Remove duplicate EIds
  in
  makeDuplicateResults_ insertedLetEId pluckedPatAndBoundExpAndOldScopeEIds newProgram originalProgram


duplicateDefinitionsPat : List PathedPatternId -> PathedPatternId -> Exp -> List SynthesisResult
duplicateDefinitionsPat sourcePathedPatIds targetPathedPatId originalProgram =
  let (pluckedPatAndBoundExpAndOldScopeEIds, _) =
    pluckAll sourcePathedPatIds originalProgram
  in
  let ((targetEId, _), targetPath) = targetPathedPatId in
  let newProgram =
    originalProgram
    |> mapExpNode
        targetEId
        (\newScopeExp ->
          pluckedPatAndBoundExpAndOldScopeEIds
          |> List.foldr
              (\(pluckedPat, pluckedBoundExp, _) newScopeExp ->
                insertPat_ (pluckedPat, pluckedBoundExp) targetPath newScopeExp
              )
              newScopeExp
        )
    |> LangParser2.freshen -- Remove duplicate EIds
  in
  makeDuplicateResults_ targetEId pluckedPatAndBoundExpAndOldScopeEIds newProgram originalProgram


insertPat_ : PatBoundExp -> List Int -> Exp -> Exp
insertPat_ (patToInsert, boundExp) targetPath exp =
  case exp.val.e__ of
    ELet ws1 letKind rec p e1 e2 ws2 ->
      case insertPat__ (patToInsert, boundExp) p e1 targetPath of
        Just (newPat, newBoundExp) ->
          replaceE__ exp (ELet ws1 letKind rec newPat newBoundExp e2 ws2)

        Nothing ->
          let _ = Debug.log "insertPat_: pattern, path " (p.val.p__, targetPath) in
          exp

    _ ->
      let _ = Debug.log "insertPat_: not ELet" exp.val.e__ in
      exp


insertPat__ : PatBoundExp -> Pat -> Exp -> List Int -> Maybe (Pat, Exp)
insertPat__ (patToInsert, boundExp) p e1 path =
  let maybeNewP_E__Pair =
    case (p.val.p__, e1.val.e__, path) of
      (PVar pws1 _ _, _, [i]) ->
        Just ( PList pws1                      (Utils.inserti i patToInsert [p] |> setPatListWhitespace "" " ") "" Nothing ""
             , EList (precedingWhitespace e1)  (Utils.inserti i boundExp [e1]   |> setExpListWhitespace "" " ") "" Nothing "" )

      (PAs pws1 _ _ _, _, [i]) ->
        Just ( PList pws1                      (Utils.inserti i patToInsert [p] |> setPatListWhitespace "" " ") "" Nothing ""
             , EList (precedingWhitespace e1)  (Utils.inserti i boundExp [e1]   |> setExpListWhitespace "" " ") "" Nothing "" )

      (PAs pws1 _ _ _, _, i::is) ->
        -- TODO: allow but mark unsafe if as-pattern is used
        let _ = Debug.log "can't insert into as-pattern yet (unsafe)" () in
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
        let _ = Debug.log "insertPat__: pattern, path " (p.val.p__, path) in
        Nothing
  in
  case maybeNewP_E__Pair of
    Just (newP_, newE__) ->
      Just (replaceP__ p newP_, replaceE__ e1 newE__) -- Hmm this will produce duplicate EIds in the boundExp when PVar or PAs are expanded into PList

    Nothing ->
      Nothing


addPatToPats : Pat -> List Int -> List Pat -> Maybe (List Pat)
addPatToPats patToInsert path pats =
  case path of
    [i] ->
      Just (Utils.inserti i patToInsert pats |> imitatePatListWhitespace pats)

    i::is ->
      Utils.maybeGet1 i pats
      |> Maybe.andThen (addPatToPat patToInsert is)
      |> Maybe.map (\newPat -> Utils.replacei i newPat pats)

    [] ->
      Nothing


addPatToPat : Pat -> List Int -> Pat -> Maybe Pat
addPatToPat patToInsert path pat =
  case (pat.val.p__, path) of
    (_, []) ->
      Nothing

    (PAs ws1 ident ws2 p, 1::is) ->
      let _ = Debug.log "adding to as pattern not allowed yet because when adding argument, pattern path will not be the same as the path for adding arguments to call sites" () in
      Nothing

    -- (PAs ws1 ident ws2 p, 1::is) ->
    --   let result = pluckPat is p in
    --   case result of
    --     Just (pluckedPat, Just remainingPat) ->
    --       Just (pluckedPat, Just <| replaceP__ pat (PAs ws1 ident ws2 remainingPat))
    --
    --     _ ->
    --       result

    (PList ws1 ps ws2 maybeTail ws3, i::is) ->
      if i == List.length ps + 1 && is /= [] then
        maybeTail
        |> Maybe.andThen (addPatToPat patToInsert is)
        |> Maybe.map (\newTail -> replaceP__ pat <| PList ws1 ps ws2 (Just newTail) ws3 )
      else if i <= List.length ps + 1 then
        addPatToPats patToInsert (i::is) ps
        |> Maybe.map (\newPs -> replaceP__ pat <| PList ws1 newPs ws2 maybeTail ws3)
      else
        Nothing

    _ ->
      Nothing


addExpToExpsByPath : Exp -> List Int -> List Exp -> Maybe (List Exp)
addExpToExpsByPath expToInsert path exps =
  case path of
    [i] ->
      Just (Utils.inserti i expToInsert exps |> imitateExpListWhitespace exps)

    i::is ->
      Utils.maybeGet1 i exps
      |> Maybe.andThen (addExpToExpByPath expToInsert is)
      |> Maybe.map (\newExp -> Utils.replacei i newExp exps)

    [] ->
      Nothing


addExpToExpByPath : Exp -> List Int -> Exp -> Maybe Exp
addExpToExpByPath expToInsert path exp =
  case (exp.val.e__, path) of
    (_, []) ->
      Nothing

    (EList ws1 es ws2 maybeTail ws3, i::is) ->
      if i == List.length es + 1 && is /= [] then
        maybeTail
        |> Maybe.andThen (addExpToExpByPath expToInsert is)
        |> Maybe.map (\newTail -> replaceE__ exp <| EList ws1 es ws2 (Just newTail) ws3 )
      else if i <= List.length es + 1 then
        addExpToExpsByPath expToInsert (i::is) es
        |> Maybe.map (\newEs -> replaceE__ exp <| EList ws1 newEs ws2 maybeTail ws3)
      else
        Nothing

    _ ->
      Nothing


------------------------------------------------------------------------------

moveEquationsBeforeEId : List EId -> EId -> Exp -> List SynthesisResult
moveEquationsBeforeEId letEIds targetEId program =
  let pathedPatIds =
    letEIds
    |> List.sortBy (locationInProgram program)
    |> List.map (\letEId -> ((letEId, 1), []))
  in
  let synthesisResults =
    pathedPatIds
    |> List.foldl
        (\pathedPatId priorResults ->
          priorResults
          |> List.concatMap
              (\(SynthesisResult {exp, isSafe}) ->
                -- "False" means don't perform simplifyAssignments (it can remove EIds we need in later iterations)
                moveDefinitionsBeforeEId_ False [pathedPatId] targetEId exp
                |> List.map (mapResultSafe ((&&) isSafe))
              )
        )
        [ synthesisResult "Original" program ]
  in
  synthesisResults
  |> List.map
      (\(SynthesisResult result) ->
        SynthesisResult
            { result | description = "Move Definitions"
                     , exp         = LangSimplify.simplifyAssignments result.exp }
      )
  -- |> Utils.dedupBy (.exp >> unparseWithUniformWhitespace True True)


------------------------------------------------------------------------------

-- Takes EId of expression to abstract, a predicate on exp and program to choose which expressions should become parameters
abstract : EId -> (Exp -> Exp -> Bool) -> Exp -> (List Exp, Exp)
abstract eid shouldBeParameter originalProgram =
  let expToAbstact = justFindExpByEId originalProgram eid in
  let eidsToParameterize =
    -- Do it like this as a moderately straightforward way to not add extra arguments in the
    -- case that shouldBeParameter returns one expression nested inside another
    expToAbstact
    |> mapExp (\e -> if shouldBeParameter e originalProgram then replaceE__ e (EVar " " "INSERT_ARGUMENT_HERE") else e)
    |> flattenExpTree
    |> List.filter (expToMaybeIdent >> (==) (Just "INSERT_ARGUMENT_HERE"))
    |> List.map (.val >> .eid)
    |> Set.fromList
  in
  -- To allow some varaible names in the body to become arguments:
  --   1. Assign absurd names to the parameters (x_ARRRG!!!)
  --   2. Use LangSimplify.changeRenamedVarsToOuter to
  --      change (\x_ARRRG!!! -> let x = x_ARRRG!!! in x + 1)
  --      to     (\x_ARRRG!!! -> let x = x_ARRRG!!! in x_ARRRG!!! + 1)
  --   3. Remove unused variables to free up the names we want.
  --      Yields (\x_ARRRG!!! -> x_ARRRG!!! + 1)
  --   4. Recompute non-colliding parameter names without the _ARRRG!!! tags.
  let (abstractionBody, (_, paramNamesARRRGTagged, paramExps)) =
    expToAbstact
    |> mapFoldExp
        (\e (namesToAvoid, paramNamesARRRGTagged, paramExps) ->
          if Set.member e.val.eid eidsToParameterize then
            let naiveName = expNameForEIdWithDefault "arg" originalProgram e.val.eid ++ "_ARRRG!!!" in
            let name = nonCollidingName naiveName 2 namesToAvoid in
            let namesToAvoid_ = Set.insert name namesToAvoid in
            (copyPrecedingWhitespace e (eVar name), (namesToAvoid_, name::paramNamesARRRGTagged, (LangParser2.clearAllIds e)::paramExps))
          else
            (e, (namesToAvoid, paramNamesARRRGTagged, paramExps))
        )
        (identifiersSet expToAbstact, [], [])
  in
  let (abstractionBodySimplified, _, paramNames) =
    -- Simplify (\x_ARRRG!!! -> let x = x_ARRRG!!! in ...) to (\x -> ...)
    let abstractionBodySimplifiedARRRGTags =
      abstractionBody
      |> LangSimplify.changeRenamedVarsToOuter
      |> LangSimplify.removeUnusedVars
      |> ensureWhitespaceExp
    in
    let arrrgTagRegex = Regex.regex "_ARRRG!!!\\d*$" in
    let removeARRRGTag name = Regex.replace (Regex.AtMost 1) arrrgTagRegex (\_ -> "") name in
    paramNamesARRRGTagged
    |> List.foldl
        (\nameARRRGTagged (abstractionBodySimplified, namesToAvoid, paramNames) ->
          let noARRRGTag = removeARRRGTag nameARRRGTagged in
          let name = nonCollidingName noARRRGTag 2 namesToAvoid in
          ( renameIdentifier nameARRRGTagged name abstractionBodySimplified
          , Set.insert name namesToAvoid
          , paramNames ++ [name]
          )
        )
        (abstractionBodySimplifiedARRRGTags, identifiersSet abstractionBodySimplifiedARRRGTags, [])
  in
  case paramExps of
    [] ->
      let funcExp = eFun [pList0 []] abstractionBodySimplified in
      ([eTuple []], funcExp)

    _::_ ->
      let funcExp = eFun (listOfPVars paramNames) abstractionBodySimplified in
      (paramExps, funcExp)


shouldBeParameterIsConstant : Exp -> Exp -> Bool
shouldBeParameterIsConstant exp originalProgram =
  case exp.val.e__ of
    EConst _ _ _ _        -> True
    EBase _ (EString _ _) -> True
    _                     -> False


shouldBeParameterIsNamedUnfrozenConstant : Exp -> Exp -> Bool
shouldBeParameterIsNamedUnfrozenConstant exp originalProgram =
  case exp.val.e__ of
    -- Ignore syncOptions.
    EConst _ _ (_, annot, ident) _ ->
      ident /= "" && annot /= frozen

    EBase _ (EString _ _) ->
      -- Is this string bound to a name?
      let bindings =
        justFindExpWithAncestorsByEId originalProgram exp.val.eid
        |> List.filterMap expToMaybeLetPatAndBoundExp
        |> List.concatMap (\(pat, boundExp) -> tryMatchExpReturningList pat boundExp)
      in
      bindings
      |> Utils.findFirst (\(ident, boundExp) -> boundExp.val.eid == exp.val.eid)
      |> Utils.maybeToBool

    _ -> False


abstractPVar : PathedPatternId -> Exp -> List SynthesisResult
abstractPVar pathedPatId originalProgram =
  case pluck pathedPatId originalProgram of
    Nothing ->
      Debug.log ("abstractPVar Could not find pathedPatternId " ++ toString pathedPatId ++ " in program\n" ++ unparseWithIds originalProgram) []

    Just ((pluckedPat, pluckedBoundExp), _) ->
      case pluckedPat.val.p__ of
        PVar _ ident _ ->
          let doAbstract shouldBeParameter =
            let ((scopeEId, _), _) = pathedPatId in
            let scopeExp = justFindExpByEId originalProgram scopeEId in
            let scopeBody = scopeExp |> expToLetBody in
            let (argumentsForCallSite, abstractedFuncExp) =
              abstract pluckedBoundExp.val.eid shouldBeParameter originalProgram
            in
            let newScopeBody =
              let varToApp varExp =
                replaceE__PreservingPrecedingWhitespace varExp (EApp "" (eVar0 ident) (argumentsForCallSite |> setExpListWhitespace " " " ") "")
              in
              transformVarsUntilBound (Dict.singleton ident varToApp) scopeBody
            in
            let newProgram =
              originalProgram
              |> replaceExpNode scopeBody.val.eid newScopeBody
              |> replaceExpNodePreservingPrecedingWhitespace pluckedBoundExp.val.eid abstractedFuncExp
            in
            newProgram
          in
          let abstractedOverAllConstantsResult =
            let newProgram = doAbstract shouldBeParameterIsConstant in
            synthesisResult ("Abstract " ++ ident ++ " over its constants") newProgram
          in
          let abstractedOverNamedUnfrozenConstantsResult =
            let newProgram = doAbstract shouldBeParameterIsNamedUnfrozenConstant in
            synthesisResult ("Abstract " ++ ident ++ " over its named unfrozen constants") newProgram
          in
          [ abstractedOverAllConstantsResult
          , abstractedOverNamedUnfrozenConstantsResult
          ]

        _ ->
          Debug.log "Can only abstract a PVar" []


abstractExp : EId -> Exp -> List SynthesisResult
abstractExp eidToAbstract originalProgram =
  let expToAbstract = justFindExpByEId originalProgram eidToAbstract in
  let doAbstract shouldBeParameter =
    let (argumentsForCallSite, abstractedFuncExp) =
      abstract eidToAbstract shouldBeParameter originalProgram
    in
    let funcName =
      let naiveName = expNameForEIdWithDefault "func" originalProgram eidToAbstract in
      let namesToAvoid = visibleIdentifiersAtEIds originalProgram (Set.singleton eidToAbstract) in
      nonCollidingName naiveName 2 namesToAvoid
    in
    let expToWrap =
      deepestAncestorWithNewline originalProgram eidToAbstract
    in
    let expToWrapWithTargetReplaced =
      expToWrap
      |> replaceExpNodePreservingPrecedingWhitespace eidToAbstract (eApp (eVar0 funcName) (argumentsForCallSite |> setExpListWhitespace " " " "))
    in
    let wrapped =
      let makeELet = if isTopLevelEId expToWrap.val.eid originalProgram then eDef else eLet in
      makeELet [(funcName, abstractedFuncExp)] expToWrapWithTargetReplaced
    in
    let newProgram =
      originalProgram
      |> replaceExpNodePreservingPrecedingWhitespace expToWrap.val.eid wrapped
    in
    newProgram
  in
  let abstractedOverAllConstantsResult =
    let newProgram = doAbstract shouldBeParameterIsConstant in
    synthesisResult ("Abstract " ++ (expToAbstract |> unparse |> Utils.squish |> Utils.niceTruncateString 20 "...") ++ " over its constants") newProgram
  in
  let abstractedOverNamedUnfrozenConstantsResult =
    let newProgram = doAbstract shouldBeParameterIsNamedUnfrozenConstant in
    synthesisResult ("Abstract " ++ (expToAbstract |> unparse |> Utils.squish |> Utils.niceTruncateString 20 "...") ++ " over its named unfrozen constants") newProgram
  in
  [ abstractedOverAllConstantsResult
  , abstractedOverNamedUnfrozenConstantsResult
  ]


------------------------------------------------------------------------------

-- TODO: relax addArg/removeArg/reorderArgs to allow (unsafe) addition/removal from anonymous functions (right now, written as if function must be named).

addArg_ : PathedPatternId -> (Exp -> Exp -> Maybe (String, Bool, Pat, Exp, Exp)) -> Exp -> List SynthesisResult
addArg_ pathedPatId funcToCaptionIsSafePatToInsertArgValExpAndNewFuncBody originalProgram =
  let ((funcEId, _), path) = pathedPatId in
  case findLetAndIdentBindingExp funcEId originalProgram of
    Just (letExp, funcName) ->
      case letExp.val.e__ of
        ELet ws1 letKind isRec letPat func letBody ws2 ->
          -- If func is passed to itself as an arg, this probably breaks. (is fixable though)
          let funcVarUsageEIds =
            if isRec
            then identifierUsageEIds funcName func ++ identifierUsageEIds funcName letBody |> Set.fromList
            else identifierUsageEIds funcName letBody |> Set.fromList
          in
          case func.val.e__ of
            EFun fws1 fpats fbody fws2 ->
              case funcToCaptionIsSafePatToInsertArgValExpAndNewFuncBody func fbody of
                Nothing ->
                  []

                Just (caption, bodyTransformationIsSafe, patToInsert, argValExp, newFBody) ->
                  case addPatToPats patToInsert path fpats of
                    Nothing ->
                      let _ = Debug.log ("Could not insert pattern into " ++ String.join " " (List.map unparsePat fpats) ++ " at path") path in
                      []

                    Just newFPats ->
                      let (newProgram, funcVarUsagesTransformed) =
                        originalProgram
                        |> replaceExpNodeE__ByEId func.val.eid (EFun fws1 newFPats newFBody fws2)
                        |> mapFoldExp
                            (\exp funcVarUsagesTransformed ->
                              case exp.val.e__ of
                                EApp appWs1 appFuncExp appArgs appWs2 ->
                                  if Set.member appFuncExp.val.eid funcVarUsageEIds then
                                    case addExpToExpsByPath (LangParser2.clearAllIds argValExp) path appArgs of
                                      Nothing ->
                                        (exp, funcVarUsagesTransformed)

                                      Just newAppArgs ->
                                        ( replaceE__ exp <| EApp appWs1 appFuncExp newAppArgs appWs2
                                        , Set.insert appFuncExp.val.eid funcVarUsagesTransformed
                                        )
                                  else
                                    (exp, funcVarUsagesTransformed)

                                _ ->
                                  (exp, funcVarUsagesTransformed)
                            )
                            Set.empty
                      in
                      let isSafe =
                        let argAdditionsSafe =
                          -- Ensure free vars in replacement still refer to the same thing after moving from callsite into function.
                          freeVars argValExp
                          |> List.all
                              (\freeVarInArgSource ->
                                let originalBindingScopeId = bindingScopeIdFor freeVarInArgSource originalProgram in
                                let freeIdentInArgSource = expToIdent freeVarInArgSource in
                                funcVarUsagesTransformed -- not exactly the same location as the free variable usage, but is always in the same scope based on the current requirements of the transformation
                                |> Set.toList
                                |> List.all
                                    (\funcVarUsageEId ->
                                      originalBindingScopeId == bindingScopeIdForIdentAtEId freeIdentInArgSource funcVarUsageEId newProgram
                                    )
                              )
                        in
                        let noDuplicateNamesInPat =
                          let newArgList = identifiersListInPats newFPats in
                          newArgList == Utils.dedup newArgList
                        in
                        bodyTransformationIsSafe
                        && funcVarUsagesTransformed == funcVarUsageEIds
                        && argAdditionsSafe
                        && noDuplicateNamesInPat
                      in
                      [ synthesisResult caption newProgram |> setResultSafe isSafe ]

            _ ->
              Debug.crash <| "CodeMotion.addArg_ should've had an EFun here"

        _ ->
          Debug.crash <| "CodeMotion.addArg_ expected findLetAndIdentBindingExp to return ELet"

    Nothing ->
      -- Can't find a name for this function. Arg addition probably unsafe.
      []


addArgs : List EId -> PathedPatternId -> Exp -> List SynthesisResult
addArgs argSourceEIds pathedPatId originalProgram =
  let (maybeNewProgram, isSafe) =
    argSourceEIds
    |> List.sortBy (locationInProgram originalProgram)
    |> List.foldr
        (\argSourceEId (maybePriorProgram, safeSoFar) ->
          case maybePriorProgram |> Maybe.map (addArg argSourceEId pathedPatId) of
            Just (SynthesisResult newResult :: _) -> (Just newResult.exp, safeSoFar && newResult.isSafe)
            _                                     -> (Nothing, False)
        )
        (Just originalProgram, True)
  in
  case maybeNewProgram of
    Just newProgram ->
      [ synthesisResult "Add Arguments" newProgram |> setResultSafe isSafe ]

    Nothing ->
      []

addArgsFromPats : List PathedPatternId -> PathedPatternId -> Exp -> List SynthesisResult
addArgsFromPats argSourcePathedPatIds pathedPatId originalProgram =
  let (maybeNewProgram, isSafe) =
    argSourcePathedPatIds
    |> List.sortBy (\((scopeEId, _), path) -> locationInProgram originalProgram scopeEId)
    |> List.foldr
        (\argSourcePathedPatId (maybePriorProgram, safeSoFar) ->
          -- Identity for post-processing fbody -- when adding multiple patterns, can't simplify assignments until the very end.
          case maybePriorProgram |> Maybe.map (addArgFromPat_ identity argSourcePathedPatId pathedPatId) of
            Just (SynthesisResult newResult :: _) -> (Just newResult.exp, safeSoFar && newResult.isSafe)
            _                                     -> (Nothing, False)
        )
        (Just originalProgram, True)
  in
  case maybeNewProgram of
    Just newProgram ->
      let newProgramWithAssignmentsSimplified =
        newProgram
        |> mapExpNode
            (pathedPatIdToScopeEId pathedPatId)
            LangSimplify.simplifyAssignments
      in
      [ synthesisResult "Add Arguments" newProgramWithAssignmentsSimplified |> setResultSafe isSafe ]

    Nothing ->
      []


addArgFromPat : PathedPatternId -> PathedPatternId -> Exp -> List SynthesisResult
addArgFromPat argSourcePathedPatId targetPathedPatId originalProgram =
  addArgFromPat_ LangSimplify.simplifyAssignments argSourcePathedPatId targetPathedPatId originalProgram


addArgFromPat_ : (Exp -> Exp) ->PathedPatternId -> PathedPatternId -> Exp -> List SynthesisResult
addArgFromPat_ postProcessFBody argSourcePathedPatId targetPathedPatId originalProgram =
  let funcToCaptionIsSafePatToInsertArgValExpAndNewFuncBody func fbody =
    case pluck argSourcePathedPatId fbody of
      Nothing ->
        let _ = Debug.log "could not pluck argument source pattern from inside the function" () in
        Nothing

      Just ((newArgPat, newArgVal), fbodyWithoutPlucked) ->
        let varUsagesSame =
          let oldScopeAreas = findScopeAreas (pathedPatIdToScopeId argSourcePathedPatId) fbody in
          identifiersListInPat newArgPat
          |> List.all
              (\ident ->
                identifierUses ident fbodyWithoutPlucked == List.concatMap (identifierUses ident) oldScopeAreas
              )
        in
        Just <|
          ( "Insert Argument " ++ (newArgPat |> unparsePat |> Utils.squish)
          , varUsagesSame
          , newArgPat
          , newArgVal
          , postProcessFBody fbodyWithoutPlucked
          )
  in
  addArg_ targetPathedPatId funcToCaptionIsSafePatToInsertArgValExpAndNewFuncBody originalProgram


addArg : EId -> PathedPatternId -> Exp -> List SynthesisResult
addArg argSourceEId pathedPatId originalProgram =
  let funcToCaptionIsSafePatToInsertArgValExpAndNewFuncBody func fbody =
    case findExpByEId fbody argSourceEId of
      Nothing ->
        let _ = Debug.log ("couldn't find argument source " ++ toString argSourceEId ++ " in function " ++ unparseWithIds func) () in
        Nothing

      Just argSourceExp ->
        let argName =
          let namesToAvoid = identifiersSet (replaceExpNode argSourceEId (eVar "DUMMY VAR") func) in
          nonCollidingName (expNameForEId originalProgram argSourceEId) 2 namesToAvoid
        in
        let patToInsert = pVar argName in -- Whitespace is going to be tricky.
        Just <|
          ( "Insert Argument " ++ argName
          , True
          , patToInsert
          , argSourceExp
          , replaceExpNodePreservingPrecedingWhitespace argSourceEId (eVar argName) fbody
          )
  in
  addArg_ pathedPatId funcToCaptionIsSafePatToInsertArgValExpAndNewFuncBody originalProgram


removeArgs : List PathedPatternId -> Exp -> List SynthesisResult
removeArgs pathedPatIds originalProgram =
  let (maybeNewProgram, isSafe) =
    pathedPatIds
    |> List.sort
    |> List.foldr
        (\pathedPatId (maybePriorProgram, safeSoFar) ->
          case maybePriorProgram |> Maybe.map (removeArg pathedPatId) of
            Just (SynthesisResult newResult :: _) -> (Just newResult.exp, safeSoFar && newResult.isSafe)
            _                                     -> (Nothing, False)
        )
        (Just originalProgram, True)
  in
  case maybeNewProgram of
    Just newProgram ->
      [ synthesisResult "Remove Arguments" newProgram |> setResultSafe isSafe ]

    Nothing ->
      []

removeArg : PathedPatternId -> Exp -> List SynthesisResult
removeArg pathedPatId originalProgram =
  let ((funcEId, _), path) = pathedPatId in
  case findLetAndIdentBindingExp funcEId originalProgram of
    Just (letExp, funcName) ->
      case letExp.val.e__ of
        ELet ws1 letKind isRec letPat func letBody ws2 ->
          -- If func is passed to itself as an arg, this probably breaks. (is fixable though)
          let funcVarUsageEIds =
            if isRec
            then identifierUsageEIds funcName func ++ identifierUsageEIds funcName letBody |> Set.fromList
            else identifierUsageEIds funcName letBody |> Set.fromList
          in
          case func.val.e__ of
            EFun fws1 fpats fbody fws2 ->
              case pluckPatFromPats path fpats of
                Just (pluckedPat, remainingArgPats) ->
                  let transformedApplicationsWithRemovedCallsiteArgument =
                    (if isRec then flattenExpTree func ++ flattenExpTree letBody else flattenExpTree letBody)
                    |> List.filterMap
                        (\exp ->
                          case exp.val.e__ of
                            EApp appWs1 appFuncExp appArgs appWs2 ->
                              if Set.member appFuncExp.val.eid funcVarUsageEIds then
                                pluckExpFromExpsByPath path appArgs
                                |> Maybe.map
                                    (\(pluckedExp, remainingArgs) ->
                                      let newAppArgs =
                                        if remainingArgPats == [] && appArgs /= [] -- appArgs should never be []
                                        then [ eTuple [] ]
                                        else remainingArgs
                                      in
                                      (exp.val.eid, replaceE__ exp (EApp appWs1 appFuncExp newAppArgs appWs2), appFuncExp.val.eid, pluckedExp)
                                    )
                              else
                                Nothing

                            _ ->
                              Nothing
                        )
                  in
                  case transformedApplicationsWithRemovedCallsiteArgument of
                    (_, _, _, argReplacementValue)::_ ->
                      let (newFBody, replacementLocationEIds) =
                        -- Either declare a new variable right inside the function body or inline all uses.
                        case (pluckedPat.val.p__, nodeCount argReplacementValue) of
                          (PVar _ argName _, 1) ->
                            -- Inline all uses.
                            let newFBody =
                              transformVarsUntilBound
                                  (Dict.singleton argName (\varExp -> LangParser2.clearAllIds argReplacementValue |> setEId varExp.val.eid)) -- need to preserve EId of replacement site for free var safety check below
                                  fbody
                            in
                            let replacementLocationEIds =
                              identifierUses argName fbody |> List.map (.val >> .eid) -- original body (eids same in new body)
                            in
                            (newFBody, replacementLocationEIds)

                          _ ->
                            let inlinedArgEId = LangParser2.maxId originalProgram + 1 in
                            let newFBody =
                              withDummyExpInfo <|
                                ELet (precedingWhitespace fbody) Let False
                                  (ensureWhitespacePat pluckedPat)
                                  (argReplacementValue |> LangParser2.clearAllIds |> setEId inlinedArgEId |> ensureWhitespaceExp)
                                  (ensureWhitespaceExp fbody) ""
                            in
                            let replacementLocationEIds = [ inlinedArgEId ] in
                            (newFBody, replacementLocationEIds)
                      in
                      let newProgram =
                        let eidToNewNode =
                          transformedApplicationsWithRemovedCallsiteArgument
                          |> List.map (\(eid, newApp, _, _) -> (eid, newApp))
                          |> Dict.fromList
                        in
                        let newArgPats =
                          if remainingArgPats == []
                          then [ pList0 [] ]
                          else remainingArgPats
                        in
                        originalProgram
                        |> replaceExpNodeE__ func (EFun fws1 newArgPats newFBody fws2)
                        |> replaceExpNodes eidToNewNode
                      in
                      let isSafe =
                        let allCallsitesTransformed =
                          let usagesTransformed =
                            transformedApplicationsWithRemovedCallsiteArgument
                            |> List.map (\(_, _, appFuncExpEId, _) -> appFuncExpEId)
                            |> Set.fromList
                          in
                          funcVarUsageEIds == usagesTransformed
                        in
                        let argReplacementSafe =
                          -- Ensure free vars in replacement still refer to the same thing after moving from callsite into function.
                          freeVars argReplacementValue
                          |> List.all
                              (\freeVarInReplacement ->
                                let originalBindingScopeId = bindingScopeIdFor freeVarInReplacement originalProgram in
                                let freeIdentInReplacement = expToIdent freeVarInReplacement in
                                replacementLocationEIds
                                |> List.all
                                    (\replacedEId ->
                                      originalBindingScopeId == bindingScopeIdForIdentAtEId freeIdentInReplacement replacedEId newProgram
                                    )
                              )
                        in
                        allCallsitesTransformed && argReplacementSafe
                      in
                      [ synthesisResult ("Remove Argument " ++ (pluckedPat |> unparsePat |> Utils.squish)) newProgram |> setResultSafe isSafe ]

                    _ ->
                      let _ = Debug.log "no uses to provide arg replacement value" transformedApplicationsWithRemovedCallsiteArgument in
                      []

                _ ->
                  let _ = Debug.log "cannot pluck argument" (path, fpats) in
                  []

            _ ->
              Debug.crash <| "CodeMotion.removeArg should've had an EFun here"

        _ ->
          Debug.crash <| "CodeMotion.removeArg expected findLetAndIdentBindingExp to return ELet"

    Nothing ->
      -- Can't find a name for this function. Arg removal probably unsafe.
      []


-- This is way nastier than I want it to be, but not sure how to make it
-- nicer and still support e.g. moving all of the variables outside of a
-- list and remove the empty list.
reorderFunctionArgs : EId -> List (List Int) -> List Int -> Exp -> List SynthesisResult
reorderFunctionArgs funcEId paths targetPath originalProgram =
  -- let ((funcEId, _), path) = pathedPatId in
  case findLetAndIdentBindingExp funcEId originalProgram of
    Just (letExp, funcName) ->
      case letExp.val.e__ of
        ELet ws1 letKind isRec letPat func letBody ws2 ->
          -- If func is passed to itself as an arg, this probably breaks. (is fixable though)
          let funcVarUsageEIds =
            if isRec
            then identifierUsageEIds funcName func ++ identifierUsageEIds funcName letBody |> Set.fromList
            else identifierUsageEIds funcName letBody |> Set.fromList
          in
          case func.val.e__ of
            EFun fws1 fpats fbody fws2 ->
              let (pluckedPats, fpatsAfterRemoved1, pathsRemoved1) =
                paths
                |> List.sort
                |> List.foldr
                    (\pathToRemove (pluckedPats, fpatsAfterRemoved, pathsRemoved) ->
                      case pluckPatFromPats pathToRemove fpatsAfterRemoved of
                        Just (pluckedPat, newFPatsAfterRemoved) ->
                          ( pluckedPat::pluckedPats, newFPatsAfterRemoved, pathToRemove::pathsRemoved )

                        _ -> (pluckedPats, fpatsAfterRemoved, pathsRemoved)
                    )
                    ([], fpats, [])
              in
              -- Adjust insert path based on all the paths removed
              let maybeInsertPath =
                pathsRemoved1
                |> Utils.foldrMaybe
                    pathAfterElementRemoved
                    (Just targetPath)
              in
              case maybeInsertPath of
                Nothing ->
                  let _ = Debug.log "can't insert at that path" (targetPath, fpats) in
                  []

                Just insertPath ->
                  let maybeFPatsAfterInsertion =
                    pluckedPats
                    |> Utils.foldrMaybe
                        (\pluckedPat newFPats -> addPatToPats pluckedPat insertPath newFPats)
                        (Just fpatsAfterRemoved1)
                  in
                  case maybeFPatsAfterInsertion of
                    Nothing ->
                      let _ = Debug.log "couldn't reorder patterns" (insertPath, fpatsAfterRemoved1) in
                      []

                    Just fpatsAfterInsertion ->
                      -- Now, trim out any empty plists created.
                      let pathsRemoved2 =
                        deadPathsInPats fpatsAfterInsertion
                      in
                      let newFPats =
                        pathsRemoved2
                        |> List.foldr
                            (justRemovePatFromPats "CodeMotion.reorderFunctionArgs shouldn't crash because we are removing paths known to exist")
                            fpatsAfterInsertion
                      in
                      let (newProgram, funcVarUsagesTransformed) =
                        originalProgram
                        |> replaceExpNodeE__ByEId func.val.eid (EFun fws1 newFPats fbody fws2)
                        |> mapFoldExp
                            (\exp funcVarUsagesTransformed ->
                              case exp.val.e__ of
                                EApp appWs1 appFuncExp appArgs appWs2 ->
                                  if Set.member appFuncExp.val.eid funcVarUsageEIds then
                                    -- 1. remove args from original locations
                                    -- 2. insert args into new locations
                                    -- 3. kill empty lists
                                    --
                                    -- 1. remove args from original locations
                                    let maybePluckedExpsAndExpsAfterRemoved1 =
                                      pathsRemoved1
                                      |> Utils.foldrMaybe
                                          (\pathToRemove (pluckedExps, remainingExps) ->
                                            pluckExpFromExpsByPath pathToRemove remainingExps
                                            |> Maybe.map (\(pluckedExp, remainingExps) -> (pluckedExp::pluckedExps, remainingExps))
                                          )
                                          (Just ([], appArgs))
                                    in
                                    -- 2. insert args into new locations
                                    case maybePluckedExpsAndExpsAfterRemoved1 of
                                      Just (pluckedExps, expsAfterRemoved1) ->
                                        let maybeExpsAfterInsertion =
                                          pluckedExps
                                          |> Utils.foldrMaybe
                                              (\pluckedExp newExps -> addExpToExpsByPath pluckedExp insertPath newExps)
                                              (Just expsAfterRemoved1)
                                        in
                                        -- 3. kill empty lists
                                        let maybeNewExps =
                                          let removeExpFromExpsByPath pathToRemove exps =
                                            pluckExpFromExpsByPath pathToRemove exps |> Maybe.map Tuple.second
                                          in
                                          pathsRemoved2
                                          |> Utils.foldrMaybe
                                              removeExpFromExpsByPath
                                              maybeExpsAfterInsertion
                                        in
                                        case maybeNewExps of
                                          Just newExps ->
                                            ( replaceE__ exp (EApp appWs1 appFuncExp newExps appWs2)
                                            , Set.insert appFuncExp.val.eid funcVarUsagesTransformed
                                            )

                                          Nothing ->
                                            (exp, funcVarUsagesTransformed)

                                      Nothing ->
                                        (exp, funcVarUsagesTransformed)

                                  else
                                    (exp, funcVarUsagesTransformed)

                                _ ->
                                  (exp, funcVarUsagesTransformed)
                            )
                            Set.empty
                      in
                      let isSafe =
                        funcVarUsageEIds == funcVarUsagesTransformed
                      in
                      [ synthesisResult "Reorder Arguments (unfinished)" newProgram |> setResultSafe isSafe ]

            _ ->
              Debug.crash <| "CodeMotion.reorderFunctionArgs should've had an EFun here"

        _ ->
          Debug.crash <| "CodeMotion.reorderFunctionArgs expected findLetAndIdentBindingExp to return ELet"

    Nothing ->
      -- Can't find a name for this function. Arg removal probably unsafe.
      []

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
        withDummyExpInfo <|
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
      case patTargetPositionToTargetPathedPatId patTarget of
        ((targetId, 1), targetPath) ->
          case findExpByEId m.inputExp targetId of
            Just scopeExp ->
              if isLet scopeExp then
                makeIntroduceVarTool_ m expIds targetId
                  (addNewEquationsInside targetPath)
              else
                []

            _ ->
              []

        _ ->
          []

makeIntroduceVarTool_ m expIds addNewVarsAtThisId addNewEquationsAt =
  let toolName =
    "Introduce Var" ++ (if List.length expIds == 1 then "" else "s")
  in
  let visibleAtTargetBody =
    let targetBodyEId =
      let exp = justFindExpByEId m.inputExp addNewVarsAtThisId in
      case exp.val.e__ of
        ELet _ _ _ _ _ e2 _ -> e2.val.eid
        _                   -> exp.val.eid
    in
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

type alias Literal =
  Either
    (LocId, (WS, Num, Loc, WidgetDecl)) -- from selectedNums
    (EId, (WS, EBaseVal))               -- from selectedBaseVals

literalEId literal =
  case literal of
    Left (eId, _)  -> eId
    Right (eId, _) -> eId

literalWS literal =
  case literal of
    Left (_, (ws,_,_,_)) -> ws
    Right (_, (ws,_))    -> ws

literalExp literal =
  case literal of
    Left (_,(_,firstNum,_,_)) -> eConst firstNum dummyLoc
    Right (_,(_,baseVal))     -> withDummyExpInfo (EBase " " baseVal)

makeMakeEqualTool m literals =
  let expIds = List.map literalEId literals in
  let firstLiteral = literalExp (Utils.head_ literals) in
  let targetLet =
    justInsideDeepestCommonScope m.inputExp (\e -> List.member e.val.eid expIds)
  in
  let targetId = targetLet.val.eid in
  let potentialNames =
    let
      names = Utils.dedupByEquality (List.map (expNameForEId m.inputExp) expIds)
      joinedNames = String.join "_" names
      commonName = commonNameForEIdsWithDefault joinedNames m.inputExp expIds
      namesToAvoid = visibleIdentifiersAtEIds m.inputExp (Set.singleton targetId)
    in
    commonName :: joinedNames :: names
    |> List.map (\name -> nonCollidingName name 1 namesToAvoid)
    |> Utils.dedupByEquality
    |> List.sortBy String.length
  in
  let results =
    potentialNames |> List.map (\newVar ->
      let newEquation = (newVar, firstLiteral) in
      let eSubst =
        List.foldl
           (\literal -> Dict.insert (literalEId literal) (EVar (literalWS literal) newVar))
           Dict.empty literals
      in
      let expWithNewVarUsed = applyESubst eSubst m.inputExp in
      let newExp =
        expWithNewVarUsed |> mapExp (\e ->
          if e.val.eid == targetId
          then addNewEquationsAround m.inputExp targetId [newEquation] e
          else e
        )
      in
      synthesisResult ("New variable: " ++ newVar) newExp
    )
  in
  [ ("Make Equal", \() -> results) ]


------------------------------------------------------------------------------

addToolCompareSubExpressions m selections =
  case selections of
    (_, _, [], _, _, _, _)          -> []
    (_, _, [_], _, _, _, _)         -> []
    ([], [], i::js, [], [], [], []) -> makeCompareSubExpressionsTool m i js
    _                               -> []

makeCompareSubExpressionsTool m firstId restIds =
  let expIds = firstId :: restIds in
  let firstExp = justFindExpByEId m.inputExp firstId in
  let restExps = List.map (justFindExpByEId m.inputExp) restIds in
  let diffs = List.concatMap (extraExpsDiff firstExp) restExps in
  if diffs == [] then
    -- a lot of duplication from makeIntroduceVarTool and makeMakeEqualTool...
    let name = expNameForEId m.inputExp firstId in
    let targetLet =
      justInsideDeepestCommonScope m.inputExp (\e -> List.member e.val.eid expIds)
    in
    let targetBodyEId = targetLet.val.eid in
    let namesToAvoid =
      visibleIdentifiersAtEIds m.inputExp (Set.singleton targetBodyEId)
    in
    let newVar = nonCollidingName name 1 namesToAvoid in
    let newEquation = (newVar, firstExp) in
    let expWithNewVarUsed =
      List.foldl
         (\eid -> replaceExpNodePreservingPrecedingWhitespace eid (eVar newVar))
         m.inputExp expIds
    in
    let newExp =
      mapExp (\e -> if e.val.eid == targetBodyEId
                    then addNewEquationsAround m.inputExp targetBodyEId [newEquation] e
                    else e
             ) expWithNewVarUsed
    in
    [ ("Eliminate Common Subexpression", \() ->
        newExp
        |> synthesisResult "Just Do It."
        |> setResultSafe True -- TODO
        |> Utils.singleton
      ) ]
  else
    [ ("Make Equal", \() ->
        (firstExp :: restExps) |> List.map (\exp ->
          let newExp =
            List.foldl
               (\eid -> replaceExpNodePreservingPrecedingWhitespace eid exp)
               m.inputExp expIds
          in
          newExp
          |> synthesisResult ("Copy expression: " ++ unparse exp)
          |> setResultSafe True -- TODO
        )
      ) ]

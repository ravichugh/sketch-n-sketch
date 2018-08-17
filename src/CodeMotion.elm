module CodeMotion exposing
  ( renamePat, renamePatByPId, renameVar
  , composeTransformations
  , swapUsages
  , moveDefinitionsPat, moveDefinitionsBeforeEId
  , moveEquationsBeforeEId
  , duplicateDefinitionsPat, duplicateDefinitionsBeforeEId
  , inlineDefinitions
  , abstractPVar, abstractExp, shouldBeParameterIsConstant, shouldBeParameterIsNamedUnfrozenConstant
  , removeArg, removeArgs, addArg, addArgFromPat, addArgs, addArgsFromPats, reorderFunctionArgs
  , pluckByPId
  , reorderExpressionsTransformation
  , introduceVarTransformation
  , makeEqualTransformation
  , copyExpressionTransformation
  , swapExpressionsTransformation
  , swapDefinitionsTransformation
  , rewriteOffsetTransformation
  , makeEIdVisibleToEIds, makeEIdVisibleToEIdsByInsertingNewBinding
  , liftLocsSoVisibleTo, copyLocsSoVisibleTo
  , resolveValueHoles
  )

import Lang exposing (..)
import LangTools exposing (..)
import LangUtils exposing (..)
import LangSimplify
import LangUnparser exposing (unparseWithIds, expsEquivalent, patsEquivalent, unparseWithUniformWhitespace)
import ElmParser as Parser
-- import DependenceGraph exposing
  -- (ScopeGraph, ScopeOrder(..), parentScopeOf, childScopesOf)
import InterfaceModel exposing
  ( Model, SynthesisResult(..)
  , synthesisResult, setResultSafe, mapResultSafe, oneSafeResult, isResultSafe, setResultDescription
  )
import LocEqn                        -- For twiddling
import Solver exposing (MathExp(..)) -- For twiddling
import Sync
import Syntax exposing (Syntax)
import Utils

import Dict exposing (Dict)
import Regex
import Set exposing (Set)
import EvalUpdate exposing (assignUniqueNames, visibleIdentifiersAtEIds, preludeIdentifiers, identifiersSetPlusPrelude)

type alias PatBoundExpIsRec = (Pat, Exp, Bool)

--------------------------------------------------------------------------------
-- Composition of Transformations
--------------------------------------------------------------------------------

composeTransformations : String -> List (Exp -> List SynthesisResult) -> Exp -> List SynthesisResult
composeTransformations finalCaption transformations originalProgram =
  transformations
  |> List.foldl
      (\transformation results ->
        results
        |> List.concatMap
            (\(SynthesisResult result) ->
              transformation (Parser.freshen result.exp) |> List.map (mapResultSafe ((&&) result.isSafe))
            )
      )
      [ synthesisResult "Original" originalProgram ]
  |> List.map (setResultDescription finalCaption)

--------------------------------------------------------------------------------
-- Swapping Usages
--------------------------------------------------------------------------------

swapUsages : PathedPatternId -> PathedPatternId -> Exp -> List SynthesisResult
swapUsages (scopeId1, path1) (scopeId2, path2) originalProgram =
  case (LangTools.findScopeExpAndPatByPathedPatternId (scopeId1, path1) originalProgram, LangTools.findScopeExpAndPatByPathedPatternId (scopeId2, path2) originalProgram) of
    (Just (scopeExp1, pat1), Just (scopeExp2, pat2)) ->
      case (LangTools.patToMaybeIdent pat1, LangTools.patToMaybeIdent pat2) of
        (Just name1, Just name2) ->
          let eidToBindingPId =
            LangTools.allVarEIdsToBindingPId originalProgram
          in
          let newProgram =
            originalProgram
            |> mapExp
                (\exp ->
                  case (Dict.get exp.val.eid eidToBindingPId, exp.val.e__) of
                    (Just (Just pid), EVar ws _) ->
                      if pid == pat1.val.pid then
                        replaceE__ exp (EVar ws name2)
                      else if pid == pat2.val.pid then
                        replaceE__ exp (EVar ws name1)
                      else
                        exp
                    _ ->
                      exp
                )
          in
          let isSafe =
            let expectedVarEIdsToBindingPId =
              eidToBindingPId
              |> Dict.map
                  (\eid maybePId ->
                    if maybePId == Just pat1.val.pid then
                      Just pat2.val.pid
                    else if maybePId == Just pat2.val.pid then
                      Just pat1.val.pid
                    else
                      maybePId
                  )
            in
            LangTools.allVarEIdsToBindingPId newProgram == expectedVarEIdsToBindingPId
          in
          let result =
            synthesisResult ("Swap usages of " ++ name1 ++ " and " ++ name2) newProgram |> setResultSafe isSafe
          in
          [result]

        _ -> []

    _ -> []

--------------------------------------------------------------------------------
-- Renaming
--------------------------------------------------------------------------------

renamePatByPId : PId -> String -> Exp -> List SynthesisResult
renamePatByPId pid newName program =
  pidToPathedPatternId program pid
  |> Maybe.map (\ppid -> renamePat ppid newName program)
  |> Maybe.withDefault []


renamePat : PathedPatternId -> String -> Exp -> List SynthesisResult
renamePat (scopeId, path) newName program =
  case LangTools.findScopeExpAndPatByPathedPatternId (scopeId, path) program of
    Just (scopeExp, pat) ->
      case LangTools.patToMaybeIdent pat of
        Just oldName ->
          let scopeAreas = LangTools.findScopeAreas scopeId scopeExp in -- ScopeAreas are the outermost exp in which the variable might appear.
          let oldUseEIds = List.concatMap (LangTools.identifierUses oldName) scopeAreas |> List.map (.val >> .eid) in
          let newScopeAreas = List.map (LangTools.renameVarUntilBound oldName newName) scopeAreas in
          let newUseEIds = List.concatMap (LangTools.identifierUses newName) newScopeAreas |> List.map (.val >> .eid) in
          let isSafe =
            oldUseEIds == newUseEIds && not (List.member newName (Lang.identifiersListInPat pat))
          in
          let newScopeExp =
            let scopeAreasReplaced =
               newScopeAreas
               |> List.foldl
                   (\newScopeArea scopeExp -> replaceExpNode newScopeArea.val.eid newScopeArea scopeExp)
                   scopeExp
            in
            LangTools.setPatName (scopeId, path) newName scopeAreasReplaced
          in
          let newProgram = replaceExpNode newScopeExp.val.eid newScopeExp program in
          let result =
            let
               descriptionStart =
                 "Rename '" ++ oldName ++ "' to"
               descriptionEnd =
                 if String.isEmpty newName then
                   "..."
                 else
                   " '" ++ newName ++ "'"
               description =
                 descriptionStart ++ descriptionEnd
            in
            synthesisResult description newProgram |> setResultSafe isSafe
          in
          [result]

        Nothing ->
          []

    Nothing ->
      []

renameVar : EId -> String -> Exp -> List SynthesisResult
renameVar varEId newName program =
  let varExp = LangTools.justFindExpByEId program varEId in
  let oldName = LangTools.expToIdent varExp in
  case LangTools.bindingPathedPatternIdFor varExp program of
    Just pathedPatternId ->
      renamePat pathedPatternId newName program

    Nothing ->
      let _ = Debug.log (oldName ++ " is free at this location in the program") () in
      []

--------------------------------------------------------------------------------

pluckAll : List PathedPatternId -> Exp -> (List (Pat, Exp, Bool), Exp)
pluckAll sourcePathedPatIds program =
    let sortedSourcePathedPatIds =
      sourcePathedPatIds
      |> List.sortBy
          (\((scopeEId, branchI), path) ->
            (locationInProgram program scopeEId, branchI, path)
          )
    in
  let (pluckedPatAndBoundExpAndIsRecs, programWithoutPlucked) =
    sortedSourcePathedPatIds
    |> List.foldr
        (\sourcePathedPatId (pluckedPatAndBoundExps, programBeingPlucked) ->
          case pluck sourcePathedPatId programBeingPlucked of
            Just ((pluckedPat, pluckedBoundExp, isRec), programWithoutPlucked) ->
              ((pluckedPat, pluckedBoundExp, isRec)::pluckedPatAndBoundExps, programWithoutPlucked)
            Nothing ->
              (pluckedPatAndBoundExps, programBeingPlucked)
        )
        ([], program)
  in
  (pluckedPatAndBoundExpAndIsRecs, programWithoutPlucked)


pluckByPId : PId -> Exp -> Maybe (PatBoundExpIsRec, Exp)
pluckByPId pid program =
  pidToPathedPatternId program pid
  |> Maybe.andThen (\ppid -> pluck ppid program)


-- Removes the binding (p, e1) from the program, returns it and the program without with binding.
--
-- If this would remove a whole pattern, the pattern is left as [] matched to [] for a later clean up step.
--
-- This technique preserves EIds and pattern paths for later insertion.
pluck : PathedPatternId -> Exp -> Maybe (PatBoundExpIsRec, Exp)
pluck ((scopeEId, scopeBranchI), path) program =
  findExpByEId program scopeEId
  |> Utils.filterMaybe isLet
  |> Maybe.andThen (\scope -> pluck_ scope path program)


pluck_ : Exp -> List Int -> Exp -> Maybe (PatBoundExpIsRec, Exp)
pluck_ scopeExp path program =
  let (maybePluckedAndNewPatAndBoundExp, (ws1, letKind, wsP, pat, fs, ws2, e1, ws3, e2), isRec) =
    case scopeExp.val.e__ of
       ELet _ _ (Declarations _ tpes anns [(isRec, [LetExp _ _ p _ _ boundExp])]) _ _ ->
         (pluck__ p boundExp path, expToLetParts scopeExp, isRec)
       _                                                                              ->
         Debug.crash <| "pluck_: bad Exp__ (note: case branches, and func args not supported) " ++ unparseWithIds scopeExp
  in
  case maybePluckedAndNewPatAndBoundExp of
    Nothing ->
      Nothing

    Just ((pluckedPat, pluckedBoundExp), newPat, newBoundExp) ->
      Just <|
        ( (pluckedPat, pluckedBoundExp, isBodyPossiblyRecursive pluckedBoundExp)
        , replaceExpNodeE__ scopeExp (
            ELet ws1 letKind (Declarations [0] [] [] [(isRec, [LetExp Nothing wsP newPat fs ws2 newBoundExp])]) ws3 e2) program
        )


pluck__ : Pat -> Exp -> List Int -> Maybe ((Pat, Exp), Pat, Exp)
pluck__ p e1 path =
  case (p.val.p__, e1.val.e__, path) of
    (_, _, []) ->
      Just <|
        ( (p, e1)
        , replaceP__ p   <| PVar (ws <| precedingWhitespacePat p) "*RemoveMe*" noWidgetDecl -- Can't leave behind a [] because an insert with path [2] right after pluck would treat [] as a list to enter rather than an atom to insert behind
        , replaceE__ e1 <| EList (ws <| precedingWhitespace e1)   [] space0 Nothing space0
        )

    (PAs _ childPat1 _ childPat2, _, i::is) ->
      -- TODO: allow but mark unsafe if as-pattern is used
      let _ = Debug.log "can't pluck out of as-pattern yet (unsafe)" () in
      Nothing

    ( PList pws1 ps pws2 maybePTail pws3
    , EList ews1 es ews2 maybeETail ews3
    , i::is
    ) ->
      if List.length ps >= i && List.length es >= i then
        let pi = Utils.geti i ps in
        let ei = Utils.geti i (Utils.listValues es) in
        pluck__ pi ei is
        |> Maybe.map
            (\(plucked, newPat, newBoundExp) ->
              let (newPs, newEs) =
                ( Utils.replacei i newPat ps
                , Utils.replacei i newBoundExp (Utils.listValues es)
                )
              in
              ( plucked
              , replaceP__ p  <| PList pws1 newPs pws2 maybePTail pws3
              , replaceE__ e1 <| EList ews1 (Utils.listValuesMake es newEs) ews2 maybeETail ews3
              )
            )
      else if List.length ps == List.length es && i == 1 + List.length ps && Utils.maybeToBool maybePTail && Utils.maybeToBool maybeETail then
        -- Recursing into the tail binding
        let pi = Utils.fromJust_ "CodeMotion1" maybePTail in
        let ei = Utils.fromJust_ "CodeMotion2" maybeETail in
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
      let _ = Debug.log ("pluck_: bad pattern " ++ Syntax.patternUnparser Syntax.Elm p) path in
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
      PWildcard _  -> []
      PConst _ _   -> Debug.log "why do you put constants in your function arguments?!" []
      PBase  _ _   -> Debug.log "why do you put base vals in your function arguments?!"[]

      PAs _ _ _ _ ->
        -- plucking out of as-pattern is generally unsafe (not allowed yet)
        -- so we shouldn't be creating dead paths inside as-patterns
        []

      PParens _ p _ ->
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

      PRecord ws1 ps ws2 ->
        deadPathsInPats (Utils.recordValues ps)
      PColonType _ _ _ _ -> []

-- Returns Maybe (pluckedPat, patsWithoutPlucked)
pluckPatFromPats : List Int -> List Pat -> Maybe (Pat, List Pat)
pluckPatFromPats path pats =
  case path of
    i::is ->
      Utils.maybeGeti1 i pats
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

    (PAs ws1 p1 ws2 p2, 1::is) ->
      let _ = Debug.log "plucking out of as-pattern is generally unsafe (not allowed yet)" () in
      Nothing

    -- (PAs ws1 wsi ident ws2 p, 1::is) ->
    --   let result = pluckPat is p in
    --   case result of
    --     Just (pluckedPat, Just remainingPat) ->
    --       Just (pluckedPat, Just <| replaceP__ pat (PAs ws1 wsi ident ws2 remainingPat))
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
      Utils.maybeGeti1 i exps
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
        pluckExpFromExpsByPath (i::is) (Utils.listValues es)
        |> Maybe.map
            (\(pluckedExp, remainingExps) ->
              (pluckedExp, Just <| replaceE__ exp (EList ws1 (Utils.listValuesMake (Utils.tail_ es) remainingExps) ws2 maybeTail ws3))
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

removeNoopResults : Exp -> List SynthesisResult -> List SynthesisResult
removeNoopResults originalProgram results =
  let originalUnparsed = unparseWithUniformWhitespace True True originalProgram in
  results
  |> List.filter -- Ignore results equivalent to original program.
      (\(SynthesisResult result) ->
        originalUnparsed /= unparseWithUniformWhitespace True True result.exp
      )

------------------------------------------------------------------------------

programOriginalNamesAndMaybeRenamedLiftedTwiddledResults
  baseDescription
  uniqueNameToOldName
  maybeNewScopeEId
  (touchedAdjective, untouchedAdjective)
  namesUniqueTouched
  varEIdsPreviouslyDeliberatelyRemoved
  insertedVarEIdToBindingPId
  originalProgramUniqueNames
  newProgramUniqueNames
  =
  let newProgramOriginalNamesResult =
    let newProgramOriginalNames = renameIdentifiers uniqueNameToOldName newProgramUniqueNames in
    [ makeResult
          baseDescription
          uniqueNameToOldName
          [] [] [] [] -- renamings liftedUniqueIdents identsInvalidlyFreeRewritten identsWithInvalidlyFreeVarsHandled
          varEIdsPreviouslyDeliberatelyRemoved
          insertedVarEIdToBindingPId
          originalProgramUniqueNames
          newProgramOriginalNames
    ]
  in
  let newProgramMaybeRenamedLiftedTwiddledResults =
    if isResultSafe (Utils.head "CodeMotion.programOriginalNamesAndMaybeRenamedLiftedTwiddledResults" newProgramOriginalNamesResult) then
      []
    else
      tryResolvingProblemsAfterTransform
          baseDescription
          uniqueNameToOldName
          maybeNewScopeEId
          (touchedAdjective, untouchedAdjective)
          namesUniqueTouched
          varEIdsPreviouslyDeliberatelyRemoved
          insertedVarEIdToBindingPId
          originalProgramUniqueNames
          newProgramUniqueNames
  in
  let originalProgram = renameIdentifiers uniqueNameToOldName originalProgramUniqueNames in
  newProgramOriginalNamesResult ++ newProgramMaybeRenamedLiftedTwiddledResults
  |> Utils.dedupBy (\(SynthesisResult {exp}) -> unparseWithUniformWhitespace False False exp)
  |> removeNoopResults originalProgram


-- Precondition: program has been run through assignUniqueNames
-- Also returns a dictionary of any identifiers lifted
liftDependenciesBasedOnUniqueNames : Exp -> (Exp, List Ident)
liftDependenciesBasedOnUniqueNames program =
  let needToLift =
    Set.diff (freeIdentifiers program) preludeIdentifiers
    |> Set.toList
  in
  let safeToLift =
    -- Don't lift idents that would result in cyclically lifting forever.
    --
    -- (def x z)
    -- (def y x)
    -- (def z y)
    --
    -- Or: (def x (+ x 1))
    -- Or: (def x x)
    --
    -- This isn't guarenteed to catch all cycles until LangTools.tryMatchExpReturningList is
    -- rewritten to match everything pluckable (rather than requiring a complete match)
    --
    -- Should catch pretty much all practical cases, however.
    let identToDependentIdents =
      -- Works because we know names are unique.
      program
      |> allSimplyResolvableLetBindings
      |> List.map (\(ident, boundExp) -> (ident, freeIdentifiers boundExp |> Set.toList))
      |> Dict.fromList
    in
    let hasCyclicDependencies seenIdents ident =
      if Set.member ident seenIdents then
        True
      else
        case Dict.get ident identToDependentIdents of
          Just dependsOnIdents -> dependsOnIdents |> List.any (hasCyclicDependencies (Set.insert ident seenIdents))
          Nothing              -> False
    in
    needToLift
    |> List.filter (not << hasCyclicDependencies Set.empty)
  in
  let bringIdentIntoScope identToLift =
    let maybeOriginalDefiningScope =
      program
      |> mapFirstSuccessNode (\e ->
        expToMaybeLetPat e |>
        Maybe.andThen (\listPat ->
          Utils.zipWithIndex listPat |>
          Utils.mapFirstSuccess (\(p, i) ->
            if identifiersListInPat p |> List.member identToLift then
              Just (e, p, i)
            else Nothing)))
    in
    case maybeOriginalDefiningScope of
      Nothing -> Nothing
      Just (originalDefiningScope, pat, letexpIndex) ->
        case pathForIdentInPat identToLift pat of
          Nothing -> Nothing
          Just path ->
            case pluck ((originalDefiningScope.val.eid, letexpIndex), path) program of
              Nothing -> Nothing
              Just ((pluckedPat, pluckedBoundExp, isRec), programWithoutPlucked) ->
                let eidToWrap = deepestCommonAncestorWithNewline program (expToMaybeIdent >> (==) (Just identToLift)) |> .val |> .eid in
                let insertedLetEId = Parser.maxId program + 1 in
                let newProgram =
                  programWithoutPlucked
                  |> mapExpNode
                      eidToWrap
                      (\expToWrap ->
                        newLetFancyWhitespace insertedLetEId isRec pluckedPat pluckedBoundExp expToWrap programWithoutPlucked
                      )
                in
                Just (newProgram, identToLift)
  in
  -- Look for an identifier we can lift
  let maybeNewProgramAndMovedIdent =
    safeToLift |> Utils.mapFirstSuccess bringIdentIntoScope
  in
  case maybeNewProgramAndMovedIdent of
    Nothing ->
      -- no more dependencies we can lift
      ( LangSimplify.simplifyAssignments program, [] )

    Just (newProgram, movedIdent) ->
      -- Try to lift some more!
      let (finalProgram, movedIdents) =
        liftDependenciesBasedOnUniqueNames newProgram
      in
      ( finalProgram, movedIdent::movedIdents )


-- Precondition: program has been run through assignUniqueNames
--
-- Not the best algorithm, but it should work for:
--
-- width = 50
-- x1 = 100
-- x2 = x1 + width
--
--   ⇕
--
-- x1 = 100
-- x2 = x1 + 50
-- width = x2 - x1
--
-- There's probably tons of strange cases where this produces undesired results.
--
-- Algorithm:
-- 1. Identify all numeric variables.
-- 2. Remember which defs had invalid numeric free variables, and where those variables are now defined.
-- 3. Repeatedly inline all invalid numeric free variables until convergence. (Simplify operations on constants.)
-- 4. From the end of the program (by new definition location of ident that is somewhere used free invalidly), for each tuple of (variable used free, ident defined in terms of invalid free var, corresponding bound exp where used free):
--    1. Do nothing if either ident already handled in this loop
--    2. Redefine the ident that is somewhere used invalidly based on that definition using it as an invalid free var. (Simplify.)
maybeSatisfyUniqueNamesDependenciesByTwiddlingArithmetic : Exp -> Maybe (Exp, List Ident, List Ident)
maybeSatisfyUniqueNamesDependenciesByTwiddlingArithmetic programUniqueNames =
  -- 1. Identify all numeric variables.
  let numericIdents = numericLetBoundIdentifiers programUniqueNames in
  -- 2. Remember which defs had invalid numeric free variables, and where those variables are now defined.
  let allFreeVars = freeVars programUniqueNames in
  let simpleLetBindings =
    allSimplyResolvableLetBindings programUniqueNames
    |> Dict.fromList
  in
  let defsWithInvalidFreeNumericVars =
    simpleLetBindings
    |> Dict.filter
        (\ident boundExp ->
          freeVars boundExp
          |> List.any (\varExp -> Set.member (expToIdent varExp) numericIdents && List.member varExp allFreeVars)
        )
  in
  let identsOriginallySomewhereInvalidlyFreeWithDefWhereUsedInvalidly =
    defsWithInvalidFreeNumericVars
    |> Dict.toList
    |> List.concatMap
        (\(ident, boundExp) ->
          freeVars boundExp
          |> List.filter (\varExp -> Set.member (expToIdent varExp) numericIdents && List.member varExp allFreeVars)
          |> List.map (\invalidlyUsedVarExp -> (expToIdent invalidlyUsedVarExp, ident, boundExp))
        )
  in
  -- 3. Repeatedly inline all invalid numeric free variables until convergence. (Simplify operations on constants.)
  let inlineInvalidFreeNumericIdentsUntilConvergence program =
    -- We are inlining definitions, which will duplicate EIds etc. so need to freshen every time.
    -- (In particular, EIds are need for List.member exp allFreeVars)
    let freshened = Parser.freshen program in
    -- let _ = Debug.log ("initial inlining:\n" ++ unparseWithIds freshened) () in
    let allFreeVars = freeVars freshened in
    -- TODO: Inlining could leave us in a worse situation than before.
    let (programInlinedOnce, somethingHappened) =
      freshened
      |> mapFoldExp -- Bottom up version.
          (\exp somethingHappened ->
            case expToMaybeIdent exp of
              Nothing -> (exp, somethingHappened)
              Just ident ->
                -- Don't duplicate any existing EIds when inlining: don't want them clobbered when freshening.
                if Set.member ident numericIdents && List.member exp allFreeVars
                then (Utils.justGet_ "maybeSatisfyUniqueNamesDependenciesByTwiddlingArithmetic" ident simpleLetBindings |> Parser.clearAllIds |> copyPrecedingWhitespace exp, True)
                else (exp, somethingHappened)
          )
          False
    in
    if somethingHappened
    then inlineInvalidFreeNumericIdentsUntilConvergence programInlinedOnce
    else freshened
  in
  -- Need to give a number to each identifier.
  -- Hmmm. Could also use pid of ident's defining pattern.
  let identToVarId =
    identifiersSetPlusPrelude programUniqueNames
    |> Set.toList
    |> Utils.zipi1
    |> List.map Utils.flip
    |> Dict.fromList
  in
  let expToMaybeMathExp exp =
    case exp.val.e__ of
      EConst _ n _ _      -> Just (MathNum n)
      EVar _ ident        -> Dict.get ident identToVarId |> Maybe.map MathVar
      EOp _ _ op operands _ ->
        case op.val of
          Plus  -> operands |> List.map expToMaybeMathExp |> Utils.projJusts |> Maybe.map (MathOp Plus)
          Minus -> operands |> List.map expToMaybeMathExp |> Utils.projJusts |> Maybe.map (MathOp Minus)
          Mult  -> operands |> List.map expToMaybeMathExp |> Utils.projJusts |> Maybe.map (MathOp Mult)
          Div   -> operands |> List.map expToMaybeMathExp |> Utils.projJusts |> Maybe.map (MathOp Div)
          _     -> Nothing

      ELet _ _ _ _ body         -> expToMaybeMathExp body
      EColonType _ e _ _ _      -> expToMaybeMathExp e
      _                         -> Nothing
  in
  let mathExpToExp mathExp =
    LocEqn.mathExpToExp unann Dict.empty (Utils.flipDict identToVarId) mathExp
  in
  let inlinedSimplifiedProgram =
    let inlinedProgram = inlineInvalidFreeNumericIdentsUntilConvergence programUniqueNames in
    let boundEIdsToSimplify =
      allSimplyResolvableLetBindings inlinedProgram
      |> List.filterMap
          (\(ident, e) ->
            Dict.get ident defsWithInvalidFreeNumericVars
            |> Maybe.map (\_ -> e.val.eid)
          )
      |> Set.fromList
    in
    inlinedProgram
    |> mapExp
        (\exp ->
          if Set.member exp.val.eid boundEIdsToSimplify then
            case expToMaybeMathExp exp of
              -- TODO: constant annotations thrown away (can't always be helped, but trivial cases should be saved)
              Just mathExp -> LocEqn.normalizeSimplify mathExp |> mathExpToExp |> copyPrecedingWhitespace exp
              Nothing     -> exp
          else
            exp
        )
    |> Parser.freshen
  in
  -- let _ = Debug.log ("inlined simplified:\n" ++ unparseWithIds inlinedSimplifiedProgram) () in
  -- 4. From the end of the program (by new definition location of ident that is somewhere used free invalidly), for each tuple of (variable used free, ident defined in terms of invalid free var, corresponding bound exp where used free):
  --    1. Do nothing if either ident already handled in this loop
  --    2. Redefine the ident that is somewhere used invalidly based on that definition using it as an invalid free var. (Simplify.)
  let (twiddledProgram, identsInvalidlyFreeRewritten, identsWithInvalidlyFreeVarsHandled) =
    identsOriginallySomewhereInvalidlyFreeWithDefWhereUsedInvalidly
    |> List.sortBy
        (\(identInvalidlyFree, identOfDefWhereUsedInvalidly, _) ->
          ( Utils.justGet_ "maybeSatisfyUniqueNamesDependenciesByTwiddlingArithmetic maybeTwiddledProgram sortBy1" identInvalidlyFree simpleLetBindings           |> expToLocation
          , Utils.justGet_ "maybeSatisfyUniqueNamesDependenciesByTwiddlingArithmetic maybeTwiddledProgram sortBy2" identOfDefWhereUsedInvalidly simpleLetBindings |> expToLocation
          )
        )
    |> List.foldr
        (\(identInvalidlyFree, identOfDefWhereUsedInvalidly, boundExpWhereUsedInvalidly) (program, identsInvalidlyFreeRewritten, identsWithInvalidlyFreeVarsHandled) ->
          let noChange = (program, identsInvalidlyFreeRewritten, identsWithInvalidlyFreeVarsHandled) in
          if Set.member identInvalidlyFree identsInvalidlyFreeRewritten || Set.member identOfDefWhereUsedInvalidly identsWithInvalidlyFreeVarsHandled then
            noChange
          else
            case expToMaybeMathExp boundExpWhereUsedInvalidly of
              Nothing  -> noChange
              Just rhs ->
                let lhs = expToMaybeMathExp (eVar identOfDefWhereUsedInvalidly) |> Utils.fromJust_ "maybeSatisfyUniqueNamesDependenciesByTwiddlingArithmetic expToMaybeMathExp (eVar identOfDefWhereUsedInvalidly)" in
                let locIdInvalidlyFree = Utils.justGet_ "maybeSatisfyUniqueNamesDependenciesByTwiddlingArithmetic Utils.justGet_ identInvalidlyFree identToVarId" identInvalidlyFree identToVarId in
                -- TODO: Explore all options non-deterministically.
                case LocEqn.solveForLocUnchecked locIdInvalidlyFree Dict.empty lhs rhs |> Maybe.map mathExpToExp of
                  Nothing     -> noChange
                  Just invalidlyFreeIdentBoundExpNew ->
                    let simpleLetBindings = allSimplyResolvableLetBindings program |> Dict.fromList in
                    let invalidlyFreeIdentBoundExpOld =
                      simpleLetBindings
                      |> Utils.justGet_ "maybeSatisfyUniqueNamesDependenciesByTwiddlingArithmetic invalidlyFreeIdentBoundExpOld" identInvalidlyFree
                    in
                    let boundExpWhereUsedInvalidlyPartiallyReduced =
                      simpleLetBindings
                      |> Utils.justGet_ "maybeSatisfyUniqueNamesDependenciesByTwiddlingArithmetic boundExpWhereUsedInvalidlyPartiallyReduced" identOfDefWhereUsedInvalidly
                    in
                    let boundExpWhereUsedInvalidlyReduced =
                      -- Inline/simplify any variables in boundExpWhereUsedInvalidly that appear in invalidlyFreeIdentBoundExpNew that were not there before in invalidlyFreeIdentBoundExpOld.
                      -- Intuition is that we are trying not to change the number of variable uses in the program.
                      -- May actually want to do this at all locations that used an ident invalidly, rather than just the location we used for solving.
                      let identsToReduce =
                        Set.diff (identifiersSet invalidlyFreeIdentBoundExpNew) (identifiersSet invalidlyFreeIdentBoundExpOld)
                        |> Set.intersect numericIdents
                      in
                      let inlineUntilConvergence boundExpWhereUsedInvalidlyPartiallyReduced =
                        -- Nothing with EIds here, don't need to freshen every time.
                        -- TODO: Inlining could leave us in a worse situation than before.
                        let (inlinedOnce, somethingHappened) =
                          boundExpWhereUsedInvalidlyPartiallyReduced
                          |> mapFoldExp -- Bottom up version.
                              (\exp somethingHappened ->
                                case expToMaybeIdent exp of
                                  Nothing -> (exp, somethingHappened)
                                  Just ident ->
                                    if Set.member ident identsToReduce
                                    then (Utils.justGet_ "maybeSatisfyUniqueNamesDependenciesByTwiddlingArithmetic inlineUntilConvergence" ident simpleLetBindings |> Parser.clearAllIds |> copyPrecedingWhitespace exp, True)
                                    else (exp, somethingHappened)
                              )
                              False
                        in
                        if somethingHappened
                        then inlineUntilConvergence inlinedOnce
                        else boundExpWhereUsedInvalidlyPartiallyReduced
                      in
                      let inlined = inlineUntilConvergence boundExpWhereUsedInvalidlyPartiallyReduced in
                      -- let _ = Debug.log ("reduced:\n" ++ unparseWithIds inlined) () in
                      let inlinedSimplified =
                        if inlined == boundExpWhereUsedInvalidlyPartiallyReduced then
                          inlined
                        else
                          case expToMaybeMathExp inlined of
                            -- TODO: constant annotations thrown away (can't always be helped, but trivial cases should be saved)
                            Just mathExp -> LocEqn.normalizeSimplify mathExp |> mathExpToExp
                            Nothing      -> inlined
                      in
                      -- let _ = Debug.log ("simplified:\n" ++ unparseWithIds inlinedSimplified) () in
                      inlinedSimplified
                    in
                    let newProgram =
                      program
                      |> replaceExpNodePreservingPrecedingWhitespace boundExpWhereUsedInvalidlyPartiallyReduced.val.eid boundExpWhereUsedInvalidlyReduced
                      |> replaceExpNodePreservingPrecedingWhitespace invalidlyFreeIdentBoundExpOld.val.eid invalidlyFreeIdentBoundExpNew
                      |> Parser.freshen
                    in
                    -- let _ = Debug.log ("new program:\n" ++ unparseWithIds newProgram) () in
                    (newProgram, Set.insert identInvalidlyFree identsInvalidlyFreeRewritten, Set.insert identOfDefWhereUsedInvalidly identsWithInvalidlyFreeVarsHandled)

        )
        (inlinedSimplifiedProgram, Set.empty, Set.empty)
  in
  if Set.size identsInvalidlyFreeRewritten > 0
  then Just (twiddledProgram, identsInvalidlyFreeRewritten |> Set.toList, identsWithInvalidlyFreeVarsHandled |> Set.toList)
  else Nothing


-- It's possible that, for our purposes, it may be sufficient
-- to determine all intended variable references from the new program
-- with unique names. Assume inserted and removed patterns and variables
-- are implicitly correct, and then check to make sure there are no
-- odd free variables and that the vars in the final program resolve to
-- the same locations as the vars in the new program with unique names.
-- (requires that these programs match structurally/freshen similarly).
--
-- TODO: verify this is correct and simplify the below if so.
makeResult
    :  String
    -> Dict String Ident
    -> List ( String, Ident, Ident )
    -> List Ident
    -> List Ident
    -> List Ident
    -> List EId
    -> Dict EId (Maybe PId)
    -> Exp
    -> Exp
    -> SynthesisResult
makeResult
    baseDescription
    uniqueNameToOldName
    renamings -- As a list of (description, oldName, newName), e.g. ("touched", "x", "x2")
    liftedUniqueIdents
    identsInvalidlyFreeRewritten
    identsWithInvalidlyFreeVarsHandled
    varEIdsDeliberatelyRemoved
    insertedVarEIdToBindingPId
    originalProgramUniqueNames
    newProgram =
  let uniqueNameToOldNameUsed =
    Dict.diff uniqueNameToOldName (List.map (\(desc, oldName, newName) -> (newName, oldName)) renamings |> Dict.fromList)
  in
  let isSafe =
    let originalVarRefs = allVarEIdsToBindingPIdList originalProgramUniqueNames in
    let newVarRefs      = allVarEIdsToBindingPIdList newProgram in
    let allOldReferencesSame =
      originalVarRefs
      |> List.all
          (\(oldVarEId, maybeOldPid) ->
            List.member oldVarEId varEIdsDeliberatelyRemoved
            || Utils.equalAsSets (List.filter (Tuple.first >> (==) oldVarEId) newVarRefs) [(oldVarEId, maybeOldPid)]
            -- || Debug.log (toString (oldVarEId, maybeOldPid) ++ unparseWithIds originalProgramUniqueNames ++ unparseWithIds newProgram) False
          )
      -- |> Debug.log "allOldReferencesSame"
    in
    let allNewReferencesGood =
      let apparentlyInsertedVarRefs = Utils.diffAsSet newVarRefs originalVarRefs in
      Utils.equalAsSets apparentlyInsertedVarRefs (Dict.toList insertedVarEIdToBindingPId)
      -- |> Debug.log "allNewReferencesGood"
    in
    let noDuplicateNamesInPats =
      allPats newProgram
      |> List.all
          (\pat ->
            let namesDefinedAtPat = identifiersListInPat pat in
            namesDefinedAtPat == Utils.dedup namesDefinedAtPat
          )
      -- |> Debug.log "noDuplicateNamesInPats"
    in
    allOldReferencesSame && allNewReferencesGood && noDuplicateNamesInPats
  in
  let caption =
    let liftingsStr =
      if List.length liftedUniqueIdents > 0 then
        let liftedNames =
          liftedUniqueIdents
          |> List.map (\uniqueIdent -> Utils.getWithDefault uniqueIdent uniqueIdent uniqueNameToOldName)
        in
        " lifting " ++ Utils.toSentence liftedNames
      else
        ""
    in
    let rewrittingsStr =
      let rewrittenThings =
        identsInvalidlyFreeRewritten ++ identsWithInvalidlyFreeVarsHandled
        |> List.map (\ident -> Utils.getWithDefault ident ident uniqueNameToOldNameUsed)
      in
      if not <| List.isEmpty rewrittenThings
      then " rewriting " ++ Utils.toSentence rewrittenThings
      else ""
    in
    let renamingsStr =
      if not <| List.isEmpty renamings
      then " renaming " ++ (renamings |> List.map (\(desc, oldName, newName) -> desc ++ " " ++ oldName ++ " to " ++ newName) |> Utils.toSentence)
      else ""
    in
    baseDescription
    ++ Utils.toSentence (List.filter ((/=) "") [liftingsStr, rewrittingsStr, renamingsStr])
  in
  -- let _ = Debug.log caption () in
  let result =
    synthesisResult caption newProgram |> setResultSafe isSafe
  in
  result


tryResolvingProblemsAfterTransform
    :  String
    -> Dict String Ident
    -> Maybe (EId, Int)
    -> (String, String)
    -> Set Ident
    -> List EId
    -> Dict EId (Maybe PId)
    -> Exp
    -> Exp
    -> List SynthesisResult
tryResolvingProblemsAfterTransform
    baseDescription
    uniqueNameToOldName
    maybeNewScopeEId
    (touchedAdjective, untouchedAdjective)
    namesUniqueTouched
    varEIdsPreviouslyDeliberatelyRemoved
    insertedVarEIdToBindingPId
    originalProgramUniqueNames
    newProgramUniqueNames =
  tryResolvingProblemsAfterTransform_
    baseDescription
    uniqueNameToOldName
    maybeNewScopeEId
    (touchedAdjective, untouchedAdjective)
    namesUniqueTouched
    varEIdsPreviouslyDeliberatelyRemoved
    insertedVarEIdToBindingPId
    originalProgramUniqueNames
    newProgramUniqueNames
    True


tryResolvingProblemsAfterTransformNoTwiddling
    :  String
    -> Dict String Ident
    -> Maybe (EId, Int)
    -> (String, String)
    -> Set Ident
    -> List EId
    -> Dict EId (Maybe PId)
    -> Exp
    -> Exp
    -> List SynthesisResult
tryResolvingProblemsAfterTransformNoTwiddling
    baseDescription
    uniqueNameToOldName
    maybeNewScopeEId
    (touchedAdjective, untouchedAdjective)
    namesUniqueTouched
    varEIdsPreviouslyDeliberatelyRemoved
    insertedVarEIdToBindingPId
    originalProgramUniqueNames
    newProgramUniqueNames =
  tryResolvingProblemsAfterTransform_
    baseDescription
    uniqueNameToOldName
    maybeNewScopeEId
    (touchedAdjective, untouchedAdjective)
    namesUniqueTouched
    varEIdsPreviouslyDeliberatelyRemoved
    insertedVarEIdToBindingPId
    originalProgramUniqueNames
    newProgramUniqueNames
    False


tryResolvingProblemsAfterTransform_
    :  String
    -> Dict String Ident
    -> Maybe (EId, Int)
    -> (String, String)
    -> Set Ident
    -> List EId
    -> Dict EId (Maybe PId)
    -> Exp
    -> Exp
    -> Bool
    -> List SynthesisResult
tryResolvingProblemsAfterTransform_
    baseDescription
    uniqueNameToOldName
    maybeNewScopeEId -- For renaming items in pattern back to original: checks for name collisions within let pat. Only successfully avoids collisions when the new vars in the pat are in uniqueNameToOldName (i.e. moved from somewhere else in the program). Safety check will properly mark unsafe if not smart enough to avoid name collision.
    (touchedAdjective, untouchedAdjective)
    namesUniqueTouched
    varEIdsPreviouslyDeliberatelyRemoved
    insertedVarEIdToBindingPId
    originalProgramUniqueNames
    newProgramUniqueNames
    tryTwiddling =
  let maybeNewPatUniqueNames =
    maybeNewScopeEId
    |> Maybe.map (\(newScopeEId, bindingNumber) ->
        justFindExpByEId newProgramUniqueNames newScopeEId |>
        expToLetPat |> flip Utils.nth bindingNumber |> Utils.fromOk "CodeMotion.maybeNewPatUniqueNames")
  in
  let uniqueNameToIntendedUses =
    flattenExpTree originalProgramUniqueNames
    |> List.filterMap (\exp -> expToMaybeIdent exp |> Maybe.map (\ident -> (ident, exp.val.eid)))
    |> Utils.pairsToDictOfLists
  in
  let resultForOriginalNamesPriority uniqueNameToOldNameDescribedPrioritized movedUniqueIdents identsInvalidlyFreeRewritten identsWithInvalidlyFreeVarsHandled varEIdsDeliberatelyRemoved insertedVarEIdToBindingPId programWithUniqueNames =
    let (newProgramPartiallyOriginalNames, _, renamingsPreserved) =
       -- Try revert back to original names one by one, as safe.
       -- If new program involves a new/updated pattern (maybeNewScopeEId), ensure we don't introduce duplicate names in that pattern.
       uniqueNameToOldNameDescribedPrioritized
       |> List.foldl
          (\(nameDesc, uniqueName, oldName) (newProgramPartiallyOriginalNames, maybeNewPatPartiallyOriginalNames, renamingsPreserved) ->
            let intendedUses = Utils.getWithDefault uniqueName [] uniqueNameToIntendedUses in
            -- let intendedUses = varsWithName uniqueName originalProgramUniqueNames |> List.map (.val >> .eid) in
            let usesInNewProgram = identifierUsesAfterDefiningPat uniqueName newProgramPartiallyOriginalNames |> List.map (.val >> .eid) in
            let identifiersInNewPat = maybeNewPatPartiallyOriginalNames |> Maybe.map identifiersListInPat |> Maybe.withDefault [] in
            -- If this name is part of the new pattern and renaming it would created a duplicate name, don't rename.
            if List.member uniqueName identifiersInNewPat && List.member oldName identifiersInNewPat && not (uniqueName == oldName && 1 == Utils.count ((==) uniqueName) identifiersInNewPat) then
              (newProgramPartiallyOriginalNames, maybeNewPatPartiallyOriginalNames, renamingsPreserved ++ [(nameDesc, oldName, uniqueName)])
            else if not <| Utils.equalAsSets intendedUses usesInNewProgram then
              -- Definition of this variable was moved in such a way that renaming can't make the program work.
              -- Might as well use the old name and let the programmer fix the mess they made.
              ( renameIdentifier uniqueName oldName newProgramPartiallyOriginalNames
              , maybeNewPatPartiallyOriginalNames |> Maybe.map (renameIdentifierInPat uniqueName oldName)
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
                , maybeNewPatPartiallyOriginalNames |> Maybe.map (renameIdentifierInPat uniqueName oldName)
                , renamingsPreserved
                )
              else
                (newProgramPartiallyOriginalNames, maybeNewPatPartiallyOriginalNames, renamingsPreserved ++ [(nameDesc, oldName, uniqueName)])
          )
          (programWithUniqueNames, maybeNewPatUniqueNames, [])
    in
    makeResult
        baseDescription
        uniqueNameToOldName
        renamingsPreserved
        movedUniqueIdents
        identsInvalidlyFreeRewritten
        identsWithInvalidlyFreeVarsHandled
        (varEIdsPreviouslyDeliberatelyRemoved ++ varEIdsDeliberatelyRemoved)
        insertedVarEIdToBindingPId
        originalProgramUniqueNames
        newProgramPartiallyOriginalNames
  in
  let (uniqueNameToOldNameTouched, uniqueNameToOldNameUntouched) =
    uniqueNameToOldName
    |> Dict.toList
    |> List.partition (\(uniqueName, oldName) -> Set.member uniqueName namesUniqueTouched)
  in
  let uniqueNameToOldNameTouchedDescribed   = uniqueNameToOldNameTouched   |> List.map (\(uniqueName, oldName) -> (touchedAdjective, uniqueName, oldName)) in
  let uniqueNameToOldNameUntouchedDescribed = uniqueNameToOldNameUntouched |> List.map (\(uniqueName, oldName) -> (untouchedAdjective, uniqueName, oldName)) in
  let twiddledResults =
    if tryTwiddling then
      []
      -- TODO: disabled momentarily, needs to abort on cyclic dependencies
      --
      -- case newProgramUniqueNames |> maybeSatisfyUniqueNamesDependenciesByTwiddlingArithmetic of
      --   Nothing -> []
      --   Just (newProgramTwiddledArithmeticToSwapDependencies, identsInvalidlyFreeRewritten, identsWithInvalidlyFreeVarsHandled) ->
      --     let oldVarEIds = allVars newProgramUniqueNames                          |> List.map (.val >> .eid) in
      --     let newVarEIds = allVars newProgramTwiddledArithmeticToSwapDependencies |> List.map (.val >> .eid) in
      --     let varEIdsDeliberatelyRemoved = Utils.listDiff oldVarEIds newVarEIds in
      --     let newInsertedVarEIdToBindingPId =
      --       let insertedVarEIds = Utils.listDiff newVarEIds oldVarEIds |> Set.fromList in
      --       allVarEIdsToBindingPIdBasedOnUniqueName newProgramTwiddledArithmeticToSwapDependencies
      --       |> Dict.filter (\eid _ -> Set.member eid insertedVarEIds)
      --       |> Dict.union insertedVarEIdToBindingPId
      --     in
      --     let (newProgramTwiddledArithmeticToSwapDependenciesAndLifted, liftedUniqueIdents) =
      --       liftDependenciesBasedOnUniqueNames newProgramTwiddledArithmeticToSwapDependencies
      --     in
      --     [ resultForOriginalNamesPriority (uniqueNameToOldNameUntouchedDescribed ++ uniqueNameToOldNameTouchedDescribed) liftedUniqueIdents identsInvalidlyFreeRewritten identsWithInvalidlyFreeVarsHandled varEIdsDeliberatelyRemoved newInsertedVarEIdToBindingPId newProgramTwiddledArithmeticToSwapDependenciesAndLifted
      --     , resultForOriginalNamesPriority (uniqueNameToOldNameTouchedDescribed ++ uniqueNameToOldNameUntouchedDescribed) liftedUniqueIdents identsInvalidlyFreeRewritten identsWithInvalidlyFreeVarsHandled varEIdsDeliberatelyRemoved newInsertedVarEIdToBindingPId newProgramTwiddledArithmeticToSwapDependenciesAndLifted
      --     ]
    else
      []
  in
  let (newProgramUniqueNamesDependenciesLifted, liftedUniqueIdents) =
    liftDependenciesBasedOnUniqueNames newProgramUniqueNames
  in
  [ resultForOriginalNamesPriority (uniqueNameToOldNameUntouchedDescribed ++ uniqueNameToOldNameTouchedDescribed) liftedUniqueIdents [] [] [] insertedVarEIdToBindingPId newProgramUniqueNamesDependenciesLifted
  , resultForOriginalNamesPriority (uniqueNameToOldNameTouchedDescribed ++ uniqueNameToOldNameUntouchedDescribed) liftedUniqueIdents [] [] [] insertedVarEIdToBindingPId newProgramUniqueNamesDependenciesLifted
  ] ++ twiddledResults


-- If multiple plucked, gathers into one list.
--
-- Returns (newProgram, insertedLetEId)
insertNewLetFromPlucked : EId -> List PatBoundExpIsRec -> Exp -> Exp -> (Exp, EId)
insertNewLetFromPlucked eidToWrap pluckedPatAndBoundExpAndIsRecs programToModify originalProgram =
  let (pluckedPats, pluckedBoundExps, isRecs) = Utils.unzip3 pluckedPatAndBoundExpAndIsRecs in
  let (newPat, newBoundExp, isRec) =
    case (pluckedPats, pluckedBoundExps, Utils.maybeConsensus isRecs) of
      ([pluckedPat], [boundExp], Just isRec) ->
        (pluckedPat, boundExp, isRec)

      (_, _, maybeRecConsensus) ->
        let isRec =
          case maybeRecConsensus of
            Just isRec -> isRec -- Program will crash in evaluator if recursive, but we will produce the correct code!
            Nothing    -> True  -- Disagreement--assume recursive. More likely to be correct. Will still crash in evaluator.
        in
        ( withDummyPatInfo <| PList space1 (pluckedPats      |> setPatListWhitespace "" " ") space0 Nothing space0
        , withDummyExpInfo <| EList space1 (List.map ((,) space0) (pluckedBoundExps |> setExpListWhitespace "" " ")) space0 Nothing space0 -- May want to be smarter about whitespace here to avoid long lines.
        , isRec
        )
  in
  let insertedLetEId = Parser.maxId originalProgram + 1 in
  let newProgram =
    programToModify
    |> mapExpNode
        eidToWrap
        (\expToWrap ->
          newLetFancyWhitespace insertedLetEId isRec newPat newBoundExp expToWrap programToModify
        )
  in
  (newProgram, insertedLetEId)


-- Insert one or more plucked pat+exps into an existing let pat.
--
-- Returns (newProgram, letEId)
insertPluckedIntoPat : PathedPatternId -> List PatBoundExpIsRec -> Exp -> (Exp, EId)
insertPluckedIntoPat targetPathedPatId pluckedPatAndBoundExpAndIsRecs program =
  let ((targetLetEId, _), targetPath) = targetPathedPatId in
  let newProgram =
    program
    |> mapExpNode
        targetLetEId
        (\newScopeExp ->
          pluckedPatAndBoundExpAndIsRecs
          |> List.foldr
              (\(pluckedPat, pluckedBoundExp, isRec) newScopeExp ->
                -- If moving a recursive definition to a non-recursive let, safety check should warn.
                insertPat_ (pluckedPat, pluckedBoundExp) targetPath newScopeExp
              )
              newScopeExp
        )
  in
  (newProgram, targetLetEId)


-- Moving a definition is safe if all identifiers resolve to the same bindings.
--
-- More specifically:
--   - All free variables in the moved assignment still resolve to the same bindings
--   - All previous references to the moved identifier still resolve to that identifer
--   - All other variables uses of the same name do not resolve to the moved identifier
--
moveDefinitions_ : Syntax -> (List PatBoundExpIsRec -> Exp -> (Exp, EId)) -> List PathedPatternId -> Exp -> List SynthesisResult
moveDefinitions_ syntax makeNewProgram sourcePathedPatIds program =
  let (programUniqueNames, uniqueNameToOldName) = assignUniqueNames program in
  let (pluckedPatAndBoundExpAndIsRecs, programWithoutPlucked) =
    pluckAll sourcePathedPatIds programUniqueNames
  in
  if pluckedPatAndBoundExpAndIsRecs == [] then
    Debug.log "could not pluck anything" []
  else
    let (pluckedPats, pluckedBoundExps, isRecs) = Utils.unzip3 pluckedPatAndBoundExpAndIsRecs in
    let pluckedPathedPatIdentifiersUnique       = Utils.unionAll <| List.map identifiersSetInPat pluckedPats in
    let pluckedBoundExpFreeIdentifiersUnique    = Utils.unionAll <| List.map freeIdentifiers pluckedBoundExps in
    let namesUniqueExplicitlyMoved = Set.union pluckedPathedPatIdentifiersUnique pluckedBoundExpFreeIdentifiersUnique in
    let (newProgramUniqueNames, newScopeEId) =
      makeNewProgram pluckedPatAndBoundExpAndIsRecs programWithoutPlucked
      |> Tuple.mapFirst LangSimplify.simplifyAssignments
    in
    let movedThingsStr =
      pluckedPats
      |> List.map (renameIdentifiersInPat uniqueNameToOldName >> Syntax.patternUnparser syntax >> Utils.squish)
      |> Utils.toSentence
    in
    programOriginalNamesAndMaybeRenamedLiftedTwiddledResults
      ("Move " ++ movedThingsStr)
      uniqueNameToOldName
      (Just (newScopeEId, 1 {- Binding nnumber -})) -- maybeNewScopeEId
      ("moved", "unmoved")
      namesUniqueExplicitlyMoved -- namesUniqueTouched
      [] -- varEIdsPreviouslyDeliberatelyRemoved
      Dict.empty -- insertedVarEIdToBindingPId
      programUniqueNames
      newProgramUniqueNames


moveDefinitionsBeforeEId : Syntax -> List PathedPatternId -> EId -> Exp -> List SynthesisResult
moveDefinitionsBeforeEId syntax sourcePathedPatIds targetEId program =
  -- let _ = Debug.log ("moving " ++ toString sourcePathedPatIds ++ " before " ++ toString targetEId ++ " in " ++ unparseWithIds program) () in
  let makeNewProgram pluckedPatAndBoundExpAndIsRecs programWithoutPluckedUniqueNames =
    insertNewLetFromPlucked
        targetEId
        pluckedPatAndBoundExpAndIsRecs
        programWithoutPluckedUniqueNames
        program
  in
  moveDefinitions_ syntax makeNewProgram sourcePathedPatIds program


moveDefinitionsPat : Syntax -> List PathedPatternId -> PathedPatternId -> Exp -> List SynthesisResult
moveDefinitionsPat syntax sourcePathedPatIds targetPathedPatId program =
  let makeNewProgram pluckedPatAndBoundExpAndIsRecs programWithoutPluckedUniqueNames =
    insertPluckedIntoPat targetPathedPatId pluckedPatAndBoundExpAndIsRecs programWithoutPluckedUniqueNames
  in
  moveDefinitions_ syntax makeNewProgram sourcePathedPatIds program


makeDuplicateResults_ syntax newScopeEId pluckedPatAndBoundExpAndIsRecs newProgram originalProgram =
  let (pluckedPats, pluckedBoundExps, isRecs) = Utils.unzip3 pluckedPatAndBoundExpAndIsRecs in
  let newScopeExp = justFindExpByEId newProgram newScopeEId in
  let newScopePats      = newScopeExp |> expToLetPat in
  let newScopeBoundExps = newScopeExp |> expToLetBoundExp in
  let newScopeBody      = newScopeExp |> expToLetBody in
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
        List.concatMap freeVars newScopeBoundExps
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
      let namesDefinedAtNewScope = List.concatMap identifiersListInPat newScopePats in
      namesDefinedAtNewScope == Utils.dedup namesDefinedAtNewScope
    in
    identUsesSafe && boundExpVarsSafe && noDuplicateNamesInPat
  in
  let caption =
    let patStrs = List.map (Syntax.patternUnparser syntax >> Utils.squish) pluckedPats in
    "Duplicate "
    ++ (if List.length patStrs == 1 then "Definition" else "Definitions")
    ++ " of "
    ++ Utils.toSentence patStrs
  in
  let result =
    synthesisResult caption newProgram |> setResultSafe isSafe
  in
  [ result ]


duplicateDefinitionsBeforeEId : Syntax -> List PathedPatternId -> EId -> Exp -> List SynthesisResult
duplicateDefinitionsBeforeEId syntax sourcePathedPatIds targetEId originalProgram =
  let (pluckedPatAndBoundExpAndIsRecs, _) =
    pluckAll sourcePathedPatIds originalProgram
  in
  let (newProgram, insertedLetEId) =
    insertNewLetFromPlucked
        targetEId
        pluckedPatAndBoundExpAndIsRecs
        originalProgram
        originalProgram
    |> Tuple.mapFirst Parser.freshen -- Remove duplicate EIds
  in
  makeDuplicateResults_ syntax insertedLetEId pluckedPatAndBoundExpAndIsRecs newProgram originalProgram


duplicateDefinitionsPat : Syntax -> List PathedPatternId -> PathedPatternId -> Exp -> List SynthesisResult
duplicateDefinitionsPat syntax sourcePathedPatIds targetPathedPatId originalProgram =
  let (pluckedPatAndBoundExpAndIsRecs, _) =
    pluckAll sourcePathedPatIds originalProgram
  in
  let (newProgram, targetLetEId) =
    insertPluckedIntoPat
        targetPathedPatId
        pluckedPatAndBoundExpAndIsRecs
        originalProgram
    |> Tuple.mapFirst Parser.freshen -- Remove duplicate EIds

  in
  makeDuplicateResults_ syntax targetLetEId pluckedPatAndBoundExpAndIsRecs newProgram originalProgram


-- You should only insert non-rec bindings.
insertPat_ : (Pat, Exp) ->         List Int -> Exp -> Exp
insertPat_ (patToInsert, boundExp) targetPath  exp =
  case exp.val.e__ of
    ELet ws1 letKind decls ws3 e2 ->
      let _ = Debug.log "TODO: CodeMotion.insertPat_ should take into account the new ELet's declarations" () in
      exp
    {-ELet ws1 letKind rec p ws2 e1 ws3 e2 ws4 ->
      case insertPat__ (patToInsert, boundExp) p e1 targetPath of
        Just (newPat, newBoundExp) ->
          replaceE__ exp (ELet ws1 letKind rec newPat ws2 newBoundExp ws3 e2 ws4)

        Nothing ->
          let _ = Debug.log "insertPat_: pattern, path " (p.val.p__, targetPath) in
          exp
    -}
    _ ->
      let _ = Debug.log "insertPat_: not ELet" exp.val.e__ in
      exp


insertPat__ : (Pat, Exp) -> Pat -> Exp -> List Int -> Maybe (Pat, Exp)
insertPat__ (patToInsert, boundExp) p e1 path =
  let maybeNewP_E__Pair =
    case (p.val.p__, e1.val.e__, path) of
      (PVar pws1 _ _, _, [i]) ->
        Just ( PList pws1                            (Utils.inserti i patToInsert [p] |> setPatListWhitespace "" " ") space0 Nothing space0
             , EList (ws <| precedingWhitespace e1)  (List.map ((,) space0) (Utils.inserti i boundExp [e1]   |> setExpListWhitespace "" " ")) space0 Nothing space0 )

      (PAs pws1 _ _ _, _, [i]) ->
        Just ( PList pws1                            (Utils.inserti i patToInsert [p] |> setPatListWhitespace "" " ") space0 Nothing space0
             , EList (ws <| precedingWhitespace e1)  (List.map ((,) space0) (Utils.inserti i boundExp [e1]   |> setExpListWhitespace "" " ")) space0 Nothing space0 )

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
               , EList ews1 (List.map ((,) space0) (Utils.inserti i boundExp (Utils.listValues es)    |> imitateExpListWhitespace (Utils.listValues es))) ews2 Nothing ews3 )
               -- TODO whitespace before commas
        else
          let _ = Debug.log "can't insert into this list (note: cannot insert on list tail)" (Syntax.patternUnparser Syntax.Elm p, Syntax.unparser Syntax.Elm e1, path) in
          Nothing

      ( PList pws1 ps pws2 maybePTail pws3
      , EList ews1 es ews2 maybeETail ews3
      , i::is
      ) ->
        if List.length ps >= i && List.length es >= i then
          let (pi, ei) = (Utils.geti i ps, Utils.geti i (Utils.listValues es)) in
          insertPat__ (patToInsert, boundExp) pi ei is
          |> Maybe.map
              (\(newPat, newBoundExp) ->
                let (newPs, newEs) =
                  ( Utils.replacei i newPat ps      |> imitatePatListWhitespace ps
                  , Utils.replacei i newBoundExp (Utils.listValues es) |> imitateExpListWhitespace (Utils.listValues es)
                  )
                in
                (PList pws1 newPs pws2 maybePTail pws3,
                 EList ews1 (List.map ((,) space0) newEs) ews2 maybeETail ews3)
                 -- TODO whitespace before commas

              )
        else if List.length ps == List.length es && i == 1 + List.length ps && Utils.maybeToBool maybePTail && Utils.maybeToBool maybeETail then
          -- Recursing into the tail binding
          let pi = Utils.fromJust_ "CodeMotion3" maybePTail in
          let ei = Utils.fromJust_ "CodeMotion4" maybeETail in
          insertPat__ (patToInsert, boundExp) pi ei is
          |> Maybe.map
              (\(newPat, newBoundExp) ->
                (PList pws1 ps pws2 (Just newPat) pws3,
                 EList ews1 es ews2 (Just newBoundExp) ews3)
              )
        else
          let _ = Debug.log "can't insert into this list (note: cannot insert on list tail)" (Syntax.patternUnparser Syntax.Elm p, Syntax.unparser Syntax.Elm e1, path) in
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
      Utils.maybeGeti1 i pats
      |> Maybe.andThen (addPatToPat patToInsert is)
      |> Maybe.map (\newPat -> Utils.replacei i newPat pats)

    [] ->
      Nothing


addPatToPat : Pat -> List Int -> Pat -> Maybe Pat
addPatToPat patToInsert path pat =
  case (pat.val.p__, path) of
    (_, []) ->
      Nothing

    (PAs ws1 p1 ws2 p2, 1::is) ->
      let _ = Debug.log "adding to as pattern not allowed yet because when adding argument, pattern path will not be the same as the path for adding arguments to call sites" () in
      Nothing

    -- (PAs ws1 wsi ident ws2 p, 1::is) ->
    --   let result = pluckPat is p in
    --   case result of
    --     Just (pluckedPat, Just remainingPat) ->
    --       Just (pluckedPat, Just <| replaceP__ pat (PAs ws1 wsi ident ws2 remainingPat))
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
      Utils.maybeGeti1 i exps
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
        addExpToExpsByPath expToInsert (i::is) (Utils.listValues es)
        |> Maybe.map (\newEs -> replaceE__ exp <| EList ws1 (List.map ((,) space0) newEs) ws2 maybeTail ws3)
        -- TODO whitespace before commas. can't just zip with old es here, since newEs has more elements.
      else
        Nothing

    _ ->
      Nothing


------------------------------------------------------------------------------

-- Duplicate lets to new position, remove old lets, check for safety/resolve any dependency problems.
-- Dup/remove workflow cleanly handles edge cases e.g. moving definition before itself.
moveEquationsBeforeEId : Syntax -> List (EId, Int) -> EId -> Exp -> List SynthesisResult
moveEquationsBeforeEId syntax letEIds targetEId originalProgram =
  Debug.log "CodeMotion TODO: update moveEquationBeforeEId with new Let syntax" []
  {-
  let letEIdsSorted =
    letEIds
    |> List.sortBy (Tuple.first >> locationInProgram originalProgram)
  in
  let maxId = Parser.maxId originalProgram in
  let letEIdToReinsertedLetEId =
    letEIds
    |> Utils.mapi1 (\(i, letEId) -> (letEId, maxId + i))
    |> Dict.fromList
  in
  let (originalProgramUniqueNames, uniqueNameToOldName) = assignUniqueNames originalProgram in
  let programWithDuplicatedLets =
    letEIdsSorted
    |> List.foldl
        (\letEIdToDup program ->
          let letExp = justFindExpByEId originalProgramUniqueNames letEIdToDup in
          program
          |> mapExpNode
              targetEId
              (\expToWrap ->
                let insertedLetEId = Utils.justGet_ "moveEquationsBeforeEId" letEIdToDup letEIdToReinsertedLetEId in
                let (ws1, _, _, pat, _, _, boundExp, _, _) = expToLetParts letExp in
                let isRec = isBodyPossiblyRecursive boundExp in
                newLetFancyWhitespace insertedLetEId isRec pat boundExp expToWrap program
                -- let letOrDef = if isTopLevelEId targetEId program then Def else Let in
                -- let newLetIndentation =
                --   -- If target expression is the body of a existing let, then use the indentation of the existing let.
                --   case parentByEId program targetEId of
                --     Just (Just parent) ->
                --       if (expToMaybeLetBody parent |> Maybe.map (.val >> .eid)) == Just targetEId
                --       then indentationAt parent.val.eid program
                --       else indentationAt targetEId program
                --     _ -> indentationAt targetEId program
                -- in
                -- ELet ws1 letOrDef isRec pat boundExp
                --     (expToWrap |> ensureWhitespaceSmartExp 1 (indentationOf letExp ++ if isLet expToWrap then "" else "  ")) space0
                -- |> withDummyExpInfoEId insertedLetEId
                -- |> ensureWhitespaceNewlineExp
                -- |> replaceIndentation newLetIndentation
              )
        )
        originalProgramUniqueNames
  in
  let programWithDuplicatedLetsRemoved =
    programWithDuplicatedLets
    |> mapExp
        (\exp ->
          if List.member exp.val.eid letEIds then
            copyPrecedingWhitespace exp (expToLetBody exp)
          else
            exp
        )
  in
  let programWithNewLetsOriginalEIds =
    let reinsertedLetEIdToOldLetEId = Utils.flipDict letEIdToReinsertedLetEId in
    programWithDuplicatedLetsRemoved
    |> mapExp
        (\exp ->
          case Dict.get exp.val.eid reinsertedLetEIdToOldLetEId of
            Just oldEId -> setEId oldEId exp
            Nothing     -> exp
        )
  in
  let (movedPats, movedBoundExps) =
    letEIdsSorted
    |> List.concatMap (justFindExpByEId originalProgramUniqueNames >> expToLetPatAndBoundExp)
    |> List.unzip
  in
  let namesUniqueExplicitlyMoved =
    [ identifiersSetInPats movedPats ] ++ List.map freeIdentifiers movedBoundExps
    |> Utils.unionAll
  in
  let movedThingsStr =
    movedPats
    |> List.map (renameIdentifiersInPat uniqueNameToOldName >> Syntax.patternUnparser syntax >> Utils.squish)
    |> Utils.toSentence
  in
  programOriginalNamesAndMaybeRenamedLiftedTwiddledResults
      ("Move " ++ movedThingsStr)
      uniqueNameToOldName
      Nothing -- maybeNewScopeEId
      ("moved", "unmoved")
      namesUniqueExplicitlyMoved -- namesUniqueTouched
      [] -- varEIdsPreviouslyDeliberatelyRemoved
      Dict.empty -- insertedVarEIdToBindingPId
      originalProgramUniqueNames
      programWithNewLetsOriginalEIds
  -}

------------------------------------------------------------------------------

-- Small bug: if variable to inline is unused and has variables in its boundExp, result will be marked as unsafe.
-- (Because the eids of those usages disappear...if variable is used those usages are inlined with same EIds from
-- the bound exp (on purpose) for the safety check.)
inlineDefinitions : Syntax -> List PathedPatternId -> Exp -> List SynthesisResult
inlineDefinitions syntax selectedPathedPatIds originalProgram =
  let (programUniqueNames, uniqueNameToOldName) = assignUniqueNames originalProgram in
  let namedPathedPatIdsToPluck =
    selectedPathedPatIds
    |> Utils.groupBy pathedPatIdToScopeId
    |> Dict.toList
    |> List.concatMap
        (\(scopeId, pathedPatternIds) ->
          let paths = List.map pathedPatIdToPath pathedPatternIds in
          case findPatByPathedPatternId (scopeId, []) programUniqueNames of
            Nothing  -> []
            Just pat ->
              identPathsInPat pat
              |> List.filter (\(ident, path) -> List.any (Utils.isPrefixOf path) paths)
              |> List.map    (\(ident, path) -> (scopeId, path))
        )
  in
  let (pluckedPatAndBoundExpAndIsRecs, programWithoutPlucked) =
    pluckAll namedPathedPatIdsToPluck programUniqueNames
  in
  let (pluckedPats, pluckedBoundExps, isRecs) = Utils.unzip3 pluckedPatAndBoundExpAndIsRecs in
  if pluckedPatAndBoundExpAndIsRecs == [] then
    Debug.log "could not pluck anything" []
  else if Utils.maybeConsensus isRecs /= Just False then
    Debug.log "Can't inline recursive definitions" []
  else
    let pluckedPathedPatIdentifiersUnique    = Utils.unionAll <| List.map identifiersSetInPat pluckedPats in
    let pluckedBoundExpFreeIdentifiersUnique = Utils.unionAll <| List.map freeIdentifiers pluckedBoundExps in
    let namesUniqueExplicitlyTouched = Set.union pluckedPathedPatIdentifiersUnique pluckedBoundExpFreeIdentifiersUnique in
    let uniqueIdentToExp =
      pluckedPatAndBoundExpAndIsRecs
      |> List.map
          (\(pat, boundExp, _) ->
            case patToMaybeIdent pat of
              Just ident -> (ident, boundExp)
              _          -> Debug.crash <| "CodeMotion.inlineDefinitions: should only have PVar or PAs here, got: " ++ toString (pat, boundExp)
          )
      |> Dict.fromList
    in
    let (newProgramUniqueNames, varEIdsRemoved) =
      programWithoutPlucked
      |> LangSimplify.simplifyAssignments
      |> mapFoldExp
          (\exp varEIdsRemoved ->
            case expToMaybeIdent exp |> Maybe.andThen (\ident -> Dict.get ident uniqueIdentToExp) of
              Just newExp -> (copyPrecedingWhitespace exp newExp, exp.val.eid::varEIdsRemoved)
              Nothing     -> (exp, varEIdsRemoved)
          )
          []
    in
    let inlinedThingsStr =
      pluckedPats
      |> List.map (renameIdentifiersInPat uniqueNameToOldName >> Syntax.patternUnparser syntax >> Utils.squish)
      |> Utils.toSentence
    in
    programOriginalNamesAndMaybeRenamedLiftedTwiddledResults
        ("Inline " ++ inlinedThingsStr)
        uniqueNameToOldName
        Nothing -- maybeNewScopeEId
        ("touched", "untouched")
        namesUniqueExplicitlyTouched -- namesUniqueTouched
        varEIdsRemoved -- varEIdsPreviouslyDeliberatelyRemoved
        Dict.empty -- insertedVarEIdToBindingPId
        programUniqueNames
        newProgramUniqueNames


------------------------------------------------------------------------------

-- Takes EId of expression to abstract, a predicate on exp and program to choose which expressions should become parameters
abstract : EId -> (Exp -> Exp -> Bool) -> Exp -> (List Exp, Exp)
abstract eid shouldBeParameter originalProgram =
  let expToAbstact = justFindExpByEId originalProgram eid in
  let eidsToParameterize =
    -- Do it like this as a moderately straightforward way to not add extra arguments in the
    -- case that shouldBeParameter returns one expression nested inside another
    expToAbstact
    |> mapExp (\e -> if shouldBeParameter e originalProgram then replaceE__ e (EVar space1 "INSERT_ARGUMENT_HERE") else e)
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
            (copyPrecedingWhitespace e (eVar name), (namesToAvoid_, name::paramNamesARRRGTagged, (Parser.clearAllIds e)::paramExps))
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
      |> LangSimplify.removeUnusedLetPats
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
        |> List.concatMap identity
        |> List.concatMap (\(pat, boundExp) -> tryMatchExpReturningList pat boundExp)
      in
      bindings
      |> Utils.findFirst (\(ident, boundExp) -> boundExp.val.eid == exp.val.eid)
      |> Utils.maybeToBool

    _ -> False


abstractPVar : Syntax -> PathedPatternId -> List EId -> Exp -> List SynthesisResult
abstractPVar syntax pathedPatId perhapsArgEIds originalProgram =
  case pluck pathedPatId originalProgram of
    Nothing ->
      Debug.log ("abstractPVar Could not find pathedPatternId " ++ toString pathedPatId ++ " in program\n" ++ unparseWithIds originalProgram) []

    Just ((pluckedPat, pluckedBoundExp, False), _) ->
      case pluckedPat.val.p__ of
        PVar _ ident _ ->
          let doAbstract shouldBeParameter =
            let ((scopeEId, _), _) = pathedPatId in
            let scopeExp = justFindExpByEId originalProgram scopeEId in
            let scopeBody = scopeExp |> expToLetBody in
            let (argumentsForCallSite, abstractedFuncExp) =
              abstract pluckedBoundExp.val.eid shouldBeParameter originalProgram
            in
            let abstractedFuncExpNiceWs =
              abstractedFuncExp
              |> replacePrecedingWhitespace " "
              |> replaceIndentation (indentationOf scopeExp ++ "  ")
            in
            let newScopeBody =
              let varToApp varExp =
                replaceE__PreservingPrecedingWhitespace varExp (EApp space0 (eVar0 ident) (argumentsForCallSite |> setExpListWhitespace " " " ") SpaceApp space0)
              in
              transformVarsUntilBound (Dict.singleton ident varToApp) scopeBody
            in
            let newProgram =
              originalProgram
              |> replaceExpNode scopeBody.val.eid newScopeBody
              |> replaceExpNode pluckedBoundExp.val.eid abstractedFuncExpNiceWs
            in
            (newProgram, expToFuncPats abstractedFuncExpNiceWs)
          in
          case perhapsArgEIds of
            [] ->
              let abstractedOverAllConstantsResult =
                let (newProgram, _) = doAbstract shouldBeParameterIsConstant in
                synthesisResult ("Abstract " ++ ident ++ " over its constants") newProgram
              in
              let abstractedOverNamedUnfrozenConstantsResult =
                let (newProgram, _) = doAbstract shouldBeParameterIsNamedUnfrozenConstant in
                synthesisResult ("Abstract " ++ ident ++ " over its named constants") newProgram
              in
              [ abstractedOverAllConstantsResult
              , abstractedOverNamedUnfrozenConstantsResult
              ]

            argEIds ->
              let shouldBeParameter e _ = List.member e.val.eid argEIds in
              let (newProgram, argPats) = doAbstract shouldBeParameter in
              -- A little bit conservative here.
              let isSafe = argEIds |> List.all (findExpByEId originalProgram >> Maybe.map isLiteral >> (==) (Just True)) in
              [ synthesisResult ("Abstract " ++ ident ++ " over " ++ (argPats |> List.map (Syntax.patternUnparser syntax >> Utils.squish) |> Utils.toSentence)) newProgram |> setResultSafe isSafe ]

        _ ->
          Debug.log "Can only abstract a PVar" []

    _ ->
      Debug.log "Cannot abstract a recursive definition" []


abstractExp : Syntax -> EId -> Exp -> List SynthesisResult
abstractExp syntax eidToAbstract originalProgram =
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
      newLetFancyWhitespace -1 False (pVar funcName) abstractedFuncExp expToWrapWithTargetReplaced originalProgram
    in
    let newProgram =
      originalProgram
      |> replaceExpNodePreservingPrecedingWhitespace expToWrap.val.eid wrapped
    in
    newProgram
  in
  let abstractedOverAllConstantsResult =
    let newProgram = doAbstract shouldBeParameterIsConstant in
    synthesisResult ("Abstract " ++ (expToAbstract |> Syntax.unparser syntax |> Utils.squish |> Utils.niceTruncateString 20 "...") ++ " over its constants") newProgram
  in
  let abstractedOverNamedUnfrozenConstantsResult =
    let newProgram = doAbstract shouldBeParameterIsNamedUnfrozenConstant in
    synthesisResult ("Abstract " ++ (expToAbstract |> Syntax.unparser syntax |> Utils.squish |> Utils.niceTruncateString 20 "...") ++ " over its named constants") newProgram
  in
  [ abstractedOverAllConstantsResult
  , abstractedOverNamedUnfrozenConstantsResult
  ]


------------------------------------------------------------------------------

-- TODO: relax addArg/removeArg/reorderArgs to allow (unsafe) addition/removal from anonymous functions (right now, written as if function must be named).

-- Try to remove a declaration/binding and put it in the argument of the selected function
addArg_ : Syntax -> PathedPatternId ->     (Exp -> Exp -> Maybe (Bool, Pat, Exp, Exp)) -> Exp -> List SynthesisResult
addArg_ syntax      pathedPatId {-target-} funcToIsSafePatToInsertArgValExpAndNewFuncBody originalProgram =
  let ((funcEId, _), path) = pathedPatId in
  case findLetAndIdentBindingExp funcEId originalProgram of
    Just (letExp, funcName) ->
      case letExp.val.e__ of
        ELet ws1 letKind decls ws3 letBody ->
          let _ = Debug.log "TODO: CodeMotion.addArg_ should take into account the new ELet's declarations. Returning nothing" () in
          []
        {-
        ELet ws1 letKind isRec letPat ws2 func ws3 letBody ws4 ->
          -- If func is passed to itself as an arg, this probably breaks. (is fixable though)
          let funcVarUsageEIds =
            if isRec
            then identifierUsageEIds funcName func ++ identifierUsageEIds funcName letBody |> Set.fromList
            else identifierUsageEIds funcName letBody |> Set.fromList
          in
          case func.val.e__ of
            EFun fws1 fpats fbody fws2 ->
              case funcToIsSafePatToInsertArgValExpAndNewFuncBody func fbody of
                Nothing ->
                  []

                Just (bodyTransformationIsSafe, patToInsert, argValExp, newFBody) ->
                  case addPatToPats patToInsert path fpats of
                    Nothing ->
                      let _ = Debug.log ("Could not insert pattern into " ++ String.join " " (List.map (Syntax.patternUnparser syntax) fpats) ++ " at path") path in
                      []

                    Just newFPats ->
                      let (newProgram, funcVarUsagesTransformed) =
                        originalProgram
                        |> replaceExpNodeE__ByEId func.val.eid (EFun fws1 newFPats newFBody fws2)
                        |> mapFoldExp
                            (\exp funcVarUsagesTransformed ->
                              case exp.val.e__ of
                                EApp appWs1 appFuncExp appArgs appType appWs2 ->
                                  if Set.member appFuncExp.val.eid funcVarUsageEIds then
                                    case addExpToExpsByPath (Parser.clearAllIds argValExp) path appArgs of
                                      Nothing ->
                                        (exp, funcVarUsagesTransformed)

                                      Just newAppArgs ->
                                        ( replaceE__ exp <| EApp appWs1 appFuncExp newAppArgs appType appWs2
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
                      let caption =
                        let baseCaption = "Insert Argument " ++ (patToInsert |> Syntax.patternUnparser syntax |> Utils.squish) in
                        let intoFuncString =
                          originalProgram
                          |> findLetAndIdentBindingExp func.val.eid
                          |> Maybe.map (\(_, ident) -> " into " ++ ident)
                          |> Maybe.withDefault ""
                        in
                        baseCaption ++ intoFuncString
                      in
                      [ synthesisResult caption newProgram |> setResultSafe isSafe ]

            _ ->
              Debug.crash <| "CodeMotion.addArg_ should've had an EFun here"
        -}
        _ ->
          Debug.crash <| "CodeMotion.addArg_ expected findLetAndIdentBindingExp to return ELet"

    Nothing ->
      -- Can't find a name for this function. Arg addition probably unsafe.
      []


addArgs : Syntax -> List EId -> PathedPatternId -> Exp -> List SynthesisResult
addArgs syntax argSourceEIds pathedPatId originalProgram =
  let (maybeNewProgram, resultDescs, isSafe) =
    argSourceEIds
    |> List.sortBy (locationInProgram originalProgram)
    |> List.foldr
        (\argSourceEId (maybePriorProgram, resultDescs, safeSoFar) ->
          case maybePriorProgram |> Maybe.map (addArg syntax argSourceEId pathedPatId) of
            Just (SynthesisResult newResult :: _) -> (Just newResult.exp, newResult.description::resultDescs, safeSoFar && newResult.isSafe)
            _                                     -> (Nothing, resultDescs, False)
        )
        (Just originalProgram, [], True)
  in
  case maybeNewProgram of
    Just newProgram ->
      let caption = Utils.mergeStrings resultDescs |> Utils.stringReplace "Argument " "Arguments " in
      [ synthesisResult caption newProgram |> setResultSafe isSafe ]

    Nothing ->
      []

addArgsFromPats : Syntax -> List PathedPatternId -> PathedPatternId -> Exp -> List SynthesisResult
addArgsFromPats syntax argSourcePathedPatIds pathedPatId originalProgram =
  let (maybeNewProgram, resultDescs, isSafe) =
    argSourcePathedPatIds
    |> List.sortBy (\((scopeEId, _), path) -> locationInProgram originalProgram scopeEId)
    |> List.foldr
        (\argSourcePathedPatId (maybePriorProgram, resultDescs, safeSoFar) ->
          -- Identity for post-processing fbody -- when adding multiple patterns, can't simplify assignments until the very end.
          case maybePriorProgram |> Maybe.map (addArgFromPat_ syntax identity argSourcePathedPatId pathedPatId) of
            Just (SynthesisResult newResult :: _) -> (Just newResult.exp, newResult.description::resultDescs, safeSoFar && newResult.isSafe)
            _                                     -> (Nothing, resultDescs, False)
        )
        (Just originalProgram, [], True)
  in
  case maybeNewProgram of
    Just newProgram ->
      let newProgramWithAssignmentsSimplified =
        newProgram
        |> mapExpNode
            (pathedPatIdToScopeEId pathedPatId)
            LangSimplify.simplifyAssignments
      in
      let caption = Utils.mergeStrings resultDescs |> Utils.stringReplace "Argument " "Arguments " in
      [ synthesisResult caption newProgramWithAssignmentsSimplified |> setResultSafe isSafe ]

    Nothing ->
      []


addArgFromPat : Syntax -> PathedPatternId -> PathedPatternId -> Exp -> List SynthesisResult
addArgFromPat syntax argSourcePathedPatId targetPathedPatId originalProgram =
  addArgFromPat_ syntax LangSimplify.simplifyAssignments argSourcePathedPatId targetPathedPatId originalProgram


addArgFromPat_ : Syntax -> (Exp -> Exp) -> PathedPatternId -> PathedPatternId -> Exp -> List SynthesisResult
addArgFromPat_ syntax postProcessFBody argSourcePathedPatId targetPathedPatId originalProgram =
  let funcToIsSafePatToInsertArgValExpAndNewFuncBody func fbody =
    case pluck argSourcePathedPatId fbody of
      Nothing ->
        let _ = Utils.log "could not pluck argument source pattern from inside the function" in
        Nothing

      Just ((newArgPat, newArgVal, False), fbodyWithoutPlucked) ->
        let varUsagesSame =
          let oldScopeAreas = findScopeAreas (pathedPatIdToScopeId argSourcePathedPatId) fbody in
          identifiersListInPat newArgPat
          |> List.all
              (\ident ->
                identifierUses ident fbodyWithoutPlucked == List.concatMap (identifierUses ident) oldScopeAreas
              )
        in
        Just <|
          ( varUsagesSame
          , newArgPat
          , newArgVal
          , postProcessFBody fbodyWithoutPlucked
          )

      Just ((_, _, True), _) ->
        let _ = Utils.log "cannot add recursive function as an argument" in
        Nothing

  in
  addArg_ syntax targetPathedPatId funcToIsSafePatToInsertArgValExpAndNewFuncBody originalProgram


addArg : Syntax -> EId -> PathedPatternId -> Exp -> List SynthesisResult
addArg syntax argSourceEId pathedPatId originalProgram =
  let funcToIsSafePatToInsertArgValExpAndNewFuncBody func fbody =
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
          ( True
          , patToInsert
          , argSourceExp
          , replaceExpNodePreservingPrecedingWhitespace argSourceEId (eVar argName) fbody
          )
  in
  addArg_ syntax pathedPatId funcToIsSafePatToInsertArgValExpAndNewFuncBody originalProgram


removeArgs : Syntax -> List PathedPatternId -> Exp -> List SynthesisResult
removeArgs syntax pathedPatIds originalProgram =
  let (maybeNewProgram, isSafe) =
    pathedPatIds
    |> List.sort
    |> List.foldr
        (\pathedPatId (maybePriorProgram, safeSoFar) ->
          case maybePriorProgram |> Maybe.map (removeArg syntax pathedPatId) of
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

removeArg : Syntax -> PathedPatternId -> Exp -> List SynthesisResult
removeArg syntax pathedPatId originalProgram =
  let ((funcEId, _), path) = pathedPatId in
  case findLetAndIdentBindingExp funcEId originalProgram of
    Just (letExp, funcName) ->
      case letExp.val.e__ of
        ELet ws1 letKind decls ws3 letBody ->
          let _ = Debug.log "TODO: CodeMotion.removeArg should be reimplemented to support ELet's new declarations" () in
          []
        {-
        ELet ws1 letKind isRec letPat ws2 func ws3 letBody ws4 ->
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
                            EApp appWs1 appFuncExp appArgs appType appWs2 ->
                              if Set.member appFuncExp.val.eid funcVarUsageEIds then
                                pluckExpFromExpsByPath path appArgs
                                |> Maybe.map
                                    (\(pluckedExp, remainingArgs) ->
                                      let newAppArgs =
                                        if remainingArgPats == [] && appArgs /= [] -- appArgs should never be []
                                        then [ eTuple [] ]
                                        else remainingArgs
                                      in
                                      (exp.val.eid, replaceE__ exp (EApp appWs1 appFuncExp newAppArgs appType appWs2), appFuncExp.val.eid, pluckedExp)
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
                                  (Dict.singleton argName (\varExp -> Parser.clearAllIds argReplacementValue |> setEId varExp.val.eid)) -- need to preserve EId of replacement site for free var safety check below
                                  fbody
                            in
                            let replacementLocationEIds =
                              identifierUses argName fbody |> List.map (.val >> .eid) -- original body (eids same in new body)
                            in
                            (newFBody, replacementLocationEIds)

                          _ ->
                            let inlinedArgEId = Parser.maxId originalProgram + 1 in
                            let newFBody =
                              newLetFancyWhitespace -1 False pluckedPat (argReplacementValue |> Parser.clearAllIds |> setEId inlinedArgEId) fbody originalProgram
                              -- withDummyExpInfo <|
                              --   ELet (precedingWhitespace fbody) Let False
                              --     (ensureWhitespacePat pluckedPat)
                              --     (argReplacementValue |> Parser.clearAllIds |> setEId inlinedArgEId |> ensureWhitespaceExp)
                              --     (ensureWhitespaceExp fbody) ""
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
                      [ synthesisResult ("Remove Argument " ++ (pluckedPat |> Syntax.patternUnparser syntax |> Utils.squish)) newProgram |> setResultSafe isSafe ]

                    _ ->
                      let _ = Debug.log "no uses to provide arg replacement value" transformedApplicationsWithRemovedCallsiteArgument in
                      []

                _ ->
                  let _ = Debug.log "cannot pluck argument" (path, fpats) in
                  []

            _ ->
              Debug.crash <| "CodeMotion.removeArg should've had an EFun here"
        -}
        _ ->
          Debug.crash <| "CodeMotion.removeArg expected findLetAndIdentBindingExp to return ELet"

    Nothing ->
      -- Can't find a name for this function. Arg removal probably unsafe.
      []


maybePathAfterPathsRemoved : List (List Int) -> List Int -> Maybe (List Int)
maybePathAfterPathsRemoved pathsRemoved path =
  pathsRemoved
  |> List.sort
  |> Utils.foldrMaybe
      pathAfterElementRemoved
      (Just path)


-- Reorder an expression list (supports raising/lowering of nested tuples)
tryReorderExps : List (List Int) -> List Int -> List (List Int) -> List Exp -> Maybe (List Exp)
tryReorderExps pathsToMove insertPath pathsToRemove exps =
  -- 1. remove exps from original locations
  -- 2. insert exps into new locations
  -- 3. remove exps as directed (when reordering arguments, this is used to kill empty lists created when all interior arguments are lifted to a higher level)
  case maybePathAfterPathsRemoved pathsToMove insertPath of
    Nothing ->
      let _ = Debug.log ("can't insert at that path in " ++ String.join " " (List.map (Syntax.unparser Syntax.Elm) exps)) insertPath in
      Nothing

    Just realInsertPath ->
      -- 1. remove exps from original locations
      let maybePluckedExpsAndExpsAfterRemoved1 =
        pathsToMove
        |> List.sort
        |> Utils.foldrMaybe
            (\pathToRemove (pluckedExps, remainingExps) ->
              pluckExpFromExpsByPath pathToRemove remainingExps
              |> Maybe.map (\(pluckedExp, remainingExps) -> (pluckedExp::pluckedExps, remainingExps))
            )
            (Just ([], exps))
      in
      -- 2. insert exps into new locations
      case maybePluckedExpsAndExpsAfterRemoved1 of
        Just (pluckedExps, expsRemainingAfterPluck) ->
          let maybeExpsAfterInsertion =
            pluckedExps
            |> Utils.foldrMaybe
                (\pluckedExp newExps -> addExpToExpsByPath pluckedExp realInsertPath newExps)
                (Just expsRemainingAfterPluck)
          in
          -- 3. remove exps as directed
          let maybeNewExps =
            let removeExpFromExpsByPath pathToRemove exps =
              pluckExpFromExpsByPath pathToRemove exps |> Maybe.map Tuple.second
            in
            pathsToRemove
            |> List.sort
            |> Utils.foldrMaybe
                removeExpFromExpsByPath
                maybeExpsAfterInsertion
          in
          maybeNewExps

        Nothing ->
          Nothing


-- This is way nastier than I want it to be, but not sure how to make it
-- nicer and still support e.g. moving all of the variables outside of a
-- list and removing the empty list.
reorderFunctionArgs : EId -> List (List Int) -> List Int -> Exp -> List SynthesisResult
reorderFunctionArgs funcEId paths targetPath originalProgram =
  case findLetAndIdentBindingExp funcEId originalProgram of -- Return LetExp and a way to rebuild the Let in the original program.
    Just (letExp, funcName) ->
      case letExp.val.e__ of
        ELet ws1 letKind decls spEq letBody ->
          let _ = Debug.log "TODO: CodeMotion.reorderFunctionArgs needs ELet's new definition to be implemented" () in
          []
        {-ELet ws1 letKind isRec letPat ws2 func ws3 letBody ws4 ->
          -- If func is passed to itself as an arg, this probably breaks. (is fixable though, with flow craziness)
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
              case maybePathAfterPathsRemoved pathsRemoved1 targetPath of
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
                                EApp appWs1 appFuncExp appArgs appType appWs2 ->
                                  if Set.member appFuncExp.val.eid funcVarUsageEIds then
                                    case tryReorderExps pathsRemoved1 targetPath pathsRemoved2 appArgs of
                                      Just newExps ->
                                        ( replaceE__ exp (EApp appWs1 appFuncExp newExps appType appWs2)
                                        , Set.insert appFuncExp.val.eid funcVarUsagesTransformed
                                        )

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
                      [ synthesisResult "Reorder Arguments" newProgram |> setResultSafe isSafe ]

            _ ->
              Debug.crash <| "CodeMotion.reorderFunctionArgs should've had an EFun here"
        -}
        _ ->
          Debug.crash <| "CodeMotion.reorderFunctionArgs expected findLetAndIdentBindingExp to return ELet"

    Nothing ->
      -- Can't find a name for this function. Arg removal probably unsafe.
      []

------------------------------------------------------------------------------

reorderExpressionsTransformation originalProgram selections =
  case selections of
    (_, _, [], _, _, _, _) -> Nothing
    (_, _, expIds, [], [], [expTarget], []) ->
      let (beforeAfter, expTargetEId) = expTarget in
      let relevantEIds = expTargetEId::expIds in
      -- tryReorderExps can handle rearrangement of nested tuples, e.g. [a [b c]] to [a b [c]]
      -- so let's find the outermost list of all relevant eids
      let maybeSharedAncestor =
        commonAncestors (\e -> List.member e.val.eid relevantEIds) originalProgram
        |> Utils.maybeLast
      in
      let reorder sharedAncestorEId expList makeNewAncestorE__ =
        let maybeInsertPath =
          case (beforeAfter, eidPathInExpList expList expTargetEId) of
            (Before, Just path) -> Just path
            (After,  Just path) -> pathRightSibling path
            _                   -> Nothing
        in
        let maybePathsToMove =
          expIds
          |> List.map (eidPathInExpList expList)
          |> Utils.projJusts
        in
        case Utils.bindMaybe2 (\pathsToMove insertPath -> tryReorderExps pathsToMove insertPath [] expList) maybePathsToMove maybeInsertPath of
          Nothing ->
            Nothing

          Just reorderedExpList ->
            let ancestorE__WithReorderedChildren =
              makeNewAncestorE__ (imitateExpListWhitespace expList reorderedExpList)
              --
            in
            let newProgram =
              replaceExpNodeE__ByEId sharedAncestorEId ancestorE__WithReorderedChildren originalProgram
            in
              Just <|
                \() ->
                  [synthesisResult "Reorder Expressions" newProgram]
      in
      case maybeSharedAncestor of
        Just sharedAncestor ->
          let sharedAncestorEId = sharedAncestor.val.eid in
          case sharedAncestor.val.e__ of
            EList ws1 listExps ws2 maybeTail ws3 -> reorder sharedAncestorEId (Utils.listValues listExps) (\newListExps -> EList ws1 (Utils.listValuesMake listExps newListExps) ws2 maybeTail ws3)
            EApp ws1 fExp argExps appType ws2    -> reorder sharedAncestorEId argExps  (\newArgExps  -> EApp ws1 fExp newArgExps appType ws2)
            EOp ws1 wso op operands ws2          -> reorder sharedAncestorEId operands (\newOperands -> EOp ws1 wso op newOperands ws2)
            _                                    -> Nothing

        _ ->
          Nothing
    _ ->
      Nothing


------------------------------------------------------------------------------

-- May want to consolidate with makeEqualTransformation
-- They're basically the same: makeEqualTransformation inserts a single variable for
-- all extraction locations, while introduceVarTransformation inserts several variables
-- for all extraction locations.
introduceVarTransformation m expIds maybeTargetPos =
  let insertNewLet insertedLetEId pat boundExp bindingNumber expToWrap program =
    ( newLetFancyWhitespace insertedLetEId False pat boundExp expToWrap program
    , Just insertedLetEId
    )
  in
  let addToExistingLet targetPath _ pat boundExp bindingNumber letExpToInsertInto _ =
    ( insertPat_ (pat, boundExp) targetPath letExpToInsertInto
    , Nothing
    )
  in
  case maybeTargetPos of
    Nothing ->
      Just <|
        \() ->
          let expToWrap = deepestCommonAncestorWithNewline m.inputExp (\e -> List.member e.val.eid expIds) in
          introduceVarTransformation_ m expIds (expToWrap.val.eid, 0 {- Binding number -}) insertNewLet

    Just (ExpTargetPosition (After, expTargetId)) ->
      Nothing

    Just (ExpTargetPosition (Before, expTargetId)) ->
      Just <|
        \() ->
          introduceVarTransformation_ m expIds (expTargetId, 0) insertNewLet

    Just (PatTargetPosition patTarget) ->
      case patTargetPositionToTargetPathedPatId patTarget of
        ((targetId, 1), targetPath) ->
          case findExpByEId m.inputExp targetId of
            Just scopeExp ->
              if isLet scopeExp then
                Just <|
                  \() ->
                    introduceVarTransformation_ m expIds (targetId, 0)
                      (addToExistingLet targetPath)
              else
                Nothing

            _ ->
              Nothing

        _ ->
          Nothing

-- Small bug: can't introduce var directly in front of expression being extracted.
introduceVarTransformation_ m eidsToExtract (addNewVarsAtThisId, addNewVarAtThisBindingNumber) makeNewLet =
  let toolName =
    "Introduce Variable" ++ (if List.length eidsToExtract == 1 then "" else "s")
  in
  let existingNamesToAvoid =
    let targetBodyEId =
       let exp = justFindExpByEId m.inputExp addNewVarsAtThisId in
       case exp.val.e__ of
         ELet _ _ _ _ e2 -> e2.val.eid
         _               -> exp.val.eid
    in
    -- If we could trust that the var target position was a higher scope, then the
    -- vars visible at the extraction site would be a superset of those at the
    -- target site, but hey we can't trust that we'll be asked to do something sane.
    visibleIdentifiersAtEIds m.inputExp (Set.fromList (targetBodyEId::eidsToExtract))
  in
  let (programUniqueNames, uniqueNameToOldName) = assignUniqueNames m.inputExp in
  let uniqueNameUsed = identifiersSetPlusPrelude programUniqueNames in
  let (newPatBoundExps, uniqueNameToOldNameAdditions, insertedVarEIdToBindingPId, programWithNewVarsUsed, newId) =
    eidsToExtract
    |> List.foldl
        (\eidToExtract (newPatBoundExps, uniqueNameToOldNameAdditions, insertedVarEIdToBindingPId, programWithSomeNewVarsUsed, newId) ->
          -- TODO version of scopeNamesLiftedThrough for EId instead of Loc?
          -- let scopes = scopeNamesLocLiftedThrough m.inputExp loc in
          -- let newVar = String.join "_" (scopes ++ [name]) in
          let name = expNameForEId m.inputExp eidToExtract in
          let namesToAvoid =
            Set.union existingNamesToAvoid
                (Set.fromList (Dict.values uniqueNameToOldNameAdditions))
          in
          let uniqueNamesToAvoid =
            Utils.unionAll [namesToAvoid, uniqueNameUsed, Dict.keys uniqueNameToOldNameAdditions |> Set.fromList]
          in
          let newVarName       = nonCollidingName name 1 namesToAvoid in
          let newVarUniqueName = nonCollidingName name 1 uniqueNamesToAvoid in
          let expToExtract = justFindExpByEId programUniqueNames eidToExtract in
          let (newPId, newEId) = (newId, newId + 1) in
          let newPat = pVar newVarUniqueName |> setPId newPId in
          let expWithNewVarUsed =
            replaceExpNodePreservingPrecedingWhitespace eidToExtract (eVar newVarUniqueName |> setEId newEId) programWithSomeNewVarsUsed
          in
          ( (newPat, expToExtract) :: newPatBoundExps
          , Dict.insert newVarUniqueName newVarName uniqueNameToOldNameAdditions
          , Dict.insert newEId (Just newPId) insertedVarEIdToBindingPId
          , expWithNewVarUsed, newId + 2
          )
        )
        ([], Dict.empty, Dict.empty, programUniqueNames, Parser.maxId m.inputExp + 1)
  in
  -- Not using Lang.patBoundExpOf b/c we need to preserve PIds/EIds for the safety checks.
  let (newPat, newBoundExp) =
    case List.unzip newPatBoundExps of
       ([singlePat], [singleBoundExp]) -> (singlePat, replacePrecedingWhitespace " " singleBoundExp)
       (pats, boundExps)               ->
         ( pList  (setPatListWhitespace "" " " pats)
         , eTuple (setExpListWhitespace "" " " boundExps)
         )
  in
  let (newProgramUniqueNames, maybeInsertedLetEId) =
    programWithNewVarsUsed
    |> mapFoldExp
        (\e maybeInsertedLetEId ->
          if e.val.eid == addNewVarsAtThisId
          then makeNewLet newId newPat newBoundExp addNewVarAtThisBindingNumber e programWithNewVarsUsed
          else (e, maybeInsertedLetEId)
        )
        Nothing
  in
  let letEIdWithNewVars = maybeInsertedLetEId |> Maybe.withDefault addNewVarsAtThisId in
  let newNames = identifiersListInPat newPat in
  let namesUniqueTouched = Set.fromList (newNames ++ identifiersList newBoundExp) in
  programOriginalNamesAndMaybeRenamedLiftedTwiddledResults
      toolName -- baseDescription
      (Dict.union uniqueNameToOldNameAdditions uniqueNameToOldName) -- uniqueNameToOldName
      (Just (letEIdWithNewVars, addNewVarAtThisBindingNumber)) -- maybeNewScopeEId
      ("touched", "untouched") -- (touchedAdjective, untouchedAdjective)
      namesUniqueTouched -- namesUniqueTouched
      [] -- varEIdsPreviouslyDeliberatelyRemoved
      insertedVarEIdToBindingPId -- insertedVarEIdToBindingPId
      programUniqueNames -- originalProgramUniqueNames
      newProgramUniqueNames -- newProgramUniqueNames


------------------------------------------------------------------------------

makeEqualTransformation originalProgram eids maybeTargetPosition =
  let insertNewLet insertedLetEId pat boundExp expToWrap program =
    ( newLetFancyWhitespace insertedLetEId False pat boundExp expToWrap program
    , Just (insertedLetEId, 1)
    )
  in
  let addToExistingLet targetPath _ pat boundExp letExpToInsertInto _ =
    ( insertPat_ (pat, boundExp) targetPath letExpToInsertInto
    , Nothing
    )
  in
  case maybeTargetPosition of
    Nothing ->
      Just <| \() ->
        let expToWrap = deepestCommonAncestorWithNewline originalProgram (\e -> List.member e.val.eid eids) in
        makeEqualTransformation_ originalProgram eids expToWrap.val.eid insertNewLet

    Just (ExpTargetPosition (After, expTargetId)) ->
      Nothing

    Just (ExpTargetPosition (Before, expTargetId)) ->
      Just <| \() ->
        makeEqualTransformation_ originalProgram eids expTargetId insertNewLet

    Just (PatTargetPosition patTarget) ->
      case patTargetPositionToTargetPathedPatId patTarget of
        ((targetId, 1), targetPath) ->
          case findExpByEId originalProgram targetId of
            Just scopeExp ->
              if isLet scopeExp then
                Just <| \() ->
                  makeEqualTransformation_ originalProgram eids targetId (addToExistingLet targetPath)
              else
                Nothing

            _ ->
              Nothing

        _ ->
          Nothing


makeEqualTransformation_ originalProgram eids newBindingLocationEId makeNewLet =
  let firstEId = Utils.head "CodeMotion.makeEqualTransform expected some eids, got []" eids in
  let potentialNames =
    let
      names = Utils.dedup (List.map (expNameForEId originalProgram) eids)
      joinedName = String.join "_" names
      commonName = commonNameForEIdsWithDefault joinedName originalProgram eids
      namesToAvoid = visibleIdentifiersAtEIds originalProgram (Set.fromList (newBindingLocationEId::eids))
    in
    commonName :: joinedName :: names
    |> List.map (\name -> nonCollidingName name 1 namesToAvoid)
    |> Utils.dedup
    |> List.sortBy (\s -> (String.length s, s))
  in
  let (originalProgramUniqueNames, uniqueNameToOldName) = assignUniqueNames originalProgram in
  let maxId = Parser.maxId originalProgram in
  let (insertedLetEId, insertedVarsEId, newBindingPId, dummyBoundExpEId) = (maxId + 1, maxId + 2, maxId + 3, maxId + 4) in
  let varTempName = "* New Variable *" in
  -- Replacement order is super wonky, but it prevents crashes when you try
  -- to insert the new binding right in front of one of the expressions you
  -- are trying to equalize. This is an odd case because you can't produce a
  -- sane program, but at least it won't crash.
  let newBoundExp = justFindExpByEId originalProgramUniqueNames firstEId in
  let (newLet, maybeNewScopeEId) =
    -- makeNewLet will either insert the variable into an existing let at
    -- newBindingLocationEId or introduce a new let around newBindingLocationEId
    makeNewLet
        insertedLetEId
        (pVar varTempName |> setPId newBindingPId)
        (newBoundExp |> setEId dummyBoundExpEId) -- newLetFancyWhitespace will set the correct whitespace for boundExp, but we don't want the expSubst below to destroy the boundExp
        (justFindExpByEId originalProgramUniqueNames newBindingLocationEId)
        originalProgramUniqueNames
  in
  let newProgramUniqueNames =
    let expSubst = eids |> List.map (\eid -> (eid, eVar varTempName |> setEId insertedVarsEId)) |> Dict.fromList in
    originalProgramUniqueNames
    |> replaceExpNode newBindingLocationEId newLet
    |> replaceExpNodesPreservingPrecedingWhitespace expSubst
    |> mapExpNode dummyBoundExpEId (setEId newBoundExp.val.eid)
  in
  let varEIdsPreviouslyDeliberatelyRemoved =
    eids
    |> List.drop 1
    |> List.concatMap (\eid -> justFindExpByEId originalProgramUniqueNames eid |> allVars)
    |> List.map (.val >> .eid)
  in
  let namesUniqueTouched = Set.insert varTempName (identifiersSet newBoundExp) in
  potentialNames
  |> List.concatMap
      (\varName ->
        programOriginalNamesAndMaybeRenamedLiftedTwiddledResults
            ("New variable: " ++ varName)
            (Dict.insert varTempName varName uniqueNameToOldName)
            maybeNewScopeEId
            ("touched", "untouched")
            namesUniqueTouched
            varEIdsPreviouslyDeliberatelyRemoved
            (Dict.singleton insertedVarsEId (Just newBindingPId)) -- insertedVarEIdToBindingPId
            originalProgramUniqueNames
            newProgramUniqueNames
      )


-- Tries to make minimal changes to expose EId to all viewers.
-- If EId already bound to a variable, either do nothing, or rename, or move the binding, as needed.
-- If EId is not bound to a variable, try to lift it and any dependencies.
-- Returns Maybe (newName, insertedVarEId, program)
makeEIdVisibleToEIds : Exp -> EId -> Set EId -> Maybe (Ident, EId, Exp)
makeEIdVisibleToEIds originalProgram mobileEId viewerEIds =
  let (originalProgramUniqueNames, uniqueNameToOldName) = assignUniqueNames originalProgram in
  let allViewerEIds = Set.insert mobileEId viewerEIds in
  let renameIfCollision mobileUniqueName uniqueNameToOldName viewerEIds program programUniqueNames =
    let mobileOriginalName = Utils.getWithDefault mobileUniqueName "" uniqueNameToOldName in
    -- Were the original name to be used, are we still good?
    let resolvesCorrectly viewerEId =
      maybeResolveIdentifierToExp mobileOriginalName viewerEId program
      |> Maybe.map (\e -> e.val.eid == mobileEId)
      |> Maybe.withDefault False
    in
    if List.all resolvesCorrectly (Set.toList viewerEIds) then
      Just (mobileOriginalName, mobileEId, program)
    else
      -- Apparently can't use original name, but it will work if we rename that variable.
      let uniqueNameToOldNameWithoutMobileName = Dict.remove mobileUniqueName uniqueNameToOldName in
      let newProgram = renameIdentifiers uniqueNameToOldNameWithoutMobileName programUniqueNames in
      Just (mobileUniqueName, mobileEId, newProgram)
  in
  case findLetAndIdentBindingExp mobileEId originalProgramUniqueNames of
    Just (bindingLet, mobileUniqueName) ->
      if viewerEIds |> Set.toList |> List.all (\viewerEId -> visibleIdentifiersAtEIds originalProgramUniqueNames (Set.singleton viewerEId) |> Set.member mobileUniqueName) then
        -- CASE 1: All viewers should already be able to see a variable binding the desired EId (discounting shadowing, which we handle by renaming below).
        renameIfCollision mobileUniqueName uniqueNameToOldName viewerEIds originalProgram originalProgramUniqueNames
      else
        -- CASE 2: EId already bound, but some viewers are not in its scope. Try to move binding.
        let expToWrap =
          deepestCommonAncestorWithNewline originalProgramUniqueNames (\e -> Set.member e.val.eid allViewerEIds)
        in
        let maybeProgramAfterMove =
          let bindingLetBoundExp = expToLetBoundExp bindingLet in
          let freeVarsAtNewLocation = freeVars expToWrap in
          -- Case 2.1: If target position is at the same level and boundExp free vars are the same at both locations (no recursive lifting needed), move the entire let. This will nicely preserve (def point @ [x y] [30 40])
          if List.member bindingLet (topLevelExps expToWrap) && List.all (
              \var -> List.member var freeVarsAtNewLocation) (List.concatMap freeVars bindingLetBoundExp) then
            moveEquationsBeforeEId Syntax.Elm [(bindingLet.val.eid, 0)] expToWrap.val.eid originalProgram -- Syntax only used for generating description, which we throw away
            |> Utils.findFirst isResultSafe -- Use a result that preserves the program binding structure.
            |> Maybe.map (\(SynthesisResult {exp}) -> exp)
          else
            -- Case 2.2: Move just the definition needed.
            let pathedPatId = bindingPathedPatternIdForUniqueName mobileUniqueName bindingLet |> Utils.fromJust_ "makeEIdVisibleToEIds: bindingPathedPatternIdForUniqueName mobileUniqueName originalProgramUniqueNames" in
            moveDefinitionsBeforeEId Syntax.Elm [pathedPatId] expToWrap.val.eid originalProgram -- Syntax only used for generating description, which we throw away
            |> Utils.findFirst isResultSafe -- Use a result that preserves the program binding structure.
            |> Maybe.map (\(SynthesisResult {exp}) -> exp)
        in
        case maybeProgramAfterMove of
          Nothing ->
            -- Not safe to move definition (e.g. may require plucking out of an as-pattern).
            -- Back-up plan: adding a new binding.
            makeEIdVisibleToEIdsByInsertingNewBinding originalProgram mobileEId viewerEIds

          Just programAfterMove ->
            let (maybeProgramAfterMoveUniqueNames, afterMoveUniqueNameToOldName) = assignUniqueNames programAfterMove in
            case findLetAndIdentBindingExp mobileEId maybeProgramAfterMoveUniqueNames of
              Nothing -> Debug.crash "makeEIdVisibleToEIds eids got screwed up somewhere"
              Just (_, mobileUniqueName) ->
                renameIfCollision mobileUniqueName afterMoveUniqueNameToOldName viewerEIds programAfterMove maybeProgramAfterMoveUniqueNames


    Nothing ->
      -- CASE 3: EId is not bound. Insert a new binding.
      makeEIdVisibleToEIdsByInsertingNewBinding originalProgram mobileEId viewerEIds


-- If you need to force the binding to be copied. makeEIdVisibleToEIds calls this as a last resort.
-- Returns Maybe (newName, insertedVarEId, program)
makeEIdVisibleToEIdsByInsertingNewBinding : Exp -> EId -> Set EId -> Maybe (Ident, EId, Exp)
makeEIdVisibleToEIdsByInsertingNewBinding originalProgram mobileEId viewerEIds =
  let (originalProgramUniqueNames, uniqueNameToOldName) = assignUniqueNames originalProgram in
  let allViewerEIds = Set.insert mobileEId viewerEIds in
  let expToWrap =
    deepestCommonAncestorWithNewline originalProgramUniqueNames (\e -> Set.member e.val.eid allViewerEIds)
  in
  let maxId = Parser.maxId originalProgramUniqueNames in
  let (insertedVarEId, newBindingPId) = (maxId + 1, maxId + 2) in
  let newProgramUniqueNames =
    let extractedExp = justFindExpByEId originalProgramUniqueNames mobileEId in
    originalProgramUniqueNames
    |> mapExpNode mobileEId
        (\e -> eVar "*EXTRACTED EXPRESSION*" |> setEId insertedVarEId)
    |> mapExpNode expToWrap.val.eid
        (\e -> newLetFancyWhitespace -1 False (pVar "*EXTRACTED EXPRESSION*" |> setPId newBindingPId) extractedExp e originalProgramUniqueNames )
  in
  let maybeNewProgramWithLiftedDependenciesOldNames =
    -- We're using tryResolvingProblemsAfterTransformNoTwiddling here to lift any needed dependencies and handle any shadowing that introduces.
    tryResolvingProblemsAfterTransformNoTwiddling
        ""        -- baseDescription
        (Dict.insert "*EXTRACTED EXPRESSION*" "*EXTRACTED EXPRESSION*" uniqueNameToOldName) -- uniqueNameToOldName
        Nothing   -- maybeNewScopeEId
        ("", "")  -- (touchedAdjective, untouchedAdjective)
        Set.empty -- namesUniqueTouched
        []        -- varEIdsPreviouslyDeliberatelyRemoved
        (Dict.singleton insertedVarEId (Just newBindingPId)) -- insertedVarEIdToBindingPId
        originalProgramUniqueNames
        newProgramUniqueNames
    |> Utils.findFirst isResultSafe
    |> Maybe.map (\(SynthesisResult {exp}) -> Parser.freshen exp)
  in
  let visibleNameSuggestion = expNameForEId originalProgram mobileEId in
  case maybeNewProgramWithLiftedDependenciesOldNames of
    Nothing -> Nothing
    Just newProgramWithLiftedDependenciesOldNames ->
      let namesToAvoid =
        let finalViewerEIds =
          newProgramWithLiftedDependenciesOldNames
          |> flattenExpTree
          |> List.filter (expToMaybeIdent >> (==) (Just "*EXTRACTED EXPRESSION*"))
          |> List.map (.val >> .eid)
          |> Set.fromList
          |> Set.union viewerEIds
        in
        visibleIdentifiersAtEIds newProgramWithLiftedDependenciesOldNames finalViewerEIds
      in
      let visibleName = nonCollidingName visibleNameSuggestion 2 namesToAvoid in
      Just (visibleName, insertedVarEId, renameIdentifier "*EXTRACTED EXPRESSION*" visibleName newProgramWithLiftedDependenciesOldNames)


-- Returns (newProgram, locIdToNewName, locIdToVarEId)
liftLocsSoVisibleTo : Exp -> Set LocId -> Set EId -> (Exp, Dict LocId Ident, Dict LocId EId)
liftLocsSoVisibleTo program mobileLocIdSet viewerEIds =
  liftLocsSoVisibleTo_ False program mobileLocIdSet viewerEIds


-- Returns (newProgram, locIdToNewName, locIdToVarEId)
copyLocsSoVisibleTo : Exp -> Set LocId -> Set EId -> (Exp, Dict LocId Ident, Dict LocId EId)
copyLocsSoVisibleTo program mobileLocIdSet viewerEIds =
  liftLocsSoVisibleTo_ True program mobileLocIdSet viewerEIds


-- Returns (newProgram, locIdToNewName, locIdToVarEId)
liftLocsSoVisibleTo_ : Bool -> Exp -> Set LocId -> Set EId -> (Exp, Dict LocId Ident, Dict LocId EId)
liftLocsSoVisibleTo_ copyOriginal program mobileLocIdSet viewerEIds =
  let makeEIdVisible =
    if copyOriginal
    then makeEIdVisibleToEIdsByInsertingNewBinding
    else makeEIdVisibleToEIds
  in
  mobileLocIdSet
  |> Set.foldl
      (\mobileLocId (program, locIdToNewName, locIdToVarEId) ->
        case locIdToEId program mobileLocId of
          Just mobileEId ->
            case makeEIdVisible program mobileEId viewerEIds of
              Just (newName, insertedEId, newProgram) ->
                -- let _ = Utils.log (newName ++ "\n" ++ LangUnparser.unparseWithIds newProgram) in
                ( newProgram
                , Dict.insert mobileLocId newName locIdToNewName
                , Dict.insert mobileLocId insertedEId locIdToVarEId
                )
              Nothing ->
                let _ = Utils.log "liftLocsSoVisibleTo: makeEIdVisible could not lift" in
                (program, locIdToNewName, locIdToVarEId)

          Nothing ->
            let _ = Utils.log "liftLocsSoVisibleTo: could not convert locId to EId" in
            (program, locIdToNewName, locIdToVarEId)
      )
      (program, Dict.empty, Dict.empty)


-- Right now, does simple loc lifting.
resolveValueHoles : Sync.Options -> Exp -> List Exp
resolveValueHoles syncOptions programWithHolesUnfresh =
  let
    programWithHoles = Parser.freshen programWithHolesUnfresh -- Need EIds on all inserted expressions.
    valHoles = programWithHoles |> flattenExpTree |> List.filter (expToMaybeHoleVal >> Utils.maybeToBool)
    holeVals = programWithHoles |> flattenExpTree |> List.filterMap expToMaybeHoleVal
    holeEIds = valHoles |> List.map (.val >> .eid)
    holeTraces = holeVals |> List.map valToTrace
    locIdsNeeded = holeTraces |> List.map (Sync.locsOfTrace syncOptions >> Set.map locToLocId) |> Utils.unionAll
    (programWithLocsLifted, locIdToNewName, _) = liftLocsSoVisibleTo programWithHoles locIdsNeeded (Set.fromList holeEIds)
    locIdToExp = locIdToExpFromFrozenSubstAndNewNames (Parser.substOf programWithHoles) locIdToNewName
  in
  Utils.zip holeEIds holeTraces
  |> List.foldl
      (\(holeEId, holeTrace) programSoFar ->
        let filledHole = traceToExp locIdToExp holeTrace in
        programSoFar |> replaceExpNode holeEId filledHole
      )
      programWithLocsLifted
  |> List.singleton


------------------------------------------------------------------------------

copyExpressionTransformation syntax originalProgram eids =
  let exps = List.map (justFindExpByEId originalProgram) eids in
  let uniqueExps = Utils.dedupBy (unparseWithUniformWhitespace True True) exps in
  if List.length uniqueExps < 2 then
    Nothing
  else
    Just <|
      \() ->
        let (originalProgramUniqueNames, uniqueNameToOldName) = assignUniqueNames originalProgram in
        uniqueExps
        |> List.map (.val >> .eid)
        |> List.concatMap
          (\eidToCopy ->
            let expToCopy = justFindExpByEId originalProgramUniqueNames eidToCopy in
            let expToCopyOriginalNames = justFindExpByEId originalProgram eidToCopy in
            let eidsToChange = List.filter ((/=) eidToCopy) eids in
            let newProgramUniqueNames =
              List.foldl
                 (\eid -> replaceExpNodePreservingPrecedingWhitespace eid expToCopy)
                 originalProgramUniqueNames
                 eidsToChange
            in
            let namesUniqueTouched = identifiersSet expToCopy in
            let varEIdsPreviouslyDeliberatelyRemoved =
              eidsToChange
              |> List.concatMap (justFindExpByEId originalProgramUniqueNames >> allVars)
              |> List.map (.val >> .eid)
            in
            programOriginalNamesAndMaybeRenamedLiftedTwiddledResults
                ("Copy expression: " ++ Utils.squish (Syntax.unparser syntax expToCopyOriginalNames))
                uniqueNameToOldName
                Nothing -- maybeNewScopeEId
                ("copied", "untouched")
                namesUniqueTouched
                varEIdsPreviouslyDeliberatelyRemoved
                Dict.empty -- insertedVarEIdToBindingPId
                originalProgramUniqueNames
                newProgramUniqueNames
          )

------------------------------------------------------------------------------

-- based off copyExpressionTransformation
swapExpressionsTransformation syntax originalProgram eid1 eid2 =
  let exp1 = justFindExpByEId originalProgram eid1 in
  let exp2 = justFindExpByEId originalProgram eid2 in
  if expsEquivalent exp1 exp2 then
    Nothing
  else if List.member exp1 (flattenExpTree exp2) || List.member exp2 (flattenExpTree exp1) then
    Nothing
  else
    Just <|
      \() ->
        let (originalProgramUniqueNames, uniqueNameToOldName) = assignUniqueNames originalProgram in
        let tempEId = Parser.maxId originalProgramUniqueNames + 1 in
        let exp1UniqueNames = justFindExpByEId originalProgramUniqueNames eid1 in
        let exp2UniqueNames = justFindExpByEId originalProgramUniqueNames eid2 in
        let newProgramUniqueNames =
          originalProgramUniqueNames
          |> replaceExpNodePreservingPrecedingWhitespace eid1 (exp1UniqueNames |> setEId tempEId)
          |> replaceExpNodePreservingPrecedingWhitespace eid2 exp1UniqueNames
          |> replaceExpNodePreservingPrecedingWhitespace tempEId exp2UniqueNames
        in
        let namesUniqueTouched = Set.union (identifiersSet exp1UniqueNames) (identifiersSet exp2UniqueNames) in
        programOriginalNamesAndMaybeRenamedLiftedTwiddledResults
            ("Swap " ++ Utils.squish (Syntax.unparser syntax exp1) ++ " and " ++ Utils.squish (Syntax.unparser syntax exp2))
            uniqueNameToOldName
            Nothing -- maybeNewScopeEId
            ("swapped", "untouched")
            namesUniqueTouched
            [] -- varEIdsPreviouslyDeliberatelyRemoved
            Dict.empty -- insertedVarEIdToBindingPId
            originalProgramUniqueNames
            newProgramUniqueNames
------------------------------------------------------------------------------

-- based off copyExpressionTransformation
swapDefinitionsTransformation syntax originalProgram pid1 pid2 =
  case (findPatAndBoundExpByPId pid1 originalProgram, findPatAndBoundExpByPId pid2 originalProgram) of
    (Just (pat1, boundExp1), Just (pat2, boundExp2)) ->
      if patsEquivalent pat1 pat2 && expsEquivalent boundExp1 boundExp2 then
        Nothing
      else if List.member boundExp1 (flattenExpTree boundExp2) || List.member boundExp2 (flattenExpTree boundExp1) then
        Nothing
      else
        Just <|
          \() ->
            let (originalProgramUniqueNames, uniqueNameToOldName) = assignUniqueNames originalProgram in
            let maxId = Parser.maxId originalProgramUniqueNames in
            let (tempPId, tempEId) = (maxId + 1, maxId + 2) in
            let (pat1UniqueNames, boundExp1UniqueNames) = findPatAndBoundExpByPId pid1 originalProgramUniqueNames |> Utils.fromJust_ "CodeMotion.swapDefinitionsTransformation" in
            let (pat2UniqueNames, boundExp2UniqueNames) = findPatAndBoundExpByPId pid2 originalProgramUniqueNames |> Utils.fromJust_ "CodeMotion.swapDefinitionsTransformation" in
            let newProgramUniqueNames =
              originalProgramUniqueNames
              |> replaceExpNodePreservingPrecedingWhitespace boundExp1UniqueNames.val.eid (boundExp1UniqueNames |> setEId tempEId)
              |> replaceExpNodePreservingPrecedingWhitespace boundExp2UniqueNames.val.eid boundExp1UniqueNames
              |> replaceExpNodePreservingPrecedingWhitespace tempEId boundExp2UniqueNames
              |> replacePatNodePreservingPrecedingWhitespace pat1UniqueNames.val.pid (pat1UniqueNames |> setPId tempPId)
              |> replacePatNodePreservingPrecedingWhitespace pat2UniqueNames.val.pid pat1UniqueNames
              |> replacePatNodePreservingPrecedingWhitespace tempPId pat2UniqueNames
            in
            let namesUniqueTouched = Utils.unionAll [identifiersSetInPat pat1UniqueNames, identifiersSetInPat pat2UniqueNames, identifiersSet boundExp1UniqueNames, identifiersSet boundExp2UniqueNames] in
            programOriginalNamesAndMaybeRenamedLiftedTwiddledResults
                ("Swap Definitions " ++ Utils.squish (Syntax.patternUnparser syntax pat1) ++ " and " ++ Utils.squish (Syntax.patternUnparser syntax pat2))
                uniqueNameToOldName
                Nothing -- maybeNewScopeEId
                ("swapped", "untouched")
                namesUniqueTouched
                [] -- varEIdsPreviouslyDeliberatelyRemoved
                Dict.empty -- insertedVarEIdToBindingPId
                originalProgramUniqueNames
                newProgramUniqueNames

    _ ->
      Nothing


------------------------------------------------------------------------------

rewriteOffsetTransformation m ppid nums =
  let eids = List.map Tuple.first nums in
  let isSafe = True in -- TODO
  if not isSafe then
    -- TODO can do some renaming to make it safe
    Nothing
  else
    case pluck ppid m.inputExp of
      Just ((p, eBase, False), _) ->
        case (p.val.p__, eBase.val.e__) of
          (PVar _ xBase _,  EConst _ nBase _ _) ->
            Just <| \() ->
              let newExp =
                List.foldl
                   (\(eid,(_,n,_,_)) ->
                     let eBasePlusOffset =
                       let diff = n - nBase in
                       if diff >= 0
                         then ePlus (eVar xBase) (eConst diff dummyLoc)
                         else eMinus (eVar xBase) (eConst (-1 * diff) dummyLoc)
                     in
                     replaceExpNodePreservingPrecedingWhitespace eid eBasePlusOffset)
                   m.inputExp nums
              in
                oneSafeResult newExp
          _ ->
            Nothing
      _ ->
        Nothing

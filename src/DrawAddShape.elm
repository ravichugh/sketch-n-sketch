module DrawAddShape exposing (addShape, maybeShapeCountAndListItemCountInContextOutput)

-- The "addShape" function that does the heavy lifting of adding some binding into the drawing context
-- and adding the new variable into the shape list / return list.
--
-- Used by Draw.addShapeToModel, InterfaceController.doDuplicate, InterfaceController.addToOutput, ValueBasedTransform.repeatUsingFunction

import AlgorithmJish
import CodeMotion
import FastParser
import FocusedEditingContext
import InterfaceModel
import Eval
import Lang exposing (..)
import LangTools
import LangSvg
import LangUnparser
import Solver
import StaticAnalysis
import Sync
import Syntax
import Types
import Utils

import Dict
import Set


maybeShapeCountAndListItemCountInContextOutput
  :  { a | showPreludeOffsets : Bool, slideNumber : Int, movieNumber : Int, movieTime : Float, syntax : Syntax.Syntax, solutionsCache : Solver.SolutionsCache, syncOptions : Sync.Options, maybeEnv : Maybe Env, editingContext : Maybe (EId, b) }
  -> Exp
  -> Maybe (Int, Int)
maybeShapeCountAndListItemCountInContextOutput model program =
  case InterfaceModel.runAndResolveAtContext model program of
    Ok (val, _, (root, shapeTree), _, _) ->
      Just <|
          ( Dict.size shapeTree
          , val |> vListToMaybeValsExcludingPoint |> Maybe.map List.length |> Maybe.withDefault 1
          )

    Err _ ->
      Nothing

-- Precondition: Incoming program should have non-dummy EIds on expressions, for the typechecker.
--
-- 1. Find all list literals.
-- 2. Make candidate programs by adding both `shape` and `[shape]` to the end of each list.
-- 3. Resolve value holes.
-- 4. Keep those programs that do not crash.
-- 5. Keep those programs that result in one more shape in the output.
-- 6. Finally, use list the others do not depend on.
addShape
  :  { a | showPreludeOffsets : Bool, slideNumber : Int, movieNumber : Int, movieTime : Float, syntax : Syntax.Syntax, solutionsCache : Solver.SolutionsCache, syncOptions : Sync.Options, maybeEnv : Maybe Env, editingContext : Maybe (EId, b) }
  -> (Exp -> Bool) -> Maybe String -> Exp -> Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Bool -> Exp -> List Exp
addShape
  model
  targetListFilter
  maybeNewShapeName
  newShapeExp
  maybeNumberOfNewShapesExpected -- If not Nothing, overridden with number of shapes actually counted in newShapeExp
  maybeNumberOfNewShapesExpectedIfListInlined -- If provided, may attempt to inline newShapeExp if it is a list
  maybeNumberOfNewListItemsExpected
  maybeNumberOfNewListItemsExpectedIfListInlined -- If provided, may attempt to inline newShapeExp if it is a list
  areCrashingProgramsOkay -- Caller may attempt alternative problem resolution. If True, above four args are ignored.
  originalProgram =
  let
    contextExp         = FocusedEditingContext.drawingContextExp model.editingContext originalProgram
    -- _ = inferredReturnType |> List.map (Syntax.typeUnparser Syntax.Elm) |> Debug.log "inferredReturnType"
    -- _ = Utils.log <| "addShape incoming program: " ++ LangUnparser.unparseWithIds originalProgram
    idToTypeAndContextThunk           = AlgorithmJish.inferTypes originalProgram
    inferredReturnTypeAndContextThunk = Dict.get (expEffectiveExp contextExp).val.eid idToTypeAndContextThunk
    inferredReturnType                = inferredReturnTypeAndContextThunk |> Maybe.map Tuple.first
    typeContextAtReturnType           = inferredReturnTypeAndContextThunk |> Maybe.map (Tuple.second >> (\thunk -> thunk ())) |> Maybe.withDefault AlgorithmJish.preludeTypeContext

    incomingExpFreshened              = FastParser.freshen newShapeExp
    incomingType                      = AlgorithmJish.inferOne typeContextAtReturnType incomingExpFreshened
    _                                 = Utils.log ("incomingType: " ++ Syntax.typeUnparser Syntax.Elm incomingType)


    eidsThatWouldMakeARecursiveFunction =
      let identBoundExps = LangTools.allSimplyResolvableLetBindings contextExp in -- If you've focused the function, it's on you if you try to make it recursive.
      LangTools.freeIdentifiers incomingExpFreshened
      |> Set.toList
      |> List.filterMap (flip Utils.maybeFind identBoundExps)
      |> List.concatMap allEIds
      |> Set.fromList


    isPossibleTargetList exp =
      isList exp &&
      targetListFilter exp &&
      (not <| Set.member exp.val.eid eidsThatWouldMakeARecursiveFunction) &&
      -- Optimization: exclude numeric lists/tuples
      case Dict.get exp.val.eid idToTypeAndContextThunk of
        Just (listExpType, _) ->
          -- childExps exp == [] || -- Empty list
          let _ = Utils.log ("listExpType: " ++ Syntax.typeUnparser Syntax.Elm listExpType) in
          List.all (AlgorithmJish.doesUnify (Types.tList incomingType)) (childTypes listExpType) || -- list literal wrapped in concat
          List.all (AlgorithmJish.doesUnify incomingType) (childTypes listExpType) || -- incoming exp is same type as list literal items
          AlgorithmJish.doesUnify incomingType listExpType -- if wrapped in concat

        Nothing -> childExps exp == [] -- Empty list


    -- 1. Find all list literals.
    possibleTargetLists = flattenExpTree contextExp |> List.filter isPossibleTargetList
    -- _ = Debug.log "possibleTargetLists" <| List.map (Syntax.unparser Syntax.Elm) possibleTargetLists

    -- 1.5 If the return value isn't a list, make some candidates where the return value is a wrapped in a singleton.
    maybeProgramWithListifiedReturnExpAndLists =
      let tryToListifyReturnValue =
        case inferredReturnType of
          Just t -> not (Types.isListNotTuple t)
          _      -> False
      in
      if tryToListifyReturnValue then
        let
          terminalExpLevels = LangTools.terminalExpLevels contextExp
          programWithListifiedReturnExp =
            terminalExpLevels
            |> Utils.foldr
                originalProgram
                (\terminalExp program ->
                  let eidToWrap = (LangTools.lastSameLevelExp terminalExp).val.eid in
                  program
                  |> mapExpNode eidToWrap (\expToWrap -> eTuple [removePrecedingWhitespace expToWrap] |> copyPrecedingWhitespace expToWrap)
                )
            |> FastParser.freshen

          possibleTargetLists =
            LangTools.justFindExpByEId programWithListifiedReturnExp contextExp.val.eid
            |> LangTools.terminalExpLevels
            |> List.map LangTools.lastSameLevelExp
            |> List.filter isList
        in
        Just (programWithListifiedReturnExp, possibleTargetLists)
      else
        Nothing

    (oldShapeCount, oldListItemsCount) = maybeShapeCountAndListItemCountInContextOutput model originalProgram |> Maybe.withDefault (0, 0)
    _ = Debug.log "(oldShapeCount, oldListItemsCount)" (oldShapeCount, oldListItemsCount)

    (incomingShapeCount, incomingListItemsCount) =
      -- model.maybeEnv is the env at the beginning of the context---which we want later for conservatively resolving holes.
      -- But here we need the env at the end of the context in case of custom drawing functions.
      -- Can't just build a pseudoprogram because in that case focusing a custom drawing tool will cause a non-terminating recursive function.
      let envAtEndOfContext =
        let endOfContextEnvEId = (expEffectiveExp contextExp).val.eid in
        Eval.doEvalEarlyAbort False (Just endOfContextEnvEId) (.val >> .eid >> (==) endOfContextEnvEId) model.syntax Eval.initEnv originalProgram
        |> Result.map (\((val, widgets), maybeContextEnv, pbeHolesSeen) -> maybeContextEnv)
        |> Result.toMaybe
        |> Maybe.withDefault Nothing
        |> Maybe.withDefault Eval.initEnv
      in
      Eval.doEval False model.syntax envAtEndOfContext newShapeExp
      |> Result.andThen
          (\((val, _), _, _) ->
            LangSvg.resolveToRootedIndexedTree False model.syntax model.slideNumber model.movieNumber model.movieTime val
            |> Result.map
                (\(root, shapeTree) ->
                  ( if Dict.size shapeTree > 1 then Dict.size shapeTree - 1 else Dict.size shapeTree -- Lists of shapes are wrapped in an ['svg' ... ...] wrapper that adds 1 to the shape count.
                  , val |> vListToMaybeValsExcludingPoint |> Maybe.map List.length |> Maybe.withDefault 1
                  )
                )
          )
      |> Result.toMaybe
      |> Maybe.withDefault (0, 0)

    _ = Debug.log "(incomingShapeCount, incomingListItemsCount)" (incomingShapeCount, incomingListItemsCount)

    -- Should we try to inline the item to add?
    (maybeReallyNumberOfNewShapesExpected, maybeReallyNumberOfNewListItemsExpected, incomingExpShouldBeInlined) =
      Debug.log "(maybeReallyNumberOfNewShapesExpected, maybeReallyNumberOfNewListItemsExpected, incomingExpShouldBeInlined)" <|
      if maybeNumberOfNewShapesExpectedIfListInlined /= Nothing || maybeNumberOfNewListItemsExpectedIfListInlined /= Nothing then
        case inferredReturnType of
          Just retType ->
            -- let _ = Utils.log ("incomingExpFreshened: "    ++ Syntax.unparser Syntax.Elm incomingExpFreshened) in
            -- let _ = Utils.log ("typeContextAtReturnType: " ++ toString (List.map (flip Utils.maybeFind typeContextAtReturnType >> Maybe.map (Syntax.typeUnparser Syntax.Elm)) (LangTools.freeIdentifiers incomingExpFreshened |> Set.toList))) in
            -- let _ = Utils.log ("retType: "                 ++ Syntax.typeUnparser Syntax.Elm retType) in
            -- let _ = Utils.log ("incomingType: "            ++ Syntax.typeUnparser Syntax.Elm incomingType) in
            if Types.isListOrTuple retType && Types.isListOrTuple incomingType && AlgorithmJish.doesUnify retType incomingType then
              ( maybeNumberOfNewShapesExpectedIfListInlined
              , maybeNumberOfNewListItemsExpectedIfListInlined
              , True
              )
            else
              (maybeNumberOfNewShapesExpected |> Maybe.map (always incomingShapeCount), maybeNumberOfNewListItemsExpected, False)

          _ ->
            (maybeNumberOfNewShapesExpected |> Maybe.map (always incomingShapeCount), maybeNumberOfNewListItemsExpected, False)
      else
        (maybeNumberOfNewShapesExpected |> Maybe.map (always incomingShapeCount), maybeNumberOfNewListItemsExpected, False)

    -- 2. Make candidate programs by adding both `shape` and `[shape]` to the end of each list.
    --    If return val is not a list, make it a list.
    candidatesForList originalProgram listExp =
      let
        (newListItemExp, programPerhapsWithNewDef) =
          case maybeNewShapeName of
            Just newShapeName ->
              let (varName, programWithNewDef) = LangTools.newVariableVisibleTo -1 newShapeName 1 newShapeExp [listExp.val.eid] originalProgram in
              (eVar varName, programWithNewDef)
            Nothing ->
              (newShapeExp, originalProgram)

        (ws1, heads, ws2, maybeTail, ws3) = LangTools.expToListParts listExp
        newListFlat    = replaceE__ listExp <| EList ws1 (List.map ((,) space0) (imitateExpListWhitespace_ heads ws3.val (heads ++ [newListItemExp]))) ws2 maybeTail ws3
        newProgramFlat = programPerhapsWithNewDef |> replaceExpNode listExp.val.eid newListFlat
        newCandidates =
          if incomingExpShouldBeInlined then
            -- In case new item is actually a list of new items instead of a single, may need to change listExp to a concat.
            let newConcat             = eCall "concat" [eTuple [removePrecedingWhitespace listExp, newListItemExp]] |> copyPrecedingWhitespace listExp in
            let newProgramConcatAdded = programPerhapsWithNewDef |> replaceExpNode listExp.val.eid newConcat in
            [newProgramFlat, newProgramConcatAdded]
          else
            let newListSingleton    = replaceE__ listExp <| EList ws1 (List.map ((,) space0) (imitateExpListWhitespace_ heads ws3.val (heads ++ [eTuple [removePrecedingWhitespace newListItemExp]]))) ws2 maybeTail ws3 in
            let newProgramSingleton = programPerhapsWithNewDef |> replaceExpNode listExp.val.eid newListSingleton in
            [newProgramFlat, newProgramSingleton]
      in
      -- 3. Resolve value holes.
      newCandidates
      -- |> List.map (\candidate -> let _ = Utils.log <| "addShape candidate before hole resolution: " ++ LangUnparser.unparseWithIds candidate in candidate)
      |> List.concatMap (CodeMotion.resolveValueAndLocHoles model.solutionsCache model.syncOptions model.maybeEnv)
      -- |> List.map (\candidate -> let _ = Utils.log <| "addShape candidate after hole resolution: " ++ LangUnparser.unparseWithIds candidate in candidate)
      |> List.map ((,) listExp.val.eid)

    listEIdWithPossiblePrograms =
      possibleTargetLists
      |> List.concatMap (candidatesForList originalProgram)
      |> (\candidates ->
        case maybeProgramWithListifiedReturnExpAndLists of
          Just (programWithListifiedReturnExp, possibleTargetLists) -> candidates ++ List.concatMap (candidatesForList programWithListifiedReturnExp) possibleTargetLists
          Nothing                                                   -> candidates
      )
      -- 4. Keep those programs that do not crash.
      -- 5. Keep those programs that result in one more shape in the output.
      |> List.filter
          (\(listEId, newProgram) ->
            -- let _ = LangTools.logProgram "checking candidate" newProgram in
            areCrashingProgramsOkay ||
            case maybeShapeCountAndListItemCountInContextOutput model newProgram of
              Just (newShapeCount, newListItemsCount) ->
                let
                  shapeCountOkay =
                    case maybeReallyNumberOfNewShapesExpected of
                      Just numberOfNewShapesExpected -> oldShapeCount + numberOfNewShapesExpected == newShapeCount
                      Nothing                        -> oldShapeCount <= newShapeCount -- Removing shapes signifies a type error

                  listItemCountOkay =
                    case maybeReallyNumberOfNewListItemsExpected of
                      -- Just numberOfNewListItemsExpected -> Debug.log "expect count" (oldListItemsCount + numberOfNewListItemsExpected) == Debug.log "actual count " (vListToMaybeValsExcludingPoint val |> Maybe.map List.length |> Maybe.withDefault 1)
                      Just numberOfNewListItemsExpected -> oldListItemsCount + numberOfNewListItemsExpected == newListItemsCount
                      Nothing                           -> oldListItemsCount <= newListItemsCount -- Removing items signifies a type error

                  -- _ = Utils.log (Syntax.unparser Syntax.Elm newProgram)
                  -- _ = Debug.log "(shapeCountOkay, listItemCountOkay)" (shapeCountOkay, listItemCountOkay)
                in
                shapeCountOkay && listItemCountOkay

              _ ->
                -- let _ = Utils.log <| "Bad program " ++ Syntax.unparser Syntax.Elm newProgram in
                False
          )

    -- _ = Debug.log "List.length listEIdWithPossiblePrograms" (List.length listEIdWithPossiblePrograms)

    -- 6. Finally, choose best program.
    --     1. Prefer modifying list the other lists did not depend on.
    --     2. Prefer shorter programs.
    (listEIds, _) = List.unzip listEIdWithPossiblePrograms
    grossDependencies = StaticAnalysis.grossDependencies originalProgram
    (_, rankedNewPrograms) =
      listEIdWithPossiblePrograms
      |> List.sortBy
          (\(listEId, candidateProgram) ->
            let
              -- For Koch curve example, don't seem to need to prefer programs that don't produce a type error at the return location(s). So let's skip this check for simplicity and speed.
              sortKey =
                ( listEIds |> List.map (\otherListEId -> if StaticAnalysis.isDependentOn grossDependencies otherListEId listEId then 1 else 0) |> List.sum
                , LangTools.nodeCount candidateProgram
                )
              -- _ = Utils.log (Syntax.unparser Syntax.Elm candidateProgram)
              -- _ = Debug.log "sortKey" sortKey
            in
            sortKey
          )
      |> List.unzip
  in
  rankedNewPrograms
  -- |> LangTools.logProgram "DrawAddShape.addShape result"

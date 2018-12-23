module FocusedEditingContext exposing (..)

import Lang exposing (..)
import LangTools
import Eval
import Syntax
import WidgetsFromEnv
import Utils

import Dict
import Set exposing (Set)


-- Where to insert new shapes?
-- Steps into function if function is focused. Returns whole program if no editingContext specified.
drawingContextExp : Maybe (EId, a) -> Exp -> Exp
drawingContextExp editingContext program =
  case maybeFocusedExp editingContext program of
    Just focusedExp -> focusedExp |> LangTools.expToMaybeFuncBody |> Maybe.withDefault focusedExp
    Nothing         -> program


-- Used to find what's in scope at the shape list site or end of focused function
-- Doesn't handle recursive functions (Instead see Draw.contextExpAndEndOfDrawingContext)
eidAtEndOfDrawingContext : Maybe (EId, a) -> Exp -> EId
eidAtEndOfDrawingContext editingContext program =
  drawingContextExp editingContext program
  |> LangTools.lastSameLevelExp
  |> (.val >> .eid)


maybeFocusedExp : Maybe (EId, a) -> Exp -> Maybe Exp
maybeFocusedExp editingContext program =
  case editingContext of
    Just (eid, _) -> findExpByEId program eid
    _             -> Nothing


-- Bug not worth fixing right now:
-- If the function is recursive, only ever returns the end of the recursive branch.
contextExpAndEndOfDrawingContextExp : Maybe (EId, a) -> Exp -> (Exp, Exp)
contextExpAndEndOfDrawingContextExp editingContext program =
  let contextExp = drawingContextExp editingContext program in
  let endOfDrawingContextExp =
    case maybeFocusedExp editingContext program |> Maybe.map (LangTools.findRecursiveBranch program) of
      Just (Just recursiveBranchExp)  -> LangTools.lastSameLevelExp recursiveBranchExp
      _                               -> LangTools.lastSameLevelExp contextExp
  in
  (contextExp, endOfDrawingContextExp)


contextInputVals : Maybe (EId, a) -> Maybe Env -> Exp -> List Val
contextInputVals editingContext maybeEnv program =
  case (maybeEnv, maybeFocusedExp editingContext program) of
    (Just env, Just focusedExp) ->
      case LangTools.expToMaybeFuncPats focusedExp of
        Just funcPats ->
          let
            -- Doing this right is a little annoying.
            -- We need the non-shadowed variables introduced at the function.
            -- So figure out which ones are not shadowed...
            contextExp     = drawingContextExp editingContext program
            patEnv         = LangTools.expPatEnvAt_ program (expEffectiveExp contextExp).val.eid |> Maybe.withDefault Dict.empty
            inputIdentPats = funcPats |> List.concatMap LangTools.indentPatsInPat
          in
          inputIdentPats
          |> List.filterMap
              (\(ident, funcPat) ->
                case Dict.get ident patEnv of
                  Just (identPat, _) ->
                    if identPat.val.pid == funcPat.val.pid
                    then Utils.maybeFind ident env
                    else Nothing
                  _ -> Nothing
              )
        _ ->
          []
    _ ->
      []


-- Locations for new binding should be no deeper than the currently focused scope.
insertionLocationEIdsForContext : Maybe (EId, a) -> Exp -> Set EId
insertionLocationEIdsForContext editingContext program =
  eidAtEndOfDrawingContext editingContext program
  |> findWithAncestorsByEId program
  |> Maybe.withDefault []
  |> List.map (.val >> .eid)
  |> Set.fromList


-- -- Locations for new binding should be no deeper than the currently focused scope.
-- isValidInsertionLocationExpForContext : Maybe (EId, a) -> Exp -> (Exp -> Bool)
-- isValidInsertionLocationExpForContext editingContext program =
--   let insertionLocationEIds = insertionLocationEIdsForContext editingContext program in
--   (\exp -> Set.member exp.val.eid insertionLocationEIds)


-- maybeSynthesisContext : Syntax.Syntax -> Maybe (EId, Maybe EId) -> Exp -> Maybe (Env, Maybe Ident, Exp, List Val)
-- maybeSynthesisContext syntax editingContext program =
--   case editingContext of
--     Just (focusedEId, Just callEId) ->
--       let
--         contextExp         = LangTools.justFindExpByEId program focusedEId
--         callExp            = LangTools.justFindExpByEId program callEId
--         (funcExp, argExps) = LangTools.expToAppFuncAndArgs callExp
--         psuedoProgram      = program |> replaceExpNodeE__ByEId callEId (eTuple (funcExp::argExps)).val.e__
--       in
--       if isFunc contextExp then
--         Eval.doEvalEarlyAbort Nothing (.val >> .eid >> (==) callEId) syntax Eval.initEnv psuedoProgram
--         |> Result.toMaybe
--         |> Maybe.andThen
--             (\((val, widgets), maybeContextEnv, _) ->
--               case val.v_ of
--                 VList (funcVal::argVals) ->
--                   case funcVal.v_ of
--                     VClosure maybeRecName funcPats funcBody closureEnv -> Just (closureEnv, maybeRecName, contextExp, argVals)
--                     _                                                  -> Nothing
--                 _ ->
--                   Nothing
--             )
--       else
--         Nothing
--
--     _ ->
--       Nothing



evalAtContext : Bool -> Syntax.Syntax -> Maybe (EId, Maybe EId) -> Exp -> Result String ((Val, Widgets), Maybe Env, List Eval.PBEHoleSeen)
evalAtContext showPreludeOffsets syntax editingContext program =
  -- let returnEnvAtExp ret env =
  --   -- What env gets returned from the evaluator is wacky.
  --   -- Here, we always want the env the expression saw.
  --   ret |> Result.map (\((v, ws), badEnv) -> ((v, ws), env))
  -- in
  case editingContext of
    Just (focusedEId, maybeCallEId) ->
      -- InterfaceModel.evalAtFocusedContext need the environment present at the evaluated expression.
      let
        abortEId = maybeCallEId |> Maybe.withDefault focusedEId
        abortPred = (.val >> .eid >> (==) abortEId)
        contextExp = LangTools.justFindExpByEId program focusedEId
        envEId =
          contextExp
          |> LangTools.expToMaybeFuncBody
          |> Maybe.withDefault contextExp
          -- |> expEffectiveExp -- For hole resolution, want the env at the beginning of the function/program, before the lets.
          |> (.val >> .eid)
      in
      Eval.doEvalEarlyAbort showPreludeOffsets (Just envEId) abortPred syntax Eval.initEnv program
      |> Result.map
          (\((val, widgets), maybeContextEnv, pbeHolesSeen) ->
            case maybeContextEnv of
              Just env ->
                let
                  withWidgetsFromEnv =
                    WidgetsFromEnv.widgetsFromEnv env
                    |> Utils.foldr
                        widgets
                        Eval.addSubsumingPriorWidgets
                in
                ((val, withWidgetsFromEnv), maybeContextEnv, pbeHolesSeen)

              Nothing ->
                ((val, widgets), maybeContextEnv, pbeHolesSeen)
          )

    Nothing ->
      -- let envEId = (expEffectiveExp program).val.eid in
      let envEId = program.val.eid in -- Want the env at the beginning of the function/program, before the lets.
      Eval.doEvalEarlyAbort showPreludeOffsets (Just envEId) Eval.runUntilTheEnd syntax Eval.initEnv program


editingContextFromMarkers : Exp -> Maybe (EId, Maybe EId)
editingContextFromMarkers program =
  let expToMaybeFocusedExp exp =
    case exp.val.e__ of
      EComment _ " *** Focused Definition ***" commentBody ->
        LangTools.firstNonComment commentBody
        |> LangTools.expToMaybeLetBoundExp
      EComment _ " *** Focused Expression ***" commentBody ->
        Just (LangTools.firstNonComment commentBody)
      _ ->
        Nothing
  in
  let expToMaybeExampleCall exp =
    case exp.val.e__ of
      EComment _ " *** Example Call ***" commentBody -> findFirstNode isApp commentBody
      _                                              -> Nothing
  in
  mapFirstSuccessNode expToMaybeFocusedExp program
  |> Maybe.map
      (\focusedExp ->
        if isFunc focusedExp then
          let maybeExampleCallEId = mapFirstSuccessNode expToMaybeExampleCall program |> Maybe.map (.val >> .eid) in
          (focusedExp.val.eid, maybeExampleCallEId)
        else
          (focusedExp.val.eid, Nothing)
      )


clearEditingContextMarkers : Exp -> Exp
clearEditingContextMarkers exp =
  exp
  |> mapExp
      (\exp ->
        case exp.val.e__ of
          EComment _ " *** Focused Definition ***" commentBody -> commentBody |> copyPrecedingWhitespace exp
          EComment _ " *** Focused Expression ***" commentBody -> commentBody |> copyPrecedingWhitespace exp
          EComment _ " *** Example Call ***"       commentBody -> commentBody |> copyPrecedingWhitespace exp
          _                                                    -> exp
      )


setEditingContextMarkers : EId -> Maybe EId -> Exp -> Exp
setEditingContextMarkers focusedEId maybeExampleCallEId program =
  program
  |> mapExp
      (\exp ->
        case (exp.val.eid == focusedEId, LangTools.expToMaybeLetBoundExp exp) of
          (True, _) ->
            eComment " *** Focused Expression ***" (exp |> replacePrecedingWhitespace ("  " ++ indentationAt exp.val.eid program))
            |> copyPrecedingWhitespace exp

          (_, Just boundExp) ->
            if List.member focusedEId (expEffectiveEIds boundExp) then
              -- Need to clear deeper markers because expression will already have been marked.
              eComment " *** Focused Definition ***" (clearEditingContextMarkers exp |> replacePrecedingWhitespace (indentationAt exp.val.eid program))
              |> copyPrecedingWhitespace exp
            else
              exp

          _ ->
            exp
      )
  |> case maybeExampleCallEId of
      Nothing             -> identity
      Just exampleCallEId ->
        mapExp
            (\exp ->
              if exp.val.eid == exampleCallEId then
                eComment " *** Example Call ***" (exp |> replacePrecedingWhitespace ("  " ++ indentationAt exp.val.eid program))
                |> copyPrecedingWhitespace exp
              else
                exp
            )

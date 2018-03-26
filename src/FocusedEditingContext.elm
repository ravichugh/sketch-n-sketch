module FocusedEditingContext exposing (..)

import Lang exposing (..)
import LangTools
import Eval
import Syntax
import WidgetsFromEnv
import Utils

import Dict


-- Where to insert new shapes?
drawingContextExp : Maybe (EId, Maybe EId) -> Exp -> Exp
drawingContextExp editingContext program =
  case editingContext of
    Just (eid, _) ->
      let contextExp = LangTools.justFindExpByEId program eid in
      contextExp |> LangTools.expToMaybeFuncBody |> Maybe.withDefault contextExp

    _ -> program


maybeFocusedExp : Maybe (EId, Maybe EId) -> Exp -> Maybe Exp
maybeFocusedExp editingContext program =
  case editingContext of
    Just (eid, _) -> findExpByEId program eid
    _             -> Nothing


contextInputVals : Maybe (EId, Maybe EId) -> Maybe Env -> Exp -> List Val
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



evalAtContext : Syntax.Syntax -> Maybe (EId, Maybe EId) -> Exp -> Result String ((Val, Widgets), Maybe Env)
evalAtContext syntax editingContext program =
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
          |> expEffectiveExp
          |> (.val >> .eid)
      in
      Eval.doEvalEarlyAbort (Just envEId) abortPred syntax Eval.initEnv program
      |> Result.map
          (\((val, widgets), maybeContextEnv) ->
            case maybeContextEnv of
              Just env ->
                let
                  withWidgetsFromEnv =
                    WidgetsFromEnv.widgetsFromEnv env
                    |> Utils.foldr
                        widgets
                        Eval.addSubsumingPriorWidgets
                in
                ((val, withWidgetsFromEnv), maybeContextEnv)

              Nothing ->
                ((val, widgets), maybeContextEnv)
          )

    Nothing ->
      let envEId = (expEffectiveExp program).val.eid in
      Eval.doEvalEarlyAbort (Just envEId) Eval.runUntilTheEnd syntax Eval.initEnv program


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

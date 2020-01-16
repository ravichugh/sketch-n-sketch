module TSEFLLP exposing (prepare, newLangValResult, mousePosition, mouseOut, selectOnePath, togglePathSelection, deselectPath, deselectAll, startTextEditing, updateTextBox, newLangValResultForTextEdit, cancelTextEditing)

import Dict
import Set

import Lang
import Sync
import Utils

import TSEFLLPTypes exposing (..)
import TSEFLLPDesugaring
import TSEFLLPResugaring
import TSEFLLPEval
import TSEFLLPActions
import TSEFLLPScrub


----------- Controller -----------

-- Do all the computation after a program run.
--
-- I hate caching but if we instead perform the work on
-- demand in the view then the GUI slows to a crawl.
prepare : TSEFLLPTypes.ModelState -> Sync.Options -> Lang.Env -> Lang.Exp -> Maybe Lang.Type -> Lang.Val -> TSEFLLPTypes.ModelState
prepare oldModelState syncOptions env program maybeValueOfInterestTypeFromLeo valueOfInterest =
  let
    renderingFunctionNames =
      expToRenderingFunctionNames program

    dataTypeDefs =
      TSEFLLPDesugaring.dataTypeDefsWithoutTBoolsTLists program

    maybeRenderingFunctionNameAndProgram =
      -- Use the previously selected function, if it's still available.
      oldModelState.maybeRenderingFunctionNameAndProgram
      |> Maybe.map (\{ renderingFunctionName } -> renderingFunctionName)
      |> Utils.filterMaybe (flip List.member renderingFunctionNames)
      |> Utils.plusMaybe (List.head renderingFunctionNames)
      |> Maybe.map
          (\renderingFunctionName ->
            let (multipleDispatchFunctions, desugaredToStringProgram) =
              TSEFLLPDesugaring.makeDesugaredToStringProgram program renderingFunctionName
            in
            { renderingFunctionName     = renderingFunctionName
            , multipleDispatchFunctions = multipleDispatchFunctions
            , desugaredToStringProgram  = desugaredToStringProgram
            }
          )

    valueOfInterestTagged =
      valueOfInterest
      |> TSEFLLPDesugaring.desugarVal
      |> TSEFLLPEval.tagVal []

    stringTaggedWithProjectionPathsResult =
      case maybeRenderingFunctionNameAndProgram of
        Just { renderingFunctionName, multipleDispatchFunctions, desugaredToStringProgram } ->
          TSEFLLPEval.evalToStringTaggedWithProjectionPaths
              dataTypeDefs
              multipleDispatchFunctions
              desugaredToStringProgram
              valueOfInterestTagged

        Nothing ->
          Err "No rendering function chosen."

    maybeValueOfInterestType =
      maybeValueOfInterestTypeFromLeo
      |> Maybe.map TSEFLLPDesugaring.replaceTBoolTListWithTVarTApp

    -- stringProjectionPathToSpecificActions =
    --   stringTaggedWithProjectionPathsResult
    --   |> Result.toMaybe
    --   |> Maybe.map (TSEFLLPActions.generateActionsForValueAndAssociateWithStringLocations dataTypeDefs maybeValueOfInterestType valueOfInterestTagged)
    --   |> Maybe.withDefault Dict.empty

  in
  { oldModelState
  | renderingFunctionNames                = renderingFunctionNames
  , dataTypeDefs                          = dataTypeDefs
  , maybeRenderingFunctionNameAndProgram  = maybeRenderingFunctionNameAndProgram
  , valueOfInterestTagged                 = valueOfInterestTagged
  , stringTaggedWithProjectionPathsResult = stringTaggedWithProjectionPathsResult
  -- , stringProjectionPathToSpecificActions = stringProjectionPathToSpecificActions
  -- , maybeNewValueOptions                  = Nothing
  , liveSyncInfo                          = TSEFLLPScrub.prepareLiveUpdates syncOptions program valueOfInterest
  }


-- showNewValueOptions : TSEFLLPTypes.ModelState -> List TaggedValue -> TSEFLLPTypes.ModelState
-- showNewValueOptions oldModelState newValueOptions =
--   { oldModelState | maybeNewValueOptions = Just newValueOptions }


newLangValResult : TaggedValue -> Result String Lang.Val
newLangValResult = TSEFLLPResugaring.taggedValToLangValResult


mousePosition : TSEFLLPTypes.ModelState -> (Int, Int) -> TSEFLLPTypes.ModelState
mousePosition oldModelState (xPx, yPx) =
  { oldModelState | mousePosition = (xPx, yPx) }


mouseOut : TSEFLLPTypes.ModelState -> TSEFLLPTypes.ModelState
mouseOut oldModelState =
  { oldModelState | mousePosition = mouseGone }


selectOnePath : TSEFLLPTypes.ModelState -> PolyPath -> TSEFLLPTypes.ModelState
selectOnePath oldModelState polyPath =
  { oldModelState | selectedPolyPaths = [polyPath] }


togglePathSelection : TSEFLLPTypes.ModelState -> PolyPath -> TSEFLLPTypes.ModelState
togglePathSelection oldModelState polyPath =
  { oldModelState | selectedPolyPaths = Utils.toggleAsSet polyPath oldModelState.selectedPolyPaths }


deselectPath : TSEFLLPTypes.ModelState -> PolyPath -> TSEFLLPTypes.ModelState
deselectPath oldModelState polyPath =
  { oldModelState | selectedPolyPaths = Utils.removeAsSet polyPath oldModelState.selectedPolyPaths }


deselectAll : TSEFLLPTypes.ModelState -> TSEFLLPTypes.ModelState
deselectAll oldModelState =
  { oldModelState | selectedPolyPaths = [] }
  -- { oldModelState | selectedPolyPaths = Set.empty, maybeNewValueOptions = Nothing }


startTextEditing : TSEFLLPTypes.ModelState -> (ProjectionPath, String) -> TSEFLLPTypes.ModelState
startTextEditing oldModelState (projectionPath, text) =
  -- Only set if not already text editing
  { oldModelState | maybeTextEditingPathAndText = oldModelState.maybeTextEditingPathAndText |> Maybe.withDefault (projectionPath, text) |> Just }


updateTextBox : TSEFLLPTypes.ModelState -> String -> TSEFLLPTypes.ModelState
updateTextBox oldModelState newText =
  { oldModelState | maybeTextEditingPathAndText = oldModelState.maybeTextEditingPathAndText |> Maybe.map (\(path, _) -> (path, newText)) }


newLangValResultForTextEdit : TSEFLLPTypes.ModelState -> Result String Lang.Val
newLangValResultForTextEdit modelState =
  case modelState.maybeTextEditingPathAndText of
    Just (path, newText) ->
      modelState.valueOfInterestTagged
      |> TSEFLLPActions.replaceAtPath path (noTag <| VString newText)
      |> TSEFLLPResugaring.taggedValToLangValResult

    Nothing ->
      Err "Not text editing right now!"


cancelTextEditing : TSEFLLPTypes.ModelState -> TSEFLLPTypes.ModelState
cancelTextEditing oldModelState =
  { oldModelState | maybeTextEditingPathAndText = Nothing }


expToRenderingFunctionNames : Lang.Exp -> List Ident
expToRenderingFunctionNames exp =
  ["toString"]

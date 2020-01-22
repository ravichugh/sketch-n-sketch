module TSEFLLP exposing (prepare, newLangValResult, mousePosition, mouseOut, selectOnePath, togglePathSelection, deselectPath, deselectAll, showActions, deleteActionsAndNewValuesForSelection, startTextEditing, updateTextBox, newLangValResultForTextEdit, handleEscapeKey, cancelTextEditing)

import Set exposing (Set)

import Lang
import Sync
import Utils

import TSEFLLPTypes exposing (..)
import TSEFLLPDesugaring
import TSEFLLPResugaring
import TSEFLLPEval
import TSEFLLPActions
import TSEFLLPScrub
import TSEFLLPSelection


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

    projectionPathToSpecificActions =
      TSEFLLPActions.makeProjectionPathToSpecificActions
          dataTypeDefs
          valueOfInterestTagged
          maybeValueOfInterestType

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
  , maybeValueOfInterestType              = maybeValueOfInterestType
  , stringTaggedWithProjectionPathsResult = stringTaggedWithProjectionPathsResult
  , projectionPathToSpecificActions       = projectionPathToSpecificActions
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
  { oldModelState | selectedPolyPaths = [polyPath], shownActions = Set.empty }


togglePathSelection : TSEFLLPTypes.ModelState -> PolyPath -> TSEFLLPTypes.ModelState
togglePathSelection oldModelState polyPath =
  { oldModelState | selectedPolyPaths = Utils.toggleAsSet polyPath oldModelState.selectedPolyPaths, shownActions = Set.empty }


deselectPath : TSEFLLPTypes.ModelState -> PolyPath -> TSEFLLPTypes.ModelState
deselectPath oldModelState polyPath =
  { oldModelState | selectedPolyPaths = Utils.removeAsSet polyPath oldModelState.selectedPolyPaths, shownActions = Set.empty }


deselectAll : TSEFLLPTypes.ModelState -> TSEFLLPTypes.ModelState
deselectAll oldModelState =
  { oldModelState | selectedPolyPaths = [], shownActions = Set.empty }
  -- { oldModelState | selectedPolyPaths = Set.empty, maybeNewValueOptions = Nothing }


showActions : TSEFLLPTypes.ModelState -> Maybe PolyPath -> Set SpecificAction -> TSEFLLPTypes.ModelState
showActions oldModelState maybePolyPath specficActions =
  -- Ensure relevant poly selected when actions appear.
  let selectedPolyPaths =
    case maybePolyPath of
      Just polyPath -> if List.member polyPath oldModelState.selectedPolyPaths then oldModelState.selectedPolyPaths else [polyPath] -- Deselect others if this shape wasn't already selected.
      Nothing       -> oldModelState.selectedPolyPaths
  in
  { oldModelState | shownActions = specficActions, selectedPolyPaths = selectedPolyPaths }


-- Delete key pressed.
deleteActionsAndNewValuesForSelection : TSEFLLPTypes.ModelState -> (Set SpecificAction, List TaggedValue)
deleteActionsAndNewValuesForSelection { valueOfInterestTagged, stringTaggedWithProjectionPathsResult, selectedPolyPaths, projectionPathToSpecificActions } =
  case stringTaggedWithProjectionPathsResult of
    Ok taggedString ->
      let
        selectedPathSet = TSEFLLPSelection.selectedPolyPathsToProjectionPathSet selectedPolyPaths valueOfInterestTagged taggedString

        actions =
          selectedPathSet
          |> Set.toList
          |> List.map (\path -> Utils.getWithDefault path Set.empty projectionPathToSpecificActions)
          |> Utils.unionAll

        deleteActions =
          actions
          |> Set.filter (specificActionMaybeChangeType >> (==) (Just Remove))

        newValuesAfterDelete =
          deleteActions
          |> Set.toList
          |> Utils.filterMap specificActionMaybeNewValue
      in
      (deleteActions, newValuesAfterDelete)

    Err err ->
      let _ = Utils.log err in
      (Set.empty, [])


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
      -- Humorously, this works for numbers even if we make a VString b/c of Mikäel's fancypants backprop.
      --
      -- But below is the "right" way anyway:
      let newSubvalueResult =
        case TSEFLLPTypes.pathToMaybeValue modelState.valueOfInterestTagged path of
          Just taggedVal ->
            case taggedVal.v of
              VString _ -> Ok <| VString newText
              VNum _    ->
                case String.toFloat newText of
                  Ok newFloat -> Ok <| VNum newFloat
                  Err msg     -> Err msg
              _ -> Err "Expected VString or VNum at the replacement path!"
          Nothing ->
            Err "Text editing path not found in value of interest!"
      in
      case newSubvalueResult of
        Ok newSubvalueUntagged ->
          modelState.valueOfInterestTagged
          |> TSEFLLPActions.replaceAtPath path (noTag <| newSubvalueUntagged)
          |> TSEFLLPResugaring.taggedValToLangValResult
        Err msg ->
          Err msg

    Nothing ->
      Err "Not text editing right now!"


handleEscapeKey : TSEFLLPTypes.ModelState -> TSEFLLPTypes.ModelState
handleEscapeKey oldModelState =
  case oldModelState.maybeTextEditingPathAndText of
    Just _  -> cancelTextEditing oldModelState
    Nothing ->
      if Set.size oldModelState.shownActions >= 1
      then { oldModelState | shownActions = Set.empty }
      else deselectAll oldModelState


cancelTextEditing : TSEFLLPTypes.ModelState -> TSEFLLPTypes.ModelState
cancelTextEditing oldModelState =
  { oldModelState | maybeTextEditingPathAndText = Nothing, shownActions = Set.empty }


expToRenderingFunctionNames : Lang.Exp -> List Ident
expToRenderingFunctionNames exp =
  ["toString"]

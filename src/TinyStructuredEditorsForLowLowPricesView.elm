module TinyStructuredEditorsForLowLowPricesView exposing (functionPickerAndEditor)

import Dict exposing (Dict)
import Set exposing (Set)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import VirtualDom exposing (text)

import Controller
import HtmlUtils
import Model exposing (Msg)
import Utils

import TinyStructuredEditorsForLowLowPricesTypes exposing (..)
import TinyStructuredEditorsForLowLowPricesActions
import TinyStructuredEditorsForLowLowPricesEval


-------------- View -------------

functionPickerAndEditor : TinyStructuredEditorsForLowLowPricesTypes.ModelState -> List (Html Msg)
functionPickerAndEditor modelState =
  let
    renderingFunctionPicker =
      let optionNames = modelState.renderingFunctionNames in
      Html.select [] (optionNames |> List.map (\optionName -> Html.option [Attr.name optionName] [text optionName]))

  in
  if modelState.showWidgets then
    [ Html.div [] [renderingFunctionPicker]
    , stringTaggedWithProjectionPathsDebug modelState.stringTaggedWithProjectionPathsResult
    , Html.div [] [text "Selected paths: ", text (pathSetToString modelState.selectedPaths)]
    , Html.div [] [text "Selected values: ", text (modelState.selectedPaths |> Set.toList |> List.map (pathToValue modelState.valueOfInterestTagged >> unparseToUntaggedString) |> String.join ", ")]
    , Html.div
        [ Attr.style [("padding", "1em")] ]
        [ structuredEditor modelState ]
    , actionAssociationsDebug modelState.stringProjectionPathToSpecificActions modelState.stringTaggedWithProjectionPathsResult
    ]
  else
    [ Html.div [] [renderingFunctionPicker]
    , Html.div
        [ Attr.style [("padding", "1em")] ]
        [ plainStringView modelState.stringTaggedWithProjectionPathsResult ]
    -- , actionAssociationsDebug modelState.stringProjectionPathToSpecificActions
    ]


pathToValue : TaggedValue -> ProjectionPath -> TaggedValue
pathToValue rootValueOfInterestTagged path =
  pathToMaybeValue rootValueOfInterestTagged path
  |> Maybe.withDefault rootValueOfInterestTagged

pathToMaybeValue : TaggedValue -> ProjectionPath -> Maybe TaggedValue
pathToMaybeValue rootValueOfInterestTagged path =
  let targetPathSet = Set.singleton path in
  rootValueOfInterestTagged
  |> foldTaggedValue Nothing
      (\subvalueOfInterestTagged maybeFound ->
        if subvalueOfInterestTagged.paths == targetPathSet
        then Just subvalueOfInterestTagged
        else maybeFound
      )


pathToString : ProjectionPath -> String
pathToString path =
  case path of
    []            -> "•"
    n::deeperPath -> toString n ++ "." ++ pathToString deeperPath


pathSetToString : Set ProjectionPath -> String
pathSetToString pathSet =
  case Set.toList pathSet of
    []    -> "∅"
    paths -> "{" ++ String.join "," (List.map pathToString paths) ++ "}"


taggedStringToNormalString : AppendedTaggedStrings t -> String
taggedStringToNormalString taggedString =
  case taggedString of
    TaggedString string _           -> string
    TaggedStringAppend left right _ -> taggedStringToNormalString left ++ taggedStringToNormalString right

actionAssociationsDebug : Dict ProjectionPath (List SpecificAction) -> Result String StringTaggedWithProjectionPaths -> Html Msg
actionAssociationsDebug stringProjectionPathToSpecificActions stringTaggedWithProjectionPathsResult =
  let
    -- pathToString : ProjectionPath -> String
    -- pathToString path =
    --   case path of
    --     []            -> "•"
    --     n::deeperPath -> toString n ++ "." ++ pathToString deeperPath
    --
    -- pathSetToString : Set ProjectionPath -> String
    -- pathSetToString pathSet =
    --   case Set.toList pathSet of
    --     []    -> "∅"
    --     paths -> "{" ++ String.join "," (List.map pathToString paths) ++ "}"

    renderStringTaggedWithActions : AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction) -> Html Msg
    renderStringTaggedWithActions stringTaggedWithSelectionPathAndProjectionPathsAndActions =
      let
        box children =
          Html.span [Attr.style [("display", "inline-block"), ("border", "solid 2px black"), ("padding", "2px")]] children

        specificActionToString specificAction =
          case specificAction of
            NewValue changeType projectionPath taggedValue -> "New Value (" ++ toString changeType ++ ") " ++ unparseToUntaggedString taggedValue ++ " at " ++ pathToString projectionPath
            Scrub projectionPath                           -> "Scrub "   ++ pathToString projectionPath
            EditText projectionPath                        -> "Edit Text "   ++ pathToString projectionPath

        renderAction specificAction =
          [ Html.span
              [Attr.style [("border", "solid 1px #ddd")]]
              [text <| specificActionToString specificAction]
          , Html.br [] []
          ]

        renderActionSet : Set SpecificAction -> Html Msg
        renderActionSet actionSet =
          Html.span []
              ( actionSet
                |> Set.toList
                |> List.concatMap renderAction
              )
      in
      case stringTaggedWithSelectionPathAndProjectionPathsAndActions of
        TaggedString string (_, _, actionSet) ->
          box
            [ Html.span [Attr.style [("font-weight", "bold"), ("color", "#0c0")]] [text <| "\"" ++ string ++ "\""]
            , Html.br [] []
            , renderActionSet actionSet
            ]

        TaggedStringAppend stringTaggedWithActions1 stringTaggedWithActions2 (_, _, actionSet) ->
          box <|
            [ renderStringTaggedWithActions stringTaggedWithActions1
            , renderStringTaggedWithActions stringTaggedWithActions2
            , Html.br [] []
            , renderActionSet actionSet
            ]
  in
  case stringTaggedWithProjectionPathsResult of
    Ok stringTaggedWithProjectionPaths ->
      let
        stringTaggedWithSelectionPathAndProjectionPathsAndActions =
          stringTaggedWithProjectionPaths
          |> assignSelectionClickAreas
          |> assignActionsToLeaves stringProjectionPathToSpecificActions
      in
      Html.div [Attr.style [("font-size", "18px")]] [renderStringTaggedWithActions stringTaggedWithSelectionPathAndProjectionPathsAndActions]
    Err errorMsg ->
      text errorMsg

  -- case stringProjectionPathToSpecificActions of
  --   Ok stringTaggedWithSpecificActions -> Html.div [Attr.style [("font-size", "18px")]] [renderStringTaggedWithActions stringTaggedWithSpecificActions]


-- Current heuristic: Click to select deepest, leftmost path.
--
-- Adds the click-to-select assignment as a Maybe next to the ordinary projection paths.
-- Only non-empty leaf strings (not appends) get a click assignment.
assignSelectionClickAreas : StringTaggedWithProjectionPaths -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath)
assignSelectionClickAreas stringTaggedWithProjectionPaths =
  assignSelectionClickAreas_ Set.empty stringTaggedWithProjectionPaths


assignSelectionClickAreas_
  : Set ProjectionPath
  -> StringTaggedWithProjectionPaths
  -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath)
assignSelectionClickAreas_ pathsInAncestors stringTaggedWithProjectionPaths =
  let pathSetWithAncestors = Set.union pathsInAncestors (stringTag stringTaggedWithProjectionPaths) in
  case stringTaggedWithProjectionPaths of
    TaggedString ""     pathSet -> TaggedString "" (Nothing, pathSet) -- No assignment for empty strings.
    TaggedString string pathSet ->
      let
        maybeDeepestLeftmostPath =
          pathSetWithAncestors
          |> Set.toList
          |> Utils.maximumBy (\path -> (List.length path, List.map negate path)) -- Deepest, leftmost
      in
      TaggedString string (maybeDeepestLeftmostPath, pathSet)

    TaggedStringAppend stringTaggedWithProjectionPaths1 stringTaggedWithProjectionPaths2 pathSet ->
      TaggedStringAppend
          (assignSelectionClickAreas_ pathSetWithAncestors stringTaggedWithProjectionPaths1)
          (assignSelectionClickAreas_ pathSetWithAncestors stringTaggedWithProjectionPaths2)
          (Nothing, pathSet)


-- After determining the selection click regions, assign actions to leaves (not appends).
--
-- Pass 1: If an action and selection click region match exactly, assign the action to it.
-- Pass 2 (disabled): Any unassigned actions are assigned to all descendant leaves of their associated append.
-- Cleanup: Deduplicate actions that have the same effect (even if labeled with different projection paths)
--
-- This scheme is an attempt to reduce the number of actions at each location.
assignActionsToLeaves
  :  Dict ProjectionPath (List SpecificAction)
  -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath)
  -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
assignActionsToLeaves stringProjectionPathToSpecificActions stringTaggedWithSelectionPathAndProjectionPaths =
  let
    assignActionsPass1
      :  AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath)
      -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
    assignActionsPass1 stringTaggedWithSelectionPathAndProjectionPaths =
      let assignSelectionClickPathActions (maybeSelectionClickPath, pathSet) =
        let actions =
          case maybeSelectionClickPath of
            Just selectionClickPath -> Set.fromList <| Utils.getWithDefault selectionClickPath [] stringProjectionPathToSpecificActions
            Nothing                 -> Set.empty -- Appends and empty leaves.
        in
        (maybeSelectionClickPath, pathSet, actions)
      in
      stringTaggedWithSelectionPathAndProjectionPaths
      |> mapStringTags assignSelectionClickPathActions

    -- Remove all the actions we just assigned.
    stringProjectionPathToRemainingSpecificActions =
      let pathsAssigned =
        stringTaggedWithSelectionPathAndProjectionPaths
        |> gatherStringTags
        |> List.filterMap (\(maybeSelectionClickPath, _) -> maybeSelectionClickPath)
      in
      pathsAssigned
      |> Utils.foldl stringProjectionPathToSpecificActions Dict.remove

    assignActionsPass2
      :  Set ProjectionPath
      -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
      -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
    assignActionsPass2 pathsInAncestors stringTaggedWithSelectionPathAndProjectionPathsAndActions =
      let
        (_, immediatePathSet, _) = stringTag stringTaggedWithSelectionPathAndProjectionPathsAndActions
        pathSetWithAncestors     = Set.union pathsInAncestors immediatePathSet
      in
      case stringTaggedWithSelectionPathAndProjectionPathsAndActions of
        TaggedString "" _ ->
          stringTaggedWithSelectionPathAndProjectionPathsAndActions

        TaggedString string (maybeSelectionClickPath, pathSet, previouslyAssignedActions) ->
          let actions =
            pathSetWithAncestors
            |> Set.toList
            |> List.concatMap (\path -> Utils.getWithDefault path [] stringProjectionPathToRemainingSpecificActions)
            |> Set.fromList
          in
          TaggedString string (maybeSelectionClickPath, pathSet, Set.union previouslyAssignedActions actions)

        TaggedStringAppend left right tag ->
          TaggedStringAppend
              (assignActionsPass2 pathSetWithAncestors left)
              (assignActionsPass2 pathSetWithAncestors right)
              tag

    -- Deduplicate actions that have the same effect (even if labeled with different projection paths).
    deduplicateEffectivelyEquivalentActions
      :  AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
      -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
    deduplicateEffectivelyEquivalentActions stringTaggedWithSelectionPathAndProjectionPathsAndActions =
      let deduplicateEffectivelyEquivalentActions (maybeSelectionClickPath, pathSet, specificActionsSet) =
        ( maybeSelectionClickPath
        , pathSet
        , specificActionsSet
          |> Set.toList
          |> Utils.dedupBy
              (\specficAction ->
                case specficAction of
                  NewValue changeType _ taggedValue -> toString changeType ++ " " ++ unparseToUntaggedString taggedValue
                  Scrub projectionPath              -> "Scrub "     ++ pathToString projectionPath
                  EditText projectionPath           -> "Edit Text " ++ pathToString projectionPath
              )
          |> Set.fromList
        )
      in
      stringTaggedWithSelectionPathAndProjectionPathsAndActions
      |> mapStringTags deduplicateEffectivelyEquivalentActions

  in
  stringTaggedWithSelectionPathAndProjectionPaths
  |> assignActionsPass1
  -- |> assignActionsPass2 Set.empty
  |> deduplicateEffectivelyEquivalentActions


plainStringView : Result String (AppendedTaggedStrings t) -> Html Msg
plainStringView stringTaggedWithProjectionPathsResult =
  case stringTaggedWithProjectionPathsResult of
    Ok taggedString ->
      Html.div [Attr.style [("font-size", "18px")]] [text (taggedStringToNormalString taggedString)]
    Err err ->
      Html.div [Attr.style [("font-size", "18px"), ("color", "#e00")]] [text err]


structuredEditor : TinyStructuredEditorsForLowLowPricesTypes.ModelState -> Html Msg
structuredEditor modelState =
  let { valueOfInterestTagged, dataTypeDefs, maybeRenderingFunctionNameAndProgram, selectedPaths, stringProjectionPathToSpecificActions, stringTaggedWithProjectionPathsResult, maybeNewValueOptions } = modelState in
  let
    render
      :  Set ProjectionPath
      -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
      -> Html Msg
    render pathsInAncestors stringTaggedWithSelectionPathAndProjectionPathsAndActions =
      let
        (maybeSelectionClickPath, immediatePathSet, actions) = stringTag stringTaggedWithSelectionPathAndProjectionPathsAndActions
        pathSetWithAncestors                                 = Set.union pathsInAncestors immediatePathSet

        -- Although the mouse region for triggering a selection is small,
        -- we display the selection over the entire area(s) of the string
        -- associated with the selected path.
        perhapsSelectedDisplayAttrs =
          if Utils.anyOverlap [immediatePathSet, selectedPaths]
          then [Attr.style [("border", "3px solid blue"), ("margin", "-3px")]]
          else []
      in
      case stringTaggedWithSelectionPathAndProjectionPathsAndActions of
        TaggedString string _ ->
          let
            (perhapsClickAttrs, isTextEditing) =
              case maybeSelectionClickPath of
                Nothing                 -> ([], False)
                Just selectionClickPath ->
                  let
                    onClickMsg =
                      if Set.member selectionClickPath selectedPaths
                      then Controller.msgDeselectTSEFLLPPath selectionClickPath
                      else Controller.msgSelectTSEFLLPPath   selectionClickPath

                    scrubSpecificActions    = actions |> Set.toList |> List.filter isScrubSpecificAction
                    editTextSpecificActions = actions |> Set.toList |> List.filter isEditTextSpecificAction

                    perhapsStartLiveSync =
                      case scrubSpecificActions of
                        [Scrub projectionPath] -> [ Html.Events.onMouseDown <| Controller.msgTSEFLLPStartLiveSync projectionPath ]
                        _                      -> []

                    perhapsStartTextEdit =
                      case (modelState.maybeTextEditingPathAndText, editTextSpecificActions) of
                        (Nothing, [EditText projectionPath]) -> [ Html.Events.onDoubleClick <| Controller.msgTSEFLLPStartTextEditing (projectionPath, string) ]
                        _                                    -> []

                    isTextEditing =
                      modelState.maybeTextEditingPathAndText
                      |> Maybe.map (\(textEditingPath, _) -> [textEditingPath] == List.map specificActionProjectionPath editTextSpecificActions)
                      |> Maybe.withDefault False

                    cursor =
                      if List.length editTextSpecificActions == 1 then
                        "text"
                      else if List.length scrubSpecificActions == 1 then
                        "ns-resize"
                      else
                        "pointer"
                  in
                  ( [ Attr.style [("cursor", cursor)]
                    , Html.Events.onClick onClickMsg
                    ] ++ perhapsStartLiveSync ++ perhapsStartTextEdit
                  , isTextEditing
                  )

            perhapsActions =
              let
                actionList = Set.toList actions

                insertActionNewValues     = actionList |> List.filter (specificActionMaybeChangeType >> (==) (Just Insert))     |> List.filterMap specificActionMaybeNewValue
                removeActionNewValues     = actionList |> List.filter (specificActionMaybeChangeType >> (==) (Just Remove))     |> List.filterMap specificActionMaybeNewValue
                changeCtorActionNewValues = actionList |> List.filter (specificActionMaybeChangeType >> (==) (Just ChangeCtor)) |> List.filterMap specificActionMaybeNewValue

                xOffset = 4 * String.length string - 5

                button yOffset color onClickMsg str =
                  Html.span
                      [ Attr.style [ ("display", "inline-block")
                                   , ("width",  "0")
                                   , ("vertical-align", toString yOffset ++ "em"),  ("margin-top", toString (-yOffset) ++ "em")
                                   , ("margin-left", toString xOffset ++ "px")
                                   , ("margin-right", toString (-xOffset) ++ "px")
                                   , ("color", color)
                                   , ("cursor", "pointer")
                                   , ("font-size", "75%")
                                   ]
                      , Html.Events.onClick onClickMsg
                      ]
                      [ text str ]

                perhapsInsertButton =
                  case insertActionNewValues of
                    []                    -> []
                    [newValueAfterInsert] -> [ button 1.05 "green" (Controller.msgTSEFLLPSelectNewValue newValueAfterInsert)       "⊕" ]
                    newValuesAfterInsert  -> [ button 1.05 "green" (Controller.msgTSEFLLPShowNewValueOptions newValuesAfterInsert) "⊕" ]

                perhapsRemoveButton =
                  case removeActionNewValues of
                    []                    -> []
                    [newValueAfterRemove] -> [ button -0.9 "red" (Controller.msgTSEFLLPSelectNewValue newValueAfterRemove)       "⊖" ]
                    newValuesAfterRemove  -> [ button -0.9 "red" (Controller.msgTSEFLLPShowNewValueOptions newValuesAfterRemove) "⊖" ]

                perhapsChangeCtorButton =
                  case changeCtorActionNewValues of
                    []                        -> []
                    [newValueAfterChangeCtor] -> [ button -1.8 "gold" (Controller.msgTSEFLLPSelectNewValue newValueAfterChangeCtor)       "∼" ]
                    newValuesAfterChangeCtor  -> [ button -1.8 "gold" (Controller.msgTSEFLLPShowNewValueOptions newValuesAfterChangeCtor) "∼" ]
              in
              perhapsInsertButton ++ perhapsRemoveButton ++ perhapsChangeCtorButton
          in
          Html.span perhapsSelectedDisplayAttrs <|
            if isTextEditing then
              let inputAttrs =
                [ Attr.type_ "text"
                , Attr.id "tsefllpTextBox"
                , Attr.defaultValue string
                , Attr.style [("font-size", "18px")]
                , Html.Events.onInput Controller.msgTSEFLLPUpdateTextBox
                , HtmlUtils.onClickWithoutPropagation Controller.msgNoop
                , HtmlUtils.onKeyDown <|
                    \keyCode ->
                      if keyCode == HtmlUtils.enterKeyCode -- Enter button
                      then Controller.msgTSEFLLPApplyTextEdit
                      else Controller.msgNoop
                ]
              in
              perhapsActions ++ [ Html.input inputAttrs [] ]
            else
              perhapsActions ++ [ Html.span perhapsClickAttrs [text string] ]

        TaggedStringAppend left right _ ->
          Html.span
              perhapsSelectedDisplayAttrs
              [ render pathSetWithAncestors left
              , render pathSetWithAncestors right
              ]
  in
  case stringTaggedWithProjectionPathsResult of
    Ok stringTaggedWithProjectionPaths ->
      case maybeNewValueOptions of
        Nothing ->
          let
            stringTaggedWithSelectionPathAndProjectionPathsAndActions =
              stringTaggedWithProjectionPaths
              |> assignSelectionClickAreas
              |> assignActionsToLeaves stringProjectionPathToSpecificActions
          in
          Html.div [Attr.style [("font-size", "18px")]] [render Set.empty stringTaggedWithSelectionPathAndProjectionPathsAndActions]

        Just newValueOptions ->
          let
            renderNewValueOption newValue =
              let displayStr =
                case maybeRenderingFunctionNameAndProgram of
                  Just { renderingFunctionName, multipleDispatchFunctions, desugaredToStringProgram } ->
                    let newStringTaggedWithProjectionPathsResult =
                      TinyStructuredEditorsForLowLowPricesEval.evalToStringTaggedWithProjectionPaths
                          dataTypeDefs
                          multipleDispatchFunctions
                          desugaredToStringProgram
                          newValue
                    in
                    newStringTaggedWithProjectionPathsResult
                    |> Result.map taggedStringToNormalString
                    |> Utils.fromResult -- Err and Ok both wrap strings, so unwrap.

                  Nothing ->
                    "New Value " ++ unparseToUntaggedString newValue
              in
              Html.div
                  [ Attr.class "text-button"
                  , Html.Events.onClick <| Controller.msgTSEFLLPSelectNewValue newValue
                  ]
                  [ text displayStr ]
          in
          Html.div
              [ Attr.style [("font-size", "18px")] ]
              (newValueOptions |> List.map renderNewValueOption)

    Err _ ->
      plainStringView stringTaggedWithProjectionPathsResult


stringTaggedWithProjectionPathsDebug : Result String StringTaggedWithProjectionPaths -> Html Msg
stringTaggedWithProjectionPathsDebug stringTaggedWithProjectionPathsResult =
  let
    renderStringTaggedWithProjectionPaths : StringTaggedWithProjectionPaths -> List (Html Msg)
    renderStringTaggedWithProjectionPaths stringTaggedWithProjectionPaths =
      let renderPathSet pathSet = Html.sup [Attr.style [("opacity", "0.5")]] [text <| pathSetToString pathSet] in
      case stringTaggedWithProjectionPaths of
        TaggedString string pathSet ->
          [ Html.span [Attr.style [("font-weight", "bold"), ("color", "#0c0")]] [text <| "\"" ++ string ++ "\""]
          , renderPathSet pathSet
          ]

        TaggedStringAppend stringTaggedWithProjectionPaths1 stringTaggedWithProjectionPaths2 pathSet ->
          [ text "(" ] ++
          renderStringTaggedWithProjectionPaths stringTaggedWithProjectionPaths1 ++
          [ text " ++ " ] ++
          renderStringTaggedWithProjectionPaths stringTaggedWithProjectionPaths2 ++
          [ text ")"
          , renderPathSet pathSet
          ]
  in
  case stringTaggedWithProjectionPathsResult of
    Ok stringTaggedWithProjectionPaths -> Html.div [Attr.style [("font-size", "18px")]] <| renderStringTaggedWithProjectionPaths stringTaggedWithProjectionPaths
    Err errorMsg                       -> text errorMsg

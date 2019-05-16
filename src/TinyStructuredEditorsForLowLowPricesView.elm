module TinyStructuredEditorsForLowLowPricesView exposing (functionPickerAndEditor)

import Dict exposing (Dict)
import Set exposing (Set)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
-- import Html.Events exposing
--   ( onClick, onInput, onMouseEnter, onMouseLeave
--   , onWithOptions, defaultOptions
--   )
import VirtualDom exposing (text)

import Controller
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
  [ Html.div [] [renderingFunctionPicker]
  , stringTaggedWithProjectionPathsDebug modelState.stringTaggedWithProjectionPathsResult
  , Html.div [] [text "Selected paths: ", text (pathSetToString modelState.selectedPaths)]
  , Html.div
      [ Attr.style [("padding", "1em")] ]
      [ structuredEditor modelState ]
  -- , actionAssociationsDebug modelState.stringProjectionPathToSpecificActions
  ]


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

-- actionAssociationsDebug : Dict ProjectionPath (List SpecificAction) -> Html Msg
-- actionAssociationsDebug stringProjectionPathToSpecificActions =
--   let
--     -- pathToString : ProjectionPath -> String
--     -- pathToString path =
--     --   case path of
--     --     []            -> "•"
--     --     n::deeperPath -> toString n ++ "." ++ pathToString deeperPath
--     --
--     -- pathSetToString : Set ProjectionPath -> String
--     -- pathSetToString pathSet =
--     --   case Set.toList pathSet of
--     --     []    -> "∅"
--     --     paths -> "{" ++ String.join "," (List.map pathToString paths) ++ "}"
--
--     renderStringTaggedWithActions : StringTaggedWithSpecificActions -> Html Msg
--     renderStringTaggedWithActions stringTaggedWithSpecificActions =
--       let
--         box children =
--           Html.span [Attr.style [("display", "inline-block"), ("border", "solid 2px black"), ("padding", "2px")]] children
--
--         specificActionToString specificAction =
--           case specificAction of
--             NewValue projectionPath taggedValue -> "New Value " ++ unparseToUntaggedString taggedValue ++ " associated at " ++ pathToString projectionPath
--             Scrub projectionPath                -> "Scrub "   ++ pathToString projectionPath
--
--         renderAction specificAction =
--           [ Html.span
--               [Attr.style [("border", "solid 1px #ddd")]]
--               [text <| specificActionToString specificAction]
--           , Html.br [] []
--           ]
--
--         renderActionSet : Set SpecificAction -> Html Msg
--         renderActionSet actionSet =
--           Html.span []
--               ( actionSet
--                 |> Set.toList
--                 |> List.concatMap renderAction
--               )
--       in
--       case stringTaggedWithSpecificActions of
--         TaggedString string actionSet ->
--           box
--             [ Html.span [Attr.style [("font-weight", "bold"), ("color", "#0c0")]] [text <| "\"" ++ string ++ "\""]
--             , Html.br [] []
--             , renderActionSet actionSet
--             ]
--
--         TaggedStringAppend stringTaggedWithActions1 stringTaggedWithActions2 actionSet ->
--           box <|
--             [ renderStringTaggedWithActions stringTaggedWithActions1
--             , renderStringTaggedWithActions stringTaggedWithActions2
--             , Html.br [] []
--             , renderActionSet actionSet
--             ]
--   in
--   case stringProjectionPathToSpecificActions of
--     Ok stringTaggedWithSpecificActions -> Html.div [Attr.style [("font-size", "18px")]] [renderStringTaggedWithActions stringTaggedWithSpecificActions]
--     Err errorMsg                       -> text errorMsg


-- Current heuristic: Click to select deepest, leftmost path.
--
-- Adds the click-to-selct assignment as a Maybe next to the ordinary projection paths.
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
    TaggedString ""     pathSet -> TaggedString "" (Nothing, pathSet)
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
-- Pass 2: Any unassigned actions are assigned to all descendant leaves of their associated append.
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
  in
  stringTaggedWithSelectionPathAndProjectionPaths
  |> assignActionsPass1
  |> assignActionsPass2 Set.empty


structuredEditor : TinyStructuredEditorsForLowLowPricesTypes.ModelState -> Html Msg
structuredEditor modelState =
  let { valueOfInterestTagged, maybeRenderingFunctionNameAndProgram, selectedPaths, stringProjectionPathToSpecificActions, stringTaggedWithProjectionPathsResult, maybeNewValueOptions } = modelState in
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
            (perhapsClickAttrs, isLeafSelected) =
              case maybeSelectionClickPath of
                Nothing                  -> ([], False)
                Just selectionClickPath ->
                  let
                    isLeafSelected = Set.member selectionClickPath selectedPaths
                    onClickMsg =
                      if isLeafSelected
                      then Controller.msgDeselectTSEFLLPPath selectionClickPath
                      else Controller.msgSelectTSEFLLPPath   selectionClickPath
                  in
                  ( [ Attr.style [("cursor", "pointer")]
                    , Html.Events.onClick onClickMsg
                    ]
                  , isLeafSelected
                  )

            perhapsActions =
              let
                actionList = Set.toList actions

                insertActionNewValues     = actionList |> List.filter (specificActionMaybeChangeType >> (==) (Just Insert))     |> List.filterMap specificActionMaybeNewValue
                removeActionNewValues     = actionList |> List.filter (specificActionMaybeChangeType >> (==) (Just Remove))     |> List.filterMap specificActionMaybeNewValue
                changeCtorActionNewValues = actionList |> List.filter (specificActionMaybeChangeType >> (==) (Just ChangeCtor)) |> List.filterMap specificActionMaybeNewValue

                button yOffset color onClickMsg str =
                  Html.span
                      [ Attr.style [ ("display", "inline-block")
                                   , ("width",  "0")
                                   , ("vertical-align", toString yOffset ++ "em"),  ("margin-top", toString (-yOffset) ++ "em")
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
          Html.span perhapsSelectedDisplayAttrs (perhapsActions ++ [ Html.span perhapsClickAttrs [text string] ])

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
                  Just { renderingFunctionName, desugaredToStringProgram } ->
                    let newStringTaggedWithProjectionPathsResult =
                      TinyStructuredEditorsForLowLowPricesEval.evalToStringTaggedWithProjectionPaths
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

    Err errorMsg ->
      text errorMsg


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

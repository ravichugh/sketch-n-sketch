module TSEFLLPView exposing (functionPickerAndEditor)

import Dict exposing (Dict)
import Set exposing (Set)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
import VirtualDom exposing (text)

import Controller
import HtmlUtils
import LeoUnparser exposing (unparseType)
import Model exposing (Msg)
import BoundsUtils
import Utils

import TSEFLLPTypes exposing (..)
import TSEFLLPActions
import TSEFLLPEval
import TSEFLLPPolys
import TSEFLLPSelection


-------------- View -------------

functionPickerAndEditor : TSEFLLPTypes.ModelState -> List (Html Msg)
functionPickerAndEditor modelState =
  if modelState.showWidgets then
    [ stringTaggedWithProjectionPathsDebug modelState.stringTaggedWithProjectionPathsResult
    , Html.pre [] [text "\n"] -- spacing
    -- , Html.div [] [text "Selected poly paths: ", text (polyPathsToString modelState.selectedPolyPaths)]
    , Html.div
        [ Attr.style [("padding", "1em")] ]
        [ structuredEditor modelState ]
    -- , actionAssociationsDebug modelState.stringProjectionPathToSpecificActions modelState.stringTaggedWithProjectionPathsResult
    ]
  else
    [ Html.div
        [ Attr.style [("padding", "1em")] ]
        [ plainStringView modelState.stringTaggedWithProjectionPathsResult ]
    -- , actionAssociationsDebug modelState.stringProjectionPathToSpecificActions
    ]


pathToValue : TaggedValue -> ProjectionPath -> TaggedValue
pathToValue rootVal path =
  pathToMaybeValue rootVal path
  |> Maybe.withDefault rootVal

pathToMaybeValue : TaggedValue -> ProjectionPath -> Maybe TaggedValue
pathToMaybeValue rootVal path =
  TSEFLLPTypes.pathToMaybeValue
      (TSEFLLPEval.tagVal [] rootVal)-- Ensure tagged if, e.g., we are exploring a new value from an action.
      path


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


polyPathsToString : List PolyPath -> String
polyPathsToString paths =
  "[" ++ String.join "," (List.map pathToString paths) ++ "]"


actionDescription : SpecificAction -> TaggedValue -> String
actionDescription specificAction valueOfInterestTagged =
  case specificAction of
    NewValue Remove path _ ->
      case pathToMaybeValue valueOfInterestTagged path of
        Just subvalue -> "Remove " ++ unparseToUntaggedString subvalue
        Nothing       -> "Path " ++ pathToString path ++ " not found in " ++ unparseToUntaggedString valueOfInterestTagged
    NewValue ChangeCtor path newValue ->
      case pathToMaybeValue newValue path of
        Just newSubvalue -> unparseToUntaggedString newSubvalue
        Nothing          -> "Path " ++ pathToString path ++ " not found in new value " ++ unparseToUntaggedString newValue
    NewValue Insert path newValue ->
      case pathToMaybeValue newValue path of
        Just newSubvalue -> "Insert " ++ unparseToUntaggedString newSubvalue
        Nothing          -> "Path " ++ pathToString path ++ " not found in new value " ++ unparseToUntaggedString newValue
    _ ->
      toString specificAction


-- actionAssociationsDebug : Dict ProjectionPath (List SpecificAction) -> Result String StringTaggedWithProjectionPaths -> Html Msg
-- actionAssociationsDebug stringProjectionPathToSpecificActions stringTaggedWithProjectionPathsResult =
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
--     renderStringTaggedWithActions : AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction) -> Html Msg
--     renderStringTaggedWithActions stringTaggedWithSelectionPathAndProjectionPathsAndActions =
--       let
--         box children =
--           Html.span [Attr.style [("display", "inline-block"), ("border", "solid 2px black"), ("padding", "2px")]] children
--
--         specificActionToString specificAction =
--           case specificAction of
--             NewValue changeType projectionPath taggedValue -> "New Value (" ++ toString changeType ++ ") " ++ unparseToUntaggedString taggedValue ++ " at " ++ pathToString projectionPath
--             Scrub projectionPath                           -> "Scrub "   ++ pathToString projectionPath
--             EditText projectionPath                        -> "Edit Text "   ++ pathToString projectionPath
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
--       case stringTaggedWithSelectionPathAndProjectionPathsAndActions of
--         TaggedString string (_, _, actionSet) ->
--           box
--             [ Html.span [Attr.style [("font-weight", "bold"), ("color", "#0c0")]] [text <| "\"" ++ string ++ "\""]
--             , Html.br [] []
--             , renderActionSet actionSet
--             ]
--
--         TaggedStringAppend stringTaggedWithActions1 stringTaggedWithActions2 (_, _, actionSet) ->
--           box <|
--             [ renderStringTaggedWithActions stringTaggedWithActions1
--             , renderStringTaggedWithActions stringTaggedWithActions2
--             , Html.br [] []
--             , renderActionSet actionSet
--             ]
--   in
--   case stringTaggedWithProjectionPathsResult of
--     Ok stringTaggedWithProjectionPaths ->
--       let
--         stringTaggedWithSelectionPathAndProjectionPathsAndActions =
--           stringTaggedWithProjectionPaths
--           |> assignSelectionClickAreas
--           |> assignActionsToLeaves stringProjectionPathToSpecificActions
--       in
--       Html.div [Attr.style [("font-size", "18px")]] [renderStringTaggedWithActions stringTaggedWithSelectionPathAndProjectionPathsAndActions]
--     Err errorMsg ->
--       text errorMsg
--
--   -- case stringProjectionPathToSpecificActions of
--   --   Ok stringTaggedWithSpecificActions -> Html.div [Attr.style [("font-size", "18px")]] [renderStringTaggedWithActions stringTaggedWithSpecificActions]


-- -- Current heuristic: Click to select deepest, leftmost path.
-- --
-- -- Adds the click-to-select assignment as a Maybe next to the ordinary projection paths.
-- -- Only non-empty leaf strings (not appends) get a click assignment.
-- assignSelectionClickAreas : StringTaggedWithProjectionPaths -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath)
-- assignSelectionClickAreas stringTaggedWithProjectionPaths =
--   assignSelectionClickAreas_ Set.empty stringTaggedWithProjectionPaths
--
--
-- assignSelectionClickAreas_
--   : Set ProjectionPath
--   -> StringTaggedWithProjectionPaths
--   -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath)
-- assignSelectionClickAreas_ pathsInAncestors stringTaggedWithProjectionPaths =
--   let pathSetWithAncestors = Set.union pathsInAncestors (stringTag stringTaggedWithProjectionPaths) in
--   case stringTaggedWithProjectionPaths of
--     TaggedString ""     pathSet -> TaggedString "" (Nothing, pathSet) -- No assignment for empty strings.
--     TaggedString string pathSet ->
--       let
--         maybeDeepestLeftmostPath =
--           pathSetWithAncestors
--           |> Set.toList
--           |> Utils.maximumBy (\path -> (List.length path, List.map negate path)) -- Deepest, leftmost
--       in
--       TaggedString string (maybeDeepestLeftmostPath, pathSet)
--
--     TaggedStringAppend stringTaggedWithProjectionPaths1 stringTaggedWithProjectionPaths2 pathSet ->
--       TaggedStringAppend
--           (assignSelectionClickAreas_ pathSetWithAncestors stringTaggedWithProjectionPaths1)
--           (assignSelectionClickAreas_ pathSetWithAncestors stringTaggedWithProjectionPaths2)
--           (Nothing, pathSet)


-- -- After determining the selection click regions, assign actions to leaves (not appends).
-- --
-- -- Pass 1: If an action and selection click region match exactly, assign the action to it.
-- -- Pass 2 (disabled): Any unassigned actions are assigned to all descendant leaves of their associated append.
-- -- Cleanup: Deduplicate actions that have the same effect (even if labeled with different projection paths)
-- --
-- -- This scheme is an attempt to reduce the number of actions at each location.
-- assignActionsToLeaves
--   :  Dict ProjectionPath (List SpecificAction)
--   -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath)
--   -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
-- assignActionsToLeaves stringProjectionPathToSpecificActions stringTaggedWithSelectionPathAndProjectionPaths =
--   let
--     assignActionsPass1
--       :  AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath)
--       -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
--     assignActionsPass1 stringTaggedWithSelectionPathAndProjectionPaths =
--       let assignSelectionClickPathActions (maybeSelectionClickPath, pathSet) =
--         let actions =
--           case maybeSelectionClickPath of
--             Just selectionClickPath -> Set.fromList <| Utils.getWithDefault selectionClickPath [] stringProjectionPathToSpecificActions
--             Nothing                 -> Set.empty -- Appends and empty leaves.
--         in
--         (maybeSelectionClickPath, pathSet, actions)
--       in
--       stringTaggedWithSelectionPathAndProjectionPaths
--       |> mapStringTags assignSelectionClickPathActions
--
--     -- Remove all the actions we just assigned.
--     stringProjectionPathToRemainingSpecificActions =
--       let pathsAssigned =
--         stringTaggedWithSelectionPathAndProjectionPaths
--         |> gatherStringTags
--         |> List.filterMap (\(maybeSelectionClickPath, _) -> maybeSelectionClickPath)
--       in
--       pathsAssigned
--       |> Utils.foldl stringProjectionPathToSpecificActions Dict.remove
--
--     assignActionsPass2
--       :  Set ProjectionPath
--       -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
--       -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
--     assignActionsPass2 pathsInAncestors stringTaggedWithSelectionPathAndProjectionPathsAndActions =
--       let
--         (_, immediatePathSet, _) = stringTag stringTaggedWithSelectionPathAndProjectionPathsAndActions
--         pathSetWithAncestors     = Set.union pathsInAncestors immediatePathSet
--       in
--       case stringTaggedWithSelectionPathAndProjectionPathsAndActions of
--         TaggedString "" _ ->
--           stringTaggedWithSelectionPathAndProjectionPathsAndActions
--
--         TaggedString string (maybeSelectionClickPath, pathSet, previouslyAssignedActions) ->
--           let actions =
--             pathSetWithAncestors
--             |> Set.toList
--             |> List.concatMap (\path -> Utils.getWithDefault path [] stringProjectionPathToRemainingSpecificActions)
--             |> Set.fromList
--           in
--           TaggedString string (maybeSelectionClickPath, pathSet, Set.union previouslyAssignedActions actions)
--
--         TaggedStringAppend left right tag ->
--           TaggedStringAppend
--               (assignActionsPass2 pathSetWithAncestors left)
--               (assignActionsPass2 pathSetWithAncestors right)
--               tag
--
--     -- Deduplicate actions that have the same effect (even if labeled with different projection paths).
--     deduplicateEffectivelyEquivalentActions
--       :  AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
--       -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
--     deduplicateEffectivelyEquivalentActions stringTaggedWithSelectionPathAndProjectionPathsAndActions =
--       let deduplicateEffectivelyEquivalentActions (maybeSelectionClickPath, pathSet, specificActionsSet) =
--         ( maybeSelectionClickPath
--         , pathSet
--         , specificActionsSet
--           |> Set.toList
--           |> Utils.dedupBy
--               (\specficAction ->
--                 case specficAction of
--                   NewValue changeType _ taggedValue -> toString changeType ++ " " ++ unparseToUntaggedString taggedValue
--                   Scrub projectionPath              -> "Scrub "     ++ pathToString projectionPath
--                   EditText projectionPath           -> "Edit Text " ++ pathToString projectionPath
--               )
--           |> Set.fromList
--         )
--       in
--       stringTaggedWithSelectionPathAndProjectionPathsAndActions
--       |> mapStringTags deduplicateEffectivelyEquivalentActions
--
--   in
--   stringTaggedWithSelectionPathAndProjectionPaths
--   |> assignActionsPass1
--   -- |> assignActionsPass2 Set.empty
--   |> deduplicateEffectivelyEquivalentActions


plainStringView : Result String (AppendedTaggedStrings t) -> Html Msg
plainStringView stringTaggedWithProjectionPathsResult =
  case stringTaggedWithProjectionPathsResult of
    Ok taggedString ->
      Html.div [Attr.style [("font-size", "18px")]] [text (taggedStringToNormalString taggedString)]
    Err err ->
      Html.div [Attr.style [("font-size", "18px"), ("color", "#e00")]] [text err]


selectionAssignmentStats selectionAssignments pixelPoly valueOfInterestTagged =
  let
    (_, nonOccludedShapeSet) = TSEFLLPSelection.findNonOccludedPathSetAndShapeSet selectionAssignments pixelPoly

    easilySelectablePathSet =
      nonOccludedShapeSet
      |> Set.toList
      |> Utils.filterMap (\shape ->
        case TSEFLLPSelection.shapeToPathSet shape selectionAssignments |> Set.toList of
          [path] -> Just path
          _      -> Nothing
      )
      |> Set.fromList

    onClickableShapesWithMultiplePathsPathSet =
      nonOccludedShapeSet
      |> Set.toList
      |> List.concatMap (\shape ->
        case TSEFLLPSelection.shapeToPathSet shape selectionAssignments |> Set.toList of
          []    -> []
          [_]   -> []
          paths -> paths
      )
      |> Set.fromList

    onlyOnClickableShapesWithMultiplePathsPathSet =
      Set.diff onClickableShapesWithMultiplePathsPathSet easilySelectablePathSet

    allPathsSet =
      valueOfInterestTagged
      |> foldTaggedValue Set.empty (\taggedValue pathSet ->
        Set.union taggedValue.paths pathSet
      )

    unselectablePathSet =
      Set.diff allPathsSet (Set.union easilySelectablePathSet onlyOnClickableShapesWithMultiplePathsPathSet)
  in
  [ Html.div [] [text "Value's path count: ",                         text (toString <| Set.size allPathsSet)]
  , Html.div [] [text "Occluded or missing paths: ",                  text (pathSetToString unselectablePathSet)]
  , Html.div [] [text "Paths without unique shape: ",                 text (pathSetToString onlyOnClickableShapesWithMultiplePathsPathSet)]
  , Html.div [] [text "Percentage uniquely selectable shape paths: ", text (toString <| 100.0 * toFloat (Set.size easilySelectablePathSet) / toFloat (Set.size allPathsSet)), text "%"]
  ]

insertActionStats projectionPathToSpecificActions insertActionLocationsAndMaybePolyPath =
  let
    insertActions =
      projectionPathToSpecificActions
      |> Dict.values
      |> Utils.unionAll
      |> Set.toList
      |> List.filter (specificActionMaybeChangeType >> (==) (Just Insert))

    insertValueSet =
      insertActions
      |> Utils.filterMap specificActionMaybeNewValue
      |> Set.fromList

      -- case Set.toList actions |> List.map specificActionMaybeNewValue |> Utils.dedup of
      --   [Just newVal] -> Controller.msgTSEFLLPSelectNewValue newVal
      --   _             -> Controller.msgTSEFLLPShowActions maybeShapePolyPath actions

  in
  [ Html.div [] [text "Counting \"reasonable\" inserts: START HERE AND DEFINE IT"]
  , Html.div [] [text "Possible insert value count: ",                         text (toString <| Set.size allPathsSet)]
  , Html.div [] [text "Occluded or missing paths: ",                  text (pathSetToString unselectablePathSet)]
  , Html.div [] [text "Paths without unique shape: ",                 text (pathSetToString onlyOnClickableShapesWithMultiplePathsPathSet)]
  , Html.div [] [text "Percentage uniquely selectable shape paths: ", text (toString <| 100.0 * toFloat (Set.size easilySelectablePathSet) / toFloat (Set.size allPathsSet)), text "%"]
  ]


structuredEditor : TSEFLLPTypes.ModelState -> Html Msg
structuredEditor modelState =
  let
    { valueOfInterestTagged, dataTypeDefs, maybeValueOfInterestType, stringTaggedWithProjectionPathsResult, selectedPolyPaths, mousePosition, projectionPathToSpecificActions, maybeTextEditingPathAndText, shownActions } = modelState

    px int = toString int ++ "px"
  in
  case stringTaggedWithProjectionPathsResult of
    Ok taggedString ->
      let
        charWidthPx  = 11
        charHeightPx = 18

        selectedColor = "#B7D7FD"
        hoverColor    = "#e1eefc"

        pixelPoly = TSEFLLPPolys.taggedStringToPixelPoly charWidthPx charHeightPx taggedString

        selectedShapes =
          selectedPolyPaths
          |> List.map (\polyPath -> TSEFLLPSelection.maybeShapeByPolyPath polyPath pixelPoly)
          |> Utils.filterJusts
          |> Utils.dedup

        selectionAssignments     = TSEFLLPSelection.associateProjectionPathsWithShapes pixelPoly
        projectionPathToShapeSet = TSEFLLPSelection.makeProjectionPathToShapeSet selectionAssignments

        -- Projection paths and shapes in output string but with no hover region b/c precisely
        -- covered by smaller (deeper) shapes.
        (occludedPathSet, occludedShapes) = TSEFLLPSelection.findOccludedPathSetAndShapes selectionAssignments pixelPoly

        maybeHoveredShape = TSEFLLPSelection.mostRelevantShapeAtPoint mousePosition selectionAssignments
        hoveredShapes     = Utils.maybeToList maybeHoveredShape

        hoveredOccludedShapes = TSEFLLPSelection.occludedShapesAtPoint mousePosition occludedShapes

        selectedPathSet = selectedShapes |> List.map (flip TSEFLLPSelection.shapeToPathSet selectionAssignments) |> Utils.unionAll
        hoveredPathSet  = hoveredShapes  |> List.map (flip TSEFLLPSelection.shapeToPathSet selectionAssignments) |> Utils.unionAll

        pathToType = TSEFLLPActions.makeProjectionPathToType dataTypeDefs maybeValueOfInterestType valueOfInterestTagged

        insertActionLocations : Dict (Int, Int) (Set SpecificAction)
        insertActionLocations =
          TSEFLLPActions.arrangeInsertActions pathToType projectionPathToShapeSet projectionPathToSpecificActions

        -- Label with type, or, if that fails, with value.
        shapeToMaybeLabel : TSEFLLPPolys.PixelShape -> Maybe String
        shapeToMaybeLabel shape =
          let pathSet = TSEFLLPSelection.shapeToPathSet shape selectionAssignments in
          if Set.size pathSet >= 1 then
            pathSet
            |> Set.toList
            |> sortByDeepestLeftmostLast
            |> List.reverse
            |> List.map (\path -> Dict.get path pathToType |> Maybe.map unparseType |> Utils.maybeWithDefaultLazy (\() -> path |> pathToValue valueOfInterestTagged |> unparseToUntaggedString))
            |> List.map Utils.squish
            |> String.join ", "
            |> Just
          else
            Nothing

        perhapsClickAttrs =
          let
            actionsForHovered =
              hoveredPathSet
              |> Set.toList
              |> Utils.filterMap (flip Dict.get projectionPathToSpecificActions)
              |> Utils.unionAll
              |> Set.toList

            scrubSpecificActions    = actionsForHovered |> List.filter isScrubSpecificAction
            editTextSpecificActions = actionsForHovered |> List.filter isEditTextSpecificAction

            perhapsStartLiveSync =
              case scrubSpecificActions of
                [Scrub projectionPath] -> [ Html.Events.onMouseDown <| Controller.msgTSEFLLPStartLiveSync projectionPath ]
                _                      -> []

            perhapsStartTextEdit =
              let pathToString path =
                 case (pathToValue valueOfInterestTagged path).v of
                   VClosure _ _ _ _  -> "textEditString - shouldn't have VClosure here"
                   VClosureDynamic _ -> "textEditString - shouldn't have VClosureDynamic here"
                   VCtor _ _         -> "textEditString - shouldn't have VCtor here"
                   VString string    -> string
                   VAppend _ _       -> "textEditString - shouldn't have VAppend here"
                   VNum float        -> toString float
              in
              case (maybeTextEditingPathAndText, editTextSpecificActions ++ scrubSpecificActions) of
                (Nothing, [EditText projectionPath]) -> [ Html.Events.onDoubleClick <| Controller.msgTSEFLLPStartTextEditing (projectionPath, pathToString projectionPath) ]
                (Nothing, [Scrub projectionPath])    -> [ Html.Events.onDoubleClick <| Controller.msgTSEFLLPStartTextEditing (projectionPath, pathToString projectionPath) ]
                _                                    -> []

            cursor =
              if List.length editTextSpecificActions == 1 then
                "text"
              else if List.length scrubSpecificActions == 1 then
                "ns-resize"
              else
                "auto"
          in
          [ Attr.style [("cursor", cursor)]
          ] ++ perhapsStartLiveSync ++ perhapsStartTextEdit

        onClickMsg =
          case maybeHoveredShape of
            Just hoveredShape ->
              case TSEFLLPSelection.shapeToMaybePolyPath hoveredShape pixelPoly of
                Just hoveredPolyPath ->
                  if List.member hoveredPolyPath selectedPolyPaths
                  then Controller.msgTSEFLLPDeselectPolyPath hoveredPolyPath
                  else Controller.msgTSEFLLPSelectPolyPath   hoveredPolyPath
                Nothing ->
                  Controller.msgTSEFLLPDeselectAllPolyPaths
            Nothing ->
              Controller.msgTSEFLLPDeselectAllPolyPaths


        pixelShapeToDivs : (TSEFLLPPolys.PixelShape -> Maybe String) -> List (Html.Attribute Msg) -> TSEFLLPPolys.PixelShape -> List (Html Msg)
        pixelShapeToDivs shapeToMaybeLabel extraAttrs ({ bounds, rightBotCornerOfLeftTopCutout, leftTopCornerOfRightBotCutout } as pixelShape) =
          let
            (left, top, right, bot) = bounds
            (startX, firstLineBot)  = rightBotCornerOfLeftTopCutout
            (endX, lastLineTop)     = leftTopCornerOfRightBotCutout

            perhapsLabel =
              case shapeToMaybeLabel pixelShape of
                Just label ->
                  [ Html.div
                      [ Attr.style [ ("margin-top", px (-charHeightPx))
                                   , ("width", "1000px")
                                   ]
                      ]
                      [ Html.span
                          [ Attr.style [ ("line-height", px charHeightPx)
                                       , ("font-size", px charHeightPx)
                                       , ("color", "blue")
                                       , ("background-color", "#eee")
                                       ]
                          ]
                          [text label]
                      ]
                  ]
                Nothing ->
                  []
          in
          if lastLineTop < firstLineBot then
            let _ = if (startX, firstLineBot) /= (left, bot)  then Debug.crash ("TSEFLLPView.structuredEditor pixelPolyToDivs unorthodox poly construction: bounding box " ++ toString bounds ++ " rightBotCornerOfLeftTopCutout " ++ toString rightBotCornerOfLeftTopCutout ++ " leftTopCornerOfRightBotCutout " ++ toString leftTopCornerOfRightBotCutout) else () in
            let _ = if (endX, lastLineTop)    /= (right, top) then Debug.crash ("TSEFLLPView.structuredEditor pixelPolyToDivs unorthodox poly construction: bounding box " ++ toString bounds ++ " rightBotCornerOfLeftTopCutout " ++ toString rightBotCornerOfLeftTopCutout ++ " leftTopCornerOfRightBotCutout " ++ toString leftTopCornerOfRightBotCutout) else () in
            [ Html.div
                  ([ Attr.style [ ("position", "absolute")
                                , ("left", px (left + 1)), ("top", px (top + 1))
                                , ("width", px (right - left)), ("height", px (bot - top))
                                ]
                  ] ++ extraAttrs)
                  perhapsLabel
            ]
          else if lastLineTop == firstLineBot then
            [ Html.div
                  ([ Attr.style [ ("position", "absolute")
                                , ("left", px (startX + 1)), ("top", px (top + 1))
                                , ("width", px (right - startX)), ("height", px (firstLineBot - top))
                                ]
                  ] ++ extraAttrs)
                  perhapsLabel
            , Html.div
                  ([ Attr.style [ ("position", "absolute")
                                , ("left", px (left + 1)), ("top", px (lastLineTop + 1))
                                , ("width", px (endX - left)), ("height", px (bot - lastLineTop))
                                ]
                  ] ++ extraAttrs)
                  []
            ]
          else
            [ Html.div
                  ([ Attr.style [ ("position", "absolute")
                                , ("left", px (startX + 1)), ("top", px (top + 1))
                                , ("width", px (right - startX)), ("height", px (firstLineBot - top))
                                ]
                  ] ++ extraAttrs)
                  perhapsLabel
            , Html.div
                  ([ Attr.style [ ("position", "absolute")
                                , ("left", px (left + 1)), ("top", px (firstLineBot + 1))
                                , ("width", px (right - left)), ("height", px (lastLineTop - firstLineBot))
                                ]
                  ] ++ extraAttrs)
                  []
            , Html.div
                  ([ Attr.style [ ("position", "absolute")
                                , ("left", px (left + 1)), ("top", px (lastLineTop + 1))
                                , ("width", px (endX - left)), ("height", px (bot - lastLineTop))
                                ]
                  ] ++ extraAttrs)
                  []
            ]


        buttonsAndMenus =
          let
            shownShapes = Utils.dedup (selectedShapes ++ hoveredShapes)

            actionDiv action =
              case specificActionMaybeNewValue action of
                Just newVal ->
                  Html.div
                      [ Attr.style [ ("cursor", "pointer") ]
                      , Html.Events.onClick (Controller.msgTSEFLLPSelectNewValue newVal)
                      ]
                      [ text (actionDescription action valueOfInterestTagged) ]
                Nothing ->
                  Html.div
                      []
                      [ text <| "who are you and why is this action here " ++ actionDescription action valueOfInterestTagged ]

            perhapsButton extraButtonStyles buttonHtmls ((right, top), actions, maybeShapePolyPath)  =
              let shownButtonActions =                    -- Deepest, leftmost first
                  Set.intersect shownActions actions
                  |> Set.toList
                  |> List.sortBy (\action -> (negate <| List.length (specificActionProjectionPath action), specificActionProjectionPath action))
                  |> Utils.dedupBy specificActionMaybeNewValue
              in
              if List.length shownButtonActions >= 1 then
                [ Html.div
                      [ Attr.style [ ("position", "absolute")
                                   , ("left", px right), ("top", px top)
                                   -- , ("width", px 500) --, ("height", px (charHeightPx)
                                   , ("border", "1px solid gray")
                                   , ("background-color", "#eee")
                                   -- , ("overflow-x", "scroll")
                                   ]
                      ]
                      (shownButtonActions |> List.map actionDiv)
                ]
              else if Set.size actions >= 1 && Set.isEmpty shownActions && maybeTextEditingPathAndText == Nothing then
                let onClick =
                  -- If only one possible remove action, apply immediately.
                  case Set.toList actions |> List.map specificActionMaybeNewValue |> Utils.dedup of
                    [Just newVal] -> Controller.msgTSEFLLPSelectNewValue newVal
                    _             -> Controller.msgTSEFLLPShowActions maybeShapePolyPath actions
                in
                [ Html.div
                      [ Attr.style <| [ ("position", "absolute")
                                      , ("left", px (right - charWidthPx//2)), ("top", px (top - charWidthPx//2))
                                      , ("width", px (charWidthPx//2)), ("height", px (charHeightPx//2))
                                      , ("font-size", px (charHeightPx//2))
                                      , ("cursor", "pointer")
                                      ] ++ extraButtonStyles
                      , Html.Events.onClick onClick
                      ]
                      buttonHtmls
                ]
              else
                []


            -- Place the visible actions in 2D where we think they should go and then
            -- consolidate the actions at overlapping locations.

            -- Prefer locations later in list (buttons associated with deeper shapes).
            consolidateActionsAtOverlappingLocations : List ((Int, Int), Set SpecificAction, Maybe PolyPath) -> List ((Int, Int), Set SpecificAction, Maybe PolyPath)
            consolidateActionsAtOverlappingLocations actionLocationsAndMaybePolyPath =
              let tolerancePx = 10 in
              actionLocationsAndMaybePolyPath
              |> Utils.foldr
                  []
                  (\(point, locationActions, maybePolyPath) deeperActionLocationsAndMaybePolyPath ->
                    let maybeClosest =
                      deeperActionLocationsAndMaybePolyPath
                      |> List.filter     (\(existingPoint, _, _) -> Utils.distanceInt point existingPoint <= tolerancePx)
                      |> Utils.minimumBy (\(existingPoint, _, _) -> Utils.distanceInt point existingPoint)
                    in
                    case maybeClosest of
                      Just ((existingPoint, existingLocationActions, existingMaybePolyPath) as existing) ->
                        let
                          consolidated =
                            ( existingPoint
                            , Set.union locationActions existingLocationActions
                            , if maybePolyPath == existingMaybePolyPath then existingMaybePolyPath else Nothing
                            )
                        in
                        deeperActionLocationsAndMaybePolyPath
                        |> Utils.replaceAll existing consolidated

                      Nothing ->
                        (point, locationActions, maybePolyPath)::deeperActionLocationsAndMaybePolyPath
                  )

            insertActionLocationsAndMaybePolyPath : List ((Int, Int), Set SpecificAction, Maybe PolyPath)
            insertActionLocationsAndMaybePolyPath =
              insertActionLocations
              |> Dict.toList
              |> List.map (\(point, insertActions) ->
                (point, insertActions, Nothing)
              )
              |> consolidateActionsAtOverlappingLocations

            (removeActionLocationsAndMaybePolyPath, changeCtorActionLocationsAndMaybePolyPath) =
              let
                shapeToMaybeRemoveInsertActionLocationsAndMaybePolyPath : TSEFLLPPolys.PixelShape -> (Maybe ((Int, Int), Set SpecificAction, Maybe PolyPath), Maybe ((Int, Int), Set SpecificAction, Maybe PolyPath))
                shapeToMaybeRemoveInsertActionLocationsAndMaybePolyPath ({ bounds, rightBotCornerOfLeftTopCutout, leftTopCornerOfRightBotCutout } as pixelShape) =
                  let
                    (left, top, right, bot) = bounds
                    (startX, firstLineBot)  = rightBotCornerOfLeftTopCutout
                    (endX, lastLineTop)     = leftTopCornerOfRightBotCutout

                    actions =
                      TSEFLLPSelection.shapeToPathSet pixelShape selectionAssignments
                      |> Set.toList
                      |> List.map (\path -> Utils.getWithDefault path Set.empty projectionPathToSpecificActions)
                      |> Utils.unionAll

                    removeActions =
                      actions
                      |> Set.filter (specificActionMaybeChangeType >> (==) (Just Remove))

                    changeCtorActions =
                      actions
                      |> Set.filter (specificActionMaybeChangeType >> (==) (Just ChangeCtor))

                    maybeShapePolyPath = TSEFLLPSelection.shapeToMaybePolyPath pixelShape pixelPoly
                  in
                  ( if Set.size removeActions     >= 1 then Just ((right + 1, top)             , removeActions    , maybeShapePolyPath) else Nothing
                  , if Set.size changeCtorActions >= 1 then Just ((left - 5,  (bot + top) // 2), changeCtorActions, maybeShapePolyPath) else Nothing
                  )
              in
              hoveredOccludedShapes ++ shownShapes
              |> List.map shapeToMaybeRemoveInsertActionLocationsAndMaybePolyPath
              |> List.unzip
              |> Utils.mapBoth Utils.filterJusts
              |> Utils.mapBoth consolidateActionsAtOverlappingLocations

            insertButtons =
              insertActionLocationsAndMaybePolyPath
              |> List.concatMap (perhapsButton [("color", "#0e0")] [text "⊕"]) -- ⊕➕

            removeButtons =
              removeActionLocationsAndMaybePolyPath
              |> List.concatMap (perhapsButton [] [text "❌"])  -- ❌✘✕✖︎✗

            changeCtorButtons =
              changeCtorActionLocationsAndMaybePolyPath
              |> List.concatMap (perhapsButton [] [text "🔽"]) -- 🔽▾▼⎊
          in
          changeCtorButtons ++ removeButtons ++ insertButtons

        perhapsTextEditBox =
          case maybeTextEditingPathAndText of
            Just (textEditingPath, string) ->
              let maybeLocation =
                Dict.get textEditingPath projectionPathToShapeSet
                |> Maybe.withDefault Set.empty
                |> Set.toList
                |> List.map TSEFLLPPolys.shapeToTopLeftCorner
                |> Utils.maximumBy (\(x,y) -> (y,x))
              in
              case maybeLocation of
                Just (left, top) ->
                  let inputAttrs =
                    [ Attr.type_ "text"
                    , Attr.id "tsefllpTextBox"
                    , Attr.defaultValue string
                    , Attr.style [ ("font-size", "18px")
                                 , ("display", "block")
                                 , ("position", "absolute")
                                 , ("left", px left), ("top", px top)
                                 ]
                    , Html.Events.onInput Controller.msgTSEFLLPUpdateTextBox
                    , HtmlUtils.onClickWithoutPropagation Controller.msgNoop
                    , HtmlUtils.onKeyDown <|
                        \keyCode ->
                          if keyCode == HtmlUtils.enterKeyCode -- Enter button
                          then Controller.msgTSEFLLPApplyTextEdit
                          else Controller.msgNoop
                    ]
                  in
                  [ Html.input inputAttrs [] ]
                Nothing ->
                  []
            Nothing ->
              []

        -- Relative to top-left corner of structured editor.
        logMousePosition xPx yPx = Controller.msgTSEFLLPMousePosition (xPx, yPx)

        sortByDeepestLeftmostLast paths =
          List.sortBy (\path -> (List.length path, List.map negate path)) paths
      in
      Html.div
          [ Attr.style [("position", "relative")] -- String -> Decoder msg -> Attribute msg
          -- , Html.Events.on "mousemove" (Json.Decode.map2 logMousePosition (Json.Decode.field "offsetX" Json.Decode.int) (Json.Decode.field "offsetY" Json.Decode.int))
          -- , Html.Events.onMouseOut Controller.msgTSEFLLPMouseOut
          ] <|
          -- (pixelPoly |> TSEFLLPPolys.flatten |> List.map TSEFLLPPolys.polyShape |> List.concatMap (pixelShapeToDivs (always Nothing) [Attr.style [("border", "solid black 1px")]])) ++
          (hoveredShapes  |> List.concatMap (pixelShapeToDivs (always Nothing) [Attr.style [("background-color", hoverColor)]])) ++ -- draw highlights under
          (selectedShapes |> List.concatMap (pixelShapeToDivs (always Nothing) [Attr.style [("background-color", selectedColor)]])) ++ -- draw highlights under
          -- [ Html.pre [Attr.style [("line-height", px charHeightPx), ("font-size", px charHeightPx), ("position", "absolute"), ("top", "0px"), ("left", "0px"), ("color", "grey")]] [text (taggedStringToNormalString taggedString)] ] ++
          [ Html.pre [Attr.style [("line-height", px charHeightPx), ("font-size", px charHeightPx), ("position", "absolute"), ("top", "0px"), ("left", "0px")]] [text (taggedStringToNormalString taggedString)] ] ++
          (hoveredShapes  |> List.concatMap (pixelShapeToDivs shapeToMaybeLabel [])) ++ -- draw labels on top
          (selectedShapes |> List.concatMap (pixelShapeToDivs shapeToMaybeLabel [])) ++ -- draw labels on top
          [ Html.pre [Attr.style [("line-height", px charHeightPx), ("font-size", px charHeightPx)]]                                                            [text (taggedStringToNormalString taggedString)] -- For spacing reasons.
          , Html.pre [] [text "\n"] -- spacing
          , Html.div [] [text "Hovered value paths: ", text (pathSetToString hoveredPathSet)]
          , Html.pre [] [text "Hovered values:\n  ", text (hoveredPathSet |> Set.toList |> sortByDeepestLeftmostLast |> List.map (pathToValue valueOfInterestTagged >> unparseToUntaggedString) |> String.join "\n  ")]
          , Html.div [] [text "Selected value paths: ", text (pathSetToString selectedPathSet)]
          , Html.pre [] [text "Selected values:\n  ", text (selectedPathSet |> Set.toList |> sortByDeepestLeftmostLast |> List.map (pathToValue valueOfInterestTagged >> unparseToUntaggedString) |> String.join "\n  ")]
          ] ++
          selectionAssignmentStats selectionAssignments pixelPoly valueOfInterestTagged ++
          [ Html.div (perhapsClickAttrs ++ [Attr.style [("position", "absolute"), ("position", "absolute"), ("left", "0px"), ("top", "0px"), ("width", "1000px"), ("height", "1000px")], Html.Events.onClick onClickMsg, Html.Events.on "mousemove" (Json.Decode.map2 logMousePosition (Json.Decode.field "offsetX" Json.Decode.int) (Json.Decode.field "offsetY" Json.Decode.int))]) []
          ] ++ buttonsAndMenus ++ perhapsTextEditBox -- Have to draw these on top of the click catcher above.

    Err err ->
      Html.div [Attr.style [("font-size", "18px"), ("color", "#e00")]] [text err]


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

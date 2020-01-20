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
  let
    renderingFunctionPicker =
      let optionNames = modelState.renderingFunctionNames in
      Html.select [] (optionNames |> List.map (\optionName -> Html.option [Attr.name optionName] [text optionName]))

  in
  if modelState.showWidgets then
    [ Html.div [] [renderingFunctionPicker]
    -- , stringTaggedWithProjectionPathsDebug modelState.stringTaggedWithProjectionPathsResult
    -- , Html.div [] [text "Selected poly paths: ", text (polyPathsToString modelState.selectedPolyPaths)]
    , Html.div
        [ Attr.style [("padding", "1em")] ]
        [ structuredEditor modelState ]
    -- , actionAssociationsDebug modelState.stringProjectionPathToSpecificActions modelState.stringTaggedWithProjectionPathsResult
    ]
  else
    [ Html.div [] [renderingFunctionPicker]
    , Html.div
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
  let targetPathSet = Set.singleton path in
  rootVal
  |> TSEFLLPEval.tagVal [] -- Ensure tagged if, e.g., we are exploring a new value from an action.
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


structuredEditor : TSEFLLPTypes.ModelState -> Html Msg
structuredEditor modelState =
  let
    { valueOfInterestTagged, dataTypeDefs, maybeValueOfInterestType, stringTaggedWithProjectionPathsResult, selectedPolyPaths, mousePosition, projectionPathToSpecificActions, shownActions } = modelState

    px int = toString int ++ "px"
  in
  case stringTaggedWithProjectionPathsResult of
    Ok taggedString ->
      let
        charWidthPx  = 11
        charHeightPx = 18

        selectedColor = "#B7D7FD"
        hoverColor    = "#d9e9fb"

        pixelPoly = TSEFLLPPolys.taggedStringToPixelPoly charWidthPx charHeightPx taggedString

        selectedShapes =
          selectedPolyPaths
          |> List.map (\polyPath -> TSEFLLPSelection.maybeShapeByPolyPath polyPath pixelPoly)
          |> Utils.filterJusts
          |> Utils.dedup

        selectionAssignments = TSEFLLPSelection.associateProjectionPathsWithShapes valueOfInterestTagged pixelPoly

        maybeHoveredShape = TSEFLLPSelection.mostRelevantShapeAtPoint mousePosition selectionAssignments
        hoveredShapes     = Utils.maybeToList maybeHoveredShape

        -- Debug.
        selectedPathSet = selectedShapes |> List.map (flip TSEFLLPSelection.shapeToPathSet selectionAssignments) |> Utils.unionAll
        hoveredPathSet  = hoveredShapes  |> List.map (flip TSEFLLPSelection.shapeToPathSet selectionAssignments) |> Utils.unionAll

        pathToType = TSEFLLPActions.makeProjectionPathToType dataTypeDefs maybeValueOfInterestType valueOfInterestTagged

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


        pixelShapeToDivs : List (Html.Attribute Msg) -> TSEFLLPPolys.PixelShape -> List (Html Msg)
        pixelShapeToDivs extraAttrs ({ bounds, rightBotCornerOfLeftTopCutout, leftTopCornerOfRightBotCutout } as pixelShape) =
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

            shapeToButtons ({ bounds, rightBotCornerOfLeftTopCutout, leftTopCornerOfRightBotCutout } as pixelShape) =
              let
                (left, top, right, bot) = bounds
                (startX, firstLineBot)  = rightBotCornerOfLeftTopCutout
                (endX, lastLineTop)     = leftTopCornerOfRightBotCutout

                actions =
                  TSEFLLPSelection.shapeToPathSet pixelShape selectionAssignments
                  |> Set.toList
                  |> List.map (\path -> Utils.getWithDefault path Set.empty projectionPathToSpecificActions)
                  |> Utils.unionAll

                deleteActions =
                  actions
                  |> Set.filter (specificActionMaybeChangeType >> (==) (Just Remove))

                changeCtorActions =
                  actions
                  |> Set.filter (specificActionMaybeChangeType >> (==) (Just ChangeCtor))

                maybeShapePolyPath = TSEFLLPSelection.shapeToMaybePolyPath pixelShape pixelPoly

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

                perhapsButton buttonHtmls actions right top =
                  let shownButtonActions =                    -- Deepest, leftmost first
                      Set.intersect shownActions actions
                      |> Set.toList
                      |> List.sortBy (\action -> (negate <| List.length (specificActionProjectionPath action), specificActionProjectionPath action))
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
                  else if Set.size actions >= 1 then
                    let onClick =
                      -- If only one possible delete action, apply immediately.
                      case Set.toList actions |> List.map specificActionMaybeNewValue of
                        [Just newVal] -> Controller.msgTSEFLLPSelectNewValue newVal
                        _             -> Controller.msgTSEFLLPShowActions maybeShapePolyPath actions
                    in
                    [ Html.div
                          [ Attr.style [ ("position", "absolute")
                                        , ("left", px (right - charWidthPx//2)), ("top", px (top - charWidthPx//2))
                                        , ("width", px (charWidthPx//2)), ("height", px (charHeightPx//2))
                                        , ("font-size", px (charHeightPx//2))
                                        , ("cursor", "pointer")
                                        ]
                          , Html.Events.onClick onClick
                          ]
                          buttonHtmls
                    ]
                  else
                    []

                perhapsDeleteButton     = perhapsButton [text "❌"] deleteActions     right top -- ❌✘✕✖︎✗
                perhapsChangeCtorButton = perhapsButton [text "🔽"] changeCtorActions left  ((bot + top) // 2) -- 🔽▾▼⎊
              in
              perhapsDeleteButton ++ perhapsChangeCtorButton
          in
          List.concatMap shapeToButtons shownShapes

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
          -- pixelPolyToDivs [Attr.style [("border", "solid black 1px"), ("margin", "-1px")]] pixelPoly ++
          (hoveredShapes  |> List.concatMap (pixelShapeToDivs [Attr.style [("background-color", hoverColor)]])) ++ -- draw highlights under
          (selectedShapes |> List.concatMap (pixelShapeToDivs [Attr.style [("background-color", selectedColor)]])) ++ -- draw highlights under
          [ Html.pre [Attr.style [("line-height", px charHeightPx), ("font-size", px charHeightPx), ("position", "absolute"), ("top", "0px"), ("left", "0px")]] [text (taggedStringToNormalString taggedString)] ] ++
          (hoveredShapes  |> List.concatMap (pixelShapeToDivs [])) ++ -- draw labels on top
          (selectedShapes |> List.concatMap (pixelShapeToDivs [])) ++ -- draw labels on top
          [ Html.pre [Attr.style [("line-height", px charHeightPx), ("font-size", px charHeightPx)]]                                                            [text (taggedStringToNormalString taggedString)] -- For spacing reasons.
          , Html.div [] [text "Hovered value paths: ", text (pathSetToString hoveredPathSet)]
          , Html.pre [] [text "Hovered values:\n  ", text (hoveredPathSet |> Set.toList |> sortByDeepestLeftmostLast |> List.map (pathToValue valueOfInterestTagged >> unparseToUntaggedString) |> String.join "\n  ")]
          , Html.div [] [text "Selected value paths: ", text (pathSetToString selectedPathSet)]
          , Html.pre [] [text "Selected values:\n  ", text (selectedPathSet |> Set.toList |> sortByDeepestLeftmostLast |> List.map (pathToValue valueOfInterestTagged >> unparseToUntaggedString) |> String.join "\n  ")]
          , Html.div [Attr.style [("position", "absolute"), ("position", "absolute"), ("left", "0px"), ("top", "0px"), ("width", "1000px"), ("height", "1000px")], Html.Events.onClick onClickMsg, Html.Events.on "mousemove" (Json.Decode.map2 logMousePosition (Json.Decode.field "offsetX" Json.Decode.int) (Json.Decode.field "offsetY" Json.Decode.int))] []
          ] ++ buttonsAndMenus -- Have to draw these on top of the click catcher above.

    Err err ->
      Html.div [Attr.style [("font-size", "18px"), ("color", "#e00")]] [text err]


-- structuredEditor : TSEFLLPTypes.ModelState -> Html Msg
-- structuredEditor modelState =
--   let { valueOfInterestTagged, dataTypeDefs, maybeRenderingFunctionNameAndProgram, selectedPolyPaths, stringProjectionPathToSpecificActions, stringTaggedWithProjectionPathsResult, maybeNewValueOptions } = modelState in
--   let
--     render
--       :  Set ProjectionPath
--       -> AppendedTaggedStrings (Maybe ProjectionPath, Set ProjectionPath, Set SpecificAction)
--       -> Html Msg
--     render pathsInAncestors stringTaggedWithSelectionPathAndProjectionPathsAndActions =
--       let
--         (maybeSelectionClickPath, immediatePathSet, actions) = stringTag stringTaggedWithSelectionPathAndProjectionPathsAndActions
--         pathSetWithAncestors                                 = Set.union pathsInAncestors immediatePathSet
--
--         -- Although the mouse region for triggering a selection is small,
--         -- we display the selection over the entire area(s) of the string
--         -- associated with the selected path.
--         perhapsSelectedDisplayAttrs =
--           if Utils.anyOverlap [immediatePathSet, selectedPolyPaths]
--           then [Attr.style [("border", "3px solid blue"), ("margin", "-3px")]]
--           else []
--       in
--       case stringTaggedWithSelectionPathAndProjectionPathsAndActions of
--         TaggedString string _ ->
--           let
--             (perhapsClickAttrs, isTextEditing) =
--               case maybeSelectionClickPath of
--                 Nothing                 -> ([], False)
--                 Just selectionClickPath ->
--                   let
--                     onClickMsg =
--                       if Set.member selectionClickPath selectedPolyPaths
--                       then Controller.msgTSEFLLPDeselectPolyPath selectionClickPath
--                       else Controller.msgTSEFLLPSelectPolyPath   selectionClickPath
--
--                     scrubSpecificActions    = actions |> Set.toList |> List.filter isScrubSpecificAction
--                     editTextSpecificActions = actions |> Set.toList |> List.filter isEditTextSpecificAction
--
--                     perhapsStartLiveSync =
--                       case scrubSpecificActions of
--                         [Scrub projectionPath] -> [ Html.Events.onMouseDown <| Controller.msgTSEFLLPStartLiveSync projectionPath ]
--                         _                      -> []
--
--                     perhapsStartTextEdit =
--                       case (modelState.maybeTextEditingPathAndText, editTextSpecificActions) of
--                         (Nothing, [EditText projectionPath]) -> [ Html.Events.onDoubleClick <| Controller.msgTSEFLLPStartTextEditing (projectionPath, string) ]
--                         _                                    -> []
--
--                     isTextEditing =
--                       modelState.maybeTextEditingPathAndText
--                       |> Maybe.map (\(textEditingPath, _) -> [textEditingPath] == List.map specificActionProjectionPath editTextSpecificActions)
--                       |> Maybe.withDefault False
--
--                     cursor =
--                       if List.length editTextSpecificActions == 1 then
--                         "text"
--                       else if List.length scrubSpecificActions == 1 then
--                         "ns-resize"
--                       else
--                         "pointer"
--                   in
--                   ( [ Attr.style [("cursor", cursor)]
--                     , Html.Events.onClick onClickMsg
--                     ] ++ perhapsStartLiveSync ++ perhapsStartTextEdit
--                   , isTextEditing
--                   )
--
--             perhapsActions =
--               let
--                 actionList = Set.toList actions
--
--                 insertActionNewValues     = actionList |> List.filter (specificActionMaybeChangeType >> (==) (Just Insert))     |> List.filterMap specificActionMaybeNewValue
--                 removeActionNewValues     = actionList |> List.filter (specificActionMaybeChangeType >> (==) (Just Remove))     |> List.filterMap specificActionMaybeNewValue
--                 changeCtorActionNewValues = actionList |> List.filter (specificActionMaybeChangeType >> (==) (Just ChangeCtor)) |> List.filterMap specificActionMaybeNewValue
--
--                 xOffset = 4 * String.length string - 5
--
--                 button yOffset color onClickMsg str =
--                   Html.span
--                       [ Attr.style [ ("display", "inline-block")
--                                    , ("width",  "0")
--                                    , ("vertical-align", toString yOffset ++ "em"),  ("margin-top", toString (-yOffset) ++ "em")
--                                    , ("margin-left", toString xOffset ++ "px")
--                                    , ("margin-right", toString (-xOffset) ++ "px")
--                                    , ("color", color)
--                                    , ("cursor", "pointer")
--                                    , ("font-size", "75%")
--                                    ]
--                       , Html.Events.onClick onClickMsg
--                       ]
--                       [ text str ]
--
--                 perhapsInsertButton =
--                   case insertActionNewValues of
--                     []                    -> []
--                     [newValueAfterInsert] -> [ button 1.05 "green" (Controller.msgTSEFLLPSelectNewValue newValueAfterInsert)       "⊕" ]
--                     newValuesAfterInsert  -> [ button 1.05 "green" (Controller.msgTSEFLLPShowNewValueOptions newValuesAfterInsert) "⊕" ]
--
--                 perhapsRemoveButton =
--                   case removeActionNewValues of
--                     []                    -> []
--                     [newValueAfterRemove] -> [ button -0.9 "red" (Controller.msgTSEFLLPSelectNewValue newValueAfterRemove)       "⊖" ]
--                     newValuesAfterRemove  -> [ button -0.9 "red" (Controller.msgTSEFLLPShowNewValueOptions newValuesAfterRemove) "⊖" ]
--
--                 perhapsChangeCtorButton =
--                   case changeCtorActionNewValues of
--                     []                        -> []
--                     [newValueAfterChangeCtor] -> [ button -1.8 "gold" (Controller.msgTSEFLLPSelectNewValue newValueAfterChangeCtor)       "∼" ]
--                     newValuesAfterChangeCtor  -> [ button -1.8 "gold" (Controller.msgTSEFLLPShowNewValueOptions newValuesAfterChangeCtor) "∼" ]
--               in
--               perhapsInsertButton ++ perhapsRemoveButton ++ perhapsChangeCtorButton
--           in
--           Html.span perhapsSelectedDisplayAttrs <|
--             if isTextEditing then
--               let inputAttrs =
--                 [ Attr.type_ "text"
--                 , Attr.id "tsefllpTextPoly"
--                 , Attr.defaultValue string
--                 , Attr.style [("font-size", "18px")]
--                 , Html.Events.onInput Controller.msgTSEFLLPUpdateTextPoly
--                 , HtmlUtils.onClickWithoutPropagation Controller.msgNoop
--                 , HtmlUtils.onKeyDown <|
--                     \keyCode ->
--                       if keyCode == HtmlUtils.enterKeyCode -- Enter button
--                       then Controller.msgTSEFLLPApplyTextEdit
--                       else Controller.msgNoop
--                 ]
--               in
--               perhapsActions ++ [ Html.input inputAttrs [] ]
--             else
--               perhapsActions ++ [ Html.span perhapsClickAttrs [text string] ]
--
--         TaggedStringAppend left right _ ->
--           Html.span
--               perhapsSelectedDisplayAttrs
--               [ render pathSetWithAncestors left
--               , render pathSetWithAncestors right
--               ]
--   in
--   case stringTaggedWithProjectionPathsResult of
--     Ok stringTaggedWithProjectionPaths ->
--       case maybeNewValueOptions of
--         Nothing ->
--           let
--             stringTaggedWithSelectionPathAndProjectionPathsAndActions =
--               stringTaggedWithProjectionPaths
--               |> assignSelectionClickAreas
--               |> assignActionsToLeaves stringProjectionPathToSpecificActions
--           in
--           Html.div [Attr.style [("font-size", "18px")]] [render Set.empty stringTaggedWithSelectionPathAndProjectionPathsAndActions]
--
--         Just newValueOptions ->
--           let
--             renderNewValueOption newValue =
--               let displayStr =
--                 case maybeRenderingFunctionNameAndProgram of
--                   Just { renderingFunctionName, multipleDispatchFunctions, desugaredToStringProgram } ->
--                     let newStringTaggedWithProjectionPathsResult =
--                       TSEFLLPEval.evalToStringTaggedWithProjectionPaths
--                           dataTypeDefs
--                           multipleDispatchFunctions
--                           desugaredToStringProgram
--                           newValue
--                     in
--                     newStringTaggedWithProjectionPathsResult
--                     |> Result.map taggedStringToNormalString
--                     |> Utils.fromResult -- Err and Ok both wrap strings, so unwrap.
--
--                   Nothing ->
--                     "New Value " ++ unparseToUntaggedString newValue
--               in
--               Html.div
--                   [ Attr.class "text-button"
--                   , Html.Events.onClick <| Controller.msgTSEFLLPSelectNewValue newValue
--                   ]
--                   [ text displayStr ]
--           in
--           Html.div
--               [ Attr.style [("font-size", "18px")] ]
--               (newValueOptions |> List.map renderNewValueOption)
--
--     Err _ ->
--       plainStringView stringTaggedWithProjectionPathsResult


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

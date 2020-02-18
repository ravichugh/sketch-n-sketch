module TSEFLLPSelection exposing (SelectionAssignments, selectedPolyPathsToProjectionPathSet, associateProjectionPathsWithShapes, findOccludedPathSetAndShapes, findNonOccludedPathSetAndShapeSet, maybeShapeByPolyPath, shapeToMaybePolyPath, mostRelevantShapeAtPoint, occludedShapesAtPoint, shapeToPathSet, shapeToOccludedPathSet, makeProjectionPathToShapeSet)

-- Module for associating overlay shapes with parts of the value of interest (projection paths).
--
-- Somewhat confusingly, current selections are also indexed by paths, but paths into the
-- tree of poly shapes, not paths into the original data structure.

import Dict exposing (Dict)
import Set exposing (Set)

import TSEFLLPPolys exposing (..)
import TSEFLLPTypes exposing (..)
import Utils


type alias SelectionAssignments = Dict PixelShape (Set ProjectionPath)


-- Only need polyPaths between runs (to not lose selection for small changes like number scrubbing), otherwise polyPaths are immediately turned into shapes
selectedPolyPathsToProjectionPathSet : List PolyPath -> StringTaggedWithProjectionPaths -> Set ProjectionPath
selectedPolyPathsToProjectionPathSet selectedPolyPaths taggedString =
  let
    pixelPoly = TSEFLLPPolys.taggedStringToPixelPoly 1 1 taggedString

    selectedShapes =
      selectedPolyPaths
      |> List.map (\polyPath -> maybeShapeByPolyPath polyPath pixelPoly)
      |> Utils.filterJusts
      |> Utils.dedup

    selectionAssignments = associateProjectionPathsWithShapes pixelPoly
  in
  selectedShapes
  |> List.map (flip shapeToPathSet selectionAssignments)
  |> Utils.unionAll


-- Version 1, by shape precisely.
--
-- Only shapes with non-empty pathsets are included. (And findNonOccludedPathSet below assumes this.)
--
-- Some subvalue paths may be occluded if their poly is entirely covered or has zero area.
associateProjectionPathsWithShapes : PixelPoly -> SelectionAssignments
associateProjectionPathsWithShapes ((Poly { pathSet, children }) as pixelPoly) =
  let
    recurse = associateProjectionPathsWithShapes
    shape = polyShape pixelPoly

    associationDictHere =
      if Set.isEmpty pathSet
      then Dict.empty
      else Dict.singleton shape pathSet
  in
  children
  |> List.map recurse
  |> Utils.foldl associationDictHere Utils.unionDictSets


-- Projection paths and shapes in output string but with no hover region b/c precisely
-- covered by smaller (deeper) shapes.
--
-- May include 0-area shapes since those are not filtered out by associateProjectionPathsWithShapes.
findOccludedPathSetAndShapes : SelectionAssignments -> PixelPoly -> (Set ProjectionPath, List PixelShape)
findOccludedPathSetAndShapes selectionAssignments pixelPoly =
  let
    allPathsSet  = Utils.unionAll <| Dict.values selectionAssignments
    allShapesSet = Set.fromList   <| Dict.keys   selectionAssignments

    (nonOccludedPathSet, nonOccludedShapeSet) = findNonOccludedPathSetAndShapeSet selectionAssignments pixelPoly
  in
  ( Set.diff allPathsSet  nonOccludedPathSet
  , Set.diff allShapesSet nonOccludedShapeSet |> Set.toList
  )


findNonOccludedPathSetAndShapeSet : SelectionAssignments -> PixelPoly -> (Set ProjectionPath, Set PixelShape)
findNonOccludedPathSetAndShapeSet selectionAssignments pixelPoly =
  let
    (nonOccludedShapeSet, _) = findNonOccludedShapeSet selectionAssignments pixelPoly
    nonOccludedPathSet =
      nonOccludedShapeSet
      |> Set.toList
      |> List.map (flip Utils.dictGetSet selectionAssignments)
      |> Utils.unionAll
  in
  ( nonOccludedPathSet
  , nonOccludedShapeSet
  )


-- Returns (non-occluded shape list, occluded area)
--
-- Since parents shapes always contain child shapes and child shapes are disjoint, we just have to sum areas
-- to determine if part of the shape is peaking out.
findNonOccludedShapeSet : SelectionAssignments -> PixelPoly -> (Set PixelShape, Int)
findNonOccludedShapeSet selectionAssignments ((Poly { children }) as pixelPoly) =
  let
    recurse = findNonOccludedShapeSet selectionAssignments

    (childrenNonOccludedShapeSet, childrenOccludedArea) =
      children
      |> List.map recurse
      |> List.unzip
      |> Utils.mapFirstSecond Utils.unionAll List.sum

    shape     = polyShape pixelPoly
    shapeArea = area shape
  in
  case Dict.member shape selectionAssignments of
    True ->
      if shapeArea > childrenOccludedArea then -- This shape has clickable area.
        ( Set.insert shape childrenNonOccludedShapeSet
        , shapeArea
        )
      else if shapeArea == childrenOccludedArea then -- Since selection assignments are by shape, it's okay if a child shape and parent shape match perfectly--the pathSet will have been caught by the child.
        ( childrenNonOccludedShapeSet
        , childrenOccludedArea
        )
      else
        let _ = Utils.log "TSEFLLPSelection.findNonOccludedPathSet invariant broken: expected parent polys to never be smaller than all their child polys" in
        ( childrenNonOccludedShapeSet
        , shapeArea
        )

    False ->
      ( childrenNonOccludedShapeSet
      , childrenOccludedArea
      )


maybeShapeByPolyPath : PolyPath -> PixelPoly -> Maybe PixelShape
maybeShapeByPolyPath targetPath rootPoly =
  maybeShapeByPolyPath_ targetPath [] rootPoly

maybeShapeByPolyPath_ : PolyPath -> PolyPath -> PixelPoly -> Maybe PixelShape
maybeShapeByPolyPath_ targetPath currentPath ((Poly { children }) as pixelPoly) =
  let recurse = maybeShapeByPolyPath_ targetPath in
  if targetPath == currentPath then
    Just (polyShape pixelPoly)
  else
    children
    |> Utils.mapi1 (\(i, child) -> recurse (currentPath ++ [i]) child)
    |> Utils.firstMaybe


-- The shallowest path is most likely the most robust across actions, so take the first success.
shapeToMaybePolyPath : PixelShape -> PixelPoly -> Maybe PolyPath
shapeToMaybePolyPath targetShape rootPoly =
  shapeToMaybePolyPath_ targetShape [] rootPoly

shapeToMaybePolyPath_ : PixelShape -> PolyPath -> PixelPoly -> Maybe PolyPath
shapeToMaybePolyPath_ targetShape currentPath ((Poly { children }) as pixelPoly) =
  let recurse = shapeToMaybePolyPath_ targetShape in
  if polyShape pixelPoly == targetShape then
    Just currentPath
  else
    children
    |> Utils.mapi1 (\(i, child) -> recurse (currentPath ++ [i]) child)
    |> Utils.firstMaybe


-- For determining what is hovered. Go for deepest (smallest).
--
-- Smallest and deepest are the same b/c each level is no larger than the level above it and doesn't overlap with siblings.
mostRelevantShapeAtPoint : (Int, Int) -> SelectionAssignments -> Maybe PixelShape
mostRelevantShapeAtPoint point selectionAssignments =
  selectionAssignments
  |> Dict.keys
  |> List.sortBy area
  |> Utils.findFirst (containsPoint point)


occludedShapesAtPoint : (Int, Int) -> List PixelShape -> List PixelShape
occludedShapesAtPoint point occludedShapes =
  occludedShapes
  |> List.filter (containsPoint point)


shapeToPathSet : PixelShape -> SelectionAssignments -> Set ProjectionPath
shapeToPathSet shape selectionAssignments =
  Utils.dictGetSet shape selectionAssignments


shapeToOccludedPathSet : PixelShape -> Set ProjectionPath -> SelectionAssignments -> Set ProjectionPath
shapeToOccludedPathSet shape occludedPathSet selectionAssignments =
  Set.intersect
    occludedPathSet
    (Utils.dictGetSet shape selectionAssignments)


makeProjectionPathToShapeSet : SelectionAssignments -> Dict ProjectionPath (Set PixelShape)
makeProjectionPathToShapeSet selectionAssignments =
  selectionAssignments
  |> Dict.toList
  |> List.concatMap (\(shape, pathSet) ->
    pathSet
    |> Set.toList
    |> List.map (\path -> Dict.singleton path (Set.singleton shape))
  )
  |> Utils.foldl Dict.empty Utils.unionDictSets

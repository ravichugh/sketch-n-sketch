module TSEFLLPSelection exposing (associateProjectionPathsWithShapes, maybeShapeByPolyPath, shapeToMaybePolyPath, mostRelevantShapeAtPoint, shapeToPathSet)

-- Module for associating overlay shapes with parts of the value of interest (projection paths).
--
-- Somewhat confusingly, current selections are also indexed by paths, but paths into the
-- tree of poly shapes, not paths into the original data structure.

import Dict exposing (Dict)
import Set exposing (Set)

import TSEFLLPPolys exposing (..)
import TSEFLLPTypes exposing (..)
import Utils


-- Current task: only need polyPaths between runs (to not lose selection for small changes like number scrubbing), otherwise polyPaths should immediately be turned into shapes
type alias SelectionAssignments = Dict PixelShape (Set ProjectionPath)


-- Version 1, by shape precisely.
--
-- Only shapes with non-empty pathsets are included.
--
-- Some subvalue paths may be occluded if their poly is entirely covered or has zero area.
associateProjectionPathsWithShapes : TaggedValue -> PixelPoly -> SelectionAssignments
associateProjectionPathsWithShapes valueOfInterestTagged ((Poly { pathSet, children }) as pixelPoly) =
  let
    recurse = associateProjectionPathsWithShapes valueOfInterestTagged
    shape = polyShape pixelPoly

    associationDictHere =
      if Set.isEmpty pathSet
      then Dict.empty
      else Dict.singleton shape pathSet
  in
  children
  |> List.map recurse
  |> Utils.foldl associationDictHere Utils.unionDictSets


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


shapeToPathSet : PixelShape -> SelectionAssignments -> Set ProjectionPath
shapeToPathSet shape selectionAssignments =
  Utils.dictGetSet shape selectionAssignments

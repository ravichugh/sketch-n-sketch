module BoundsUtils exposing (..)


type alias Bounds a = (a, a, a, a) -- Left Top Right Bot


-- Enclosing bounding box
enclosureOfBoundsPair : Bounds comparable -> Bounds comparable -> Bounds comparable
enclosureOfBoundsPair (left1, top1, right1, bot1) (left2, top2, right2, bot2) =
  ( min  left1  left2
  , min   top1   top2
  , max right1 right2
  , max   bot1   bot2
  )


-- Inclusive on left/top edges and exclusive on bot/right edges.
containsPoint : Bounds comparable -> (comparable, comparable) -> Bool
containsPoint (left, top, right, bot) (x, y) =
  left <= x && x < right &&
  top  <= y && y < bot


width : Bounds number -> number
width (left, _, right, _) = right - left


height : Bounds number -> number
height (_, top, _, bot) = bot - top


area : Bounds number -> number
area bounds = width bounds * height bounds


maybeEnclosureOfAllBounds : List (Bounds comparable) -> Maybe (Bounds comparable)
maybeEnclosureOfAllBounds bounds =
  case bounds of
    []          -> Nothing
    first::rest -> Just (rest |> List.foldl enclosureOfBoundsPair first)


-- Returns Nothing if list is empty; otherwise returns Just (left, top, right, bot)
-- Yet unused, but will be needed for code dedduplication between selectablePointToMaybeXY and maybeShapeBounds and maybeWidgetBounds.
pointsToMaybeBounds : List (comparable, comparable) -> Maybe (Bounds comparable)
pointsToMaybeBounds points =
  let (xs, ys) = List.unzip points in
  case (List.minimum xs, List.minimum ys, List.maximum xs, List.maximum ys) of
    (Just left, Just top, Just right, Just bot) -> Just (left, top, right, bot)
    _                                           -> Nothing

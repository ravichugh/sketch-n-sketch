module Record exposing (
  mapWithNth,
  MergeOperation,
  getMergeOperations,
  getMergeMapping,
  mergeLabelValues,
  getPatternMatch)

import Lang exposing (..)
import ImpureGoodies
import Utils

mapWithNth: (a -> a -> Bool) -> List a -> List (a, Int)
mapWithNth sameKeys labelValues  =
  List.indexedMap (\i lv ->
    let n = nth labelValues i (sameKeys lv) in
    (lv, n)
  ) labelValues

nthIndexWhere: List a -> Int -> (a -> Bool) -> Maybe (a, Int)
nthIndexWhere a nth f =
  let aux it index n =
    case it of
       [] -> Nothing
       head::tail ->
         if f head then
           if n + 1 == nth then Just (head, index)
           else aux tail (index + 1) (n + 1)
         else aux tail (index + 1) n
  in aux a 0 0

nthlastIndexWhere: List a -> Int -> (a -> Bool) -> Maybe (a, Int)
nthlastIndexWhere a nth f =
  nthIndexWhere (List.reverse a) nth f
  |> Maybe.map (\(elem, x) -> (elem, List.length a - 1 - x))

type MergeOperation a b =
  --case class InsertAtPos(i: Int, nth: Int, origIndex: Int, labelValue: (String, Expr)) extends MergeOperation
    InsertAtPos Int Int Int b
  -- case class ReplaceAtPos(i: Int, origIndex: Int, labelValue: (String, Expr)) extends MergeOperation
  | ReplaceAtPos Int Int (a, b)

-- returns n where the label at position index is the n-th among bv from the right*/
nth: List a -> Int -> (a -> Bool) -> Int
nth bv index labelMatcher =
  (bv |>
  List.drop (index + 1) |>
  Utils.count labelMatcher) + 1

-- Returns the merge operations to do when merging records av ++ bv, or for pattern matching av against bv. */
getMergeOperations: (a -> key) -> (b -> key) -> List a -> List b -> List (MergeOperation a b)
getMergeOperations keyA keyB av bv =
  let lastIndex = { value = -2 } in
  let lastIndexSet = ImpureGoodies.mutateRecordField lastIndex "value" in
  -- We should insert and replace from the end, so the furthest operations should be done first.
  -- Replacement at a point should be prioritary than inserting at this point.
  -- Inserting a more recent label-value should be done firs  t. (ni, nj comparison)
  let sorter: MergeOperation a b  -> MergeOperation a b -> Order
      sorter x y =
        case (x, y) of
          (InsertAtPos -1 _ _ _, _) -> LT
          (_, InsertAtPos -1 _ _ _) -> GT
          (InsertAtPos i ni _ _, InsertAtPos j nj _ _) ->
            if i > j || (i == j && ni < nj) then LT
            else if i == j && ni == nj then EQ else GT
          (ReplaceAtPos i _ _, ReplaceAtPos j _ _) -> if i > j then LT else if i == j then EQ else GT
          (InsertAtPos i ni _ _, ReplaceAtPos j _ _) -> if i > j then LT else GT
          (ReplaceAtPos i _ _, InsertAtPos j nj _ _) -> if i >= j then LT else GT
  in
  let bvIndexed = Utils.zipWithIndex bv in
  let sameBB = \x y-> keyB x == keyB y in
  let sameBA = \x y-> keyB x == keyA y in
  bvIndexed |>
  List.map (\(lv, i) ->
    let n = nth bv i (sameBB lv) in
    case nthlastIndexWhere av n (sameBA lv) of
      Nothing ->
        if lastIndex.value == -2 then -- Start, we did not have any clue where to insert.
          --println("wait ! no index had been found previously")
          let possibilities =
               List.drop (i+1) bvIndexed
            |> List.filterMap (\(m, k) ->
            --//println(s"Trying to insert at $m")
                let n2 = nth bv k (sameBB m) in
                --//println(s"nth = $n2, av = $av, label = $label")
                nthlastIndexWhere av n2 (sameBA m)
                |> Maybe.map Tuple.second
            )
          in
          --//println(s"Insertion possibilities: $possibilities")
          let headOfPossibilities = case possibilities of
            head::tail-> head
            _ -> -1
          in
          InsertAtPos headOfPossibilities n i lv
        else
          InsertAtPos (lastIndex.value + 1) n i lv
      Just (elem, positionToReplace) ->
        let _ = lastIndexSet positionToReplace in
        ReplaceAtPos positionToReplace i (elem, lv)
  ) |>
  List.sortWith sorter

type MergeMapping = InsertToLeft Int | InsertToRight Int

getPatternMatch: (pat -> k) -> (field -> k) -> List pat -> List field -> Maybe (List (pat, field))
getPatternMatch keyPat keyRecord patElems recordElems =
  getMergeOperations keyRecord keyPat recordElems patElems
  |> List.map (\mo -> case mo of
    InsertAtPos _ _ _ _ -> Nothing
    ReplaceAtPos ia ib (a, b) -> Just (b, a))
  |> Utils.projJusts

-- For record union update
getMergeMapping: (a -> key) -> List a -> List a -> List a -> List MergeMapping
getMergeMapping keyA av bv out =
  let mergingOperations = getMergeOperations keyA keyA av bv in
  let avIndices = List.range 0 (List.length av - 1) in
  List.foldl (\p avNew ->
    case p of
      InsertAtPos -1 _ origIndex lv -> avNew ++ [InsertToRight origIndex]
      ReplaceAtPos toReplace origIndex (av, lv) -> Utils.updated avNew toReplace (InsertToRight origIndex)
      InsertAtPos toInsert _ origIndex lv -> Utils.inserted avNew toInsert (InsertToRight origIndex)
  ) (List.map InsertToLeft avIndices) mergingOperations

-- For record union eval
mergeLabelValues: (a -> key) -> List a -> List a -> List a
mergeLabelValues keyA av bv =
  let mergingOperations = getMergeOperations keyA keyA av bv in
  let newRecord: List a
      newRecord = List.foldl (\p avNew ->
        case p of
          InsertAtPos -1 _ _ lv -> avNew ++ [lv]
          ReplaceAtPos toReplace _ (_, lv) -> Utils.updated avNew toReplace lv
          InsertAtPos toInsert _ _ lv -> Utils.inserted avNew toInsert lv
       ) av mergingOperations
  in
  newRecord

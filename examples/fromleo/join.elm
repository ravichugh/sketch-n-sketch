substring = String.substring
take = String.take
drop = String.drop
length = String.length

Results = LensLess.Results

preferStringInsertionToLeft_ sa inserted sb = True

ok1 x = Ok [x]

joinEmpty list = 
  let aux acc list = case list of
      [] -> acc
      [head] -> acc + head
      (head::tail) -> aux (acc + head) tail
    in 
  {
  apply list = aux "" list
  update {input, newOutput, oldOutput, diffs} =
    case diffs of
      VStringDiffs l ->
        let lastIndex = len input - 1 in
        -- startHead is the real ending position in the oldOutput of the last element considered, hence the start position of the head of input.
        -- Since the first element might be modified recursively, the length of head might be affected, but the end position should still be the original one.
        -- Hence we need to offset startHead + length head by deltaLengthHeadInput to compute endHead.       
        let gather lastIndexDeleted input indexInput startHead deltaLengthHeadInput offsetOutput diffs =
          let _ = Debug.log ("""gather@(":") @lastIndexDeleted @input @indexInput @startHead @deltaLengthHeadInput @offsetOutput @diffs""") [] in
          Debug.log ("""gather@(":") @lastIndexDeleted @input @indexInput @startHead @deltaLengthHeadInput @offsetOutput @diffs""") <|
          case diffs of
          [] -> ok1 (input, [])
          ((StringUpdate start end replaced) :: diffTail) ->
            case input of
              [] ->
                let inserted = substring (start + offsetOutput) (end + offsetOutput + replaced) newOutput in
                ok1 ([inserted], [(indexInput, ListElemInsert 1)])
              (head::tail) ->
                let endHead = startHead + length head - deltaLengthHeadInput in
                --We assume that start >= startHead (invariant)
                if start < endHead && end > endHead then
                  let firstReplaced =
                    gather lastIndexDeleted input indexInput startHead deltaLengthHeadInput offsetOutput ((StringUpdate start endHead 0)::(StringUpdate endHead end replaced)::diffTail)
                  in
                  if replaced == 0 then firstReplaced else firstReplaced |> Results.andAlso (
                    gather lastIndexDeleted input indexInput startHead deltaLengthHeadInput offsetOutput ((StringUpdate start endHead replaced)::(StringUpdate endHead end 0)::diffTail)
                  )
                else 
                  -- Now either start >= endHead || end <= endHead
                  -- Here we postpone the diff to afterwards if not the end and the diff happens strictly after
                if (start > endHead || start == endHead && end > start) && indexInput /= lastIndex then
                  gather lastIndexDeleted tail (indexInput + 1) endHead 0 offsetOutput diffs |> Results.andThen (\(tail, diffTail) ->
                    ok1 (head::tail, diffTail)
                  )
                else 
                  -- Now: (start >= endHead || end <= endHead) && start <= endHead && (start < endHead || (end == start && indexInput == lastIndex))
                if start == endHead && end == start then
                  -- Now: end == endHead && start == endHead && indexInput != lastIndex
                  let inserted = substring (start + offsetOutput) (end + offsetOutput + replaced) newOutput in
                  let sa = substring 0 start oldOutput in
                  let sb = drop end oldOutput in
                  let newHead = head + inserted in
                  let newOffsetOutput = offsetOutput + replaced - (end - start) in
                  let appendNow = gather -1 tail (indexInput + 1) endHead 0 newOffsetOutput diffTail |>
                    Results.andThen (\(newTail, newDiffTail) ->
                    ok1 (newHead :: newTail, (indexInput, ListElemUpdate (VStringDiffs [StringUpdate (start - startHead) (end - startHead) replaced])) :: newDiffTail)
                    )
                  in
                  if indexInput == lastIndex then
                    appendNow
                  else
                   let appendLater = gather -1 tail (indexInput + 1) endHead 0 offsetOutput diffs |>
                        Results.andThen (\(newTail, newDiffTail) ->
                        ok1 (head::newTail, newDiffTail)
                      )
                   in
                  if preferStringInsertionToLeft_ sa inserted sb
                  then appendNow |> Results.andAlso appendLater
                  else appendLater |> Results.andAlso appendNow
                else 
                  let offsetChange = replaced - (end - start) in
                  let newOffsetOutput = offsetOutput + offsetChange in
                  if start == startHead && end == endHead && replaced == 0 then
                  -- If the entire string was deleted, one option is to delete the element alltogether. We display only this option if chars from the left and from the right were deleted as well.
                  let resultsWithDelete = gather end tail (indexInput + 1) endHead 0 newOffsetOutput diffTail |> Results.andThen (\(newTail, newDiffTail) ->
                    ok1 (newTail, (indexInput, ListElemDelete 1)::newDiffTail)
                    )
                  in
                  let deleteAnyway = lastIndexDeleted == start && (case diffTail of
                    ((StringUpdate start2 end2 0) :: _) -> start2 == endHead && end2 > start2
                    _ -> indexInput == lastIndex )
                  in
                  let resultsWithEmptyString = gather end tail (indexInput + 1) endHead 0 newOffsetOutput diffTail |> Results.andThen (\(newTail, newDiffTail) ->
                    ok1 ("" :: newTail, (indexInput, ListElemUpdate (VStringDiffs [StringUpdate 0 (endHead - startHead) 0]))::newDiffTail)
                  )
                  in
                  if deleteAnyway then
                    resultsWithDelete
                  else
                    resultsWithDelete |> Results.andAlso resultsWithEmptyString
                else -- start
                  let inserted = substring (start + offsetOutput) (start + offsetOutput + replaced) newOutput in
                  let newHead = substring 0 (start - startHead - deltaLengthHeadInput) head +
                    inserted + drop (end - startHead - deltaLengthHeadInput) head in
                  let newDeltaLengthHeadInput = deltaLengthHeadInput - offsetChange in
                  let thisDiff = StringUpdate (start - startHead) (end - startHead) replaced in
                  gather (if replaced == 0 then end else -1) (newHead::tail) indexInput startHead newDeltaLengthHeadInput newOffsetOutput diffTail |> Results.andThen (\(newList, newDiffTail) ->
                    let finalDiffs = 
                      case newDiffTail of
                        ((i, ListElemUpdate (VStringDiffs l))::tail) ->
                          if i == indexInput then
                            (i, ListElemUpdate (VStringDiffs (thisDiff::l)))::tail
                          else
                             (indexInput, ListElemUpdate (VStringDiffs [thisDiff]))::newDiffTail
                        _ -> (indexInput, ListElemUpdate (VStringDiffs [thisDiff]))::newDiffTail
                    in
                    ok1 (newList, finalDiffs)
                  )
        in
        case gather -1 input 0 0 0 0 l of
          Ok x -> Ok (InputsWithDiffs (x |> List.map (\(value, diff) -> (value, Just (VListDiffs diff)))))
          other -> other
      _ -> error ("expected VStringDiffs, got " + toString diffs)
}.apply list

Html.h3 [] [] <| joinEmpty ["Hello", " ", "world!"]
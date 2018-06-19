{resultValuesWithDiffs, splitListDiffsAt, pairDiff4} = Update

-- Reversible foldl
-- f takes a list containing one element, the accumulator and the element itself.
-- On update, f pushes back to its first argument whatever it wants to do with it (e.g. insert new elements, delete, ...)
foldl f b l = 
  letrec aux b l = case l of
    [] -> b
    _ ->
      let (h1, t1) = List.split 1 l in
      aux (f h1 b) t1
  in aux b l

concatMap insert_in_empty f l =
  case l of -- Insertion to an emptylist.
    [] -> { apply l = freeze []
            update {input=l, outputNew=lp} = {values = [insert_in_empty lp]}
          }.apply l
    _ ->
  applyLens { -- Back-propagating a ghost boolean indicating if the next element in the computation had its first element deleted.
    apply (x, y) = freeze x
    update {output, diffs} = {
      values = [(output, True)],
      diffs=[Just (VRecordDiffs { _1 = diffs, _2 = VConstDiffs})]
  } } <| foldl (\headList (oldAcc, dummyBool) ->
    let lengthAcc = List.length oldAcc in
    {apply (f, oldAcc, headList, dummyBool) = freeze (oldAcc ++ f headList, dummyBool)
     update {input=(f, oldAcc, headList, _) as input, outputNew=(newAccFHeadList, prevDeletedOrLast), diffs=ds} =
       let handleDiffs = case ds of
          VRecordDiffs {_1=VListDiffs diffs} -> \continuation -> continuation diffs
          _ -> \continuation -> {values = [input], diffs=[Nothing]}
       in handleDiffs <| \diffs ->
       splitListDiffsAt lengthAcc 0 newAccFHeadList diffs
       |> List.concatMap (\(newAcc, newAccDiffs, newFHeadList, newFHeadListDiffs) ->
         let finalAccDiffs = if newAccDiffs == [] then Nothing else Just (VListDiffs newAccDiffs) in
         let finalAccDeletedRight =
           letrec aux diffs = case diffs of
             [] -> False
             (i, d)::tail -> case d of
               ListElemDelete count -> if count + i == lengthAcc then True else
                 aux tail
               _ -> aux tail
           in aux newAccDiffs
         in
         let firstElementDeleted = case newFHeadListDiffs of
           (0, ListElemDelete x) :: tail -> True
           _ -> False
         in
         let deleteBoolUpdate = if firstElementDeleted then Just VConstDiffs else Nothing in
         let surroundingElementsDeleted = finalAccDeletedRight && prevDeletedOrLast in
         let headListDeletable = newFHeadList == []  && (firstElementDeleted || surroundingElementsDeleted) in
         let atLeastOneSurroundingElementDeleted = finalAccDeletedRight || prevDeletedOrLast in
         (if atLeastOneSurroundingElementDeleted && headListDeletable then [] else
         case newFHeadListDiffs of
           [] -> [Ok ((f, newAcc, headList, firstElementDeleted),
                      Update.pairDiff4 Nothing finalAccDiffs Nothing deleteBoolUpdate)]
           _ ->
         case Update.updateApp{fun (f, x) = f x, input=(f, headList), output=newFHeadList, diffs=VListDiffs newFHeadListDiffs} of
           {values = fAndNewHeadLists, diffs = diffsList} ->
              LensLess.List.map2 (\(newF, newHeadList) diff ->
                case diff of
                  Nothing ->  Ok ((f, newAcc, headList, firstElementDeleted),
                    Update.pairDiff4 Nothing finalAccDiffs Nothing deleteBoolUpdate)
                  Just (VRecordDiffs d) -> 
                    let newFDiff = case d of {_1} -> Just _1; _ -> Nothing in
                    let newHeadListDiffs = case d of {_2} -> Just _2; _ -> Nothing in
                    Ok ((newF, newAcc, newHeadList, firstElementDeleted), Update.pairDiff4 newFDiff finalAccDiffs newHeadListDiffs deleteBoolUpdate)
              ) fAndNewHeadLists diffsList
           {error = msg} -> [Err msg]
         ) ++ (
           if headListDeletable then
             [Ok ((f, newAcc, [], True),
                  Update.pairDiff4 Nothing finalAccDiffs (Just (VListDiffs [(0, ListElemDelete 1)])) (Just VConstDiffs))]
           else
             [])
        )
        |> resultValuesWithDiffs
    }.apply (f, oldAcc, headList, dummyBool)) (Update.freeze [], Update.softFreeze False) l

-- We can do insertions of numbers, but not delete empty elements for now.


<span>@(toString (concatMap (\x -> [x]) (\[head] as headList -> case head of
  [] -> head
  _ :: _ -> head
  _ -> headList) (Update.debug "list" [[1, 2], [], 3, [4, 5]])))</span>
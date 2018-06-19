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

{valuesWithDiffs, pairDiff3, splitStringDiffsAt} = Update

-- An example of using reversible foldl to join strings without separators
-- Here no insertion of element is possible, but we can remove elements.
-- We use a trick to propagate a value that is never computed, in order to know if, during update,
-- the last string had its first char deleted
join x = case x of
    [] -> {apply x = freeze "", update {outputNew} = {values=[[outputNew]]}}.apply x
    _ ->
  applyLens {
    apply (x, y) = freeze x
    update {output, diffs} = {
      values = [(output, True)],
      diffs=[Just (VRecordDiffs { _1 = diffs, _2 = VConstDiffs})]
    } } <| foldl (\oldHeadList (oldAcc, dummyBool) -> 
  { apply (oldAcc, [head], dummyBool) = freeze (oldAcc + head, dummyBool)
    update {input=(oldAcc, [head], _) as input,outputNew=(newAcc, prevDeletedOrLast),diffs=ds} =
      let handleDiffs = case ds of
        VRecordDiffs {_1=VStringDiffs diffs} -> \continuation -> continuation diffs
        _ -> \continuation -> {values = [input], diffs=[Nothing]}
      in handleDiffs <| \diffs ->
      splitStringDiffsAt (String.length oldAcc) 0 (oldAcc + head) newAcc diffs
      |> List.concatMap (\(leftValue, leftDiffs, rightValue, rightDiffs) ->
        let lastCharLeftDeleted =
          letrec aux leftDiffs = case leftDiffs of
            [StringUpdate _ end 0] -> end == String.length oldAcc
            head::tail -> aux tail
            _ -> leftValue == []
          in aux leftDiffs
        in
        let firstCharDeleted = case rightDiffs of (StringUpdate 0 i 0) :: tail -> i > 0; _ -> False in
        let surroundingElementsDeleted = lastCharLeftDeleted && prevDeletedOrLast in
        let atLeastOneSurroundingElementDeleted = lastCharLeftDeleted || prevDeletedOrLast in
        let elemDeleted = rightValue == "" && (firstCharDeleted || surroundingElementsDeleted) in
        (if atLeastOneSurroundingElementDeleted && elemDeleted then [] else
          [((leftValue, [rightValue], firstCharDeleted),
            pairDiff3
              (if leftDiffs == [] then Nothing else Just (VStringDiffs leftDiffs))
              (if rightDiffs == [] then Nothing else 
               Just (VListDiffs [(0, ListElemUpdate (VStringDiffs rightDiffs))]))
              (if firstCharDeleted then Just VConstDiffs else Nothing)
          )]) ++
        (if elemDeleted then -- The string was deleted, one solution is to remove it.
          [((leftValue, [], True),
            pairDiff3
              (if leftDiffs == [] then Nothing else Just (VStringDiffs leftDiffs))
              (Just (VListDiffs [(0, ListElemDelete 1)]))
              (Just VConstDiffs)
              )]
        else [])
      )
      |> valuesWithDiffs
  }.apply (oldAcc, oldHeadList, dummyBool)
  ) (freeze "", Update.softFreeze False) x

big = "big"
world = " world"
list = ["Hello", "2", "", big, world, "!"]

highlightcolor = "red"
highlight x = <span style="""color:@highlightcolor""">@x</span>

<span><code style="font-size:1.5em">join @(toString list) =<br>@highlight(toString<|join list)</code><br>
Try the following on @highlight("""the result above in @highlightcolor""") to see of join is cleverly thought::
<ul>
<li>Insert 1 to the left of 2 - it first snaps with numbers</li>
<li>Insert 1 to the right of 2 - if first snaps with empty string, then number</li>
<li>Insert 'a' without quotes to the right of 2</li>
<li>Delete 'big'</li>
<li>Delete 'big w'</li>
<li>Delete ' world'</li>
<li>Delete 'g world'</li>
<li>Replace 'Hello2' by 'Hi'</li>
</ul>
</span>
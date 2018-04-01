mapListLens =
  { apply (f,xs) =
      Update.freeze (List.simpleMap f xs)

  , update { input = (f, oldInputList)
           , outputOld = oldOutputList
           , outputNew = newOutputList } =
      letrec walk diffOps maybePreviousInput oldInputs acc =

        case (diffOps, oldInputs) of
          ([], []) ->
            acc

          (KeepValue :: moreDiffOps, oldHead :: oldTail) ->
            let newTails = walk moreDiffOps (Just oldHead) oldTail acc in
            List.simpleMap (\newTail -> oldHead::newTail) newTails

          (DeleteValue :: moreDiffOps, oldHead :: oldTail) ->
            let newTails = walk moreDiffOps (Just oldHead) oldTail acc in
            newTails

          ((UpdateValue newVal) :: moreDiffOps, oldHead :: oldTail) ->
            let newTails = walk moreDiffOps (Just oldHead) oldTail acc in
            let newHeads = Update.updateApp {fun = f, input = oldHead, output = newVal} in
            List.cartesianProductWith List.cons newHeads.values newTails

          ((InsertValue newVal) :: moreDiffOps, _) ->
            let headOrPreviousHead =
              case (oldInputs, maybePreviousInput) of
                (oldHead :: _, _) -> oldHead
                ([], Just previousOldHead) -> previousOldHead
            in
            let newTails = walk moreDiffOps maybePreviousInput oldInputs acc in
            let newHeads = Update.updateApp {fun = f, input = headOrPreviousHead, output = newVal} in
            List.cartesianProductWith List.cons newHeads.values newTails
      in
      let newInputLists =
        walk (Update.listDiff oldOutputList newOutputList) Nothing oldInputList [[]]
      in
      let newFuncAndInputLists =
        List.simpleMap (\newInputList -> (f, newInputList)) newInputLists
      in
      { values = newFuncAndInputLists }
  }

mapList f xs =
  Update.applyLens mapListLens (f, xs)

-----------------------------------------------

transformedValues =
  mapList
    (\n -> n + 1)
    [0,1,2,3]

main =
  Html.p [] [] (toString transformedValues)

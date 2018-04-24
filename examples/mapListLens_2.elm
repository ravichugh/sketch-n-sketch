listMapLens =
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
            let tails = walk moreDiffOps (Just oldHead) oldTail acc in
            List.simpleMap (\newTail -> (f, oldHead) :: newTail) tails

          (DeleteValue :: moreDiffOps, oldHead :: oldTail) ->
            let tails = walk moreDiffOps (Just oldHead) oldTail acc in
            tails

          ((UpdateValue newVal) :: moreDiffOps, oldHead :: oldTail) ->
            let tails = walk moreDiffOps (Just oldHead) oldTail acc in
            let heads =
              (Update.updateApp {fun (a,b) = a b, input = (f, oldHead), output = newVal}).values
            in
            List.cartesianProductWith List.cons heads tails

          ((InsertValue newVal) :: moreDiffOps, _) ->
            let headOrPreviousHead =
              case (oldInputs, maybePreviousInput) of
                (oldHead :: _, _) -> oldHead
                ([], Just oldPreviousHead) -> oldPreviousHead
            in
            let tails = walk moreDiffOps maybePreviousInput oldInputs acc in
            let heads =
              (Update.updateApp {fun (a,b) = a b, input = (f, headOrPreviousHead), output = newVal}).values
            in
            List.cartesianProductWith List.cons heads tails
      in
      let newLists =
        walk (Update.listDiff oldOutputList newOutputList) Nothing oldInputList [[]]
      in
      let newFuncAndInputLists =
        List.simpleMap (\newList ->
          let (newFuncs, newInputList) = List.unzip newList in
          let newFunc = Update.merge f newFuncs in
          (newFunc, newInputList)
        ) newLists
      in
      { values = newFuncAndInputLists }
  }

listMap f xs =
  Update.applyLens listMapLens (f, xs)

-----------------------------------------------

transformedValues =
  listMap
    (\n -> n + 1)
    [0,1,2,3]

main =
  Html.p [] [] (toString transformedValues)

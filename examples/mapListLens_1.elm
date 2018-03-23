mapListSimple f list =
  Update.freeze (case list of [] -> []; x::xs -> f x :: mapListSimple f xs)

mapListLens =
  { apply [f,xs] =
      Update.freeze (mapListSimple f xs)

  , update { input = [f,oldInputList]
           , outputOld = oldOutputList
           , outputNew = newOutputList } =

      letrec walk diffOps maybePreviousInput oldInputs acc =

        case [diffOps, oldInputs] of
          [[], []] ->
            acc

          [["Keep"] :: moreDiffOps, oldHead :: oldTail] ->
            let newTails = walk moreDiffOps (Just oldHead) oldTail acc in
            map (\newTail -> oldHead::newTail) newTails

          [["Delete"] :: moreDiffOps, oldHead :: oldTail] ->
            let newTails = walk moreDiffOps (Just oldHead) oldTail acc in
            newTails

          [["Update", newVal] :: moreDiffOps, oldHead :: oldTail] ->
            let newTails = walk moreDiffOps (Just oldHead) oldTail acc in
            let newHeads = updateApp {fun = f, input = oldHead, output = newVal} in
            List.cartesianProductWith List.cons newHeads.values newTails

          [["Insert", newVal] :: moreDiffOps, _] ->
            let headOrPreviousHead =
              case [maybePreviousInput, oldInputs] of
                [_                        , oldHead :: _] -> oldHead
                [["Just", oldPreviousHead], []          ] -> oldPreviousHead
            in
            let newTails = walk moreDiffOps maybePreviousInput oldInputs acc in
            let newHeads = updateApp {fun = f, input = headOrPreviousHead, output = newVal} in
            List.cartesianProductWith List.cons newHeads.values newTails
      in
      let newInputLists =
        walk (listDiff diff oldOutputList newOutputList) Nothing oldInputList [[]]
      in
      { values = mapListSimple (\newInputList -> [f, newInputList]) newInputLists }
  }

mapList f xs =
  applyLens mapListLens [f, xs]

-----------------------------------------------
-- TODO not using this example yet

states =
  [ ["Alabama", "AL", "Montgomery"]
  , ["Alaska", "AK", "Juneau"]
  , ["Arizona", "AZ", "Phoenix"]
  , ["Arkansas", "AR", "Little Rock"]
  , ["California", "CA", "Sacramento"]
  , ["Colorado", "CO", "Denver"]
  , ["Connecticut", "CT", "Hartford"]
  ]

displayState =
  (\[a,b,c] -> [a, c + ", " + b])

transformedStates =
  mapList displayState states
-------

transformedValues =
  mapList
    (\n -> n + 1)
    [0,1,2,3]

main =
  Html.p [] [] (toString transformedValues)

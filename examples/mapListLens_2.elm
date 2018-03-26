mapListSimple f list =
  Update.freeze (case list of [] -> []; x::xs -> f x :: mapListSimple f xs)

mapListLens =
  { apply [f,xs] =
      mapListSimple f xs

  , update { input = [f,oldInputList]
           , outputOld = oldOutputList
           , outputNew = newOutputList } =

      -- TODO this will happen automatically
      let Update = __extendUpdateModule__ {updateApp=updateApp, diff=diff, merge=merge} in

      letrec walk diffOps maybePreviousInput oldInputs acc =

        case [diffOps, oldInputs] of
          [[], []] ->
            acc

          [["Keep"] :: moreDiffOps, oldHead :: oldTail] ->
            let [newFuncs, newTails] = walk moreDiffOps (Just oldHead) oldTail acc in
            [ newFuncs, map (\newTail -> oldHead::newTail) newTails ]

          [["Delete"] :: moreDiffOps, oldHead :: oldTail] ->
            let [newFuncs, newTails] = walk moreDiffOps (Just oldHead) oldTail acc in
            [ newFuncs, newTails ]

          [["Update", newVal] :: moreDiffOps, oldHead :: oldTail] ->
            let [newFuncs1, newTails] = walk moreDiffOps (Just oldHead) oldTail acc in
            let [newFuncs2, newHeads] =
              List.unzip
                (Update.updateApp {fun [a,b] = a b, input = [f, oldHead], output = newVal}).values
            in
            [ newFuncs1 ++ newFuncs2
            , List.cartesianProductWith List.cons newHeads newTails
            ]

          [["Insert", newVal] :: moreDiffOps, _] ->
            let headOrPreviousHead =
              case [oldInputs, maybePreviousInput] of
                [oldHead :: _, _] -> oldHead
                [[], ["Just", oldPreviousHead]] -> oldPreviousHead
            in
            let [newFuncs1, newTails] = walk moreDiffOps maybePreviousInput oldInputs acc in
            let [newFuncs2, newHeads] =
              List.unzip
                (Update.updateApp {fun [a,b] = a b, input = [f, headOrPreviousHead], output = newVal}).values
            in
            [ newFuncs1 ++ newFuncs2
            , List.cartesianProductWith List.cons newHeads newTails
            ]
      in
      let [newFuncs, newInputLists] =
        walk (Update.listDiff oldOutputList newOutputList) Nothing oldInputList [[],[]]
      in
      -- TODO merge newFuncs
      { values = mapListSimple (\newInputList -> [f, newInputList]) newInputLists }
  }

mapList f xs =
  Update.applyLens mapListLens [f, xs]

-----------------------------------------------

transformedValues =
  mapList
    (\n -> n + 1)
    [0,1,2,3]

main =
  Html.p [] [] (toString transformedValues)

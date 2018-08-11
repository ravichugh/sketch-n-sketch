-- prepend lSmall with its own element until it reaches the size of lBig
copyLengthFrom lBig lSmall =
  let aux acc lb ls = case [lb, ls] of
    [[], ls] -> acc
    [head::tail, []] -> aux acc lb lSmall
    [head::tail, headS::tailS] -> aux (headS::acc) tail tailS
  in aux [] lBig lSmall

splitByLength listLength list =
  let aux length lPrev l = case length of
    [] -> [lPrev, l]
    head::tail -> case l of
      lHead::lTail -> aux tail (append lPrev [lHead]) lTail
      [] -> []
  in aux listLength [] list

map f l = {
  apply [f, l] =
    let aux = case of
      [] -> []
      head::tail -> f head :: aux tail
    in aux l
  update {input = [f, input], output, outputOriginal} =
    let aux newFuns newInputs inputElements thediff = case thediff of
      [] -> [newFuns, newInputs]
      {same,elements}::tailDiff ->
        let [inputsElementsKept, inputElementsTail] = splitByLength elements inputElements in
        aux newFuns (append newInputs inputsElementsKept) inputElementsTail tailDiff
      {removed,elements=deleted}::{added,elements=inserted}::tailDiff ->
        let [inputsRemoved, remainingInputs] = splitByLength deleted inputElements in
        let inputsAligned = copyLengthFrom inserted inputsRemoved in
        -- inputsAligned has now the same size as added.
        letrec recoverInputs newFs newIns oldIns newOuts = case [oldIns, newOuts] of
          [[], []] -> [newFs, newIns]
          [inHd::inTail, outHd::outTail] ->
            case Update.updateApp (\[f, x] -> f x) [f, inHd] (f inHd) outHd of
              [newF, newIn]::_ -> recoverInputs (append newFs [newF]) (append newIns [newIn]) inTail outTail
              _ -> "Error: no solution to update problem." + 1
          [inList, outList] -> ("Internal error: lists do not have the same type" + toString inList + ", " + toString outList) + 1
        in
        let [newFs, inputsRecovered] = recoverInputs [] [] inputsAligned inserted in
        aux (append newFuns newFs) (append newInputs (inputsRecovered)) remainingInputs tailDiff
      {removed,elements=deleted}::tailDiff ->
        let [_, remainingInputs] = splitByLength deleted inputElements in
        aux newFuns newInputs remainingInputs tailDiff
      {added,elements=inserted}::tailDiff ->
        let oneInput = case inputElements of
          head::tail -> head
          _ -> case newInputs of
            head::tail -> head
            _ -> "Error: Cannot update a call to a map if there is no input" + 1
        in
        letrec recoverInputs newFs newIns newOuts = case newOuts of
          [] -> [newFs, newIns]
          outHd::outTail ->
            case Update.updateApp (\[f, x] -> f x) [f, oneInput] (f oneInput) outHd of
              [newF, newIn]::_ -> recoverInputs (append newFs [newF]) (append newIns [newIn]) outTail
              _ -> "Error: no solution to update problem." + 1
        in
        let [newFs, inputsRecovered] = recoverInputs [] [] inserted in
        aux (append newFuns newFs) (append newInputs inputsRecovered) inputElements tailDiff
    in
    let [newFun::_, newInputs]  = aux [] [] input (diff outputOriginal output) in
    [[newFun, newInputs]]
  }.apply [f, l]

data = [
  {name = "John", age = 31 },
  {name = "Mary", age = 30 },
  {name = "Lully", age = 5 }
]

["ul", [], map (\{name, age} -> ["li", [], [["TEXT", name + " is " + toString age + " years old"]]]) data ]
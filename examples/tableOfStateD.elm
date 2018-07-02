-- prepend lSmall with its own element until it reaches the size of lBig
map f l = 
  { apply [f, l] =
      letrec aux = case of
        [] -> []
        head::tail -> f head :: aux tail
      in aux l
    update {input = [f, input], output, outputOriginal} =
      let copyLengthFrom lBig lSmall =
        letrec aux acc lb ls = case [lb, ls] of
          [[], ls] -> acc
          [head::tail, []] -> aux acc lb lSmall
          [head::tail, headS::tailS] -> aux (headS::acc) tail tailS
        in aux [] lBig lSmall
      in
      let splitByLength listLength list =
        letrec aux length lPrev l = case length of
          [] -> [lPrev, l]
          head::tail -> case l of
            lHead::lTail -> aux tail (append lPrev [lHead]) lTail
            [] -> []
        in aux listLength [] list
      in
      letrec aux newFuns newInputs inputElements thediff = case thediff of
        [] -> [newFuns, newInputs]
        {kept}::tailDiff ->
          let [inputsElementsKept, inputElementsTail] = splitByLength kept inputElements in
          aux newFuns (append newInputs inputsElementsKept) inputElementsTail tailDiff
        {deleted}::{inserted}::tailDiff ->
          let [inputsRemoved, remainingInputs] = splitByLength deleted inputElements in
          let inputsAligned = copyLengthFrom inserted inputsRemoved in
          -- inputsAligned has now the same size as inserted.
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
        {deleted}::tailDiff ->
          let [_, remainingInputs] = splitByLength deleted inputElements in
          aux newFuns newInputs remainingInputs tailDiff
        {inserted}::tailDiff ->
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
      let [funs, newInputs]  = aux [] [] input (diff outputOriginal output) in
      let newFun = merge f funs in
      [[newFun, newInputs]]
  }.apply [f, l]
-- move to lens library

zipWithIndex xs =
  { apply x = zip (range 0 (len xs - 1)) xs
    update {output} = [map (\(i, x) -> x) output]  }.apply xs

mapi f xs = map f (zipWithIndex xs)
 
indexedMap f xs = mapi (\(i,x) -> f i x) xs


customUpdate record x =
  record.apply x

customUpdateFreeze =
  customUpdate { apply x = x, update p = [p.input] }

-- move to table library

addRowFlags =
  { apply rows =
      map (\row -> [freeze False, row]) rows
  , unapply rows =
      (\x -> Just x) <|
        concatMap (\[flag,row] ->
          if flag == True
            then [ row, ["","",""] ]
            else [ row ]
        ) rows
  }

customUpdateTable =
  customUpdate addRowFlags

trWithButton showButton flag styles attrs children =
  if showButton == False then
    tr styles attrs children

  else
    let [hasBeenClicked, nope, yep] =
      ["has-been-clicked", customUpdateFreeze "gray", customUpdateFreeze "coral"]
    in
    let onclick =
      """
      var hasBeenClicked = document.createAttribute("@hasBeenClicked");
      var buttonStyle = document.createAttribute("style");
      
      if (this.parentNode.getAttribute("@hasBeenClicked") == "False") {
        hasBeenClicked.value = "True";
        buttonStyle.value = "color: @yep;";
      } else {
        hasBeenClicked.value = "False";
        buttonStyle.value = "color: @nope;";
      }
      
      this.parentNode.setAttributeNode(hasBeenClicked);
      this.setAttributeNode(buttonStyle);
      """
    in
    let button = -- text-button.enabled is an SnS class
      [ "span"
      , [ ["class", "text-button.enabled"]
        , ["onclick", onclick]
        , ["style", [["color", nope]]]
        ]
      , [textNode "+"]
      ]
    in
    tr styles
      ([hasBeenClicked, toString flag] :: attrs)
      (snoc button children)

------------------------------------------------

-- State, Abbreviation, Capital
states = [
  ["Alabama", "AL", "Montgomery"],
  ["Alaska", "AK", "Juneau"],
  ["Arizona", "AZ", "Phoenix"],
  ["Arkansas", "AR", "Little Rock"],
  ["California", "CA", "Sacramento"],
  ["Colorado", "CO", "Denver"],
  ["Connecticut", "CT", "Hartford"]
]

headers =
  ["State", "", "Capital"]

rows =
  --  |> customUpdateTable
  states

padding =
  ["padding", "3px"]

theTable =
  let headerRow =
    let styles = [padding, ["text-align", "left"], ["background-color", "coral"]] in
    tr [] [] (map (th styles []) headers)
  in
  let stateRows =
    let colors = ["lightyellow", "white"] in
    indexedMap (\i row ->
      let color =
        nth colors (mod i (len colors))
      in
      let columns =
        map (td [padding, ["background-color", color]] []) row
      in
--      trWithButton True flag [] [] columns
      ["tr", [["onclick", "this.parentElement.insertBefore(this.cloneNode(true), this);"]], columns]
    ) rows
  in
  table
    [padding, ["border", "8px solid lightgray"]]
    []
    (headerRow :: stateRows)

main =
  theTable
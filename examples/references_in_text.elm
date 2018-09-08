-- TODO: This file is obsolete
compareStr a b = if a == b then 0 else if a < b then -1 else 1

quicksort compare list = case list of
    [] -> []
    pivot::t -> let tBefore = List.filter (\e -> compare e pivot < 0) t in
      let tAfter = List.filter (\e -> compare pivot e < 0) t in
      quicksort compare tBefore ++ [pivot] ++ quicksort compare tAfter

addReferences references node =
  let collectedAddedReferences = Update.lens2
    { apply (references, node) = node
      update {input = (references, node) as input, outputNew} =
        let refAddRegex = """\[\+\s*((?:(?!\]).)*)\]""" in
        let addedReferences = Html.find refAddRegex outputNew in
        if addedReferences == [] then Ok (InputsWithDiffs [(input, Nothing)])
        else 
          let (newNode, (_, newReferences)) = Html.foldAndReplace refAddRegex (\{submatches=[name]} (newRefNum, newReferences) ->
              ([["TEXT", """[@newRefNum]"""]], (newRefNum + 1, newReferences ++ [name]))
            ) (List.length references + 1, references) outputNew
          in 
          let newInput = (newReferences, newNode) in
          Ok (Inputs [newInput])
    }
  in
  let refRegex = """\[(\d+)\]""" in
  let -- returns a list of sorted references according to some criterion and an updated node.
    sortReferences references node = 
      let (newPermutation, newReferences) = List.zipWithIndex references
      |> quicksort (\(i, ref1) (i2, ref2) -> 
         compareStr ref1 ref2)
      |> List.unzip
      in
      if newReferences == references then (references, node)
      else
        let d = List.map2 (,) newPermutation (List.range 0 (List.length newPermutation - 1))
          |> Dict.fromList in
        let newNode = Html.replace refRegex (\{submatches=[ref],match} ->
          let nref = String.toInt ref in
          let _ = Debug.log "permutation:" d in
          case Dict.get (nref - 1) d of
            Just nnref  ->
              [["TEXT", """[@(nnref + 1)]"""]]
            Nothing ->
              [["TEXT", match]]
          ) node
        in
        (newReferences, newNode)
  in
  let  referencesDict = Dict.fromList (
    List.range 1 (List.length references) |> List.map (toString)
    |> map2 (flip (,)) references)
  in
  let lenReferences = List.length references in
  let (sortedReferences, sortedNode, applysort) = {
    apply (references, node) = (references, node, "")
    update {outputNew = (newReferences, newNode, newApplySort), diffs} =
      if newApplySort /= "" then
        Ok (Inputs [sortReferences newReferences newNode])
      else
        Ok (InputsWithDiffs [((newReferences, newNode), Just diffs)])
    }.apply (references, node)
  in
  let finalReferences = Update.lens2 {
    apply (references, node) = references
    update {input=(references, node), outputNew=newReferences, diffs=(VListDiffs diffs) as listDiffs} =
      let aux j offset currentNode nodeHasChanged diffs = case diffs of
        [] -> if nodeHasChanged then
            case __diff__ node currentNode of
              Err msg -> Err msg
              Ok x ->
                let finalDiffs =  case x of
                  Nothing -> {_1 = listDiffs}
                  Just x ->  {_1 = listDiffs, _2 = x}
                in
                Ok (InputsWithDiffs [((newReferences, currentNode), Just (VRecordDiffs finalDiffs))])
          else
            Ok (InputsWithDiffs [((newReferences, currentNode), Just (VRecordDiffs {_1 = listDiffs}))])
        d::diffsTail ->
          case d of
          ListElemSkip count -> aux (j + count) offset currentNode nodeHasChanged diffsTail
          ListElemUpdate _ -> aux (j + 1) offset currentNode nodeHasChanged diffsTail
          ListElemInsert count ->
            let newNode = Html.replace refRegex (\{submatches=[ref],match} ->
              let nref = String.toInt ref in
              if nref >= j + 1 + offset && nref <= lenReferences then [["TEXT", """[@(nref + count)]"""]]
              else [["TEXT", match]]
              ) currentNode
            in
            aux j (offset + count) newNode True diffsTail
          ListElemDelete count ->
            let newNode = Html.replace refRegex (\{submatches=[ref],match} ->
              let nref = String.toInt ref in
              if nref >= j + 1 + offset + count then [["TEXT", """[@(nref - count)]"""]]
              else if nref >= j + 1 + offset && nref <= lenReferences then
                [["TEXT", """[0] (deleted ref to '@(Dict.get ref referencesDict |> Maybe.withDefault "?"))')"""]]
              else [["TEXT", match]]
              ) currentNode
            in
            aux (j + count) (offset - count) newNode True diffsTail
      in
      aux 0 node False diffs |> Debug.log "aux"
  } sortedReferences sortedNode
  in
  [Html.replace refRegex  (\{submatches=[ref],match} ->
    Dict.get ref referencesDict |> 
    Maybe.map (\name -> Update.sizeFreeze [<abbr title=name>@match</abbr>]) |>
    Maybe.withDefaultLazy (\_ -> Update.sizeFreeze
      [<abbr style="color:red" title="Unknown reference">@match</abbr>]))
    (collectedAddedReferences references node)
  , Update.expressionFreeze <ul
  contenteditable="true">@(List.indexedMap (\i x -> <li>@(Html.text """[@(i + 1)] @x""")</li>) finalReferences)</ul>
  , Html.button "Sort references" "Sort the list of references alphabetically" applysort (\_ -> "#")
  ]

  
-- We should not remove references or insert them in this list,
-- instead, we should do these operations from the output to
-- keep in sync with the text
references = [
 "Mayer",
 "Work that is not that relevant really",
 "Chugh et al. 2016"]

example =  addReferences references <span>
  <h2>Related work</h2>
 The work in [1] both suggested that it is possible to modify
 an interface without modifying the code. However, it also
 pin-points that neglecting raw access to the code is a key
 impedance to adoption. The work of [2] could also be relevant.
 In [3], the authors demonstrate that dual editing of both code
 and view increases the productivity. [4] applies a more
 general-purpose approach to edit programmatically generated
 documents such as this one.
</span>


















 
Html.forceRefresh  <div id="content" style="margin:20px;cursor:text">
<style>#example { font-family: 'Computer Modern Serif'; }</style>
<span>
<h1>HTML Regex replacement</h1>
<p>
  The function <code>addReferences</code> adds to all occurrences of [number]
  a tooltip indicating the reference name, and the list of references at the end.
  It can also propagate changes in the reference list to the text.</p>
<p>
  <b>Modify references:</b>
  You can change any reference title by simply editing it,
  either in the UL, or even in the title of a citation.
  For example, change the first "Mayer" to "Game Programming
  by Demonstration, Mayer et Kuncak, 2013".
</p>
<p>
  <b>Adding or inserting references:</b>
  Position the cursor at the end of an element in the list,
  press ENTER and enter '[1] The reference name',
  and <code>addReferences</code> takes care of shifting indices.
  Try to insert "[4] Mayer, Chugh and Kuncak 2016".
</p>
<p>
  <b>Deleting references:</b>
  If you delete a reference, <code>addReferences</code> takes
  care of transforming any citation to [0] and inlines the name.
  Try to delete '[2] Work that is not that relevant really'
  below.
</p>
<p>
  <b>Insert, delete and modify citations</b>
  You can delete any citation that looks like [1], insert
  new ones by typing [2] for example, or change a citation by
  changing its number.
</p>
<p>
  <b>Sort references</b>
  Click on the "Sort references" button to sort the references.
  The references are also sorted in the text.
</p>
</span>
<div style="border:4px solid black;padding:20px" id="example">
@example
</div>
</div>
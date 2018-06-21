addReferences references node =
  let refRegex = """\[(\d+)\]""" in
  let  referencesDict = Dict.fromList (
    List.range 1 (List.length references) |> List.map (toString)
    |> map2 (flip (,)) references)
  in
  let lenReferences = List.length references in
  let finalReferences = {
    apply (references, node) = freeze references
    update {input=(references, node), outputNew=newReferences, diffs=(VListDiffs diffs) as listDiffs} =
      letrec aux offset currentNode nodeHasChanged diffs = case diffs of
        [] -> if nodeHasChanged then
            case __diff__ node currentNode of
              Err msg -> {error = msg}
              Ok x ->
                let finalDiffs =  case x of
                  Nothing -> {_1 = listDiffs}
                  Just x ->  {_1 = listDiffs, _2 = x}
                in
                {values=[(newReferences, currentNode)], diffs=[Just (VRecordDiffs finalDiffs)]}
          else
            {values=[(newReferences, currentNode)], diffs=[Just (VRecordDiffs {_1 = listDiffs})]}
        (j, d)::diffsTail ->
          case d of
          ListElemUpdate _ -> aux offset currentNode nodeHasChanged diffsTail
          ListElemInsert count ->
            let newNode = Html.replace refRegex (\{submatches=[ref],match} ->
              let nref = String.toInt ref in
              if nref >= j + 1 + offset && nref <= lenReferences then [["TEXT", """[@(nref + count)]"""]]
              else [["TEXT", match]]
              ) currentNode
            in
            aux (offset + count) newNode True diffsTail
          ListElemDelete count ->
            let newNode = Html.replace refRegex (\{submatches=[ref],match} ->
              let nref = String.toInt ref in
              if nref >= j + 1 + offset + count then [["TEXT", """[@(nref - count)]"""]]
              else if nref >= j + 1 + offset && nref <= lenReferences then
                [["TEXT", """[0] (deleted ref to '@(Dict.get ref referencesDict |> Maybe.withDefault "?"))')"""]]
              else [["TEXT", match]]
              ) currentNode
            in
            aux (offset - count) newNode True diffsTail
      in
      aux 0 node False diffs |> Debug.log "aux"
  }.apply (references, node)
  in
  [Html.replace refRegex  (\{submatches=[ref],match} ->
    Dict.get ref referencesDict |> 
    Maybe.map (\name -> Update.sizeFreeze [<abbr title=name>@match</abbr>]) |>
    Maybe.withDefaultLazy (\_ -> Update.sizeFreeze
      [<abbr style="color:red" title="Unknown reference">@match</abbr>])) node
  , Update.expressionFreeze <ul
  contenteditable="true">@(List.indexedMap (\i x -> <li>@(Html.text """[@(i + 1)] @x""")</li>) finalReferences)</ul>
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
</span>
<div style="border:4px solid black;padding:20px" id="example">
@example
</div>
</div>
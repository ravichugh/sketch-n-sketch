// output: html
// download: hu2004.html

let addrbook = { tag= "addrbook",
children = [
{ tag= "person",
  children = [
    {tag="name",  children = [{tag = "Masato Takeichi", children = []}]},
    {tag="email", children = [{tag = "takeichi@acm.org", children = []}]},
    {tag="tel",   children = [{tag = "+81-3-5841-7430", children = []}]}
  ]
},
{ tag= "person",
  children = [
    {tag="name",  children = [{tag = "Zhenjiang Hu", children = []}]},
    {tag="email", children = [{tag = "hu@mist.i.u-tokyo.ac.jp", children = []}]},
    {tag="email", children = [{tag = "hu@ipl.t.u-tokyo.ac.jp", children = []}]},
    {tag="tel",   children = [{tag = "+81-3-5841-7411", children = []}]}
  ]
},
{ tag= "person",
  children = [
    {tag="name",  children = [{tag = "Shin-Cheng Mu", children = []}]},
    {tag="email", children = [{tag = "scm@mist.i.u-tokyo.ac.jp", children = []}]},
    {tag="tel",   children = [{tag = "+81-3-5841-7411", children = []}]}
  ]
}]} in
// We have sort and map 

letrec take n array =
  if(n==array.length) array else (array match {
    case [] => array
    case init ++ [last] =>
      take n init
  }) in
letrec drop n array =
  if(n==0) array else (array match {
    case [] => array
    case [head] ++ tail =>
      let newN = n - 1 in
      drop newN tail
  }) in
  
let map = !(\f. {
  apply= \input.
  letrec map_aux = {
    apply = \l. l match {
      case [head] ++ tail => [f head] ++ (map_aux tail)
      case [] => []
    } /*,
    unapply = \lOut.(lOut match {
      case [] => {value = []}
      case [f head] ++ (map_aux tail) => {value = [head] ++ tail }
      case _ => {}
    }) */
  } in map_aux input,
  updateAlign = \m.
    let [input, index, deleted, inserted] = [m.input, m.insertionIndex, m.deletedCount, m.inserted] in
    let updateIndex = if(index == input.length) index - 1 else index in
    letrec aux acc inserted = inserted match {
      case [] =>
        { value = take index input ++ acc ++ drop (index+deleted) input }
      case [head] ++ tail =>
        (update f (input updateIndex) head) match {
          case {value=v} => aux (acc ++ [v]) tail
          case _ => {}
        }
    } in aux [] inserted
}) in

letrec insert toComparable prev sorted elem = sorted match {
  case [] => prev ++ [elem]
  case [head] ++ tail => if(toComparable head > toComparable elem) (prev ++ [elem, head] ++ tail) else (let newPrev = prev ++ [head] in insert toComparable newPrev tail elem)
} in

letrec sort = !(\toComparable. { apply=\list.
  letrec sort_aux acc toInsert = toInsert match {
    case [] => acc
    case [head] ++ tail => sort_aux (insert toComparable [] acc head) tail
  } in sort_aux [] list,
  updateAlign=\m. (
    let [list, origOut, out, index, deleted, inserted] = [m.input, m.outputOriginal, m.outputNew, m.insertionIndex, m.deletedCount, m.inserted] in
    let zipWithIndex list =
      letrec aux index acc list = list match {
        case [] => acc
        case [head] ++ tail => let newAcc = acc ++ [[head, index]] in
          let newIndex = index + 1 in
          aux newIndex newAcc tail
      } in aux 0 [] list in
    let listI = zipWithIndex list in
    let sortedListI = sort (\x. toComparable (x 0)) listI in
    // Now we can compare against outputOriginal and the insertions.
    if(inserted.length == 1 && deleted == 1) (
      // Replacement. We get the index thanks to the second element and we update it in the original list.
      let modifiedIndex = sortedListI index 1 in
      {value = (take modifiedIndex list) ++ inserted ++ (drop (modifiedIndex+1) list) }
    ) else if(deleted == 0) ( // just inserted. We insert the new values at the end
      {value = list ++ inserted}
    ) else if(inserted.length == 0) ( //Only deletion
      if(deleted == 1) (
        let modifiedIndex = sortedListI index 1 in
        {value = (take modifiedIndex list) ++ (drop (modifiedIndex+1) list) }
      ) else (
        {} // Delete more?!
      )
    ) else if(inserted.length == 1 && deleted == 2) ( // The first one is modified, the second is deleted
      let modifiedIndex1 = sortedListI index 1 in
      let deletedIndex2 = sortedListI (index+1) 1 in
      let res1 = take modifiedIndex1 list ++ inserted ++ drop (modifiedIndex1 +1) list in
      {value = take deletedIndex2 res1 ++
        drop (deletedIndex2+1) res1 }
    ) else {}
  )
}) in

letrec get prop elz item = item match {
  case {tag = _, children = c} => get prop elz c
  case [{tag=tag, children=[{tag=value}]}] ++ tail =>
    if(tag == prop) value else get prop elz tail
  case _ => elz
} in

let prettyPrint = { apply=\x.
letrec prettyPrinted indent tree = tree match {
  case {tag = tag, children = [{tag=value, children=[]}]} => indent + tag + ":" + value + "\n"
  case {tag = tag, children = children} => indent + tag + ":\n" + prettyPrinted indent children
  case [head] ++ tail => prettyPrinted (indent + "  ") head + prettyPrinted indent tail
  case [] => ""
} in 
[{tag="textarea", attributes={onchange="this.textContent = this.value"}, styles={width="50%",`min-height`="500px"},children=[{tag="#text", value=prettyPrinted "" x}]}],
  update=\p.
letrec parse_aux indent out =
  if(matchIn ("^" + indent + ".") out) (
    let tag = replaceFirstIn ("^" + indent + "(.*):[\\s\\S]*") out "$1" in
    if(matchIn ("^" + indent + "(.*):.") out) (
      let value = replaceFirstIn ("^" + indent + ".*:(.*)\n[\\s\\S]*") out "$1" in
      let children = [{tag = value, children = []}] in
      let remaining = replaceFirstIn ("^" + indent + ".*:.*\n?") out "" in
      [{tag = tag, children = children}] ++ parse_aux indent remaining
    ) else (
      let remaining = replaceFirstIn ("^" + indent + "(.*):\n?") out "" in
      parse_aux (indent + "  ") remaining match {
        case children ++ [rem] => [{tag=tag, children=children}] ++ parse_aux indent rem
      }
    )
  ) else (
    [out]
  )
in 
let out = p.outputNew in
out match {
  case init ++ [{tag="textarea", children=[{tag="#text", value=inner}]}] =>
    if(((p.outputOriginal (p.outputOriginal.length - 1)).children 0).value != inner) (
      parse_aux "" inner match {
        case [t, ""] => {value=t}
        case _ => {}
      }
    ) else {}
  case _ => {}
} } in


/* Compare the readability of the following code with the high-level language of [Hu 2004]:
sortX ˆ; Dup ˆ;
applyX [0] (modifyRootX Index ˆ; Map keepX) ˆ;
moveX [0] [0, 0] ˆ; hoistX Dup
*/

/* Citation from [Hu 2004]
Finally, it would be attractive to look into the possibility of making the
existing transformation languages like XSLT to be efficiently bidirectional, instead
of designing a new bidirectional language
*/

let transform addrbook =
  let toCompare = get "name" "" in
  let sortedChildren = sort toCompare addrbook.children in
  let renderIndexEntry person = 
    { tag="name",
      children = [
        { tag = get "name" "anonymous" person, children = []}
      ]
    } in 
  { tag="sortedaddrbook",
    children = [
      { tag="index",
        children =
          map renderIndexEntry sortedChildren}
    ] ++ sortedChildren
  } in

[{tag="h1", children="(2004) XML transformations"},
``In <a href="https://dl.acm.org/citation.cfm?id=1014025">[Hu 2004]</a>, they synchronized an address book and a modified version of it with a sorted index using the following program:
<pre>
sortX ˆ; Dup ˆ;
applyX [0] (modifyRootX Index ˆ; Map keepX) ˆ;
moveX [0] [0, 0] ˆ; hoistX Dup
</pre>
Unfortunately, you could not modify the "Index" tag from the output, and that you cannot add elements to the index list.
Using our approach, you can do that. The program is clearer:
<pre>
let sortedChildren = sort (get "name" "") addrbook.children in
let renderIndexEntry person = 
  { tag="name", children = [
    { tag = get "name" "anonymous" person, children = []}]} in 
{ tag="addrbook", children = [
    { tag="index", children =
        map renderIndexEntry sortedChildren}
 ] ++ sortedChildren }
</pre>
``,
"Click here to validate the changes below",{tag= "br"},
prettyPrint addrbook,
prettyPrint (transform addrbook)]


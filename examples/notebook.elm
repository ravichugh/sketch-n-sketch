change model controller =  {
  apply model = freeze """/*@getCurrentTime*/this.setAttribute('onclick', " " + this.getAttribute('onclick'))"""
  update {input, outputNew} =
    if String.take 1 outputNew == " " then
    { values = [controller model] }
    else
    { values = [input], diffs = [Nothing]}
  }.apply model

insertAt index newElem elements = 
  List.take index elements ++ [newElem] ++ List.drop index elements

sample kind = case kind of
  "title" -> <h1>New title</h1>
  "subtitle" -> <h2>New subtitle</h2>
  "paragraph" -> <p>New paragraph</p>
  "code" -> <code>--Enter your code here</code>

inserter index elements =
  let insert_a kind = change elements (insertAt index <| sample kind) in
  <div class="inserter"><span
    >+...</span><button onclick=(insert_a "title")
    ><b>title</b></button><button onclick=(insert_a "subtitle")
    >subtitle</button><button onclick=(insert_a "paragraph")
    >paragraph</button><button onclick=(insert_a "code")
    ><code>code</code></button></div>

elements =
  [ <h1>Literate Programming Notebook</h1>
  , <p>This is a notebook in which you can embed literate programming statements:</p>
  , <code>oranges = 10
unitprice = 2
totalprice = unitprice * oranges</code>
  , <p>To reuse a computed result in the text, just put a dollar
in front of the variable name in a paragraph.
If you need $oranges oranges each costing $unitprice$$,
then you will need $totalprice$$ in total. </p>]

elementsWithInserters = Tuple.first <|
  List.foldl (\(index, element) (accElems, prevCode) -> 
    let (finalElement, newCode) =
      case element of
        ["code", _, [["TEXT", newCode]]] ->
          -- TODO: pre-parse this code and show parse errors
          (element, prevCode ++ "\n\n" ++ newCode)
        ["p", _, [["TEXT", content]]] ->
          (["p", _, [["TEXT", Regex.replace "\\$\\$|\\$(\\w+)" (\m ->
            if m.match == "$$" then "$" else
            let varname = nth m.group 1 in
            -- TODO: Possibility to incorporate HTML !
            toString (evaluate (prevCode ++ "\n\n" ++ varname))
          ) content]]], prevCode)
        _ -> (element, prevCode)
    in
    (accElems ++ [finalElement, inserter (index + 1) elements], newCode)
  ) ([inserter 0 elements], "") (zipWithIndex elements)

<span id="notebook" style="margin:10px;position:absolute;">
<style>
.inserter {
  float:right;
  opacity:0.3;
  margin-bottom:-10px;
}
.inserter:hover {
  float:none;
  text-align:right;
  opacity: 1;
  background:lightgreen;
  margin-bottom:0px;
}
.inserter &gt; *:not(:first-child) {
  display: none;
}
.inserter:hover &gt; * {
  display: inline-block;
}
#notebook &gt; code {
  display: block;
  white-space: pre;
  width: 100%;
  border: 0px;
}
#notebook &gt; * {
  cursor: text;
}
</style>
@elementsWithInserters</span>
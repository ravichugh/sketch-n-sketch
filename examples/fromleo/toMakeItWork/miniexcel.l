// download: miniexcel.html
// hideprogram: false
// output: html
let delay = {apply=\x.x, unapply=\out.{value = if(out <= 0) 0 else out}} 1 in // Delay in second before triggering changes
let reversiblechoice a b = {
  apply=\cond. if(cond) a else b,
  unapply=\out. if(out == a) {value = true} else if(out == b) {value = false} else {}
} in
let library = ``
import {
  sum = !(\l.
  (letrec aux acc list = list match {
     case [] => acc
     case [head] ++ tail => aux (acc + head) tail
  } in aux 0 l))
}._
`` in

let onkeyup = ``clearTimeout(typeof mwintimer == "undefined" ? 0 : mwintimer); mwintimer = setTimeout((function(self) { return function() { self.textContent = self.value } })(this), @(delay*1000) )`` in

let cell formula value displayFormula =
  [{tag="textarea",
    attributes={title=print value, onblur=``this.textContent = this.value; this.parentNode.children[1].style.display = "inline-block";this.style.display = "none";``, placeholder="Enter a program here"},
    styles = {`vertical-align` = "middle", width = "100%", height="100%", display=reversiblechoice "inline-block" "none" displayFormula},
    children=[{tag="#text", value=formula}]},
   {tag="span",
    attributes={title=formula, ondblclick = ``this.parentNode.firstChild.style.display = "inline-block";this.style.display = "none";this.parentNode.firstChild.focus();``},
    styles = {display = reversiblechoice "none" "inline-block" displayFormula},
    children=[value]}] in

let range min max =
  letrec aux acc i = if(i == max) acc ++ [max] else aux (acc ++ [i]) (i + 1) in
  aux [] min in

letrec columnToInteger x = {A=1, B=2, C=3, D=4, E=5, F=6, G=7, H=8, I=9, J=10, K=11, L=12, M=13, N=14, O=15, P=16, Q=17, R=18, S=19,T=20,U=21,V=22,W=23, X=24,Y=25,Z=26,
default = \x.
  if(matchIn "^[a-z]$" x) (
    columnToInteger x.toUpperCase
  ) else if(matchIn "^[a-zA-Z]+$" x) (
   let last     = replaceAllIn "^(.*)(.)$" x "$2" in
   let init = replaceAllIn "^(.*)(.)$" x "$1" in
   (columnToInteger init)*26 + (columnToInteger last)
  ) else 0
}  x in

let integerToCOlumn = [" ", "A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S","T","U","V","W","X","Y","Z"] in

letrec rowToInteger x = {`1`=1, `2`=2, `3`=3, `4`=4, `5`=5, `6`=6, `7`=7, `8`=8, `9`=9, `0`=0,
default = \x.
  if(matchIn "^[0-9]+$" x) (
   let last = replaceAllIn "^(.*)(.)$" x "$2" in
   let init = replaceAllIn "^(.*)(.)$" x "$1" in
   (rowToInteger init)*10 + (rowToInteger last)
  ) else 0
}  x in

let formulas =
  [[``"Type"``,   ``"€"``,       ``"$"``,        ``"100€ = $"+E1``, ``117``],
   [``"Transport"`` , ``150``,   ``B2*!E1/100``, ``""``,           ``""``],
   [``"Hotel"``     , ``470``,   ``B3*!E1/100``, ``""``,           ``""``],
   [``"Conference"``, ``110``,   ``B4*!E1/100``, ``""``,           ``""``],
   [``"Totals:"``   , ``sum [B2, B3, B4]``,   ``sum [C2, C3, C4]``,  ``""``,           ``""``]] in

let map f list =
  letrec aux acc l = l match {
    case [] => acc
    case [head] ++ tail => aux (acc ++ [f head]) tail
  } in aux [] list in

let addColumn formulas =
  map (\row. row ++ [``"-"``]) formulas in
let addRow formulas =
  formulas ++ [(map (\i. ``"-"``) (range 1 (formulas 0).length))] in
let ifActivated inputUpdater = { apply= \i. "false",
    update= \p. if(p.outputNew == "true") {value = inputUpdater p.input} else {value = p.input}
  } in

/*
let fix computation init =
  letrec aux current =
    let next = computation current in
    if(next == current) next else (aux next)
  in aux init in*/ // Use this fix function when the equality will work at compile-time.

let evaluationlength = 3 in

let fix computation init =
  letrec aux n current = if(n == 0) current else
    let newN = n - 1 in
    let newComputation = computation current in
    aux newN newComputation in
  aux evaluationlength init in

let displayFormulas = false in

let values =
   fix (\computations.
   let inlineKnown computation = replaceAllIn ``\b([a-zA-Z]{1,2})([1-9][0-9]*)\b`` computation (\m.
     let column = columnToInteger (m.group 1) in
     let row = rowToInteger (m.group 2) in
     if(row <= computations.length) (
       if(column <= (computations 0).length) (
         let res = computations (row - 1) (column - 1) in
         res match {
           case "" + _ => (m.group 0)
           case {value = v} => print v
           case {error = _} => (m.group 0)
         }
       ) else (m.group 0)
     ) else (m.group 0)
   ) in
   let oneStep computation =
     let i = inlineKnown computation in
     evaluate (library + i) match {
     case x & {value = v} => {value = v}
     case x & {error = r} => x & {computation  = computation}
   } in
   map (\row.
     map (\content. content match {
       case { value = v } => content // No need to change the computation result.
       case { computation = computation } => oneStep computation
       case ""+computation => oneStep computation
     }) row
   ) computations) formulas in



let display formulas values =
   let display_cell formula value =
     let valueForCell = value match {
       case {value = v} => v
       case {error=m} => {tag="span", styles={color="red"}, children = [{tag="#text", value=m}]}
       case "" + _ => {tag="span", styles={color="red"}, children = [{tag="#text", value="No evaluation made"}]}
     } in
     {tag="td", children=cell formula valueForCell ({apply=\l.l, update=\p. {value = p.input}} displayFormulas)}
   in

   letrec display_cells index acc fs vs = [fs, vs] match {
     case [[fcell] ++ ftail, [vcell] ++ vtail] =>
       let newAcc = acc ++ [display_cell fcell vcell] in
       display_cells index newAcc ftail vtail
     case _ => {tag="tr", children=[{tag="td", children=[{tag="#text", value = "" + index}]}] ++ acc}
   } in
   letrec display_rows acc fs vs = [fs, vs] match {
     case [[frow] ++ ftail, [vrow] ++ vtail] =>
       let newAcc = acc ++ [display_cells (acc.length + 1) [] frow vrow] in
       display_rows newAcc ftail vtail
     case _ =>
       {tag="table", attributes={class="table-bordered"}, children=[
         {tag="tbody", children =
            [{tag = "tr", children =
               map (\i.{tag="th", children=[{tag="#text", value = integerToCOlumn (i- 1)}]}) (range 1 (acc 0).children.length) ++
              [{tag="th",attributes={class="lineadder",addcolumn=ifActivated addColumn formulas, onclick="this.setAttribute('addcolumn','true')"}, children=[{tag="#text", value="+"}]}]
            }] ++
            acc ++
            [{tag="tr", children=
              [{tag="td", attributes={class="lineadder", addrow=ifActivated addRow formulas, onclick="this.setAttribute('addrow','true')"}, children=[{tag="#text", value="+"}]}]}]
            }]}
   } in display_rows [] formulas values
in
[{tag="h1", children="Reversible Spreadsheet"},
``Below is an excel-like table that contains formulas and displays only the values. By double-clicking on an element (e.g.  C2, C3, C4, D5, ...), the formula appear and you can modify it.
But you can also <span style="color:red"><b>modify the values that were computed from formulas</b></span>, something no other spreadsheet editor can afford!<br>
We apologize that, for now, you need to wait a bit when you double-click to display the formula, because the re-generation makes the focus to be lost.<br>
<br>
For example,
<li>try to modify the formula of A1 to "First" + C1, then edit the value "FirstName" to insert a space between the two words.</li>
<li>Add a new column E, and double click on the dash in E2 to insert the formula <code>A2 + " " + B2 + " died at age " + D2</code></li>``,
``Note that the spreadsheet performs @evaluationlength rounds of computation. If you need more (to e.g. replace <code>A2 + " " + B2</code> by <code>C2</code> in the last example), increase this number.``,
{tag="style", children=[{tag="#text", value=``th, td:first-child {
  background-color: #EEE;
  color: #222;
  text-align: center;
  font-weight: normal;
  white-space: nowrap;
}

.lineadder {
  cursor: pointer;
}
.lineadder:hover {
  background-color: #44E;
  color: #FFF;
}
``}]},
display formulas values,{tag="br"},
"Show all formulas: ", displayFormulas]
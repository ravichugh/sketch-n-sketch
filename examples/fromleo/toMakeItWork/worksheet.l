// download: worksheet.html
// output: html
// updatedelay: 0
// hideprogram: true

// Allows you to write sentences of the form
// varname = varexpr
// It will auto-complete to the right with spaces and put = result

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
      let newN = n- 1 in
      drop newN tail
  }) in
let map = !(\f. {
  apply= \input.
  letrec map_aux = {
    apply = \l. l match {
      case [head] ++ tail => [f head] ++ (map_aux tail)
      case [] => []
    }
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

let expressions = [
  { name = "people", expr = ``200``},
  { name = "regFee", expr = ``50``},
  { name = "sponsors", expr = ``!17000``},
  { name = "lunch", expr = ``(minoredBy 20) 30``},
  { name = "venue", expr = ``30000``},
  { name = "days", expr = ``!3``},
  { name = "income", expr = ``!people*regFee + !sponsors``},
  { name = "expenses", expr = ``!people*lunch*days + !venue``},
  { name = "surplus", expr = ``income - expenses``}
] in

import {
  regexLine = ``^(.*?) *//((?:=|\?).*)\r?\n([\s\S]*)``,
  regexParseAssign = ``^\s*(\w[\w\d_]*?)\s*=\s*(.*)``,
  regexParseExpr = ``^\s*(.*)``,
  removeComment = \input. replaceFirstIn ``^(.*?) *//.*`` input "$1",
  nextLine = \input. if(matchIn ``^.*\r?\n`` input) replaceFirstIn ``^.*?\r?\n`` input "" else "",
  removeNextLines = \input. replaceFirstIn ``\r?\n[\s\S]*`` input ""
}._

let parseExpressions output =
  letrec aux acc output = 
    if(output == "") acc else
    if(matchIn regexParseAssign output)
      let name =  removeNextLines (replaceFirstIn regexParseAssign output "$1") in
      let expr = removeComment (removeNextLines (replaceFirstIn regexParseAssign output "$2")) in
      let remaining = nextLine output in
      aux (acc ++ [{name = name, expr = expr}]) remaining
    else ( // Pure expression
      let expr = removeComment (removeNextLines (replaceFirstIn regexParseExpr output "$1")) in
      let remaining = nextLine output in
      let newAcc = if(expr == "") acc else (acc ++ [{name = "", expr = expr}]) in
      aux newAcc remaining
    )
  in aux [] output in

// Returns true if only the program did not change.
// Returns false otherwise
let programDidNotChange oldTxt newTxt = 
  letrec aux oldTxt newTxt =
    if(oldTxt == "" && newTxt == "") true
    else if(oldTxt == "" || newTxt == "") false // Something added or removed
    else (
      let newProg = replaceFirstIn regexLine newTxt "$1" in
      let newValue = replaceFirstIn regexLine newTxt "$2" in
      let newRem = replaceFirstIn regexLine newTxt "$3" in
      let oldProg = replaceFirstIn regexLine oldTxt "$1" in
      let oldValue = replaceFirstIn regexLine oldTxt "$2" in
      let oldRem = replaceFirstIn regexLine oldTxt "$3" in
      if(newProg != oldProg) false
      else if(newRem.length >= newTxt.length || oldRem.length >= oldTxt.length) false
      else aux oldRem newRem
    ) in aux oldTxt newTxt in

/*programDidNotChange ``greet = "Hello" + " "  //="Hello "
whom = "World"         //="World"
greet + whom           //="Hello World"`` ``greet = "Hello" + " "  //="Hello "
whom = "World"         //="World"
greet + whom           //="Hello World"``*/

let render = {
  apply=\expressions.
    letrec padRightTo sp length str =
      if(str.length < length) padRightTo sp length (str + sp) else str in
  
    let postProcessPad pad =
      letrec postProcess acc assignOrAnonymous values = assignOrAnonymous match {
        case [assign] ++ assignTail =>
          let newAcc1 = acc + padRightTo " " pad assign in
          values match {
            case [{value=value}] ++ valuesTail => 
              let newAcc = newAcc1 + "//=" +print value + "\n" in
              postProcess newAcc assignTail valuesTail
            case [{error=msg}] ++ valuesTail => 
              let newAcc = newAcc1 + "//???" + "\n"  in
              postProcess newAcc assignTail valuesTail
            case [x] ++ valuesTail =>
              let newAcc = newAcc1 + "//?" + print x + "\n" in
              postProcess newAcc assignTail valuesTail
            case [] => acc
          }
        case [] => acc
      } in postProcess in
      
      
    letrec aux acc maxLength assign values exprs = exprs match {
      case [] => postProcessPad (maxLength+2) "" acc values
      case [head] ++ tail =>
        head match {
          case {name = name, expr = expr} =>
            let newValues = values ++ [evaluate (assign + expr)] in
            let [newAssign, assignExpr] = 
              if(name == "") [
                assign,
                expr]
              else [
                assign + "let " + name + " = " + expr + " in ",
                name + " = " + expr] in
            let newLength = if(assignExpr.length > maxLength) assignExpr.length else maxLength in
            let newAcc = acc ++ [assignExpr] in
            aux newAcc newLength newAssign newValues tail
            
          case _ => aux acc maxLength assign values tail
        }
    } in
    aux [] 10 "" [] expressions,
  update = \p.
    // If the LHS part of every line is not touched (only the values were changed), we let the reversion operation process on unapply
    if(programDidNotChange p.outputOriginal p.outputNew)
      {}
    else
      {value = parseExpressions p.outputNew}
} in

let tValue = render expressions in

let code id codestr =
 { tag="code", attributes={onclick="document.getElementById(\"" + id + "\").textContent = this.textContent"}, styles={cursor="pointer"},
   children = [{tag="#text", value = codestr}]} in

[{tag="h1",children=[{tag="#text", value="Reversible Worksheet"}]},
``The text zone below is a reversible worksheet.
You can type in, one per line, assignments on the form <code>name = expr</code>, or expressions <code>expr</code>.<br>
Furthermore, you can edit the outputs that appear after 2 seconds on the right of each expression.<br>
Try the examples below the textarea by clicking them:``,
{tag="textarea", attributes={id="worksheetarea", onkeyup=``if(typeof worksheetTimer != "undefined") clearTimeout(worksheetTimer);worksheetTimer = setTimeout((function(self) { return function() { self.textContent = self.value } }) (this), 2000)``,placeholder=``Enter assignments or expressions here. One line per expression``}, styles = {margin = "0px", height = "250px", width = "100%", `font-family` = "monospace"}, children=[{tag="#text", value=tValue}]},

{tag = "li", children = [code "worksheetarea" ``a = "Hello world"
b = replaceAllIn "l" a "|_"
b + "!"
``]},
{tag = "li", children = [code "worksheetarea" ``people = 200
registrationFee = 50
sponsors = !20000
lunch = !20
venue = 30000 
days = !3
income = !people*registrationFee + !sponsors
expenses = !people*lunch*days + !venue
surplus = income - expenses  /* Set me to 0 */``]}]
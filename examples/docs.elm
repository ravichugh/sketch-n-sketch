ensureNatural {minbound} n = { apply x = freeze x, update {input, outputNew} =
  let (f, c) = (max minbound <| floor outputNew, max minbound <| ceiling outputNew) in
  if f /= outputNew || c /= outputNew then let solutions = (if f == input then [] else [f]) ++
  (if c == input || c == f then [] else [c]) in
  { values = solutions } else { values = [outputNew] } }.apply n

format n = { apply n = freeze (toString n |> Regex.replace """(\d)(?=(?:(?:\d{3})+$))""" (\m -> nth m.group 1 + ","))
             update {outputNew} = {values=[String.toInt (Regex.replace "," "" outputNew)]}}.apply n

nbpostdocs     = ensureNatural {minbound=0} 2
nbgraduates    = ensureNatural {minbound=1} 4
nbyears        = ensureNatural {minbound=1} 2
postdocsalary  = ensureNatural {minbound=45000} 47000
graduatesalary = ensureNatural {minbound=25000} 28136
salariesPerYear = postdocsalary * nbpostdocs + graduatesalary * nbgraduates
years callback = List.range 1 nbyears |> List.map callback
plural x = if x > 1 then "s" else ""
pageContent = <div class="pagecontent" contenteditable="true">
<h1 style="text-align:center">NSF Grant proposal</h1><h2>Requested funding</h2>
We want to sustain for @nbyears year@plural<|nbyears:
<ul>
  <li>@nbpostdocs postdoc@plural<|nbpostdocs paid @format<|(postdocsalary)$ per year.</li>
  <li>@nbgraduates graduate student@plural<|nbgraduates paid @format<|(graduatesalary)$ per year.</li>
</ul>
Therefore, we ask for @format<|(nbyears*salariesPerYear)$.
Detailed budget can be found in <a class="sectionref" href="#budget">the budget section</a>.
<h2 id="budget">Detailed budget</h2>
  <table>
    <tr><th>Who</th>
    @(years <| \i -> <th>Year #@(freeze i + 0)</th>)<th>Total</th></tr>
    <tr><td>@nbgraduates graduates</td>
      @(years <| \i -> <td>@format<|(nbgraduates * graduatesalary)$</td>)
      <td>@(years (always <| nbgraduates * graduatesalary) |> List.sum |> format)$</td></tr>
    <tr><td>@nbpostdocs postdocs</td>
      @(years <| \i -> <td>@format<|(nbpostdocs * postdocsalary)$</td>)
      <td>@(years (always <| nbpostdocs * postdocsalary) |> List.sum |> format)$</td></tr>
    <tr><td><b>Total</b></td>
    @(years (always <| <td>@(format salariesPerYear)$</td>))
    <td><b>@(salariesPerYear * nbyears |> format)$</b></td>
    </tr>
  </table>
  <h2>Research statement</h2>
  We can create very nice shiny documents that can embed formulas, but we would like to research how we can<br>
  <ul><li>Make it collaborative<br></li>
  <li>Ensure that we can format paragraphs that contain values properly</li>
  <li>Deal with tables nicely by back-propagating expressions</li></ul>
</div>










-- Very useful link: http://alistapart.com/article/boom
-- https://code.tutsplus.com/tutorials/create-a-wysiwyg-editor-with-the-contenteditable-attribute--cms-25657
<div id="app">
<div id="menu" style="padding-left:10px">
@Html.forceRefresh<|<style>
#menu > a {
  text-decoration: none;
  color: black;
  padding: 4px;
  margin-left: 10px;
}
#menu > a:hover {
  background: #cccccc;
}
#menu > a ul, #menu > a ol {
 padding-left: 0px;
}
#menu {
  margin-top: 5px;
  height: 2em;
  margin-bottom-: 5px;
  border-bottom: 2px solid #cccccc;
  background: #eeeeee;
}
#app {
  position: relative;
  background: #eeeeee;
  height: 100%;
  width: 100%;
}
.page {
  position: relative;
  margin: auto;
  margin-top: 10px;
  top: 0px;
  width: 8.5in;
  height: 11in;
  box-sizing: border-box;
  box-shadow: 0px 0px 5px #aaa;
  background: white;
}
.pagecontent {
   display: inline-block;
   padding: 17mm 16mm 27mm 16mm;
   cursor: text;
   width: 100%;
   height: 100%;
   box-sizing: border-box;
}
th, td {
    padding: 5px;
}
body {
  counter-reset: h2-counter;
}
h2::before {
    counter-increment: h2-counter;
    content: counter(h2-counter) ". ";
}
@@media print {
  body * {
    visibility: hidden;
  }
  .code-panel {
    display: none;
  }
  .page, .page * {
    visibility: visible;
  }
  .page {
    position: absolute;
    left: 0;
    top: 0;
    right: 0;
    bottom: 0;
    box-shadow: none;
  }
  .pagecontent {
    padding: 0;
    font-size: 1.5em;
  }
  .output-panel {
    left: 0 !important;
    top: 0 !important;
    right: 0 !important;
    bottom: 0 !important;
  }
}
</style>
<b>Sketch-n-Sketch docs</b>
<a href="#" data-command='formatBlock:h1' title="First-level header">H1</a>
<a href="#" data-command='formatBlock:h2' title="Second-level header">H2</a>
<a href="#" data-command='undo' style="display:none">⟲ Undo</a>
<a href="#" data-command='redo' style="display:none">⟳ Redo</a>
<a href="#" data-command='createlink' title="Create a link"><u style="color:blue">Link</u></a>
<a href="#" data-command='justifyLeft' title="Align text to the left">
<div style="font-size:0.05em;display:inline-block">=====<br>==<br>====</div></a>
<a href="#" data-command='justifyCenter' title="Center text">
<div style="font-size:0.05em;display:inline-block;text-align:center">=====<br>==<br>====</div></a>
<a href="#" data-command='superscript' title="superscript">X<sup>b</sup></a>
<a href="#" data-command='subscript' title="subscript">X<sub>b</sub></a>
<a href="#" data-command='insertUnorderedList' title="bullet point list"><ul
  style="font-size:0.05em;display:inline-block"
  ><li>---</li><li>---</li><li>---</li></ul></a>
<a href="#" data-command='insertOrderedList' title="numbered list"><ol
  style="font-size:0.05em;display:inline-block"
  ><li>---</li><li>---</li><li>---</li></ol></a>
<a href="#" data-command='print' title="Print the document -- look for @media print for configurable options">🖶
</a></div>
@Html.forceRefresh<|<div id="body">
<div class="page">
@pageContent
</div>
</div>
@Html.forceRefresh<|<script>
/*var el = document.getElementById("body");
var range = document.createRange();
var sel = window.getSelection();
range.setStart(el.childNodes[0], 0);
range.collapse(true);
sel.removeAllRanges();
sel.addRange(range);*/
var as = document.querySelectorAll("a[data-command]")
for(var i = 0; i < as.length; i++) {
  var a = as[i];
  var c = a.dataset.command;
  var cs = c.split(":")
  var command = cs[0];
  a.onclick = (function(command, argument) { return function() {
    if(command == "createlink") {
      argument = prompt('Enter the link here: ','http:\/\/');
    }
    document.execCommand(command, false, argument);
  } })(cs[0], cs[1] ? cs[1] : null);
}
</script>
</div>
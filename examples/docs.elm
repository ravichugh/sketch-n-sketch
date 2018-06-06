-- Very useful link: http://alistapart.com/article/boom

ensureInt n = { apply x = freeze x, update {input, outputNew} =
  let f = floor outputNew in
  let c = ceiling outputNew in
  if f /= outputNew || c /= outputNew then
    let solutions = (if f == input then [] else [f]) ++
      (if c == input then [] else [c]) in
    { values = solutions }
  else { values = [outputNew] }
}.apply n

nbpostdocs = ensureInt 5
nbyears = ensureInt 2
postdocsalary = 60000

<div id="app">
<div id="menu">
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
#menu {
  margin-top: 5px;
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
  width: 7in;
  height: 9.25in;
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
</style>
<b>Sketch-n-Sketch docs</b>
<a href="#" data-command='h1'>H1</a>
<a href="#" data-command='h2'>H2</a>
<a href="#" data-command='undo'>⟲ Undo</a>
<a href="#" data-command='redo'>⟳ Redo</a>
<a href="#" data-command='createlink'>Link</a>
<a href="#" data-command='justifyLeft'>Justify</a>
<a href="#" data-command='superscript'>Superscript</a></div>
@Html.forceRefresh<|<div id="body">
<div class="page">
<div class="pagecontent" contenteditable="true">
NSF Grant proposal<br><br>
We want to sustain @nbpostdocs postdocs for @nbyears years,
each one having a salary of @(postdocsalary)$ per year.<br>
Therefore, we ask for @(nbyears*postdocsalary*nbpostdocs)$.<br></div>
</div>
</div>
@Html.forceRefresh<|<script>
var el = document.getElementById("body");
var range = document.createRange();
var sel = window.getSelection();
range.setStart(el.childNodes[0], 5);
range.collapse(true);
sel.removeAllRanges();
sel.addRange(range);
</script>
</div>
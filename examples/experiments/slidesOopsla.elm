fs x= [["style", [["font-size", """@(x/10)em"""]]]]

main = <div id="app"><div id="app2" spellcheck="false">
<div class="slides" id="slides" contenteditable="true">
  @fullscreenbutton
  <slide ignore-position="current">
    <h1 class="center1">Bidirectional Evaluation with Direct Manipulation</h1>
    <h2 class="center2">Mikaël Mayer, Ravi Chugh and Viktor Kunčak</h2>
    <div @(fs 5)>XXX, @currentDate</div>
  </slide>
  <slide ignore-position="future" class="bwo">
    <h1>How much time would it take</h1>
    <span @(fs 7)>to create a website from scratch</span
    ><span @step @(fs 8)>,<br>that displays options</span
    ><span @step @(fs 9)><br>that users can choose from</span
    ><span @step @(fs 10)><br>where they can modify their choice</span
    ><span @step @(fs 11)>,<br>everything in two languages</span
    ><span @step @(fs 12)>,<br>easy to update & maintain</span
    ><span @step @(fs 13)><br>w/o spitting "spreadsheet"</span> ?
  </slide>
  <slide ignore-position="future" class="bwo">
    <h1>Answer: a few minutes and 20 LoC</h1>
    <span @step @(fs 8)>provided we have a computer which truly understands our desires...<br></span>
    <span @step @(fs 12)>DEMO: Pizza Doodle</span>
  </slide>
  <slide ignore-position="future" class="rules">
    <h1>Constants</h1>
<table @(fs 14)>
<tr><td>
@(envexp env [] [] <span> <span class="comment">{- comment -}</span>  1</span>)
</td><td class="arrows"><span @(stepi 1)>@rightarrow @(value "1")</span><br><span @(stepi 2)>@(
  updatestoresults (value "2") )</span></td><td @(stepi 3)>
@(envexp env [] [] <span> <span class="comment">{- comment -}</span>  2</span>)
</td></tr>
</table><br><br>
    <h1 @(stepi 4)>Closures</h1>
<table @(fs 14) @(stepi 5)>
<tr><td>
@(envexp env [] [] <span> \x -> @(e 1)</span>)
</td><td class="arrows"><span @(stepi 6)>@rightarrow @(value <span>(@env, λx. @(e 1))</span>)</span><span @(stepi 7)><br>@(
  updatestoresults (value <span>(@envp, λx. @(ep 1))</span>) )</span></td><td @(stepi 8)>
@(envexp envp [] [] <span> \x -> @(ep 1)</span>)
</td></tr>
</table>
</slide>
<slide ignore-position="future" class="rules">
<h1>Variables</h1>
<table @(fs 14)>
<tr><td>
@(envexp envanon envanon [("x", "v")] <span>  x</span>)
</td><td class="arrows"><span @(stepi 1)>@rightarrow @(value "v")</span><span @(stepi 2)><br>@(
  updatestoresults (value vp))</span></td><td @(stepi 3)>
@(envexp envanon envanon [("x", vp)] <span>  x</span>)
</td></tr>
</table>
</slide>
<slide ignore-position="future" class="rules">
<h1>Parentheses</h1>
<table @(fs 14)>
<tr><td @(stepi 1)>
@(envexp env [] [] <span>  (
 @e(1))</span>)
</td><td><span @(stepi 2)>@(updatestoresults (value vp))</span></td><td @(stepi 8)>
@(envexp envp [] [] <span>  (
 @ep(1))</span>)
</td></tr>
<tr><td @(stepi 3)>
@downarrow
</td><td></td><td @(stepi 7)>
@uparrow
</td></tr>
<tr><td @(stepi 4)>
@(envexp env [] [] <span>@e(1)</span>)<br><br>
</td><td @(stepi 5)>@(updatestoresults (value vp))</td><td @(stepi 6)>
@(envexp envp [] [] <span>@ep(1)</span>)<br><br>
</td></tr>
</table>
</slide>
<slide ignore-position="future" class="rules">
<h1 style="display:inline-block">If then else</h1>
<span @(stepi 2)>@(evaluatesTo (envexp env [] [] <span>@e(1)</span>) (value "True"))</span><br>
<table @(fs 13)>
<tr><td @(stepi 1)>
@(envexp env [] [] <span>if @e(1) then
  @e("then")
else @e("else")</span>)
</td><td @(stepi 3)>@(updatestoresults (value vp))</td><td @(stepi 9)>
@(envexp envp [] [] <span>if @e(1) then
  @ep("then")
else @e("else")</span>)
</td></tr>
<tr><td @(stepi 4)>
@downarrow
</td><td></td><td @(stepi 8)>
@uparrow
</td></tr>
<tr><td @(stepi 5)>
@(envexp env [] [] <span>@e("then")</span>)<br><br>
</td><td @(stepi 6)>@(updatestoresults (value vp))</td><td @(stepi 7)>
@(envexp envp [] [] <span>@ep("then")</span>)<br><br>
</td></tr>
</table>
</slide>
<slide ignore-position="future" class="rules">
<h1>Let ... in ...</h1>
<table @(fs 13)>
<tr><td @(stepi 1)>
@(envexp env [] [] <span>let
  x = @e(2) in
@e(1)</span>)
</td><td @(stepi 2)>@(updatestoresults (value vp))</td><td @(stepi 12)>
@(envexp envp12 [] [] <span>let
  x = @ep(2) in
@ep(1)</span>)
</td></tr>
<tr><td @(stepi 3)>
@downarrow
</td><td>
<table>
<tr><td @(stepi 4)>
@(envexp env [] [] <span> @e(2)</span>)
</td><td class="arrows"><span @(stepi 5)>@rightarrow @(value (vi 2))</span><span @(stepi 10)><br>@(updatestoresults (value (vip 2)))</span></td><td @(stepi 11)>
@(envexp (envip 1) [] [] <span> @ep(2)</span>)
</td></tr>
</table>
</td><td @(stepi 9)>
@uparrow
</td></tr>
<tr><td @(stepi 6)>
@(envexp env [] [("x", value (vi 2))] <span>@e(1)</span>)<br><br>
</td><td @(stepi 7)>@(updatestoresults (value vp))</td><td @(stepi 8)>
@(envexp (envip 1) [] [("x", value (vip 2))] <span>@e(1)</span>)<br><br>
</td></tr></table>
</slide>
<slide ignore-position="future" class="rules">
<h1>Function application</h1>
<table @(fs 11)>
<tr><td>
@(envexp env [] [] <span>@f @e(2)</span>)
</td><td @(stepi 1)>@(updatestoresults (value vp))</td><td @(stepi 15)>
@(envexp envp12 [] [] <span>@fp @ep(2)</span>)
</td></tr>
<tr><td @(stepi 2)>
@downarrow
</td><td>
<table>
<tr><td @(stepi 3)>
@(envexp env [] [] <span> @f</span>)
</td><td class="arrows"><span @(stepi 4)>@rightarrow @(value <span>(@(envi f), λx. @e(1))</span>)</span>
<span @(stepi 13)><br>@(
  updatestoresults (value <span>(@(envip f), λx. @ep(1))</span>))</span></td><td @(stepi 14)>
@(envexp (envip 2) [] [] <span> @fp</span>)
</td></tr></table>
<table>
<tr><td  @(stepi 5)>
@(envexp env [] [] <span> @e(2)</span>)
</td><td class="arrows"><span @(stepi 6)>@rightarrow @(value (vi 2))</span>
<span @(stepi 11)><br>@(updatestoresults (value (vip 2)))</span></td><td @(stepi 12)>
@(envexp (envip 2) [] [] <span> @ep(2)</span>)
</td></tr></table>
</td><td @(stepi 10)>
@uparrow
</td></tr>
<tr><td @(stepi 7)>
@(envexp (envi f) [] [("x", value (vi 2))] <span>@e(1)</span>)
</td><td @(stepi 8)>@(updatestoresults (value vp))</td><td @(stepi 9)>
@(envexp (envip f) [] [("x", value (vip 2))] <span>@ep(1)</span>)
</td></tr></table>
  </slide>
  <slide ignore-position="future" class="bwo">
    <h1>Other applications</h1>
    <ul>
      <li @step>Budget balance</li>
      <li @step>LaTeX - to - HTML (bidirectional)</li>
      <li @step>Proportional Recipe Editor</li>
      <li @step>NSF grant documents</li>
      <li @step>Faithful tutorials</li>
      <li @step>Dixit scoresheet</li>
      <li @step>Interactive slides</li>
    </ul>
  </slide>
  <slide ignore-position="future" class="bwo">
    <h1>Questions?</h1>
    <a href="https://github.com/ravichugh/sketch-n-sketch">github.com/ravichugh/sketch-n-sketch</a><br><br>
    To try out, tutorials:<br>
    <a href="https://mikaelmayer.github.io/TutorialStudentGrades.html">bit.ly/studentGrades</a><br>
    <a href="https://mikaelmayer.github.io/TutorialMemory.html">bit.ly/tutorialMemory</a><br>
    <br>
    <img width="30px" src="https://www.iconsdb.com/icons/preview/white/twitter-xxl.png">@@MikaelMayer @@ravi_chugh @@vjkuncak<br>
  </slide>
</div>
</div>
<style>
slide {
  color: black;
  background: none;
}
slide.bwo a {
  font-size: 0.75em;
  color: cyan;
}
slide.bwo a:visited{
  color: cyan !important;
}
slide.bwo {
  color: orange;
  background: black;
}
slide.rules {
  font-size: 0.5em;
}
slide.rules h1 {
  font-size: 2em;
  margin-bottom: 0px !important;
  margin-top: 0px !important;
}
slide.bwo h1 {
  color: white;
  margin-bottom: 0px !important;
  margin-top: 0px !important;
  font-size: 1em;
}
slide.bwo :not(h1) {
  font-weight: bold;
}
.slides {
  background: lightblue;
  font-family: "Raleway", "Roboto", "Avenir", sans-serif;
  font-size: 2em;
}
.slides h1 {
  font-size: 1em;
}
.slides h2 {
  font-size: 0.8em;
}
.code {
  font-family: "Consolas", monospace;
}
</style>
@(Html.forceRefresh<|<script>
var container = document.querySelector("#slides");
if(container !== null) {
  container.onscroll = function () {
    container.scrollLeft = 0;
  }
}
</script>)
@(Html.forceRefresh<|<script>
var container = document.querySelector("#slides");
if(typeof keyDown != "undefined" && container !== null) {
  container.removeEventListener("keydown", keyDown, false);
}

sortByOrder = function(array) {
  array.sort(function(a, b) {
    var oa = a.getAttribute("order");
    var ob = b.getAttribute("order");
    var na = typeof oa == "undefined" || oa == "" ? 0 : parseInt(oa);
    var nb = typeof ob == "undefined" || ob == "" ? 0 : parseInt(ob);
    return na - nb;
  });
  return array;
}

keyDown = function (e) {
  var keyCode = e.keyCode;
  var current = document.querySelector("slide[ignore-position=current]");
  if(keyCode == 39 ) { // Right
    var currentFutureSteps = [].slice.call(current.querySelectorAll("[ignore-step=future]"));
    if(currentFutureSteps.length == 0) {
      var next = current.nextElementSibling;
      while(next != null && next.tagName != "SLIDE"){
        next = next.nextElementSibling
      }
      if(next != null) {
        next.setAttribute("ignore-position","current");
        current.setAttribute("ignore-position", "past");
      }
    } else {
      console.log("before", currentFutureSteps);
      var sorted = sortByOrder(currentFutureSteps);
      console.log("after", sorted);
      sorted[0].setAttribute("ignore-step", "current");
    }
    return false;
  } else if(keyCode == 37) { // Left
    var currentFutureSteps = [].slice.call(current.querySelectorAll("[ignore-step=current]"));
    if(currentFutureSteps.length == 0) {
      var prev = current.previousElementSibling;
      while(prev != null && prev.tagName != "SLIDE"){
        prev = prev.previousElementSibling
      }
      if(prev != null) {
        prev.setAttribute("ignore-position","current");
        current.setAttribute("ignore-position", "future");
      }
      return false;
    } else {
      var sorted = sortByOrder(currentFutureSteps);
      sorted[sorted.length - 1].setAttribute("ignore-step", "future");
    }
  }
  return true;
}
if(container !== null) {
  container.addEventListener("keydown", keyDown, false);
}
</script>)
@(let center translateY = """{
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%) translateY(@translateY);
  width: 100%;
  text-align: center;
}""" in <style>
#fullscreenbutton {
  z-index: 1001;
  position: absolute;
  opacity: 0.2;
}
#fullscreenbutton:hover {
  opacity: 1;
}
#app {
  display: table;
  position: absolute;
  height: 100%;
  width: 100%;
}
#app2 {
  display: table-cell;
  vertical-align: middle;
}
.slides {
  display: block;
  width: 600px;
  height: 400px;
  position: relative;
  overflow: hidden;
  cursor: text;
  margin-left: auto;
  margin-right: auto;
}
slide {
  position: absolute;
  top: 0; bottom: 0; left: 0;
  width: 100%;
  font-size: 1em;
  padding: 20px;
  box-sizing: border-box;
}
[ignore-position="current"] {
  left: 0;
  transition: @delay;
}
[ignore-position="future"] {
  left: 100%;
  width: 100%;
  transition: @delay;
}
[ignore-position="past"] {
  left: -100%;
  width: 100%;
  transition: @delay;
}
[ignore-step="future"] {
  display: none !important;
}
[ignore-step="current"] {
}
slide h1, slide h2 {
  margin-top: 0px;
}
.center1 @center("-2em")
.center2 @center("0em")

/* Demo of rules */
.comment {
  font-style: italic;
  color: #666;
}
td {
  text-align: center;
}
td.arrows {
  text-align: left;
}
.envexp {
  display: inline-block;
  padding: 0px 3px 7px 3px;
  background: lightgreen;
  text-align: center;
  margin-top: 2px;
}
.namevalue::before {
  content: "<br>"
}
.env {
  padding: 5px;
  display: inline-block;
}
.exp {
  font-family: monospace;
  padding: 5px;
  border: 1px solid black;
  background: #ddd;
  font-size: 1.2em;
  white-space: pre;
  display: inline-block;
  text-align: left;
}
.value {
  font-weight: bold;
  font-style: italic;
  color: blue;
}
.update {
  border: 2px solid #ccc;
  display: table-cell;
  padding: 2px;
}
.updateouter {
  display: table;
  margin: 0px auto;
  margin-bottom: -15px;
}
</style>)
</div>

delay = "0s"

displayError msg = <span style="color:red;white-space:pre;">@msg</span>

minieval x =
  <span class="code">@x<div><b>⇨ </b
      >@(case __evaluate__ (__CurrentEnv__) x of
    Ok x -> toString x 
    Err msg -> displayError msg)</div></span>

minievalx x =
  <span class="code">@x<br><b>⇨ </b>@(case __evaluate__ (__CurrentEnv__) x of
    Ok x -> x
    Err msg -> displayError msg
  )</span>

step = [["ignore-step", "future"]]

stepi x = [["ignore-step", "future"], ["order", """@x"""]]

fullscreenbutton = [
  <button id="fullscreenbutton" onclick="""
if(typeof isFullScreen == "undefined")
  isFullScreen = false;
isFullScreen = !isFullScreen;
var d = document.getElementById("fullscreenstyle");


var elem = document.documentElement;

/* View in fullscreen */
function openFullscreen() {
  if (elem.requestFullscreen) {
    elem.requestFullscreen();
  } else if (elem.mozRequestFullScreen) { /* Firefox */
    elem.mozRequestFullScreen();
  } else if (elem.webkitRequestFullscreen) { /* Chrome, Safari and Opera */
    elem.webkitRequestFullscreen();
  } else if (elem.msRequestFullscreen) { /* IE/Edge */
    elem.msRequestFullscreen();
  }
}

/* Close fullscreen */
function closeFullscreen() {
  if (document.exitFullscreen) {
    document.exitFullscreen();
  } else if (document.mozCancelFullScreen) { /* Firefox */
    document.mozCancelFullScreen();
  } else if (document.webkitExitFullscreen) { /* Chrome, Safari and Opera */
    document.webkitExitFullscreen();
  } else if (document.msExitFullscreen) { /* IE/Edge */
    document.msExitFullscreen();
  }
}

if(isFullScreen) {
  d.innerHTML = `<style>
body {
  background: black;
}
body * {
  visibility: hidden;
}
#outputCanvas {
  height: auto !important;
  overflow: visible;
}
#app {
  position: absolute;
  overflow: visible;
  width: 100vw !important;
  height: 100vh !important;
}
#app, #app * {
  visibility: visible;
}
.code-panel {
  display: none;
}
.output-panel {
  left: 0 !important;
  top: 0 !important;
  right: 0 !important;
  bottom: 0 !important;
}
#slides {
  transform: scale(2);
  cursor: default;
}
</style>`
  openFullscreen();
} else {
  d.innerHTML = "";
  closeFullscreen();
}

"""><img width="12px" src="https://image.flaticon.com/icons/svg/61/61728.svg"></button>,
<div><transient id="fullscreenstyle"></transient></div>]

currentDate = __jsEval__ """
function formatDate(date) {
  var monthNames = [
    "January", "February", "March",
    "April", "May", "June", "July",
    "August", "September", "October",
    "November", "December"
  ];

  var day = date.getDate();
  var monthIndex = date.getMonth();
  var year = date.getFullYear();

  return day + ' ' + monthNames[monthIndex] + ' ' + year;
}
formatDate(new Date())"""

-- Explanation of the rules
envexp prefixEnv suffixEnv env exp =
  <span class="envexp"
    ><span class="env"
      >@prefixEnv@(List.map (\(name, value) ->
    <span class="namevalue">, <span class="name">@name</span>→<span class="value">@value</span></span>
  ) env)@(if suffixEnv /= [] then <span>, @suffixEnv</span> else [])</span><br><span class="exp">@exp</span></span>
evaluatesTo eexp v =
  <span>if @eexp = @v</span>
apo = [<span style="position:absolute">'</span>] --'
apos = [<span>'</span>] --'
e x = <span>e<sub>@x</sub></span>
f = <span>f</span>
fp = <span>f@apos</span>
fi x = <span>f<sub>@x</sub></span>
vi x = <span>v<sub>@x</sub></span>
vip x = <span>v@apo<sub>@x</sub></span>
vp = <span>v@apo</span>
env = <span>Env</span>
envp = <span>@env@apos</span>
envi x = <span>@env<sub>@x</sub></span>
envip x = <span>@envp<sub>@x</sub></span>
envp12 = <span>@envip(1) ⊕ <sub>@env</sub>@envip(2)</span>
ep x = <span>e@apo<sub>@x</sub></span>
value x = <span class="value">@x</span>
envanon = <span>...</span>
downarrow = <span style="font-size:3.5em;max-height:1.1em;display:inline-block;margin-top:-15px">↓</span>
uparrow = <span style="font-size:3.5em;max-height:1.1em;display:inline-block;margin-top:-15px">↑</span>
shouldproducearrow = <span style="font-size:1.5em;vertical-align:middle">↜ </span>
rightarrow = <span style="font-size:1.5em;vertical-align:-2px"> →</span>
updatestoresults x = [shouldproducearrow, x, rightarrow]
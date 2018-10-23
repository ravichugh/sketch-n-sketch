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
    <h1>Answer: 5 minutes and 20 lines of code</h1>
    <span @step @(fs 8)>provided we have a computer which truly understands our desires...<br></span>
    <span @step @(fs 12)>DEMO: Pizza Doodle</span>
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
    <h1>Behind magic</h1>
    <ul>
      <li @step>Interpreted lambda calculus</li>
      <li @step>Lenses</li>
      <li @step>Spaces in abstract syntactic trees</li>
      <li @step>Elm and Javascript</li>
    </ul>
  </slide>
  <slide ignore-position="future" class="bwo">
    <h1>Questions?</h1>
    <a href="https://github.com/ravichugh/sketch-n-sketch">github.com/ravichugh/sketch-n-sketch</a><br><br>
    To try out, tutorials:<br>
    <a href="https://mikaelmayer.github.io/TutorialStudentGrades.html">bit.ly/studentGrades</a><br>
    <a href="https://mikaelmayer.github.io/TutorialMemory.html">bit.ly/tutorialMemory</a><br>
    <br>
    Mikaël Mayer: mikaelm@@uchicago.edu
    <img width="30px" src="https://www.iconsdb.com/icons/preview/white/twitter-xxl.png">@@MikaelMayer<br>
  </slide>
</div>
</div>
<style>
slide {
  color: black;
  background: none;
}
slide a {
  font-size: 0.75em;
  color: cyan;
}
slide a:visited{
  color: cyan !important;
}
slide.bwo {
  color: orange;
  background: black;
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

keyDown = function (e) {
  var keyCode = e.keyCode;
  var current = document.querySelector("slide[ignore-position=current]");
  if(keyCode == 39 ) { // Right
    var currentFutureSteps = current.querySelectorAll("[ignore-step=future]");
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
      currentFutureSteps[0].setAttribute("ignore-step", "current");
    }
    return false;
  } else if(keyCode == 37) { // Left
    var currentFutureSteps = current.querySelectorAll("[ignore-step=current]");
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
      currentFutureSteps[currentFutureSteps.length - 1].setAttribute("ignore-step", "future");
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
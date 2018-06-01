delay = "0.5s"

displayError msg = <span style="color:red;white-space:pre;">@msg</span>

minieval x =
  <span class="code">@x<br><b>⇨ </b>@(case __evaluate__ (__CurrentEnv__) x of
    Ok x -> toString x 
    Err msg -> displayError msg)</span>

minievalx x =
  <span class="code">@x<br><b>⇨ </b>@(case __evaluate__ (__CurrentEnv__) x of
    Ok x -> x
    Err msg -> displayError msg
  )</span>

-- 
titleWrite = "Write programs"

<div>
<div class="slides" id="slides">
  <slide ignore-position="current">
    <h1 class="center1">Sketch-n-Sketch 2.0</h1>
    <h2 class="center2">Ravi Chugh, Brian Hempel, Mikaël Mayer</h2>
  </slide>
  <slide ignore-position="future">
    <h1>@titleWrite</h1>
    Use standard Elm
    <ul>
      <li>@(minieval "let x = \"Hello \" in x + x + \"world\"")</li>
      <li>@(minieval "map (\\x -> x + 5) [1, 2, 4]")</li>
    </ul>
  </slide>
  <slide ignore-position="future">
    <h1>@titleWrite</h1>
    You can use HTML syntax!
    <ul>
      <li>@(minievalx "let f x = <span title=\"I said \"+x>@x world</span> in f 'Hi'")</li>
      <li>@(minievalx "map (\\x -> <i style=\"\"\"color:@x\"\"\"> @x bottle</i>) [\"red\", 'yellow', 'blue']")</li>
    </ul>
  </slide>
  <slide ignore-position="future">
    <h1>Update their outputs</h1>
  </slide>
</div>
<script>
var container = document.querySelector("#slides");
if(typeof keyDown != "undefined" && container !== null) {
  container.removeEventListener("keydown", keyDown, false);
}
recenter = function() {
  if(container != null) {
    container.scrollLeft = 0;
    container.scrollRight = 0;
  }
}

keyDown = function (e) {
  var keyCode = e.keyCode;
  var current = document.querySelector("slide[ignore-position=current]");
  if(keyCode == 39 ) { // Right
    var next = current.nextElementSibling;
    while(next != null && next.tagName != "SLIDE"){
      next = next.nextElementSibling
    }
    if(next != null) {
      next.setAttribute("ignore-position","current");
      current.setAttribute("ignore-position", "past");
    }
    recenter();
    //e.preventDefault();
    return false;
  } else if(keyCode == 37) { // Left
    var prev = current.previousElementSibling;
    while(prev != null && prev.tagName != "SLIDE"){
      prev = prev.previousElementSibling
    }
    if(prev != null) {
      prev.setAttribute("ignore-position","current");
      current.setAttribute("ignore-position", "future");
    }
    recenter();
    //e.preventDefault();
    return false;
  }
  return true;
}
if(container !== null) {
  container.addEventListener("keydown", keyDown, false);
}
</script>
<style>
slide {
  color: black;
  background: none;
}
.slides {
  background: lightblue;
  font-family: "Roboto", "Avenir", sans-serif;
}
.code {
  font-family: "Consolas", monospace;
}
</style>


<style>
.slides {
  display: block;
  width: 100%;
	padding-bottom: 56.25%; /* 16:9 */
	position: relative;
	overflow: hidden;
}
slide {
	position: absolute;
	top: 0; bottom: 0; left: 0; right: 0;
	font-size: 24px;
	padding: 20px;
}
[ignore-position="current"] {
  left: 0;
  right: 0;
  transition: @delay;
}
[ignore-position="future"] {
  left: 100%;
  right: -100%;
  transition: @delay;
}
[ignore-position="past"] {
  left: -100%;
  right: 100%;
  transition: @delay;
}
.slides.fullscreen {
  position: absolute;
  top: 0; left: 0; right: 0; bottom: 0;
  z-index: 1000;
}
.center1 {
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%) translateY(-1em);
  width: 100%;
  text-align: center;
}
.center2 {
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%) translateY(1em);
  width: 100%;
  text-align: center;
}
</style>
</div>
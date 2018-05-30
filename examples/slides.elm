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

-- Modify me !
titleWrite = "Write your programs"

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
      <li>@(minieval "let x = \"Hi \" in x + x + \"world\"")</li>
      <li>@(minieval "map (\\x -> x + 1) [1, 2, 4]")</li>
    </ul>
  </slide>
  <slide ignore-position="future">
    <h1>@titleWrite</h1>
    You can use HTML syntax!
    <ul>
      <li>@(minievalx "let f x = <span title=\"I said \"+x>@x world</span> in f 'Hello'")</li>
      <li>@(minievalx "map (\\x -> <i style=\"\"\"color:@x\"\"\"> @x bottle</i>) [\"red\", 'green', 'blue']")</li>
    </ul>
  </slide>
  <slide ignore-position="future">
    <h1>Update their outputs</h1>
  </slide>
</div>
<script>
if(typeof keyDown != "undefined") {
  document.removeEventListener("keydown", keyDown, false);
}

keyDown = function (e) {
  console.log("keydown ! ", e)
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
    return false;
  }
  return true;
}

document.addEventListener("keydown", keyDown, false);
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
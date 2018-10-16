-- Scrubbing calculator revisited
-- TODO: When we have html expression replacement,
-- replace each number with a <span class="adjustable"></span>
-- Add a script that keeps track of mouse drags over adjustables and change them accordingly.

scrub x = <div class="scrub" contenteditable="true" >@(
  Regex.replace "(.*)= *" (\m ->
    let raw_computation = nth m.group 1 in
    let computation = Regex.replace "[a-zA-Z_$][\\sa-zA-Z_$]*" "" raw_computation in
    let result = case __evaluate__ [] computation of
        Ok r -> toString r
        Err msg -> msg
    in raw_computation + "= " + result) x)</div>

<div id="appoutput" style="margin:20px">
<h1><a href="http://worrydream.com/ScrubbingCalculator/">Scrubbing calculator</a> revisited</h1>
<p>You want to change the bar height so that the first result 768. Try changing the result!</p>
Here is a special number: <span class="adjustable">956</span> you can change by dragging on it !
<div>
@scrub<|"60 top margin + 140 bottom margin + 8 * 20 gap + 9 * 100 bar height ="
</div>
<style>
.error {
  color: red
}
.scrub {
  font-family:Georgia,"Times New Roman",Times,serif;
  display:inline-block;
  white-space: pre;
}
.adjustable {
  cursor: ew-resize;
}
.adjustable:hover {
  background: lightblue;
}
</style>
@Html.forceRefresh<|<script>
adjustables = document.querySelectorAll(".adjustable");
appoutput = document.querySelector("#appoutput");
lastXPosition = 0;
target = null;
for(var i = 0; i < adjustables.length; i++) {
  adjustables[i].onmousedown = function(event) {
    lastXPosition = event.clientX;
    target = event.target;
    console.log("onmousedown");
  }
}
appoutput.onmouseup = function(event) {
  target = null;
  console.log("onmouseup");
}
appoutput.onmouseleave = function(event) {
  target = null;
  console.log("onmouseleave");
}
appoutput.onmousemove = function(event) {
    if(!target) return;
    event.preventDefault();
    console.log("lastXPosition", lastXPosition);
    var prevInt = parseInt(target.innerText);
    console.log("prevInt", prevInt);
    var delta = event.clientX - lastXPosition;
    console.log("delta", delta);
    lastXPosition = event.clientX;
    var nextInt = prevInt + delta;
    target.innerText = "" + nextInt
  }
</script>
</div>
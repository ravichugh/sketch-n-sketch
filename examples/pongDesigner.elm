# updatedelay: 0
initstate = { x = 50, y = 400, vx = 5, vy = -10, color = "green" }
state     = { x = 50, y = 400, vx = 5, vy = -10, color = "green" }
isPlaying = False
record    = True
g = 2
deltaTime = 1
physics s = { s | x = s.x + s.vx, y = s.y + s.vy, vy = s.vy + g*deltaTime }
prev = physics state
rules = [ {-n is the model after physics, p is the one before-}
          "n -- The new model, p the old model. when X do Y is a shorthand for if X then {n | Y } else n."
        , {-If the block goes down, change its color-}
          "when p.vy <= 0 && n.vy > 0 do color='red'"
        , {-Bounce the block against the floor -}
          "when n.y >= 400 do y = 800-n.y, vy = 0-p.vy"
        , {-If the block goes up, change its color-}
          "when p.vy >= 0 && n.vy < 0 do color='blue'"]
shortcut = Regex.replace "^when (.*)do(.*)$" "if $1 then {n| $2 } else n"
resNext = List.foldl (\rule n -> case n of
  Err msg -> n
  Ok n -> __evaluate__ [("init", initstate),
  ("p", state), ("n", Debug.log "n" n)] (shortcut rule)) (Ok prev) rules

next = resNext.args._1

rect s = <div style="""position:absolute;left:@(s.x)px;top:@(s.y)px;width:@(s.width)px;height:@(s.height)px;background:@(s.color)"""></div>
display opacity s = 
  <div style="""opacity:@opacity""">
  @(rect {s | width=50, height=50 } )
  @(rect {x=0,y=450,width=500,height=50,color="black"})
  </div>

<div style="margin:10px">
<h1>Pong Designer Lite</h1>
This is an attempt of recreating in less than 100 <abbr title="lines of code">LoC</abbr>
the basic functions of the game <a href="">Pong Designer</a> (Mayer et Kuncak, 2013).
You can add rules such as "when n.x > 100 do x = 50", play, pause the execution, go backward and forward in time.<br> If you change the color of the box when it is just being changed, the rule is modified.<br>
@(Html.button "Reset" "Go back to the initial state" state (\_ -> initstate))
@(if isPlaying then
   <button onclick="pause()">||</button>
  else [
   <@(Html.freshTag ())>
   <button title="Back one tick" onclick="prev()">&lt;|</button
   ><button title="Run the simulation" onclick="play()">&gt;</button
   ></@>,
  Html.button "|>" "Next step" (state, record) (\_ -> (next, True))
])
@(if isPlaying then [<span></span>] else (display 0.3 state))
@(case resNext of
  Err msg -> <span></span>
  Ok next -> display 1 next)
<ul class="script" contenteditable="true">@(map (\rule -> <li>@rule</li>) rules)</ul>
@(case resNext of
  Err msg -> <pre>@msg</pre>
  Ok next -> <span></span>)
@Html.forceRefresh<|<span id="p"
  nextstate=Html.onChangeAttribute(state)(\_ _ -> next)
  setpause=Html.onChangeAttribute(isPlaying)(\_ _ -> False)
  setplay=Html.onChangeAttribute(isPlaying)(\_ _ -> True)
  setprev=Html.onChangeAttribute(state)(\_ ns -> evaluate ns)
  setrecord=Html.onChangeAttribute(record)(\_ ns -> ns == "true")
  ></span>
<script>
if(typeof lastStates == "undefined") lastStates = [];
@(if record then """lastStates.push(`@(toString state)`);""" else "")
var isPlaying = @(if isPlaying then "true" else "false");
function set(k, v) { document.querySelector("#p").setAttribute(k, v) }
if(isPlaying) cancel = setTimeout(function() { set("nextstate","#")}, @deltaTime);
function pause() {
  if(typeof cancel !== "undefined") clearTimeout(cancel);
  cancel = undefined;
  set("setpause","#");
}
function play() {
  set("setplay","#");
  set("setrecord", "true")
}
function prev() {
  @(if record then """lastStates.pop();""" else "");
  set("setprev",lastStates.pop());
  set("setrecord", "false");
}
</script>
<style>.script { font-family: monospace; }</style>
</div>
final = True

content = <div class="slides" id="slides" contenteditable="true">
  <slide ignore-position="current">
    <h1 class="center1">Bidirectional Evaluation<br>with Direct Evaluation</h1>
    @Html.forceRefresh<|
    <h2 class="center2">Mikaël Mayer, Ravi Chugh, Viktor Kuncak</h2>
    Friday, August 9<sup>th</sup>, 2018
  </slide>
  <style>.c { color: gray } sc { font-variant: small-caps; } </style>
  @animate(if final then 1 else 43)(43)<|
  <slide ignore-position="future">
    <h1>Evaluation vs. Update rules</h1>
    <div style="display:inline-block;margin-right:2em">
    Evaluation syntax:<br>
    @after(11)("(")@env @co(1)<|"an environment"
     @after(2)<|<span>@vdash @e@after(11)(")")</span> @co(2)<|"an expression under this environment"
      @after(3)(right) @co(3)<|"evaluates to"
       @after(4)(v) @co(4)<|"a value"
    </div>
    <div style="display:inline-block;vertical-align:top;">
     @after(5)<|<span>Update syntax:<br></span>
    @after(6)<|<span>@after(11)("(")@env @vdash @e@after(11)(")")</span> @co(6)<|"an expression under this environment"
    @after(7)(left) @co(7)<|"that we want to update with..."
    @after(8)(vp) @co(8)<|"...with a new value"
    @after(9)(gives) @co(9)<|"...what is the result of that?"
    @after(10)<|<span>@after(11)("(")@envp @vdash @ep@after(11)(")")</span> @co(10)<| "A new environment and expression."
    @co(11)(" Parenthesized version")
    </div>
    @after(12)<|<h3>Rules</h3>
    @before(22)<|after(13)<|<span>
    <sc>E-Const : </sc>@after(13)(evalTo (vd env c) c)<br>
    <sc>U-Const : </sc>@after(14)(preUpd (vd env c) cp) @after(15)(postUpd (vd env cp))<br>
    <br><sc>E-Fun : </sc>@after(16)(evalTo (vd env pf) pfv)<br>
    <sc>U-Fun : </sc>@after(17)(preUpd (vd env pf) pfvp) @after(18)(postUpd (vd envp pfp))
    <br><br><sc>E-Var : </sc>@after(19)(
      evalTo (vd <span>@env1, @x @to @v, @env2</span> x) v)<br>
    <sc>U-Var : </sc>@after(20)(
      preUpd (vd <span>@env1, @x @to @v, @env2</span> x) vp)
        @after(21)(postUpd (vd <span>@env1, @x @to @vp, @env2</span>
        <span>@after(22)(x)</span>))
    </span>
    @before(36)<|after(23)<|<span>
    <sc>E-Let : </sc>@after(24)(
      evalTo (vd env (let_ x e1 e2)) (after 27 v))@quad @after(25)("where")
      @after(25)(evalTo (vd env e1) v1)@quad@after(26)(
        evalTo (vd <span>@env, @x@to@v1</span> e2) v)<br>
    <sc>U-Let : </sc>@after(28)(
       preUpd (vd env (let_ x e1 e2)) vp)
       @after(34)(postUpd (vd <span>@before(35)("??")@after(36)(envp)</span> (let_ x e1p e2p)))
       <br>@quad@after(29)("where")
       <br>@quad@after(29)(evalTo (vd env e1) v1)@quad
       @after(30)(preUpd (vd <span>@env, @x@to@v1</span> e2) vp)
       @after(31)(postUpd (vd <span>@env1, @x@to@v1p</span> e2p))
       <br>@quad@after(32)(preUpd (vd env e1) v1p)@after(33)(postUpd (vd env2 e1p))
       @after(35)<|<span>@quad@envp = merge @env @env1 @env2</span>
    </span>
    @after(37)<|<span>
    <sc>E-App : </sc>@after(38)(
      evalTo (vd env (app e1 e2)) (after 42 v))@quad @after(39)("where")
      @after(39)(evalTo (vd env e1) pfv1)@quad
      @after(40)(evalTo (vd env e2) v2)@quad
      @after(41)(
        evalTo (vd <span>@env, @x@to@v2</span> "f") v)<br>
    <sc>U-App : </sc>@after(43)("On the blackboard")
    </span>
  </slide>
  @animate(if final then 1 else 9)(9)<|<slide ignore-position="future">
    <h1>How to Merge Environments</h1>
    <ul>
      <li>Conservative Two-Way merge:
        <ol>
 @after(2)<|<li>Only merge two values if they are the same...</li>
 @after(3)<|<li>...except maybe if the variable is not used.</li>
 @after(4)<|<li>Correctness: if
            @(updatesTo (vd env e) vp (vd envp ep)), then @(evalTo (vd envp ep) vp)</li>
        </ol>
      </li>
      @after(5)<|<li>Optimistic Three-Way merge
        <ol>
   @after(6)<|<li>Give priority to <i>updated</i> values</li>
   @after(7)<|<li>If conflict, 3-merge values</li>
   @after(8)<|<li>If base case conflict, pick one </li>
   @after(9)<|<li>Correctness not guaranteed, but not desirable!</li>
        </ol>
      </li>
    </ul>
  </slide>
  <slide ignore-position="future">
    <h1>Goodies</h1>
    <ul>
      <li>Lenses</li>
      <li>Tail-recursion</li>
      <li>Regular expression processing </li>
      <li>Examples</li>
    </ul>
  </slide>
</div>

e1 = <span>e<sub>1</sub></span>
e1p = <span>e'<sub>1</sub></span>
e2 = <span>e<sub>2</sub></span>
e2p = <span>e'<sub>2</sub></span>
v1 = <span>v<sub>1</sub></span>
v2 = <span>v<sub>2</sub></span>
v1p = <span>v'<sub>1</sub></span> --'
let_ x e1 e2 = <span>let @x = @e1 in @e2</span>
app e1 e2 = <span>@e1 @e2</span>
co i txt = at i <i class="c"> : @txt</i>
after i elem = <span filter=(<= i)>@elem</span>
before i elem = <span filter=(>= i)>@elem</span>
at i elem    = <span filter=(== i)>@elem</span>

filter i elem = case Debug.log ("Filtering" + toString i) elem of
  ["span", [["filter", f]], children] -> 
    if f i then
    case children of
      [head] -> filter i head
      _ -> <span>@(List.map (filter i) children)</span>
    else <span></span>
  [tag, attrs, children] ->
    [tag, attrs, List.map (filter i) children]
  x -> x

-- Transform this element in a sequence of elements, unfolding one element at a time.
animate start end elem =
  List.range start end |> List.map (\i -> filter i elem)

vdash = "⊢"
quad = <span style="display:inline-block;width:1.5em"></span>
space = <span style="display:inline-block;width:0.5em"></span>
vd x y = <span>@x@vdash@y</span>
evalTo x y = <span>@x @right @y</span>
preUpd x v = <span>@x @left @v@space@gives</span>
postUpd y = <span>@space@y</span>
updatesTo x v y = <span>@preUpd(x)(v)@postUpd(y)</span>

env = "E"
envp = "E'"
env1 = <span>@env<sub>1</sub></span>
env2 = <span>@env<sub>2</sub></span>
e = "e"
ep = "e'"
c = "c"
cp = "c'"
right = " ⇒ "
left = " ⇐ "
gives = "  ⇝  "
v = "v"
vp = "v'"

p = "λx."
pf = p + "f"
pfp = p + "f'"
pfv = <span>(@env, @pf)</span>
pfvp = <span>(@envp, @pfp)</span>
pfv1 = <span>(@env1, @pf)</span>

delay = "0s"
x = "x"
to = "→"
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

titleWrite = "Write your program"
-- 

<div>
@content
<input type="checkbox" id="runcheckbox" checked><label for="runcheckbox">Run mode</label>
<style>
#runcheckbox, #runcheckbox + label {
  opacity: 0.2
}
#runcheckbox:hover, #runcheckbox + label:hover {
  opacity: 1
}
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
@Html.forceRefresh<|<script>
var container = document.querySelector("#slides");
if(container !== null) {
  container.onscroll = function () {
    container.scrollLeft = 0;
  }
}
</script>
@Html.forceRefresh<|<script>
var container = document.querySelector("#slides");
if(typeof keyDown != "undefined" && container !== null) {
  container.removeEventListener("keydown", keyDown, false);
}

keyDown = function (e) {
  if(!document.querySelector("#runcheckbox").checked) return;
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
if(container !== null) {
  container.addEventListener("keydown", keyDown, false);
}
</script>
@let center translateY = """{
  position: absolute;
  top: 50%;
  left: 50%;
  transform: translate(-50%, -50%) translateY(@translateY);
  width: 100%;
  text-align: center;
}""" in <style>
.slides {
  display: block;
  width: 100%;
  padding-bottom: 56.25%; /* 16:9 */
  position: relative;
  overflow: hidden;
}
slide {
  position: absolute;
  top: 0; bottom: 0; left: 0;
  width: 100%;
  font-size: 24px;
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
.slides.fullscreen {
  position: absolute;
  top: 0; left: 0; right: 0; bottom: 0;
  z-index: 1000;
}
slide h1, slide h2 {
  margin-top: 0px;
}
.center1 @center("-2em")
.center2 @center("0em")
</style>
</div>
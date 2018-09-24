cachedNamesList = []
cachedNames = Dict.fromList cachedNamesList
commands = ["\"Hello\"" , "res1 + \" \" + \"world\""]
commandPrefix = "> "
svgTags = Set.fromList ["rect","line"]

displayEvaluationResult x = case x of
  Err msg -> <span class="error" style="white-space:pre">@msg</span>
  Ok result -> 
    let displayHtml () = case result of
      [tag, _, _] ->
        if Set.member tag svgTags then
          <svg class="svgterminal">@result</svg>
        else
          result
    in
    let displayText () =
      <span class="result">@(toString result)</span>
    in
    case result of
      [s, a, t] -> case a of
        [] -> displayHtml ()
        head::tail -> displayHtml ()
        _ -> displayText ()
      _ -> displayText ()
  

content = 
  let aux i commands prevDefinitions revAcc =
    case commands of
    [] -> List.reverse revAcc
    cmd::tail ->
      let defaultName = """res@i""" in
      let name = {
        apply x = Dict.get defaultName cachedNames |>
          case of Nothing -> defaultName; Just n -> n
        update {input=(a, b), outputNew=newName} = Ok (Inputs (
           [(a, Dict.insert defaultName newName cachedNames)]))
        }.apply (defaultName, cachedNames) in
      let res = __evaluate__ prevDefinitions cmd in
      let newPrevDefinitions = case res of
        Ok r -> (name, r)::prevDefinitions
        _ -> prevDefinitions
      in
      aux (i + 1) tail newPrevDefinitions (
        <div class="result">@name: @(displayEvaluationResult res)</div> ::
        <div class="command">@commandPrefix@cmd</div> :: revAcc)
  in
  aux 1 commands [("append", append)] []

addCommand commands newCommand = commands ++ [newCommand]

examples = [
  ("Reset",  "/!\\ Erases the content of the terminal",
    ([], [])),
  ("Line example",  "An example with SVG",
    ([("res1", "x"), ("res2", "y"), ("res3","right")],
     ["10", "100", "x+70", "<line x1=x y1=y x2=right y2=(y+10) stroke='red' stroke-width='3px'/>"])),
  ("Hello world","String concatenations",([], [ "\"Hello\"",
            "res1 + \" \" + \"world\""
          ]))
]

<div style="margin:10px">
<span>
<h1>Read Eval Print Loop with SVG support</h1>
 This REPL lets you:
<ul>
<li title="Input some characters and press ENTER on the last line">Enter new commands</li>
<li title="If you do this, the commands are re-executed">Change the commands</li>
<li title="The commands are updated and re-executed">Change the results</li>
<li title="Just click on the variable and rename it">Change a variable's name.</li>
<li title="Simply edit the quoted part of the program in the output">Fix a parsing error from the error message</li>
</ul><div>Please give it a try! You can also save the state of the REPL and reset it using the button below.</div><br>
<div class="terminal">
@content@commandPrefix<input type="text" id="command"
  v=(Html.onChangeAttribute commands addCommand) onchange="this.setAttribute('v', this.value); this.value=''">
</div>
@(examples |> List.map (\(name, title, (names, cmds)) ->
  Html.button name title (cachedNamesList, commands) (\_ -> 
  (names, cmds)
)))
<br>
@(Html.button "Save" "Save the current state of the REPL to examples"
  examples (\_ -> examples ++ [("Example", "You can modify the name and this title in the DOM", (cachedNamesList, commands))]))
</span>
<style>
#command {
  border: none;
  outline: none;
  color: white;
  background: black;
  font-size: 1em;
}
.command {
}
.result {
}
.error {
  color: red;
}
.terminal {
  color: white;
  background: black;
  font-family: consolas, monospace;
}
.svgterminal {
  color: black;
  background: white;
}
</style>
@Html.forceRefresh<|<script>document.getElementById("command").focus();</script>
</div>
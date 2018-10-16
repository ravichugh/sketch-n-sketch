-- #updatedelay:0
model = {
  computation = "s^2"
  vars = [("s", 1.7320508075688772)]
}

controller = {
  compute model = 
    case __evaluate__ model.vars model.computation of
      Err msg -> <span class="error">Err</span>
      Ok x -> x

  modify f model =
    {model | computation = f model.computation }
  
  appendString string = modify (\s -> s + string)
  
  backspace = modify (String.dropRight 1)
  
  erase = modify (\_ -> "")
  
  storeVar name value model = 
    let replaceIn = case of
      [] -> [(name, value)]
      (n, nr)::tail -> if n == name then (n, value)::tail else (n, nr)::replaceIn tail
    in { model | vars = replaceIn model.vars }
}

view model = 
  let button text title action =
    Html.button text title model action in
  let result = controller.compute(model) in
  <div style="margin:20px" contenteditable="">
  <div class="calculator">
    <div class="display">
      <span class="computation">@model.computation</span><br>
      <span class="result">@result</span>
    </div>
    <div class="buttons">
      @(List.range 0 9 |> List.map (\i ->
        button i "" (controller.appendString (toString i)))
      )@(button "<=" "Delete last char" controller.backspace
      )@(button "CLR" "Erase formula" controller.erase
      )@(["(",")","+","*","/","%","-","^"] |> List.map (\s ->
        button s "" (controller.appendString s))
      )<br>@(model.vars |> List.concatMap (\(name, value) ->
        [button name (toString value) (controller.appendString name),
         button ("→"+name) """Store @result to @name""" (controller.storeVar name result)])
      )
      <input type="text" v=(Update.lens {
          apply model = "d"
          update {input=model,outputNew} = Ok (Inputs [controller.storeVar outputNew result model])
        } model) placeholder="+var" onchange="this.setAttribute('v', this.value)">
    </div>
  </div>
  <style>@styles</style>
</div>

main = view model

styles = """
.calculator {
  width: 405 px;
  max-width: 405px;
  border: 2px solid black;
  display: inline-block;
  padding: 10px;
  border-radius: 10px;
}
.display {
  width: 380px;
  border: 1px solid #AAA;
  background: #EEE;
  font-size: 4em;
  padding: 10px;
  text-align: right;
  display: inline-block;
  font-family: monospace;
}
.display .error {
  color: red;
  font-size: 0.5em;
  white-space:pre;
}
.result {
  font-size: 0.68em;
  color: #888;
}
.calculator button {
  height: 64px;
  width: 64px;
  font-size: 20px;
  margin-top: 10px;
  margin-left: 10px;
}
.calculator input {
  height: 63.5px;
  font-size: 20px;
  margin-bottom: 10px;
  padding-left: 10px;
  margin-top: 10px;
  margin-left: 10px;
  width: 64px;
}
"""
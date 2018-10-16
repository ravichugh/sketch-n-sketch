model = {
  computation = "24*3600"
}

controller = {
  compute model = 
    case __evaluate__ [] model.computation of
      Err msg -> <span class="error">Err</span>
      Ok x -> x
  modify f model =
    {model | computation = f model.computation }
  
  appendString string = modify (\s -> s + string)
  
  backspace = modify (String.dropRight 1)
}

button text action model =
  Html.button text "" model action

view = <div style="margin:20px" contenteditable="">
  <div class="calculator">
    <div class="display">
      <span class="computation">@model.computation</span><br>
      <span class="result">@controller.compute(model)</span>
    </div>
    <div class="buttons">
      @(List.range 0 9 |> List.map (\i ->
        button(i)(controller.appendString (toString i))(model)))
      @button("DEL")(controller.backspace)(model)
      @(["(",")","+","*","/","%"] |> List.map (\i ->
        button(i)(controller.appendString i)(model)))
    </div>
  </div>
  <style>@styles</style>
</div>

main = view


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
  font-size: 0.8em;
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
}
"""
--#updatedelay:0
-- Calculator.

model = {
  computation = "7*s"
  storedVariables = [("s", 86400)]
  variable = "s"
}

controllers = {
  compute model =
    __evaluate__ model.storedVariables model.computation

  showResult = case of
      Ok r -> <span><span contenteditable="false">=</span>@r</span>
      Err x -> <span class="error">@x</span>
      
  nameThisResult name value m =
    let replaceIn = case of
      [] -> [(name, value)]
      (n, nr)::tail -> if n == name then (n, value)::tail else (n, nr)::replaceIn tail
    in {m | storedVariables = replaceIn m.storedVariables }
    
  replaceComputationBy newComputation m =
    { m | computation = newComputation }
    
} 

view =
  let {showResult,compute,nameThisResult,replaceComputationBy} = controllers in
  let result = compute model in
  let button name controller = Html.button name "" model controller in
  <div style="margin:20px" contenteditable="">
    <h1>Model-View-Controller Calculator</h1
    >Although you do not need to, it's straightforward to create a Model-View-Controller interface.
    Note that the controller refer to the model, and the view binds the controllers to the model.<br>
    <div class="calculator">
    @Update.expressionFreeze<|<span class="display" contenteditable="false">
      <span class="computation">@model.computation</span><br>
      <span class="result">@showResult(result)</span>
    </span>
    <div class="commands">
    <br><br>
    @(case result of
       Err _ -> []
       Ok r -> <span>
         @button("Name the result ")(
           nameThisResult model.variable r)@(Html.input "text" model.variable)<br></span>)
    @List.concatMap(\(name,value) ->
      [button("""@name = @value""")(replaceComputationBy name)] ++
        (case result of
         Err _ -> []
         Ok r -> [button("""Store the result in @name""")(nameThisResult name r)]) ++
       [<br>])(model.storedVariables)
    </div>
    </div>
    
    
<style>
.calculator {
  width: 450 px;
  border: 2px solid black;
  display: inline-block;
  padding: 10px;
  border-radius: 10px;
}
.display {
  width: 400px;
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
}
.result {
  font-size: 0.8em;
  color: #888;
}
.calculator button {
  height: 64px;
  font-size: 20px;
  margin-bottom: 10px;
}
.calculator input {
  height: 58px;
  font-size: 20px;
  margin-bottom: 10px;
}
</style>
</div>

main = view
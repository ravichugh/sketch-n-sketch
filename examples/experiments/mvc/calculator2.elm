model = {
  computation = "7*8"
}

controller = {
  compute model = 
    evaluate model.computation
}

view = <div style="margin:20px" contenteditable="">
  <div class="calculator">
    <div class="display">
      <span class="computation">@model.computation</span><br>
      <span class="result">@controller.compute(model)</span>
    </div>
    <div class="buttons">
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
  font-size: 0.72em;
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
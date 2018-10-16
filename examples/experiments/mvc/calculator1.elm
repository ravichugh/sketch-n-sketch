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
      <span class="computation">@model.computation</span>
      =<span class="result">@controller.compute(model)</span>
    </div>
    <div class="buttons">
    </div>
  </div>
</div>

main = view
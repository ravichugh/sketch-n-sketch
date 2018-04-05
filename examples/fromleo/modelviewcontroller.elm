# updatedelay:0

modify defaultContent trigger f x = 
  { apply i = freeze defaultContent,
    update {input, outputNew} = if outputNew == trigger then {values = [f input]} else {value = [input]}
  }.apply x

UI model = {
  button name controller = 
    ["button", [
      ["trigger", modify "" "#" controller model],
       ["onclick", "this.setAttribute('trigger', '#')"]
    ], [Html.textNode name]]
}

model = {
  n = 16
  custom = "if n % 2 == 0 then n / 2 else n*3+1"
  multiplier = 2
}

{button} = UI model

controllers = {
  addOne model = { model | n = model.n + 1 }
  changeN f model = { model | n = f model.n }
  customEval model= { model | n = evaluate """let n = @(model.n) in @(model.custom)""" }
  changeMultiplier f model = { model | multiplier = f model.multiplier }
}

{addOne, changeN, customEval, changeMultiplier} = controllers

view = 
  Html.div [["margin","20px"]] [] (
  [ Html.h1 [] [] "Model-View-Controller"] ++
  html """Using special lenses, you can architect your software as the usual model-view-controller.<br>
n = <span>@(model.n)</span><br> What do you want to do?<br>""" ++ [
  button """Increment""" (changeN <| \n -> n + 1),
  button """Decrement""" (changeN <| \n -> n - 1), Html.br,
  button """Multiply by @(model.multiplier)""" (changeN <| \n -> n * model.multiplier),
  button """Divide by @(model.multiplier)"""   (changeN <| \n -> n / model.multiplier), Html.br,
  button """increase multiplier to @(model.multiplier + 1)""" (changeMultiplier <| \m -> m + 1),
  if model.multiplier > 2 then
    button """Decrease multiplier to @(model.multiplier - 1)""" (changeMultiplier <| \m -> m - 1)
  else
    Html.span [] [] [],
  Html.br,
  button """Custom:""" customEval, Html.span [["font-family","monospace"]] [] [Html.textNode (freeze " " + model.custom)]])
  
view

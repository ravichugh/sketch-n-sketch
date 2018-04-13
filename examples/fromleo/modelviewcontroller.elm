# updatedelay:0

ui model =
  let
    stringFlagForUpdate state1 state2 f model =
      Update.applyLens
        { apply _ = Update.freeze state1
        , update {input = model, outputNew} =
            if outputNew == state2
              then {values = [f model]}
              else {values = [model]}
        } model
  in
  { button name controller =
      [ "button"
      , [ ["trigger", stringFlagForUpdate "" "#" controller model]
        , ["onclick", "this.setAttribute('trigger', '#')"]
        ]
      , [Html.textNode name]
      ]
  }

model = {
  n = 16
  customCode = "if n % 2 == 0 then n / 2 else 1+n*3"
  multiplier = 2
}

controllers = {
  addOne model = { model | n = model.n + 1 }
  changeN f model = { model | n = f model.n }
  customEval model = { model | n = evaluate """let n = @(model.n) in @(model.customCode)""" }
  changeMultiplier f model = { model | multiplier = f model.multiplier }
}

view =
  let {button} = ui model in
  let {addOne, changeN, customEval, changeMultiplier} = controllers in
  Html.div [["margin","20px"]] [] (
    [ Html.h1 [] [] "Model-View-Controller"]
    ++
    Html.parse
      """
      Using lenses, you can architect your HTML page using model-view-controller.
      <br><br>
      n = <span>@(model.n)</span>
      <br><br>
      What do you want to do?
      <br><br>
      """
    ++
    [ button """Increment""" (changeN <| \n -> n + 1)
    , button """Decrement""" (changeN <| \n -> n - 1)
    , Html.br
    , button """Multiply by @(model.multiplier)""" (changeN <| \n -> n * model.multiplier)
    , button """Divide by @(model.multiplier)"""   (changeN <| \n -> n / model.multiplier)
    , Html.br
    , button """Increase multiplier to @(model.multiplier + 1)""" (changeMultiplier <| \m -> m + 1)
    , if model.multiplier > 2 then
        button """Decrease multiplier to @(model.multiplier - 1)""" (changeMultiplier <| \m -> m - 1)
      else
        Html.span [] [] []
    , Html.br
    , button """Custom Code:""" customEval
    , Html.span [["font-family","monospace"]] [] [Html.textNode (Update.freeze " " + model.customCode)]
    ]
  )

main = view

averages (head::tail) =
  let sum n acc ls = case ls of
    [] -> (n, acc)
    (l :: ltail) -> sum (n + 1) (map2 (\x y -> x + y) acc l) ltail
  in
  let (n, acc) = sum 1 head tail in
  map (\x -> x / n) acc
step = 0.125 / 8
List = { List | map2 = map2 }

relu x = if x < 0 then 0 else x

-- Neuron back-propagation using lenses
linksFrom inputLayer biasWeights = {
    apply (inputLayer, biasWeights) =
      biasWeights |> List.map (\bias :: weights ->
         relu <| bias + (map2 (\x y -> x * y) inputLayer weights |> List.foldl (\x y -> x + y) 9))
    update {input=(inputLayer, biasWeights), oldOutput, newOutput} =
       -- first for each neuron, we need to compute how it wants to change the input  layer and the weights
       -- then we average the two changes and apply it to the input
       let changes = map2 (\(bias::weights) [o, n] -> 
         -- First increase the weights and the bias in proportion to the wanted connexion
         let delta = n - o in
         let (newBias, newdelta) = (bias + delta * step, delta * (1 - step)) in
         let newWeights = map2 (\w i -> w + i * step * newdelta ) weights inputLayer in
         let inputLayerDelta = map2 (\w i -> w * step * newdelta ) weights inputLayer in
         ((newBias::newWeights), inputLayerDelta)
         ) biasWeights (zip oldOutput newOutput)
       in
       let (newBiasWeights, inputLayerDeltas) = List.unzip changes in
       let averagedInputLayerDeltas = averages inputLayerDeltas in
       let newInputLayer = List.map2 (\x y -> x + y) inputLayer averagedInputLayerDeltas in
       Ok (InputsWithDiffs [(newInputLayer, newBiasWeights)])
  }.apply (inputLayer, biasWeights)

create_network input biasWeightsList = { input = input, biasWeightsList = biasWeightsList }

neuron_network = create_network [5.1, 1.75] [
  [1 :: [1, 0.5], -0.5 :: [-1, 0.25], 0.4 :: [-0.75, 0.25]],
  [1 :: [1, 0.5, 0.25], -0.5 :: [-1, 0.25, -0.25]]
 ]

midred = 0.1
midgreen = 1
midblue = 10

nonlinear mid weight = floor (freeze 255/(freeze 1 + freeze mid/weight))

colorOf weight = if weight == 0 then "black" else 
  if weight <= 0 then
    let weight = 0 - weight in
    """rgb(@(nonlinear midblue weight), @(nonlinear midgreen weight), @(nonlinear midred weight))"""
  else
  """rgb(@(nonlinear midred weight), @(nonlinear midgreen weight), @(nonlinear midblue weight))"""


x_init = 40
y_init = 40
y_increment = 45
circle_radius = 20
x_increment = 80
bias_width = 5
bias_height = 10

draw_layer x layer = 
  layer |> indexedMap (\i v ->
    <circle cx=x cy=(i*y_increment+y_init)
            r=circle_radius fill=(colorOf v) stroke-width="1" stroke="black" /> )

iterate init elems fun = List.foldl (flip fun) init elems

renderBias bias x y =
  <rect x=(x - circle_radius - bias_width) y=(y -bias_height / 2)
   width=bias_width height=bias_height fill=(colorOf bias) stroke-width=0 />

renderLinks x biasWeights = 
  Tuple.first <|
  iterate ([],     y_init)  biasWeights <|
         \(shapes, y2) (bias::weights) ->
      (\(shapes, _) -> (renderBias bias (x + x_increment) y2 :: shapes, y2 + y_increment)) <|
      iterate (shapes, y_init) weights <|
             \(shapes, y1) weight ->
        ( <line x1=(x + circle_radius) y1=y1 x2=(x + x_increment - circle_radius - bias_width)  y2=y2
           style="""stroke:@(colorOf weight);stroke-width:2""" /> :: shapes
        , y1 + y_increment )


draw_network network =
  draw_layer x_init network.input ++ (Tuple.first <| List.foldl (\biasWeights (shapes, (x, inputLayer)) ->
    let newLayer = linksFrom inputLayer biasWeights in
    (shapes ++ draw_layer x newLayer ++ renderLinks (x - x_increment) biasWeights, (x + x_increment, newLayer))
  ) ([], (x_init + x_increment, network.input)) network.biasWeightsList)

--[neuron5, neuron6] = [0.1, 0.75] |>
--  neuron_layer [1 :: [1, 0.5], -0.5 :: [-1, 0.25], 0.4 :: [-0.75, 0.25]] |>
--  neuron_layer [1 :: [1, 0.5, 0.25], -0.5 :: [-1, 0.25, -0.25]]
--Html.h3 [] [] (toString (neuron5, neuron6))

<div>
<svg>@(draw_network neuron_network)</svg>
</div>
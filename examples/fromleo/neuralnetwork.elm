averages (head::tail) =
  letrec sum n acc ls = case ls of
    [] -> (n, acc)
    (l :: ltail) -> sum (n + 1) (map2 (\x y -> x + y) acc l) ltail
  in
  let (n, acc) = sum 1 head tail in
  map (\x -> x / n) acc
step = 0.125 / 8
List = { List | map2 = map2 }
  
-- Neuron back-propagation using lenses
linksFrom inputLayer biasWeights = {
    apply (inputLayer, biasWeights) =
      biasWeights |> List.map (\bias :: weights ->
         bias + (map2 (\x y -> x * y) inputLayer weights |> List.foldl (\x y -> x + y) 9))
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
       {values = [(newInputLayer, newBiasWeights)]}
  }.apply (inputLayer, biasWeights)

neuron0 = 0.1
neuron1 = 0.75

[neuron2, neuron3, neuron4] = 
  linksFrom [neuron0, neuron1] <|
  [1 :: [1, 0.5], -0.5 :: [-1, 0.25], 0.4 :: [-0.75, 0.25]]
  

[neuron5, neuron6] = 
  linksFrom [neuron2, neuron3, neuron4] <|
  [1 :: [1, 0.5, 0.25], -0.5 :: [-1, 0.25, -0.25]]

Html.h3 [] [] (toString (neuron5, neuron6))

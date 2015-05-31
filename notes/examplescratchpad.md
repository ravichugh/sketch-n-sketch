##Bar graph
(let [x0 y0 sep] [28 28 60]
(svg 
  (map
    (\[i j] (rect 'red' (+ x0 (mult i sep)) (+ y0 (mult j sep)) 50 100))
    [[0 4] [1 2] [2 1]])))

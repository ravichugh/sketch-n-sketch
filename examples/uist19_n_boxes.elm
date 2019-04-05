
boxes =
  map (\i ->
      rect 200 [ 50 + i * 76, 110] 55 195)
    (zeroTo 7{0-15})

svg (concat [
  boxes
])
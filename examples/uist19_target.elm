
point = [236, 241]

circles =
  map (\i ->
      circle (if mod i 2! == 0! then 0 else 466) point (22 + i * 46))
    (reverse (zeroTo 5{0-15}))

svg (concat [
  circles
])
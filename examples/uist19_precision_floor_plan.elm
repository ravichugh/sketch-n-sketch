
point = [82, 136]

h = 239

w = 444

floorRect = rect 36 point w h

tableRect = rect 188 point (w / 3!) h

svg (concat [
  [floorRect],
  [tableRect]
])
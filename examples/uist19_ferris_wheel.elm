
point = [307, 334]

r = 166

attachmentPts = nPointsOnCircle 7{0-10} 0.06280000000000001{-3.14-3.14} point r

color = 434

spokeFunc point2 =
  line color 5 point point2

spokes =
  map spokeFunc attachmentPts

carFunc center2 =
  squareByCenter 48 center2 25

cars =
  map carFunc attachmentPts

capFunc point2 =
  circle 364 point2 9

caps =
  map capFunc attachmentPts

ring1 = ring color 7 point r

hub = circle 362 point 44

svg (concat [
  [hub],
  cars,
  spokes,
  [ring1],
  caps
])
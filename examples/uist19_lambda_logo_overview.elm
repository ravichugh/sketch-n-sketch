
y = 127

num = 158

w = 156

color = 360

strokeWidth = 5

logoFunc x y w color strokeWidth =
  let topLeft = [x, y] in
  let square1 = square "#fcc712" topLeft w in
  let y2 = y + w in
  let xYPair = [ x+ w, y2] in
  let line1 = line color strokeWidth topLeft xYPair in
  let line2 = line color strokeWidth [x, y2] [ (2! * x + w)/ 2!, (2! * y + w) / 2!] in
  let polygon1 =
    let pts = [[x, y], [ x+ w, y], xYPair, [x, y2]] in
    let [color, strokeColor, strokeWidth] = ["none", 360, 5] in
      polygon color strokeColor strokeWidth pts in
  [square1, line1, line2, polygon1]

squareLineLine = logoFunc num y w color strokeWidth

svg (concat [
  squareLineLine
])
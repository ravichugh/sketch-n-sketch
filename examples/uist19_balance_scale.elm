
[centerX, pillarTop] as point2 = [236, 147]

y3 = 181

[x, y] as point = [89, 231]

[x3, y3] as point3 = [noWidgets (sqrt (pow centerX 2! - 2! * centerX * x + pow x 2! - 2! * pillarTop * y + 2! * pillarTop * y3 + pow y 2! - pow y3 2!) + centerX), y3]

trayWireWireFunc ([x, y] as topPoint) hangDistance =
  let yOffset = y + hangDistance in
  let [x1, y1] as point1 = [x, yOffset] in
  let trayHalfW = 81 in
  let left = x1 - trayHalfW in
  let right = x + trayHalfW in
  let tray = ellipse 40 point1 trayHalfW 30 in
  let color = 434 in
  let strokeWidth = 5 in
  let wire1 = line color strokeWidth topPoint [left, yOffset] in
  let wire2 = line color strokeWidth topPoint [right, yOffset] in
  [tray, wire1, wire2]

baseCenter = [centerX, 496]

color = 208

pillar = line color 20 point2 baseCenter

base = ellipse color baseCenter 109 33

strokeWidth = 15

leftArm = line color strokeWidth point2 point

rightArm = line color strokeWidth point2 point3

hangDistance = 171

hangingTray1 = trayWireWireFunc point hangDistance

hangingTray2 = trayWireWireFunc point3 hangDistance

svg (concat [
  [pillar],
  [base],
  [leftArm],
  [rightArm],
  hangingTray1,
  hangingTray2
])
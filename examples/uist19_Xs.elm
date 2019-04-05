
[x, y] as point = [198, 216]

squareW = 66

halfWidth =squareW / 2!

n = 2{0-10}

fill = 72

squareByCenter2Func center2 =
  squareByCenter fill center2 halfWidth

fill2 = 218

squareByCenter2Func2 center2 =
  squareByCenter fill2 center2 halfWidth

squareByCenter2Func3 center2 =
  squareByCenter fill center2 halfWidth

squareByCenter2Func4 center2 =
  squareByCenter fill2 center2 halfWidth

boxyXFunc ([x, y] as point) squareW n =
  let xOffset = x + squareW in
  let xOffset2 = x - squareW in
  let yOffset = y - squareW in
  let yOffset2 = y + squareW in
  let squareByCenter1 = squareByCenter 426 point ( halfWidth) in
  let ySep =0! - squareW in
  let nPointsSepBy2 = nPointsSepBy n [xOffset, yOffset] squareW ( ySep) in
  let repeatedSquareByCenter2Func =
    map squareByCenter2Func nPointsSepBy2 in
  let nPointsSepBy3 = nPointsSepBy n [xOffset, yOffset2] squareW squareW in
  let repeatedSquareByCenter2Func21 =
    map squareByCenter2Func2 nPointsSepBy3 in
  let nPointsSepBy4 = nPointsSepBy n [xOffset2, yOffset2] ySep squareW in
  let repeatedSquareByCenter2Func3 =
    map squareByCenter2Func3 nPointsSepBy4 in
  let nPointsSepBy5 = nPointsSepBy n [xOffset2, yOffset] ySep (0! - squareW) in
  let repeatedSquareByCenter2Func4 =
    map squareByCenter2Func4 nPointsSepBy5 in
  let squareByCenterSingleton =
    [squareByCenter1] in
  concat [squareByCenterSingleton, repeatedSquareByCenter2Func, repeatedSquareByCenter2Func21, repeatedSquareByCenter2Func3, repeatedSquareByCenter2Func4]

boxyX = boxyXFunc point squareW n

boxyXFunc1 = boxyXFunc [506, 225] squareW 1{0-10}

boxyXFunc2 = boxyXFunc [329, 666] squareW 3{0-10}

svg (concat [
  boxyX,
  boxyXFunc1,
  boxyXFunc2
])
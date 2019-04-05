
[left, top] as topLeft = [95, 171]

height = 335

stoneWidth = 73

width = 325

archFunc ([left, top] as topLeft) width height stoneWidth =
  let lintel = rect 210 topLeft width stoneWidth in
  let pillarHeight =height - stoneWidth in
  let pillarTop = top + stoneWidth in
  let leftPillar = rect 0 [left, pillarTop] stoneWidth ( pillarHeight) in
  let rightPillar = rect 134 [ left + width- stoneWidth, pillarTop] stoneWidth pillarHeight in
  [lintel, leftPillar, rightPillar]

arch = archFunc topLeft width height stoneWidth

svg (concat [
  arch
])
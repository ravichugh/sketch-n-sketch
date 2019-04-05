-- Need to draw the bot-right delta offsets before drawing the midpoint
-- Because (ugh) getting the offsets to draw from the correct base points is hard.
-- Abstracted (after grouping w/o gathering dependencies)

[x, y] as point = [88, 104]

w = 331

∂ = 32

lambdaFunc ([x, y] as point) w ∂ leftColor botColor bigColor =
  let yOffset = y + w in
  let xOffset = x + w in
  let [x1, y1] as point1 = [x, yOffset] in
  let x1Offset = x1 + ∂ in
  let yOffsetOffset = yOffset - ∂ in
  let [x2, y2] as point2 = [xOffset, y] in
  let y2Offset = y2 + w in
  let [x3, y3] as point3 = [xOffset, y2Offset] in
  let x3Offset = x3 - ∂ in
  let y2OffsetOffset = y2Offset - ∂ in
  let xOffset2 = x + ∂ in
  let yOffset2 = y + ∂ in
  let midpoint2 = midpoint point point3 in
  let [x4, _] = midpoint2 in
  let fstOffset = x4 - ∂ in
  let [_, y4] = midpoint2 in
  let sndOffset = y4 + ∂ in
  let leftTri =
    let [_, y5] = midpoint2 in
    let pts = [[x, yOffset2], [fstOffset, y5], [x, yOffsetOffset]] in
    let [color, strokeColor, strokeWidth] = [leftColor, 360, 2] in
      polygon color strokeColor strokeWidth pts in
  let botTri =
    let [x5, _] = midpoint2 in
    let pts = [[x1Offset, yOffset], [x5, sndOffset], [x3Offset, y2Offset]] in
    let [color, strokeColor, strokeWidth] = [botColor, 360, 2] in
      polygon color strokeColor strokeWidth pts in
  let bigTri =
    let pts = [[xOffset2, y], point2, [xOffset, y2OffsetOffset]] in
    let [color, strokeColor, strokeWidth] = [bigColor, 360, 2] in
      polygon color strokeColor strokeWidth pts in
  [leftTri, botTri, bigTri]

lambda = lambdaFunc point w ∂ 27 245 148

svg (concat [
  lambda
])
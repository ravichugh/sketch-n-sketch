[x9, y9] as point9 = [130, 562]

[x, y] as point = [102, 109]

bigW = 271

[x1, y1] as point1 = [ x+ bigW, y]

yOffset = y + bigW

xOffset = x + bigW

y1Offset = y1 + bigW

cutW = 58

yOffset2 = y + cutW

xOffset2 = x + cutW

[x2, y2] as point2 = [xOffset2, y]

y2Offset = y2 + cutW

yOffsetOffset = yOffset - cutW

[x3, y3] as point3 = [x, yOffset]

x3Offset = x3 + cutW

[x4, y4] as point4 = [x3Offset, yOffset]

y4Offset = y4 - cutW

xOffsetOffset = xOffset - cutW

[x5, y5] as point5 = [xOffset, y]

y5Offset = y5 + cutW

[x6, y6] as point6 = [xOffsetOffset, y]

y6Offset = y6 + cutW

[x7, y7] as point7 = [x1, y1Offset]

x7Offset = x7 - cutW

y1OffsetOffset = y1Offset - cutW

[x8, y8] as point8 = [x7Offset, y1Offset]

y8Offset = y8 - cutW

color = 39

topDownTemplate =
  let pts = [[xOffset2, y2Offset], point2, point6, [xOffsetOffset, y6Offset], [xOffset, y5Offset], [x1, y1OffsetOffset], [x7Offset, y8Offset], point8, point4, [x3Offset, y4Offset], [x, yOffsetOffset], [x, yOffset2]] in
  let [strokeColor, strokeWidth] = [360, 2] in
    polygon color strokeColor strokeWidth pts

baseW = bigW - 2! * cutW

xOffset2Offset = xOffset2 + baseW

x3OffsetOffset = x3Offset + baseW

x9Offset = x9 + baseW

y9Offset = y9 - cutW

[x10, y10] as point10 = [x9Offset, y9]

y10Offset = y10 - cutW

y11 = 692

num = 367

onLine2 = onLine point9 [num, y11] (baseW / sqrt (pow x9 2! - 2! * x9 * num + pow num 2! + pow y9 2! - 2! * y9 * y11 + pow y11 2!))

[x11, _] = onLine2

fstOffset = x11 + baseW

[_, y12] = onLine2

[_, y13] = onLine2

[x13, y13] as point13 = [fstOffset, y13]

y13Offset = y13 - cutW

x9OffsetY10OffsetPair = [x9Offset, y10Offset]

xY9OffsetPair = [x9, y9Offset]

boxBack =
  let pts = [point9, xY9OffsetPair, x9OffsetY10OffsetPair, point10] in
  let [color, strokeColor, strokeWidth] = [color, 360, 2] in
    polygon color strokeColor strokeWidth pts

boxBot =
  let pts = [point9, point10, point13, onLine2] in
  let [color, strokeColor, strokeWidth] = [color, 360, 2] in
    polygon color strokeColor strokeWidth pts

[x14, y14] as point14 = [fstOffset, y13Offset]

x14Offset = x14 - baseW

boxRight =
  let pts = [point10, x9OffsetY10OffsetPair, point14, point13] in
  let [color, strokeColor, strokeWidth] = [color, 360, 2] in
    polygon color strokeColor strokeWidth pts

x14OffsetY13OffsetPair = [x14Offset, y13Offset]

boxLeft =
  let pts = [xY9OffsetPair, x14OffsetY13OffsetPair, onLine2, point9] in
  let [color, strokeColor, strokeWidth] = [color, 360, 2] in
    polygon color strokeColor strokeWidth pts

boxFront =
  let pts = [onLine2, x14OffsetY13OffsetPair, point14, point13] in
  let [color, strokeColor, strokeWidth] = [color, 360, 2] in
    polygon color strokeColor strokeWidth pts

svg (concat [
  [topDownTemplate],
  [boxBack],
  [boxBot],
  [boxRight],
  [boxLeft],
  [boxFront]
])
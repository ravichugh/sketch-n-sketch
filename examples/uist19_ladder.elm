-- 2012 Quickdraw Fig. 1; cited as originally from a math text
-- Trick is to draw offset first, then snap line to it exactly, then repeat the line.

w = 126

color = 366

strokeWidth = 8

line1Func ([x, y] as point) =
  let xOffset = x + w in
  line color strokeWidth point [xOffset, y]

left = 104

top = 119

rungs =
  map line1Func (nVerticalPointsSepBy 4{0-10} [left, top] 50)

bot = 346

leftLine = line color strokeWidth [left, top] [left, bot]

rightLine = line color strokeWidth [ left+ w, top] [ left+ w, bot]

svg (concat [
  rungs,
  [leftLine],
  [rightLine]
])
-- Final as in paper, but depth 2


equiTriPt [x3, y3] [x2, y2] =
  [ (x2 + x3 + sqrt 3! * (y2 - y3))/ 2!, (y2 + y3 - sqrt 3! * (x2 - x3)) / 2!]

oneThirdPt [x3, y3] [x, y] =
  [ x / 1.5!+ x3 / 3!, y / 1.5! + y3 / 3!]

point = [39, 314]

point2 = [490, 301]

makeKochPts depth point point2 =
  let oneThirdPt2 = oneThirdPt point point2 in
  let oneThirdPt3 = oneThirdPt point2 point in
  let equiTriPt2 = equiTriPt oneThirdPt3 oneThirdPt2 in
  if depth < 2 then
    [point, oneThirdPt3, equiTriPt2, oneThirdPt2]
  else
    let makeKochPts2 = makeKochPts (depth - 1) point oneThirdPt3 in
    let makeKochPts3 = makeKochPts (depth - 1) oneThirdPt3 equiTriPt2 in
    let makeKochPts4 = makeKochPts (depth - 1) equiTriPt2 oneThirdPt2 in
    let makeKochPts5 = makeKochPts (depth - 1) oneThirdPt2 point2 in
      concat [makeKochPts2, makeKochPts3, makeKochPts4, makeKochPts5]

depth = 2{1-5}

topPts = makeKochPts depth point point2

botCorner = equiTriPt point2 point

rightPts = makeKochPts depth point2 botCorner

leftPts = makeKochPts depth botCorner point

snowflakePts = concat [topPts, rightPts, leftPts]

polygon1 =
  let pts = snowflakePts in
  let [color, strokeColor, strokeWidth] = [124, 360, 2] in
    polygon color strokeColor strokeWidth pts

svg (concat [
  [polygon1]
])
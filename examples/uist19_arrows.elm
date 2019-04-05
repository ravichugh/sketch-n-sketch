
pt2 = [405, 134]

pt1 = [109, 238]

color = 0

strokeWidth = 5

arrowFunc pt1 pt2 color strokeWidth =
  let onLine2 = onLine pt1 pt2 0.7112162162162161 in
  let onPerpendicularLine2 = onPerpendicularLine onLine2 pt2 1! in
  let onPerpendicularLine3 = onPerpendicularLine onLine2 pt2 -1! in
  let line1 = line color strokeWidth pt1 pt2 in
  let line2 = line color strokeWidth onPerpendicularLine2 pt2 in
  let line3 = line color strokeWidth onPerpendicularLine3 pt2 in
  [line1, line2, line3]

arrow = arrowFunc pt1 pt2 color strokeWidth

arrowFunc1 = arrowFunc [286, 292] [476, 334] 0 5

arrowFunc2 = arrowFunc [297, 446] [179, 353] 0 5

svg (concat [
  arrow,
  arrowFunc1,
  arrowFunc2
])
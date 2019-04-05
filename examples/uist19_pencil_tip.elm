
[taperStartX, y] as point = [253, 269]

bodyHalfL = 118

pencilHalfW = 61

ratio = 0.62

pencilFunc [taperStartX, y] pencilHalfW bodyHalfL taperL ratio =
  let bodyCenterX = taperStartX - bodyHalfL in
  let top = y - pencilHalfW in
  let bot = y + pencilHalfW in
  let tipX = taperStartX + taperL in
  let body = rectByCenter 42 [bodyCenterX, y] bodyHalfL pencilHalfW in
  let tipPt = [tipX, y] in
  let taperStartTopPt = [taperStartX, top] in
  let leadStartTopPt = onLine taperStartTopPt tipPt ratio in
  let taperStartBotPt = [taperStartX, bot] in
  let leadStartBotPt = onLine taperStartBotPt tipPt ratio in
  let shavedWood =
    let pts = [taperStartBotPt, taperStartTopPt, leadStartTopPt, leadStartBotPt] in
    let [color, strokeColor, strokeWidth] = [460, 360, 0] in
      polygon color strokeColor strokeWidth pts in
  let lead =
    let pts = [leadStartBotPt, leadStartTopPt, tipPt] in
    let [color, strokeColor, strokeWidth] = [397, 360, 0] in
      polygon color strokeColor strokeWidth pts in
  [body, shavedWood, lead]

pencil = pencilFunc point pencilHalfW bodyHalfL 205 ratio

svg (concat [
  pencil
])
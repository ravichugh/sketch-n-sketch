
[branchLeft, branchY] as branchAnchorPt = [53, 542]

rhombusFunc [x, y] halfW halfH =
  let xOffset = x + halfW in
  let xOffset2 = x - halfW in
  let yOffset = y - halfH in
  let yOffset2 = y + halfH in
  let pts = [[x, yOffset], [xOffset, y], [x, yOffset2], [xOffset2, y]] in
  let [color, strokeColor, strokeWidth] = [121, 360, 2] in
    polygon color strokeColor strokeWidth pts

rhombusFunc2 ([x, y] as point) =
  let halfW = 40 in
  let halfH = 83 in
  rhombusFunc point halfW halfH

branchHalfW = 48

branchTop = branchY - branchHalfW

branchBot = branchY + branchHalfW

branchRight = branchLeft + 405

branch =
  let pts = [[branchLeft, branchTop], [branchRight, branchY], [branchLeft, branchBot]] in
  let [color, strokeColor, strokeWidth] = [29, 360, 2] in
    polygon color strokeColor strokeWidth pts

deadspace = 72

leafAttachmentStartX = branchLeft + deadspace

leafAttachmentEndX = branchRight - deadspace

leafAttachmentPts = pointsBetweenSepBy [leafAttachmentStartX, branchY] [leafAttachmentEndX, branchY] 100

leaves =
  map rhombusFunc2 leafAttachmentPts

svg (concat [
  [branch],
  leaves
])
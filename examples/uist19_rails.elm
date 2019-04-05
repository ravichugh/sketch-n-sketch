
[railsLeft, y]= [87, 375]

railsRight = railsLeft + 401

halfGauge = 71

topRailY = y - halfGauge

botRailY = y + halfGauge

railOverextension = 37

firstTieX = railsLeft + railOverextension

endTiesX = railsRight - railOverextension

pointsBetweenSepBy2 = pointsBetweenSepBy [firstTieX, y] [endTiesX, y] 53

tieOverExtension = 34

tieFunc point =
  rectByCenter 24 point 20 (halfGauge + tieOverExtension)

repeatedTieFunc =
  map tieFunc pointsBetweenSepBy2

color = 446

strokeWidth = 13

topRail = line color strokeWidth [railsLeft, topRailY] [railsRight, topRailY]

botRail = line color strokeWidth [railsLeft, botRailY] [railsRight, botRailY]

svg (concat [
  repeatedTieFunc,
  [topRail],
  [botRail]
])
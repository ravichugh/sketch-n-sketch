-- TODO library
mapListSimple = map

mapMaybeSimple f mx =
  freeze (case mx of [] -> []; [x] -> [f x])

mapMaybeLens default =
  { apply [f, mx] =
      freeze <| mapMaybeSimple f mx

  , update {input = [f, mx], outputNew = my} =
      case my of
        []  -> { values = [[f, []]] }
        [y] ->
          let x = case mx of [x] -> x; [] -> default in
          let results = updateApp {fun [f,x] = f x, input = [f, x], output = y} in
          { values = mapListSimple (\[newF,newX] -> [newF, [newX]]) results.values }
  }

mapMaybe default f mx =
  applyLens (mapMaybeLens default) [f, mx]

mapMaybeState =
  mapMaybe ["Alabama", "AL", "Montgomery"]

displayState =
  (\[a,b,c] -> [a, c + ", " + b])

maybeState1 = mapMaybeSimple displayState []
maybeState2 = mapMaybeSimple displayState [["New Jersey", "NJ", "Edison"]]

maybeState3 = mapMaybeState displayState []
maybeState4 = mapMaybeState displayState [["New Jersey", "NJ", "Edison"]]

showList list =
  div_ [] [] (map (\x -> h3 [] [] (toString x)) list)

main =
  showList [maybeState1, maybeState2, maybeState3, maybeState4]

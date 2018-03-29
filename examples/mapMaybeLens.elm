mapMaybeSimple f mx =
  Update.freeze (case mx of [] -> []; [x] -> [f x])

mapMaybeLens default =
  { apply (f, mx) =
      mapMaybeSimple f mx

  , update {input = (f, mx), outputNew = my} =
      case my of
        []  -> { values = [(f, [])] }
        [y] ->
          let x = case mx of [x] -> x; [] -> default in
          let results = Update.updateApp {fun (f,x) = f x, input = (f, x), output = y} in
          { values = List.map (\(newF,newX) -> (newF, [newX])) results.values }
  }

mapMaybe default f mx =
  Update.applyLens (mapMaybeLens default) (f, mx)

mapMaybeState =
  mapMaybe ("Alabama", "AL", "Montgomery")

displayState =
  (\(a,b,c) -> (a, c + ", " + b))

maybeState1 = mapMaybeSimple displayState []
maybeState2 = mapMaybeSimple displayState [("New Jersey", "NJ", "Edison")]

maybeState3 = mapMaybeState displayState []
maybeState4 = mapMaybeState displayState [("New Jersey", "NJ", "Edison")]

showValues values =
  Html.div_ [] [] (List.map (\x -> ["h3", [], Html.text <| toString x]) values)

main =
  showValues [maybeState1, maybeState2, maybeState3, maybeState4]

maybeMapSimple f mx =
  case mx of [] -> []; [x] -> [f x]

maybeMapLens default =
  { apply (f, mx) =
      Update.freeze (maybeMapSimple f mx)

  , update {input = (f, mx), outputNew = my} =
      case my of
        []  -> { values = [(f, [])] }
        [y] ->
          let z = case mx of [x] -> x; [] -> default in
          let results =
            Update.updateApp {fun (func, arg) = func arg, input = (f, z), output = y}
          in
          { values = List.map (\(newF,newZ) -> (newF, [newZ])) results.values }
  }

maybeMap default f mx =
  Update.applyLens (maybeMapLens default) (f, mx)

maybeMapState =
  maybeMap ("?", "?", "?")

displayState =
  (\(a,b,c) -> (a, c + ", " + b))

maybeRowA = maybeMapSimple displayState [("New Jersey", "NJ", "Edison")]
maybeRowB = maybeMapSimple displayState []

maybeRow1 = maybeMapState displayState [("New Jersey", "NJ", "Edison")]
maybeRow2 = maybeMapState displayState []

showValues values =
  Html.div [] [] (List.map (\x -> ["h3", [], Html.text <| toString x]) values)

main =
  showValues [maybeRowA, maybeRowB, maybeRow1, maybeRow2]

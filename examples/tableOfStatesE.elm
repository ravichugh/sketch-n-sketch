states = -- type State = State String String String
  [ State "Alabama" "AL?" ""
  , State "Alaska" "AL?" ""
  , State "Arizona" "AR?" ""
  , State "Arkansas" "AR?" ""
  , State "California" "CA" ""
  , State "Colorado" "CO?" ""
  , State "Connecticut" "CO?" "" ]

main = let
  headers = ["State", "Capital"]
  rows = states |> List.map
           (\State state abbrev cap ->
             [state, cap + ", " + abbrev])
  padding = ["padding", "3px"]
  headerRow =
    Html.tr [] [] (List.map (Html.th [padding] []) headers)
  stateRows = let
    colors = ["lightgray", "white"]
    drawRow i row = let
      color = List.nth colors (mod i (List.length colors))
      columns = row |> List.map 
          (Html.td [padding, ["background-color", color]] [])
      in
        Html.tr [] [] <| columns
          -- ++ [ Html.buttonToDuplicateEnclosing "tr" [] "+"]
    in
      List.indexedMap drawRow rows
  in
    Html.table [padding] [] (headerRow :: stateRows)                     |>






Html.forceRefresh
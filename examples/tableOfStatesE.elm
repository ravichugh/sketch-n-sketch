-- type State = State String String String

states = -- states : List State
  [ State "Alabama" "AL?" ""
  , State "Alaska" "AL?" ""
  , State "Arizona" "AR?" ""
  , State "Arkansas" "AR?" ""
  , State "California" "CA" ""
  , State "Colorado" "CO?" ""
  , State "Connecticut" "CO?" ""]

main =
  let
    padding = ["padding", "3px"]

    stateRows =
      let
        colors = ["lightgray", "white"]

        drawRow i (State state abbrev cap) =
          let
            color =
              List.nth colors (mod i (List.length colors))
              
            columns =
              List.map
                (Html.td [padding, ["background-color", color]] [])
                [state, cap + ", " + abbrev]
          in
            Html.tr [] [] <| columns
            -- ++ [ Html.buttonToDuplicateEnclosing "tr" [] "+"]
      in
        List.indexedMap drawRow states

    headerRow =
      Html.tr [] []
        (List.map (Html.th [padding] [])
                  ["State", "Capital"])
  in
    Html.table [padding] [] (headerRow :: stateRows)                     |>






Html.forceRefresh
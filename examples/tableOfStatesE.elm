padding = ["padding", "3px"]

colors = ["lightgray", "white"]

-- type State = State String String String

states = -- states : List State
  [ State "Alabama" "AL?" ""
  , State "Alaska" "AL?" ""
  , State "Arizona" "AR?" ""
  , State "Arkansas" "AR?" ""]

main =
  let stateRows =
    let drawRow i (State state abbrev cap) =
      let
        color =
          List.nth colors (mod i (List.length colors))
          
        columns =
          List.map
            (Html.td [padding, ["background-color", color]] [])
            [state, cap + ", " + abbrev]
      in
        Html.tr [] [] <| columns
        --  ++ [ Html.buttonToDuplicateEnclosing "tr" [] "+"]
    in
      List.indexedMap drawRow states
  in
  let headerRow =
    Html.tr [] []
      (List.map (Html.th [padding] [])
                ["State", "Capital"])
  in
    Html.table [padding] [] (headerRow :: stateRows)          |>






Html.integerRefresh (List.length states)
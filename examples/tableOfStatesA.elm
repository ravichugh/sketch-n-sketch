states =
  [ ["Alabama", "AL?", "?"]
  , ["Alaska", "AL?", "?"]
  , ["Arizona", "AR?", "?"]
  , ["Arkansas", "AR?", "?"]
  , ["California", "CA", "?"]
  , ["Colorado", "CO?", "?"]
  , ["Connecticut", "CO?", "?"] ]

main =
  let headers = ["State", "Capital"] in
  let rows =
    List.map
      (\[state, abbrev, capital] -> [state, capital + ", " + abbrev])
      states
  in
  let padding = ["padding", "3px"] in
  let headerRow =
    let styles = [padding] in
    Html.tr [] [] (map (th styles []) headers)
  in
  let stateRows =
    let colors = ["lightgray", "white"] in
    let drawRow i row =
      let color = List.nth colors (mod i (List.length colors)) in
      let columns = map (Html.td [padding, ["background-color", color]] []) row in
      Html.tr [] [] columns
    in
    List.indexedMap drawRow rows
  in
  Html.table [padding] [] (headerRow :: stateRows)

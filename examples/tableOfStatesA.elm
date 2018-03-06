-- TODO match formatting in paper

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
    map
      (\[state, abbrev, capital] -> [state, capital + ", " + abbrev])
      states
  in
  let padding = ["padding", "3px"] in
  let headerRow =
    let styles = [padding] in
    tr [] [] (map (th styles []) headers)
  in
  let stateRows =
    let colors = ["lightgray", "white"] in
    let drawRow i row =
      let color = nth colors (mod i (len colors)) in
      let columns = map (td [padding, ["background-color", color]] []) row in
      tr [] [] columns
    in
    indexedMap drawRow rows
  in
  table [padding] [] (headerRow :: stateRows)

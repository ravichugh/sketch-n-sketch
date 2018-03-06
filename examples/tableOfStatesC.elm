states =
  [ ["Alabama", "AL", "Montgomery"]
  , ["Alaska", "AK", "Juneau"]
  , ["Arizona", "AZ", "Phoenix"]
  , ["Arkansas", "AR", "Little Rock"]
  , ["California", "CA", "Sacramento"]
  , ["Colorado", "CO", "Denver"]
  , ["Connecticut", "CT", "Hartford"]
  ]

main =
  let headers = ["State", "Capital"] in
  let rows =
    tableWithButtons.mapData
      (\[state, abbrev, capital] -> [state, capital + ", " + abbrev])
      (customUpdate tableWithButtons.wrapData states)
  in
  let padding = ["padding", "3px"] in
  let headerRow =
    let styles = [padding, ["text-align", "left"], ["background-color", "coral"]] in
    tr [] [] (map (th styles []) headers)
  in
  let stateRows =
    let colors = ["lightyellow", "white"] in
    let drawRow i [flag,row] =
      let color = nth colors (mod i (len colors)) in
      let columns = map (td [padding, ["background-color", color]] []) row in
      tableWithButtons.tr flag [] [] columns
    in
    indexedMap drawRow rows
  in
  table [padding] [] (headerRow :: stateRows)

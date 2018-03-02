------------------------------------------------

-- State, Abbreviation, Capital
states = [
  ["Alabama", "AL?", "?"],
  ["Alaska", "AL?", "?"],
  ["Arizona", "AR?", "?"],
  ["Arkansas", "AR?", "?"],
  ["California", "CA", "?"],
  ["Colorado", "CO?", "?"],
  ["Connecticut", "CO?", "?"]
]

headers =
  ["State", "Capital"]

rows =
  map
    (\[state, abbrev, capital] -> [state, capital + " " + abbrev])
    states

padding =
  ["padding", "3px"]

theTable =
  let headerRow =
    let styles = [padding] in
    tr [] [] (map (th styles []) headers)
  in
  let stateRows =
    let colors = ["lightgray", "white"] in
    indexedMap (\i row ->
      let color =
        nth colors (mod i (len colors))
      in
      let columns =
        map (td [padding, ["background-color", color]] []) row
      in
      tr [] [] columns
    ) rows
  in
  table [padding] [] (headerRow :: stateRows)

main =
  theTable

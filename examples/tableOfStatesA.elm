-- TODO match formatting in paper

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

theTable =
  let padding = ["padding", "3px"] in
  let headerRow =
    let styles = [padding] in
    tr [] [] (map (th styles []) headers)
  in
  let stateRows =
    let colors = ["lightgray", "white"] in
    -- TODO pull out stateRow function if helpful for paper
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

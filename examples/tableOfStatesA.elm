------------------------------------------------
--- misc library

indexedMap f xs = mapi (\[i,x] -> f i x) xs


headers =
  ["State", "Abbreviation", "Capital"]

states = [
  ["Alabama", "AL?", ""],
  ["Alaska", "AL?", ""],
  ["Arizona", "AR?", ""],
  ["Arkansas", "AR?", ""],
  ["California", "CA", ""],
  ["Colorado", "CO?", ""],
  ["Connecticut", "CO?", ""]
]

padding =
  ["padding", "3px"]

theTable =
  let headerRow =
    let styles = [padding] in
    tr [] []
      [ th styles [] (nth headers 0)
      , th styles [] (nth headers 1)
      , th styles [] (nth headers 2)
      ]
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
    ) states
  in
  table [padding] [] (headerRow :: stateRows)

main =
  theTable

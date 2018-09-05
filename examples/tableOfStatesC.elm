states =
  [ ["Alabama", "AL", "Montgomery"]
  , ["Alaska", "AK", "Juneau"]
  , ["Arizona", "AZ", "Phoenix"]
  , ["Arkansas", "AR", "Little Rock"]
  , ["California", "CA", "Sacramento"]
  , ["Colorado", "CO", "Denver"]
  , ["Connecticut", "CT", "Hartford"] ]

TableWithButtons = TableWithButtons.new []

main =
  let headers = ["State", "Capital"] in
  let rows =
    TableWithButtons.mapData
      (\[state, abbrev, cap] -> [state, cap + ", " + abbrev])
      (TableWithButtons.wrapData states)
  in
  let padding = ["padding", "3px"] in
  let headerRow =
    let styles = [padding, ["background-color", "coral"]] in
    Html.tr [] [] (List.map (Html.th styles []) headers)
  in
  let stateRows =
    let colors = ["lightyellow", "white"] in
    let drawRow i (flag,row) =
      let color = List.nth colors (mod i (List.length colors)) in
      let columns =
        List.map
          (Html.td [padding, ["background-color", color]] [])
          row
      in
      TableWithButtons.tr flag [] [] columns
    in
    List.indexedMap drawRow rows
  in
  Html.table [padding] [] (headerRow :: stateRows)

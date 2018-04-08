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
  let _Table = TableWithButtons.new [] in
  let rows =
    _Table.mapData
      (\[state, abbrev, cap] -> [state, cap + ", " + abbrev])
      (_Table.wrapData states)
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
      _Table.tr flag [] [] columns
    in
    List.indexedMap drawRow rows
  in
  Html.table [padding] [] (headerRow :: stateRows)

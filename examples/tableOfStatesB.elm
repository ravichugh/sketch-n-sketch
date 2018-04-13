---------------------------------------------------------------------
-- Like the previous example, but with the TableWithButtons
-- module included in this file.

TableWithButtons =
  let wrapData rows =
    let blankRow =
      let numColumns =
        case rows of
          []     -> 0
          row::_ -> List.length row
      in
      List.repeat numColumns "?"
    in
    Update.applyLens
      { apply rows =
          Update.freeze
            (List.map (\row -> (Update.freeze False, row)) rows)

      , update {outputNew = flaggedRows} =
          let processRow (flag, row) =
            if flag == True
              then [ row, blankRow ]
              else [ row ]
          in
          { values = [List.concatMap processRow flaggedRows] }
      }
      rows
  in
  let mapData f flaggedRows =
    List.map (Tuple.mapSecond f) flaggedRows
  in
  --
  -- The globalBool flag is used to determine whether to insert "" or " "
  -- before a couple attribute values. Toggling this choice in between
  -- subsequent runs helps work around our issue forcing Elm to re-render.
  --
  let tr globalBool flag styles attrs children =
    let (hasBeenClicked, nope, yep) =
      ("has-been-clicked", Update.softFreeze "gray", Update.softFreeze "coral")
    in
    let dummyStrPrefix =
      Update.softFreeze <| if globalBool then "" else " "
    in
    let onclick =
      """
      var hasBeenClicked = document.createAttribute("@hasBeenClicked");
      var buttonStyle = document.createAttribute("style");

      if (this.parentNode.getAttribute("@hasBeenClicked").endsWith("False")) {
        hasBeenClicked.value = "@(dummyStrPrefix)True";
        buttonStyle.value = "color: @yep;";
      } else {
        hasBeenClicked.value = "@(dummyStrPrefix)False";
        buttonStyle.value = "color: @dummyStrPrefix@nope;";
      }

      this.parentNode.setAttributeNode(hasBeenClicked);
      this.setAttributeNode(buttonStyle);
      """
    in
    let button = -- text-button.enabled is an SnS class
      [ "span"
      , [ ["class", "text-button.enabled"]
        , ["onclick", onclick]
        , ["style", [["color", dummyStrPrefix + nope]]]
        ]
      , [textNode "+"]
      ]
    in
    Html.tr styles
      ([hasBeenClicked, dummyStrPrefix + toString flag] :: attrs)
      (snoc button children)
  in
  { wrapData = wrapData
  , mapData = mapData
  , tr = tr
  }

TableWithButtons =
  -- Toggle the global boolean flag, to workaround the force re-render issue.
  { new _ =
      { TableWithButtons | tr = TableWithButtons.tr (toggleGlobalBool []) }
  }

---------------------------------------------------------------------

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

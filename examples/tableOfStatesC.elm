-- TODO match formatting in paper

-- move to lens library

customUpdate record x =
  record.apply x

customUpdateFreeze =
  customUpdate { apply x = x, update p = [p.input] }

-- move to table library

tableWithButtons = {

  wrapData =
    { apply rows   = rows |> map (\row -> [freeze False, row])
    , unapply rows = rows |> concatMap (\[flag,row] ->
                               if flag == True
                                 then [ row, ["","",""] ]
                                 else [ row ]
                             )
                          |> just
    }

  mapData f =
    map (mapSecond f)

  tr flag styles attrs children =
    let [hasBeenClicked, nope, yep] =
      ["has-been-clicked", customUpdateFreeze "gray", customUpdateFreeze "coral"]
    in
    let onclick =
      """
      var hasBeenClicked = document.createAttribute("@hasBeenClicked");
      var buttonStyle = document.createAttribute("style");
      
      if (this.parentNode.getAttribute("@hasBeenClicked") == "False") {
        hasBeenClicked.value = "True";
        buttonStyle.value = "color: @yep;";
      } else {
        hasBeenClicked.value = "False";
        buttonStyle.value = "color: @nope;";
      }
      
      this.parentNode.setAttributeNode(hasBeenClicked);
      this.setAttributeNode(buttonStyle);
      """
    in
    let button = -- text-button.enabled is an SnS class
      [ "span"
      , [ ["class", "text-button.enabled"]
        , ["onclick", onclick]
        , ["style", [["color", nope]]]
        ]
      , [textNode "+"]
      ]
    in
    tr styles
      ([hasBeenClicked, toString flag] :: attrs)
      (snoc button children)

}

------------------------------------------------

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

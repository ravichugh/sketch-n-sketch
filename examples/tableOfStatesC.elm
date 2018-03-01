--- misc library

indexedMap f xs = mapi (\[i,x] -> f i x) xs

nothing = ["Nothing"]
just x  = ["Just", x]

------- library helpers for user-defined updates ------

apply record x = record.apply x

lens record x = apply record x

unapply record = record.unapply

------- library helpers for adding rows ------

addRowFlags =
  { apply =
      map <| \row -> [False,row]
  , unapply rows =
      just <|
        concatMap (\[flag,row] ->
          if flag == True
            then [ row, row ]
            else [ row ]
        ) rows
  }

-- addAndIgnoreRowFlags rows =
--   rows
--     |> lens addRowFlags
--     |> map snd

trWithButton showButton flag styles attrs children =
  if showButton == False then
    tr styles attrs children

  else
    let [hasBeenClicked, nope, yep] =
      -- TODO want to freeze gray and coral (! or library)
      ["has-been-clicked", "gray", "coral"]
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

-- TODO just for testing.
simulateButtonClicks indices rows =
  let setFlags =
    indexedMap <| \i [flag,row] ->
      if elem i indices
        then [True,row]
        else [flag,row]
  in
  rows |> setFlags
       |> unapply addRowFlags
       |> apply addRowFlags

------------------------------------------------

headers =
  ["State", "Abbreviation", "Capital"]

states = [
  ["Alabama", "AL", "Montgomery"],
  ["Alaska", "AK", "Juneau"],
  ["Arizona", "AZ", "Phoenix"],
  ["Arkansas", "AR", "Little Rock"],
  ["California", "CA", "Sacramento"],
  ["Colorado", "CO", "Denver"],
  ["Connecticut", "CT", "Hartford"]
]

-- states =
--   states
--     |> map (\[a,b,c] -> [b,a,c])
--     |> addAndIgnoreRowFlags

-- TODO not working until map lens

states =
  states
    |> lens addRowFlags

padding =
  ["padding", "3px"]

theTable =
  let headerRow =
    let styles = [padding, ["text-align", "left"], ["background-color", "coral"]] in
    tr [] []
      [ th styles [["colspan", "2"]] (nth headers 0)
      , th styles [] (nth headers 2)
      ]
  in
  let stateRows =
    let colors = ["lightyellow", "white"] in
    indexedMap (\i [flag,row] ->
      let color =
        nth colors (mod i (len colors))
      in
      let columns =
        map (td [padding, ["background-color", color]] []) row
      in
      trWithButton True flag [] [] columns
    ) states
  in
  table
    [padding, ["border", "8px solid lightgray"]]
    []
    (headerRow :: stateRows)

main =
  theTable

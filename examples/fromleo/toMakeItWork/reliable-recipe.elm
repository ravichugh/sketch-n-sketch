ingredient = "We need [50g of flour{for}20 servings]"

servings = 20

quantity = "(\\d+)"

weightunits = [("Kg", 1), ("g", 1000), ("oz", 35.274), ("lb", 2.204623)]
weightunitsDict = Dict.fromList weightunits
volumetricunits = [
  ("L", 1), ("mL", 1000),
  ("cup", 4.22675), ("Tbsp", 67.6280454), ("ts", 202.884)]
volumetricunitsDict = Dict.fromList volumetricunits

weightUnitsNames = List.map Tuple.first weightunits
volumetricUnitNames = List.map Tuple.first volumetricunits
units = weightUnitsNames ++ volumetricUnitNames

freezeRight = String.update.freezeRight

freezeLeft = String.update.freezeLeft

kgOf1Liter =
  [ ("flour", 0.55)
  , ("water", 1)
  , ("milk", 1.031)
  , ("butter", 0.865)
  , ("sugar", 0.89)]

kgOf1LiterOf name =
  let aux l = case l of
    [] -> Err ("<1Kg" + name + Update.freezeRight " = " + {
      apply _ = "??"
      update {input, outputNew} =
        if Regex.matchIn quantity outputNew then
          Ok (Inputs [(name, String.toInt outputNew / 1000)::input])
        else
          Ok (Inputs [input], diffs = [Nothing] )
      }.apply kgOf1Liter + Update.freezeLeft "/1000L>")
    ((reg, q)::tail) ->
     if Regex.matchIn reg name then
       Ok q
     else aux tail
  in aux kgOf1Liter

unitsregex = units |> String.join "|"
volumetricUnitRegex = "^" + (volumetricUnitNames |> String.join "|") + "$"
weightUnitRegex = "^" + (weightUnitsNames |> String.join "|") + "$"
isVolumetricUnit x = Regex.matchIn volumetricUnitRegex x
isWeightUnit x = Regex.matchIn weightUnitRegex x
getRelativeWeight x = Dict.apply weightunitsDict x
getRelativeVolume x = Dict.apply volumetricunitsDict x


mbunit = "\\s*("+unitsregex+")s?"
text = "((?:(?!\\{for\\}).)*)"
separator = "\\{for\\}"
servings = quantity ++ "\\s*servings"

regex = "\\[" + quantity + mbunit + text + separator + servings + "\\]"

chosenUnitIndex = 8

chosenUnitName = nth units chosenUnitIndex

servings = 20

nicedisplay x = toString (floor x)

computeQuantity quantity origUnit chosenUnitName ingredient origServing =
  let _ = Debug.log "quantity" quantity in
  if isVolumetricUnit origUnit then
    let origRef = getRelativeVolume origUnit in
    if isVolumetricUnit chosenUnitName then
      let newRef = getRelativeVolume chosenUnitName in
      HTml.textNode <| nicedisplay <| quantity * servings * Update.freeze (newRef / (origRef * origServing))
    else if isWeightUnit chosenUnitName then
      let newRef = getRelativeWeight chosenUnitName in
      case kgOf1LiterOf ingredient of
        Err msg -> Html.textNode msg
        Ok massPerVolume -> Html.textNode <| nicedisplay <| quantity * servings * Update.freeze (newRef / (origRef * massPerVolume * origServing))
    else -- No unit
      Html.textNode <| Update.freeze chosenUnitName ++ " is not a valid unit name"
  else if isWeightUnit origUnit then
    let origRef = getRelativeWeight origUnit in
    if isWeightUnit chosenUnitName then
      let newRef = getRelativeWeight chosenUnitName in
      let _ = Debug.log """origUnit: @origUnit, origRef: @origRef, newRef: @newRef""" [] in
      Html.textNode <| nicedisplay <| quantity * servings * Update.freeze (newRef / (origRef * origServing))
    else if isVolumetricUnit chosenUnitName then
      let newRef = getRelativeVolume chosenUnitName in
      case kgOf1LiterOf ingredient of
        Err msg -> Html.textNode msg
        Ok massPerVolume -> Html.textNode <| nicedisplay <| quantity * servings * Update.freeze (newRef * massPerVolume / (origRef * origServing))
    else -- No unit
      Html.textNode <| Update.freeze chosenUnitName ++ " is not a valid unit name"
  else -- No unit originally
    Html.textNode <| nicedisplay <| quantity * servings / Update.freeze origServing
  
rendering = case Regex.extract regex ingredient of
  Just [quantity, origUnit, what, origServing] ->
    [computeQuantity (String.toInt quantity)
      origUnit chosenUnitName what (String.toInt origServing),
     Html.select [] units chosenUnitIndex,
     Html.textNode what]
  Nothing -> ingredient

<div style = "margin:20px">
<h1 style="font-family:cursive">Special cakes</h1>
<span class="ingredient">
<style>
span.ingredient select {
  font-size: 1em;
  border: none;
  -webkit-appearance: none;
}
</style>
For @"""@servings""" servings, you need <br>
<ul>
  <li>@rendering</li>
</ul>
</span></div>
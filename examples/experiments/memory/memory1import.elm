-- TODO: Import images from https://www.englisch-hilfen.de/en/words/kitchen.htm

importStep = <table>
@Update.expressionFreeze<|<tbody>@(
  images
  |> List.map (\Duo name url ->
     <tr><td><img src=url/></td><td>@name</td></tr>)
  |> Update.onUpdate (
     Html.filter (not << Html.isEmptyText))
  )</tbody>
</table>

main = importStep

images = [
  Duo "dog" "https://upload.wikimedia.org/wikipedia/commons/7/7d/Labrador_Chocolate.jpg"
]
user = "Mikael"
userdata = [("Mikael", 1)]
options = ["Margherita", "Reine", "Montagnarde"]

dictionnaire = [
  ("English", [
              ]),
  ("Français", [
               ])
]
indexLangue = 1

Html.forceRefresh <|
Html.translate dictionnaire indexLangue <|
  <span>Salut @user!<br>
Tu veux quelle pizza?
@Html.select[]("Choisis ta pizza"::options)(
  listDict.get user userdata
  |> Maybe.orElseReplace (freeze (Just 0))
  |> Maybe.getUnless 0)
<br><br>
@Html.select[](List.map Tuple.first dictionnaire)(indexLangue)<br><br>
Choix finaux:<br>
@(List.map (\(name, id) ->
  <span>@name a choisi une pizza @(nth options (id - 1)).<br></span>
) userdata)
</span>
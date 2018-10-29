user = "Mikael"
userdata = [("Mikael", 1), ("Ravi", 2), ("Viktor", 3)]
options = ["Margharita", "Queen", "Vegi"]

dictionnaire = [
  ("English", [ ("Tuveuxquellepizz1", "Which pizza do you want")
              , ("Choixfinaux1", "Final choices")
              , ("achoisiunepizza1", "wants a pizza")
              , ("Choisistapizza", "Choose your pizza")
              ]),
  ("Français", [ ("Tuveuxquellepizz1", "Tu veux quelle pizza")
               , ("Choixfinaux1", "Choix finaux")
               , ("achoisiunepizza1", "a choisi une pizza")
               , ("Choisistapizza", "Choisis ta pizza")
               ])
]
indexLangue = 0

Html.forceRefresh <|
Html.translate dictionnaire indexLangue <|
  <div style="margin:20px"><span>@Html.select[](List.map Tuple.first dictionnaire)(indexLangue)<h1>@user,</h1>$Tuveuxquellepizz1?
@Html.select[]("$Choisistapizza"::options)(
  listDict.get user userdata
  |> Maybe.orElseReplace (freeze (Just 0))
  |> Maybe.getUnless (== 0))
<br><br>$Choixfinaux1@(List.map (\(name, id) ->
  <span>@name $achoisiunepizza1 @(nth options (id - 1)).</span>
) userdata)
</span></div>
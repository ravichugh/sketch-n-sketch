user = "Mikael"
userdata = []
options = ["Margherita", "Reine", "Montagnarde"]

<span>Salut @user!<br>
Tu veux quelle pizza?
@Html.select[]("Choisis ta pizza"::options)(
  listDict.get user userdata
  |> Maybe.orElseReplace (freeze (Just 0))
  |> Maybe.getUnless (== 0))
</span> |> Html.forceRefresh
students = """Milo Dacruz
Efrain Bottorff
Dorethea Seedorf
Wendie Deshon
Magen Veit
Gabriel Warring
Jacques Matteson
Crystle Saidi
Gala Decker
Zada Sampsel""" |> Regex.split "\r?\n"

partner_preference =
  [
  ]

partner_preference_dict = Dict.fromList partner_preference

teams = 
  students |>
  List.map (\student ->
    case Dict.get student partner_preference_dict of
      Just partner ->
        case Dict.get partner partner_preference_dict of
          Just studentX -> if student == studentX then
              <span>@student and @partner form a team<br></span>
            else
              <span>@student wants @partner but @partner wants @studentX<br></span>
          Nothing -> <span>@student wants @partner but @partner did not confirm<br></span>
      Nothing -> <span>@student did not express a will<br></span>
  ) |> Update.freeze

others = students
  |> List.filter (\student -> student /= login)

selectedIndex = Update.lens {
    apply _ =
      case Dict.get login partner_preference_dict of
        Just other -> 1 + List.indexOf other others
        Nothing -> 0
    update {outputNew = newIndex} =
      if newIndex > 0 then
        Ok (Inputs [Dict.insert login (nth others (newIndex - 1)) partner_preference_dict])
      else
        Ok (inputsWithDiffs [(partner_preference, Nothing)])
  } partner_preference_dict

main = (rw {read=["all"],write=["admin"]} (\login selectedIndex teams -> <span>
@login, who do you want ot be on your team?
@Html.select([])("Choose a partner" :: others)(selectedIndex)
<br><br>
Teams built:<br>
@(teams)
</span>) <br>) login selectedIndex teams

List = { List |
  indexOf elem list =
    let aux i list = case list of
      [] -> -1
      head :: tail -> if head == elem then i else aux (i + 1) tail
    in aux 0 list
  }

access name rw data default = {
  apply data = if List.contains name rw.read || List.contains "all" rw.read then data else default
  update {input=data, outputNew = newData,diffs} =
    if List.contains name rw.write || List.contains "all" rw.write then Ok (InputsWithDiffs [(newData, Just diffs)]) else
      Err ("""Cannot change data, only @(rw.write |> String.join ",") have write access, not @name.""")
}.apply data

rw = access login

-- Variable normally provided after authentication
login = "Magen Veit"
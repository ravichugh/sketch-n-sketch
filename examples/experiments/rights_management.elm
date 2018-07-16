-- Authorization-based diffs

usernames = freeze ["admin", "user42", "outsider"]

i = 0

username = nth usernames i

List = { List | contains n =
  letrec aux l = case l of
    [] -> False
    head::tail -> if head == n then True else aux tail
  in aux }

access name rw data default = {
  apply data = if List.contains name rw.read || List.contains "all" rw.read then data else default
  update {input=data, outputNew = newData,diffs} =
    if List.contains name rw.write || List.contains "all" rw.write then Ok (InputsWithDiffs [(newData, Just diffs)]) else
      Err ("""Cannot change data, only @(rw.write |> String.join ",") have write access, not @name.""")
}.apply data

rw = access username

my_name     = rw { read= ["all"], write=["user42", "admin"] } "Galactic Traveler" "<hidden name>"
my_email    = rw { read= ["user42","admin"], write=["user42"] } "user43@gmail.com" "<hidden email>"
my_password = rw { read= ["user42"], write=["user42"] } "longBeach42!" "<hidden>"

templateBuilder = rw { read= ["all"], write = ["admin"]} (\who ->
  <span>
    Hello @who.name !<br>
    <input type="checkbox">Yes I'd like to opt-in for notifications at @who.email.<br>
    My password is currently: <span>@who.password</span>
  </span>) (\_ -> <span>No read access</span>)

<div style="margin:10px">
<h1>Rights management</h1>



<span>
View this page with the rights of @Html.select[](usernames)(i):<br>
@Html.forceRefresh<|templateBuilder{name=my_name, email=my_email, password=my_password}
</span></div>
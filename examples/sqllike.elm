select = List.map
from = identity
where = List.filter
aggregate merger l = case l of
  h::j::t -> aggregate merger (merger h j :: t)
  [h] -> h
  [] -> ""

_1 x y = x
_2 x y = y

status x = x.status
name x = x.name
nationality x = x.nationality
table = [
  {name="Ravi", status="Professor", nationality="US"},
  {name="Mikael", status="Postdoc", nationality="French"},
  {name="Brian", status="Postdoc", nationality="US"},
  {name="John", status="Postdoc", nationality="Greek"},
  {name="Zechao", status="Postdoc", nationality="Chinese"}]

<pre>@(
     from table
  |> where (status == "Postdoc" && nationality /= "French")
  |> select (name + " is a " + nationality + " " + status)
  |> aggregate (_1 + "\n" + _2))</pre>
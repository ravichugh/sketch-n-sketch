{-
In this experience, we can modify the data, and if we delete a line, it offers two possibilities:
* It either deletes the person's line
* Or it delete the entire cities table.
There is a placeholder code for inserting new data, e.g. to create missing rows in one table.
-}

tablePeople =
  { name="people"
    headers = ["ID", "Name", "CityID"]
    data = [
    ["1", "Mikael", "2"],
    ["2", "Ravi", "1"],
    ["3", "Brian", "1"],
    ["4", "Marion", "3"]
  ]}
  
tableCities =
  { name="cities"
    headers = ["ID", "Name", "Population"]
    data = [
      ["1", "Chicago", "2705000"],
      ["2", "Menoncourt", "400"],
      ["3", "Paris", "2244000"]
    ]
  }
  
nthOf elem list = 
  let aux i l = case l of
    [] -> -1
    h :: t -> if h == elem then i else aux (i+1) t
  in aux 0 list

on = 0

join table1 table2 on (key1, key2) =
  let key1pos = nthOf key1 table1.headers in
  let key2pos = nthOf key2 table2.headers in
  { name = table1.name + " join with " + table2.name + " on " + key1 + "==" + key2
    headers = List.map (+ <| table1.name + ".") table1.headers ++ List.map (+ <| table2.name + ".") table2.headers
    data =
      List.concatMap_ (\x -> [x]) (\[table1line] as table1Lines ->
        List.concatMap_ (\x -> [x]) (\[table2line] as table2Lines ->
          if nth table1line key1pos == nth table2line key2pos then
            {apply ([table1line], [table2line]) = [ table1line ++ table2line ]
             update {input=([table1line] as table1Lines, [table2line] as table2Lines), outputNew} =
               case outputNew of
                 [] -> Ok (Inputs [(table1Lines, []),([], table2Lines),([], [])])
                 [head] ->
                   case Update.updateApp { fun (a, b) = a ++ b, input = (table1line, table2line), output = head } of
                     Err msg -> Err msg
                     Ok (InputWithDiffs vsds)-> Ok (Inputs (List.map (\((nt1, nt2), diffs) -> ([nt1], [nt2])) vsds))
                 addedLines -> Err "I do not know how to add lines"
            }.apply (table1Lines, table2Lines)
          else []
        ) table2.data
      ) table1.data
  }

display table =
  [<h1>@table.name</h1>
  ,<table>
    <tr>@(List.map (\h -> <th>@h</th>) table.headers)</tr>
    @(List.map (\row -> <tr>@(List.map (\elem -> <td>@elem</td>) row)</tr>) table.data)
  </table>]

main = Html.forceRefresh <pre>@(display <| join tablePeople tableCities on ("CityID", "ID"))</pre>
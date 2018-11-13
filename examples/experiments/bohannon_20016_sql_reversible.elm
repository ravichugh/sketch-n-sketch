{-
In this experience, we can modify the data, and if we delete a line, it offers two possibilities:
* It either deletes the person's line
* Or it delete the entire cities table.
There is a placeholder code for inserting new data, e.g. to create missing rows in one table.
-}

albums =
  { name="albums"
    headers = ["Album", "Quantity"]
    data = [
    ["Disintegration", 6],
    ["Show", 3],
    ["Galore", 1],
    ["Paris", 4],
    ["Wish", 5]
  ]}
  
tracks =
  { name="tracks"
    headers = ["Track", "Date", "Rating", "Album"]
    data = [
      ["Lullaby", "1989", 3, "Galore"],
      ["Lullaby", "1989", 3, "Show"],
      ["Lovesong", "1989", 5, "Galore"],
      ["Lovesong", "1989", 5, "Paris"],
      ["Trust", "1992", 4, "Wish"]
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
    headers = table1.headers ++ List.removeAt key2pos table2.headers
    data =
      List.concatMap_ (\x -> [x]) (\[table1line] as table1Lines ->
        List.concatMap_ (\x -> [x]) (\[table2line] as table2Lines ->
          if nth table1line key1pos == nth table2line key2pos then
            {apply ([table1line], [table2line]) = [ table1line ++ (List.removeAt key2pos table2line |> Maybe.map Tuple.second |> Maybe.withDefault []) ]
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

join_dl table1 table2 =
  table1.headers |>
    List.mapFirstSuccess (\header1 ->
      table2.headers |>
      List.mapFirstSuccess (\header2 ->
        if header1 == header2 then Just header1
        else Nothing)
    ) |>
  case of
    Just key -> join table1 table2 on (key, key)
    Nothing -> error "the tables do not share keys"



display table =
  [<h1>@table.name</h1>
  ,<table>
    <tr>@(List.map (\h -> <th>@h</th>) table.headers)</tr>
    @(List.map (\row -> <tr>@(List.map (\elem -> <td>@elem</td>) row)</tr>) table.data)
  </table>]

main = Html.forceRefresh <pre>@(display <| join_dl tracks albums)</pre>
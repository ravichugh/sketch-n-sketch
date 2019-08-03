partition_result_list (xs : ('a, 'b) result list) : ('a list, 'b list) =
  let
    helper us vs 
  match xs with
    | [] ->
        ([], [])

result_list_commute (xs : ('a, 'b) result list) : ('a list, 'b) result =
  match xs with
    | [] -> Ok []
    | head :: rest ->
        begin match head with
          | Ok x -> result_list_commute rest
          | Error e -> Error e
        end

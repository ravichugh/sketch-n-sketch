let filter_somes xs =
  let rec helper acc =
    function
      | [] ->
          List.reverse acc

      | head :: tail ->
          begin match head with
            | Some x ->
                helper (x :: acc) tail

            | None ->
                helper acc tail
          end
  in
    helper [] xs

let maximum =
  function
    | [] ->
        None

    | head :: tail ->
        Some @@ List.fold_left max head tail

let pure_bind xs f =
  List.map f xs

let pure x =
  [x]

let bind xs f =
  List.map f xs
    |> List.concat

let filter_somes xs =
  let rec helper acc =
    function
      | [] ->
          List.rev acc

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
        Some (List.fold_left max head tail)

let repeat n x =
  let rec helper k acc =
    if k <= 0 then
      acc
    else
      helper (k - 1) (x :: acc)
  in
    helper n []

let sequence mxs =
  List.fold_right
    ( fun xs acc ->
        bind xs @@ fun x ->
        pure_bind acc @@ fun ys ->
          x :: ys
    )
    mxs
    ([[]])

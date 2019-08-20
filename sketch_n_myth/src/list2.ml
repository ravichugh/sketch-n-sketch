let pure_bind xs f =
  List.map f xs

let pure x =
  [x]

let bind xs f =
  List.map f xs
    |> List.concat

let concat_map f xs =
  bind xs f

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

let filter_map f xs =
  let rec helper acc =
    function
      | [] ->
          List.rev acc

      | head :: tail ->
          begin match f head with
            | Some y ->
                helper (y :: acc) tail

            | None ->
                helper acc tail
          end
  in
    helper [] xs

let filter_somes xs =
  filter_map identity xs

let intersperse sep xs =
  let rec helper acc =
    function
      | [] -> List.rev acc
      | [x] -> List.rev (x :: acc)
      | head :: tail -> helper (sep :: head :: acc) tail
  in
    helper [] xs

let range ~low ~high =
  ListLabels.init ~len:(high - low + 1) ~f:((+) low)

let remove_first y xs =
  let rec helper acc =
    function
      | [] ->
          List.rev acc

      | head :: tail ->
          if head = y then
            List.rev_append acc tail
          else
            helper (head :: acc) tail
  in
    helper [] xs

let permutations ys =
  (* Source: https://stackoverflow.com/a/40099411 *)
  let rec permutations' xs =
    if xs = [] then
      [[]]
    else
      bind xs @@ fun x ->
      bind (permutations' (remove_first x xs)) @@ fun permutation ->
        [ x :: permutation ]
  in
    List.sort_uniq compare (permutations' ys)

let map3 f xs1 xs2 xs3 =
  List.map2
    (fun (x1, x2) x3 -> f x1 x2 x3)
    (List.combine xs1 xs2)
    xs3

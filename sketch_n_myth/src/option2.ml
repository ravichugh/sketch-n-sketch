let map f =
  function
    | Some x ->
        Some (f x)

    | None ->
        None

let bind ox f =
  match ox with
    | Some x ->
        f x

    | None ->
        None

let sequence xs =
  let rec helper acc =
    function
      | [] ->
          Some (List.rev acc)

      | head :: tail ->
          begin match head with
            | Some x ->
                helper (x :: acc) tail

            | None ->
                None
          end
  in
    helper [] xs

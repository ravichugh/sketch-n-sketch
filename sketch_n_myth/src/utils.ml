let compose f g =
  fun x -> f (g x)

let pair_map2 f (x, y) =
  (f x, f y)

let map_left f (x, y) =
  (f x, y)

let map_right f (x, y) =
  (x, f y)

let option_map f =
  function
    | Some x ->
        Some (f x)

    | None ->
        None

let option_bind ox f =
  match ox with
    | Some x ->
        f x

    | None ->
        None

let option_sequence xs =
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

let result_map f =
  function
    | Ok x ->
        Ok (f x)

    | Error e ->
        Error e

let result_pure_bind x f =
  result_map f x

let result_bind rx f =
  match rx with
    | Ok x ->
        f x

    | Error e ->
        Error e

let result_sequence xs =
  let rec helper acc =
    function
      | [] ->
          Ok (List.rev acc)

      | head :: tail ->
          begin match head with
            | Ok x ->
                helper (x :: acc) tail

            | Error e ->
                Error e
          end
  in
    helper [] xs

let map f =
  function
    | Ok x ->
        Ok (f x)

    | Error e ->
        Error e

let pure_bind x f =
  map f x

let bind rx f =
  match rx with
    | Ok x ->
        f x

    | Error e ->
        Error e

let sequence xs =
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

let to_option r =
  match r with
    | Ok x ->
        Some x

    | Error _ ->
        None

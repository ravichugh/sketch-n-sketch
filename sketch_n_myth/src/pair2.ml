let pair x y =
  (x, y)

let map_left f (x, y) =
  (f x, y)

let map_right f (x, y) =
  (x, f y)

let lift_right_result (x, r) =
  match r with
    | Ok y ->
        Ok (x, y)
    | Error e ->
        Error e

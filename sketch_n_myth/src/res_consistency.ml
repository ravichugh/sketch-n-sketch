open Lang

let rec check r1 r2 =
  if r1 == r2 then
    Some []

  else
    match (r1, r2) with
      | (RTuple comps1, RTuple comps2) ->
          if List.length comps1 <> List.length comps2 then
            None
          else
            List.map2 check comps1 comps2
              |> Option2.sequence
              |> Option2.map List.concat

      | (RCtor (_, arg1), RCtor (_, arg2)) ->
          check arg1 arg2

      | _ ->
          begin match Res_util.res_to_value r1 with
            | Some v1 ->
                Some [(r2, v1)]

            | None ->
                begin match Res_util.res_to_value r2 with
                  | Some v2 ->
                      Some [(r1, v2)]

                  | None ->
                      None
                end
          end

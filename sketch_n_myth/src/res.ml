open Lang

let rec final r =
  determinate r || indeterminate r

and final_env (env : env) : bool =
  List.for_all (fun (_x, e) -> final e) env

and determinate r =
  match r with
    | RFix (env, _, _, _) ->
        final_env env

    | RTuple comps ->
        List.for_all final comps

    | RCtor (_, arg) ->
        final arg

    | _ ->
        false

and indeterminate r =
  match r with
    | RHole (env, _) ->
        final_env env

    | RApp (r1, r2) ->
        indeterminate r1 && final r2

    | RProj (_, _, arg) ->
        indeterminate arg

    | RCase (env, scrutinee, _) ->
        final_env env && indeterminate scrutinee

    | _ ->
        false

let rec res_to_value r =
  match r with
    | RTuple comps ->
        comps
          |> List.map res_to_value
          |> Option2.sequence
          |> Option2.map (fun vcomps -> VTuple vcomps)

    | RCtor (name, arg) ->
        Option2.map
          (fun v -> VCtor (name, v))
          (res_to_value arg)

    | _ ->
      None

let rec value_to_res v =
  match v with
    | VTuple comps ->
        RTuple (List.map value_to_res comps)

    | VCtor (name, v_arg) ->
        RCtor (name, value_to_res v_arg)

let rec value_to_example v =
  match v with
    | VTuple comps ->
        ExTuple (List.map value_to_example comps)

    | VCtor (name, v_arg) ->
        ExCtor (name, value_to_example v_arg)

let rec consistent r1 r2 =
  if r1 = r2 then
    Some []

  else
    match (r1, r2) with
      | (RTuple comps1, RTuple comps2) ->
          if List.length comps1 <> List.length comps2 then
            None
          else
            List.map2 consistent comps1 comps2
              |> Option2.sequence
              |> Option2.map List.concat

      | (RCtor (_, arg1), RCtor (_, arg2)) ->
          consistent arg1 arg2

      | _ ->
          begin match res_to_value r1 with
            | Some v1 ->
                Some [(r2, v1)]

            | None ->
                begin match res_to_value r2 with
                  | Some v2 ->
                      Some [(r1, v2)]

                  | None ->
                      None
                end
          end

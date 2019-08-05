open Lang

let final r =
  determinate r || indeterminate r

and final_env (env : env) : bool =
  List.for_all (fun (_x, e) -> final e) env

and determinate r =
  match r with
    | RFix of (env, _, _, _) ->
        final_env env

    | RTuple of comps ->
        List.for_all final comps

    | RCtor of (name, arg) ->
        final arg

    | _ ->
        False

and rec indeterminate r =
  match r with
    | RHole of (env, _) ->
        final_env env

    | RApp (r1, r2) ->
        indeterminate r1 && final r2

    | RProj (_, arg) ->
        indeterminate arg

    | RCase of (env, scrutinee, _) ->
        final_env env && indeterminate scrutinee

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

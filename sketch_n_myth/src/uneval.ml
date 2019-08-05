open Lang

let simplify delta sigma rcs =
  let simplify_one (res, value) =
    if Res_util.final res then
      uneval delta sigma [] res (Res_util.value_to_example value)
    else
      None
  in
    rcs
      |> List.map simplify_one
      |> Option2.sequence
      |> Option2.and_then Hole_constraints.merge_all

and uneval delta sigma hf res ex =
  match (res, ex) with
    | (_, ExTop) ->
        Some []

    | (RTuple comps1, ExTuple comps2) ->
        if List.length comps1 = List.length comps2 then
          List.map2 (uneval delta sigma hf) comps1 comps2
            |> Option2.sequence
            |> Option2.map Hole_constraints.merge_all
        else
          None

    | (RCtor (name1, arg1), ExCtor (name2, arg2)) ->
        if name1 = name2 then
          uneval delta sigma hf arg1 arg2
        else
          None

    | (RHole (env, hole_name), _) ->
        Some @@
          Hole_constraints.singleton hole_name (env, ex)

    | (RFix (_, _, _, _), ExInputOutput (input, output)) ->
        live_bidirectional_eval delta sigma hf
          (EApp (res, Res_util.value_to_res input))
          output

    | _ ->
        raise "TODO"

and
  live_bidirectional_eval delta sigma hf res ex =
    Option.bind (Result.to_option @@ Eval.resume hf res) @@ fun (r, rcs) ->
    Option.bind (uneval delta sigma hf r ex) @@ fun ks ->
      Hole_constraints.merge
        (simplify delta sigma rcs)
        ks

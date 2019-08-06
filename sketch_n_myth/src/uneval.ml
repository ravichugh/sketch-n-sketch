open Lang

let guesses
  (_delta : hole_ctx)
  (_sigma : datatype_ctx)
  (_r : res)
  : hole_filling Nondet.t =
    (* TODO call termgen *)
    Nondet.none

let rec simplify delta sigma rcs =
  let simplify_one (res, value) =
    if Res_util.final res then
      uneval
        delta
        sigma
        Hole_filling.empty
        res
        (Res_util.value_to_example value)
    else
      Nondet.none
  in
    rcs
      |> List.map simplify_one
      |> Nondet.one_of_each
      |> Nondet.map Hole_constraints.merge_all
      |> Nondet.collapse_option

and uneval delta sigma hf res ex =
  match (res, ex) with
    | (_, ExTop) ->
        Nondet.pure Hole_constraints.empty

    | (RTuple comps1, ExTuple comps2) ->
        if List.length comps1 = List.length comps2 then
          List.map2 (uneval delta sigma hf) comps1 comps2
            |> Nondet.one_of_each
            |> Nondet.map Hole_constraints.merge_all
            |> Nondet.collapse_option
        else
          Nondet.none

    | (RCtor (name1, arg1), ExCtor (name2, arg2)) ->
        if name1 = name2 then
          uneval delta sigma hf arg1 arg2
        else
          Nondet.none

    | (RHole (env, hole_name), _) ->
        Nondet.pure @@
          Hole_constraints.singleton hole_name (Unsolved [(env, ex)])

    | (RFix (_, _, _, _), ExInputOutput (input, output)) ->
        live_bidirectional_eval delta sigma hf
          (RApp (res, Res_util.value_to_res input))
          output

    | (RApp (r1, r2), _) ->
        begin match Res_util.res_to_value r2 with
          | Some v2 ->
              uneval delta sigma hf r1 @@
                ExInputOutput (v2, ex)

          | None ->
              Nondet.none
        end

    | (RProj (n, i, arg), _) ->
        uneval delta sigma hf arg @@
          ExTuple
            ( List2.repeat (i - 1) ExTop
                @ [ex]
                @ List2.repeat (n - i) ExTop
            )

    | (RCase (env, scrutinee, branches), _) ->
        Nondet.bind
          ( guesses delta sigma scrutinee
          ) @@ fun hf_guesses ->
        Nondet.bind
          ( Nondet.lift_option @@ Hole_filling.extend hf hf_guesses
          ) @@ fun hf' ->
        let
          ks_guesses =
            Hole_constraints.from_filling hf_guesses
        in
        Nondet.bind
          ( Nondet.lift_result @@ Eval.resume hf' scrutinee
          ) @@ fun (r_scrutinee, rcs_scrutinee) ->
        Nondet.bind
          ( simplify delta sigma rcs_scrutinee
          ) @@ fun ks_scrutinee ->
        let
          possible_ks_branch =
            begin match r_scrutinee with
              | RCtor (ctor_name, r_arg) ->
                  begin match List.assoc_opt ctor_name branches with
                    | Some (arg_name, body) ->
                        live_bidirectional_eval delta sigma hf'
                        ( RApp
                            ( RFix (env, None, arg_name, body)
                            , r_arg
                            )
                        )
                        ex

                    | None ->
                        Nondet.none
                  end

              | _ ->
                  Nondet.none
            end
        in
          Nondet.bind possible_ks_branch @@ fun ks_branch ->
          Nondet.lift_option @@
            Hole_constraints.merge_all
              [ks_guesses; ks_scrutinee; ks_branch]

    | _ ->
        Nondet.none

and
  live_bidirectional_eval delta sigma hf res ex =
    match Eval.resume hf res with
      | Ok (r, rcs) ->
          Nondet.bind (simplify delta sigma rcs) @@ fun ks1 ->
          Nondet.bind (uneval delta sigma hf r ex) @@ fun ks2 ->
          Nondet.lift_option @@
            Hole_constraints.merge ks1 ks2

      | Error _ ->
          Nondet.none

open Lang
open Nondet.Syntax

let minimal_uneval =
  ref true

(* Note: fuel gets applied at case expressions in non-minimal mode. *)
module FuelLimited = struct
  let rec check fuel delta sigma hf exp worlds =
    let
      check_one (env, ex) =
        match Eval.eval env exp with
          | Ok (r, []) ->
              begin match Eval.resume hf r with
                | Ok (r', []) ->
                    uneval fuel delta sigma hf r' ex

                | _ ->
                    Nondet.none
              end

          | _ ->
              Nondet.none
    in
      worlds
        |> List.map check_one
        |> Nondet.one_of_each
        |> Nondet.map Constraints.merge
        |> Nondet.collapse_option

  and uneval fuel delta sigma hf res ex =
    let rec blocking_hole (r : res) : hole_name option =
      match r with
        (* Determinate results *)

        | RFix (_, _, _, _)
        | RTuple _
        | RCtor (_, _) ->
            None

        (* Indeterminate results *)

        | RHole (_, hole_name) ->
            Some hole_name

        | RApp (head, _) ->
            blocking_hole head

        | RProj (_, _, arg) ->
            blocking_hole arg

        | RCase (_, scrutinee, _) ->
            blocking_hole scrutinee

        | RCtorInverse (_, arg) ->
            blocking_hole arg
    in
    let guesses
      (delta : hole_ctx)
      (sigma : datatype_ctx)
      (res : res)
      : hole_filling Nondet.t =
        let* hole_name =
          Nondet.lift_option @@
            blocking_hole res
        in
        let* (gamma, tau, dec, _) =
          Nondet.lift_option @@
            List.assoc_opt hole_name delta
        in
          Nondet.map
            (Hole_map.singleton hole_name)
            (Term_gen.up_to_e sigma 1 (gamma, tau, dec))
    in
    let* _ =
      Nondet.guard (fuel > 0)
    in
    let* _ =
      Nondet.guard @@
        Timer.Single.check Timer.Single.Total
    in
    match (res, ex) with
      | (_, ExTop) ->
          Nondet.pure Constraints.empty

      | (RTuple comps1, ExTuple comps2) ->
          if List.length comps1 = List.length comps2 then
            List.map2 (uneval fuel delta sigma hf) comps1 comps2
              |> Nondet.one_of_each
              |> Nondet.map Constraints.merge
              |> Nondet.collapse_option
          else
            Nondet.none

      | (RCtor (name1, arg1), ExCtor (name2, arg2)) ->
          if name1 = name2 then
            uneval fuel delta sigma hf arg1 arg2
          else
            Nondet.none

      | (RHole (env, hole_name), _) ->
          Nondet.pure @@
            Constraints.unsolved_singleton hole_name [(env, ex)]

      | (RFix (env, f, x, body), ExInputOutput (input, output)) ->
          let fix_extension =
            begin match f with
              | Some f_name ->
                  (f_name, res) :: env

              | None ->
                  env
            end
          in
            check fuel delta sigma hf body @@
              [((x, Res.from_value input) :: fix_extension, output)]

      | (RApp (r1, r2), _) ->
          begin match Res.to_value r2 with
            | Some v2 ->
                uneval fuel delta sigma hf r1 @@
                  ExInputOutput (v2, ex)

            | None ->
                Nondet.none
          end

      | (RProj (n, i, arg), _) ->
          uneval fuel delta sigma hf arg @@
            ExTuple
              ( List2.repeat (i - 1) ExTop
                  @ [ex]
                  @ List2.repeat (n - i) ExTop
              )

      | (RCase (env, scrutinee, branches), _) ->
          if !minimal_uneval then
            let* hf_guesses =
              guesses delta sigma scrutinee
            in
            let* hf' =
              Nondet.lift_option @@
                Constraints.merge_solved [hf; hf_guesses]
            in
            let
              ks_guesses =
                (hf_guesses, Hole_map.empty)
            in
            let* (r_scrutinee, rcs_scrutinee) =
              Nondet.lift_result @@ Eval.resume hf' scrutinee
            in
            let* ks_scrutinee =
              simplify_assertions fuel delta sigma rcs_scrutinee
            in
            let* ks_branch =
              begin match r_scrutinee with
                | RCtor (ctor_name, r_arg) ->
                    begin match List.assoc_opt ctor_name branches with
                      | Some (arg_name, body) ->
                          check fuel delta sigma hf' body @@
                            [((arg_name, r_arg) :: env, ex)]

                      | None ->
                          Nondet.none
                    end

                | _ ->
                    Nondet.none
              end
            in
              Nondet.lift_option @@
                Constraints.merge [ks_guesses; ks_scrutinee; ks_branch]
          else
            let try_branch (ctor_name, (arg_name, body)) =
              let* k1 =
                uneval (fuel - 1) delta sigma hf scrutinee
                  (ExCtor (ctor_name, ExTop))
              in
              let* k2 =
                check (fuel - 1) delta sigma hf body @@
                  [((arg_name, RCtorInverse (ctor_name, scrutinee)) :: env, ex)]
              in
                Nondet.lift_option @@
                  Constraints.merge [k1; k2]
            in
              branches
                |> Nondet.from_list
                |> Nondet.and_then try_branch

      | (RCtorInverse (name, arg), _) ->
          uneval fuel delta sigma hf arg (ExCtor (name, ex))

      | _ ->
          Nondet.none

  and simplify_assertions fuel delta sigma rcs =
    let simplify_one (res, value) =
      if Res.final res then
        uneval
          fuel
          delta
          sigma
          Hole_map.empty
          res
          (Example.from_value value)
      else
        Nondet.none
    in
      rcs
        |> List.map simplify_one
        |> Nondet.one_of_each
        |> Nondet.map Constraints.merge
        |> Nondet.collapse_option
end

let check delta sigma hf exp worlds =
  FuelLimited.check Params.uneval_case_budget delta sigma hf exp worlds

let uneval delta sigma hf res ex =
  FuelLimited.uneval Params.uneval_case_budget delta sigma hf res ex

let simplify_assertions delta sigma rcs =
  FuelLimited.simplify_assertions Params.uneval_case_budget delta sigma rcs

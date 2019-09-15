open Lang
open Nondet.Syntax

(* Core algorithm *)

let rec iter_solve params delta sigma ((hf, us_all), k_assumed) =
  Timing.check_cutoff
    ~max_time:Timing_constants.max_total
    ~initial_time:params.initial_time;
  match Constraints.delete_min us_all with
    | None ->
        let+ _ =
          Nondet.guard @@
            Constraints.satisfies hf k_assumed
        in
          (Constraints.from_hole_filling hf, delta)

    | Some ((hole_name, worlds), us) ->
        let* (gamma, typ, dec, match_depth) =
          Nondet.lift_option @@
            List.assoc_opt hole_name delta
        in
        let* (k_asserted', k_assumed', delta') =
          Fill.fill
            { params with
                max_match_depth = params.max_match_depth - match_depth
            }
            delta
            sigma
            hf
            (hole_name, ((gamma, typ, dec), worlds))
        in
        let* k_asserted_merged =
          Nondet.lift_option @@
            Constraints.merge [(hf, us); k_asserted']
        in
        let* k_assumed_merged =
          Nondet.lift_option @@
            Constraints.merge [k_assumed; k_assumed']
        in
        let delta_merged =
          delta' @ delta
        in
          iter_solve
            params
            delta_merged
            sigma
            (k_asserted_merged, k_assumed_merged)

(* Staging *)

type stage =
  | One
  | Two
  | Three
  | Four
  | Five

let all_stages : stage list =
  [ One; Two; Three; Four; Five ]

let expand_stages (xs : 'a list) : (stage * 'a) list =
  List2.concat_map
    (fun s -> List.map (fun x -> (s, x)) xs)
    all_stages

let solve_any initial_time delta sigma constraints_nd =
  let rec helper problems =
    match problems with
      | [] ->
          Nondet.none

      | (stage, constraints) :: rest_problems ->
          let (max_scrutinee_size, max_match_depth, max_term_size) =
            match stage with
              | One ->
                  (1, 0, 10)

              | Two ->
                  (1, 1, 10)

              | Three ->
                  (1, 2, 10)

              | Four ->
                  (6, 2, 10)

              | Five ->
                  (6, 3, 10)
          in
          let params =
            { initial_time; max_scrutinee_size; max_match_depth; max_term_size }
          in
          let solution_nd =
            Nondet.map (Pair2.map_fst fst) @@
              iter_solve params delta sigma (constraints, Constraints.empty)
          in
            if Nondet.is_empty solution_nd then
              helper rest_problems
            else
              solution_nd
  in
    constraints_nd
      |> Nondet.to_list
      |> expand_stages
      |> helper

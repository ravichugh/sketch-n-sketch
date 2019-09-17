open Lang
open Nondet.Syntax

let current_solution_count =
  ref 0

let should_continue () =
  let timer_check =
    Timer.Single.check Timer.Single.Total
  in
  let count_check =
    match Params.max_solution_count with
      | Some n ->
          !current_solution_count < n

      | None ->
          true
  in
    timer_check && count_check

(* Core algorithm *)

let rec iter_solve params delta sigma ((hf, us_all), k_assumed) =
  let* _ =
    Nondet.guard @@
      should_continue ()
  in
  match Constraints.delete_min us_all with
    | None ->
        let+ _ =
          Nondet.guard @@
            Constraints.satisfies hf k_assumed
        in
          current_solution_count := !current_solution_count + 1;
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
  | PreTwo
  | Two
  | PreThree
  | Three
  | PreFour
  | Four
  | PreFive
  | Five

let all_stages : stage list =
  [ One; PreTwo; Two; PreThree; Three; PreFour; Four; PreFive; Five ]

let expand_stages (xs : 'a list) : (stage * 'a) list =
  List2.concat_map
    (fun s -> List.map (fun x -> (s, x)) xs)
    all_stages

let solve_any delta sigma constraints_nd =
  let rec helper problems =
    match problems with
      | [] ->
          Nondet.none

      | (stage, constraints) :: rest_problems ->
          let (max_scrutinee_size, max_match_depth, max_term_size) =
            match stage with
              | One ->
                  (1, 0, 13)

              | PreTwo ->
                  (1, 1, 10)

              | Two ->
                  (1, 1, 13)

              | PreThree ->
                  (1, 2, 10)

              | Three ->
                  (1, 2, 13)

              | PreFour ->
                  (6, 2, 10)

              | Four ->
                  (6, 2, 13)

              | PreFive ->
                  (6, 3, 10)

              | Five ->
                  (6, 3, 13)
          in
          let params =
            { max_scrutinee_size; max_match_depth; max_term_size }
          in
          current_solution_count := 0;
          Timer.Multi.reset Timer.Multi.Guess;
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

open Lang
open Nondet.Syntax

(* Core algorithm *)

let step params sigma acc (hole_name, worlds) =
  let _ = Debug.readln () in
  let* ((f_previous, us_previous), delta_previous) =
    acc
  in
  let* (gamma, typ, dec, match_depth) =
    Nondet.lift_option @@
      List.assoc_opt hole_name delta_previous
  in
  Debug.println
    "--------------------------------------------------------------------------------";
  Debug.println @@
      "stepping on ??"
      ^ string_of_int hole_name
      ^ " ("
      ^ string_of_int params.max_scrutinee_size
      ^ ", "
      ^ string_of_int params.max_match_depth
      ^ ", "
      ^ string_of_int params.max_term_size
      ^ ")";
  let fill_goal =
    (hole_name, ((gamma, typ, dec), worlds))
  in
  Debug.println "\ntype:";
  Debug.print_typ typ;
  Debug.println "\nprevious:";
  Debug.print_hf f_previous;
  Debug.println "";
  let* ((f_new, us_new), delta_new) =
    Fill.fill
      { params with max_match_depth = params.max_match_depth - match_depth }
      delta_previous
      sigma
      f_previous
      fill_goal
  in
  Debug.println "\nnew:";
  Debug.print_hf f_new;
  Debug.println "";
  let+ f_merged =
    Nondet.lift_option @@
      Constraints.merge_solved [f_previous; f_new]
  in
  Debug.println "merged:";
  Debug.print_hf f_merged;
  Debug.println "";
  let us_merged =
    Constraints.merge_unsolved [us_previous; us_new]
  in
  let delta_merged =
    delta_new @ delta_previous
  in
    ((f_merged, us_merged), delta_merged)

let rec solve params delta sigma (f0, unsolved_constraints) =
  if Hole_map.is_empty unsolved_constraints then
    Nondet.pure (f0, delta)
  else
    (* Here is where we choose an order arbitrarily *)
    let unsolved_bindings =
      Hole_map.bindings unsolved_constraints
    in
    let* (k_final, delta_final) =
      List.fold_left
        (step params sigma)
        (Nondet.pure (Constraints.from_hole_filling f0, delta))
        unsolved_bindings
    in
      solve params delta_final sigma k_final

(* Staging *)

type stage =
  | One
  | Two
  | Three
  | Four
  | Five

let next_stage (stage : stage) : stage option =
  match stage with
    | One ->
        Some Two

    | Two ->
        None (* Some Three *)

    | Three ->
        Some Four

    | Four ->
        Some Five

    | Five ->
        None

let staged_solve delta sigma constraints =
  let rec helper stage_opt =
    let* stage =
      Nondet.lift_option stage_opt
    in
    let (max_scrutinee_size, max_match_depth, max_term_size) =
      match stage with
        | One ->
            (1, 0, 13)

        | Two ->
            (1, 1, 13)

        | Three ->
            (1, 2, 13)

        | Four ->
            (6, 2, 13)

        | Five ->
            (6, 3, 13)
    in
    let params =
      { max_scrutinee_size; max_match_depth; max_term_size }
    in
    let solution_nd =
      solve params delta sigma constraints
    in
      if Nondet.is_empty solution_nd then
        helper (next_stage stage)
      else
        solution_nd
  in
    helper (Some One)

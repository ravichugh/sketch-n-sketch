open Lang
open Nondet.Syntax

let step params sigma acc (hole_name, worlds) =
  let* ((f_previous, us_previous), delta_previous) =
    acc
  in
  let* (gamma, typ, dec) =
    Nondet.lift_option @@
      List.assoc_opt hole_name delta_previous
  in
  let fill_goal =
    (hole_name, ((gamma, typ, dec), worlds))
  in
  let* ((f_new, us_new), delta_new) =
    Fill.fill params delta_previous sigma f_previous fill_goal
  in
  let+ f_merged =
    Nondet.lift_option @@
      Constraints.merge_solved [f_previous; f_new]
  in
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

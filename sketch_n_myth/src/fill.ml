open Lang
open Nondet.Syntax

let refine_or_branch params delta sigma _hf (hole_name, synthesis_goal) =
  let+ (exp, subgoals) =
    Nondet.union
      [ Nondet.lift_option @@
          Refine.refine
            delta
            sigma
            synthesis_goal
      ; if params.max_match_depth > 0 then
          Branch.branch
            params.max_scrutinee_size
            delta
            sigma
            synthesis_goal
        else
          Nondet.none
      ]
  in
  let delta' =
    List.map (Pair2.map_snd fst) subgoals
  in
  let solved_constraints =
    Hole_map.singleton hole_name exp
  in
  let unsolved_constraints =
    subgoals
      |> List.map
           ( fun (hole_name, (_, worlds)) ->
               Hole_map.singleton hole_name worlds
           )
      |> Constraints.merge_unsolved
  in
    ( Constraints.from_both
        solved_constraints
        unsolved_constraints
    , delta'
    )

let guess_and_check params delta sigma hf (hole_name, (gen_goal, worlds)) =
  let* exp =
    Term_gen.up_to_e sigma params.max_term_size gen_goal
  in
  let binding =
    Hole_map.singleton hole_name exp
  in
  let* extended_hf =
    Nondet.lift_option @@
      Constraints.merge_solved [binding; hf]
  in
  let* constraints =
    Uneval.check delta sigma extended_hf exp worlds
  in
    Nondet.lift_option @@
      Option2.sequence_fst
        ( Constraints.merge
            [ Constraints.from_hole_filling binding
            ; constraints
            ]
        , []
        )

let defer _params _delta _sigma _hf (hole_name, (_gen_goal, worlds)) =
  if
    List.length worlds > 0
      && List.for_all (fun (_, ex) -> ex = ExTop) worlds
  then
    Nondet.pure
      ( Constraints.solved_singleton hole_name (EHole hole_name)
      , []
      )
  else
    Nondet.none

let fill params delta sigma hf fill_goal =
  Nondet.union @@
    List.map
      (fun rule -> rule params delta sigma hf fill_goal)
      [ refine_or_branch; guess_and_check; defer ]

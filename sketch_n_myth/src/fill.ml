open Lang

type params =
  { max_scrutinee_size : int
  ; max_match_depth : int
  ; max_term_size : int
  }

let refine_branch params delta sigma hf goal =
  let refinement_nd =
    Refine.refine delta sigma goal
      |> Nondet.lift_option
  in
  let branch_nd =
    if params.max_match_depth > 0 then
      Branch.branch
        { max_scrutinee_size = params.max_scrutinee_size }
        delta
        sigma
        goal
    else
      Nondet.none
  in
  Nondet.bind
    ( refinement_nd :: branch_nd
    ) @@ fun (exp, subgoals) ->
  let new_params =
    match exp with
      | ECase (_, _) ->
          { params with max_match_depth = params.max_match_depth - 1 }

      | _ ->
          params
  in
  let delta' =
    List.map
      ( fun { gamma; hole_name; goal_type } ->
        (hole_name, (gamma, goal_Type, NoSpec))
      )
      subgoals
  in
  let new_delta =
    delta' @ delta
  in
  let child_nd =
    subgoals
      (* TODO what about updating hf? *)
      |> List.map (fill new_params new_delta sigma hf)
      |> Nondet.one_of_each
      |> Nondet.map
           ( List.split
               >> Pair2.map_left Constraints.merge
               >> Pair2.map_right List.concat
               >> Option2.sequence_left
           )
      |> Nondet.collapse_option
  in
  Nondet.bind child_nd @@ fun (child_constraints, child_delta) ->
  Nondet.lift_option @@
    Option2.sequence_left @@
      ( Constraints.merge
          [ Constraints.solved_singleton goal.hole_name exp
          ; child_constraints
          ]
      , child_delta @ delta'
      )

let fill params delta sigma hf goal =
  raise Exit

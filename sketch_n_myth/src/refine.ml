open Lang

let filter (ws : worlds) : worlds =
  List.filter (fun (_env, ex) -> ex <> ExTop) ws

let refine _delta sigma ({ gamma; goal_type; } as g : synthesis_goal) =
  let open Option2.Syntax in
  let
    filtered_worlds =
      filter g.worlds
  in
    match goal_type with
      (* Refine-Fix *)

      | TArr (tau1, tau2) ->
          let hole_name =
            Fresh.gen_hole ()
          in
          let f_name =
            Term_gen.fresh_ident
              gamma
              Term_gen.function_char
          in
          let x_name =
            Term_gen.fresh_ident
              gamma
              Term_gen.function_char
          in
          let+ refined_worlds =
            filtered_worlds
              |> List.map
                   ( fun (env, io_ex) ->
                       match io_ex with
                         | ExInputOutput (v, ex) ->
                             Some
                               ( (x_name, Res.value_to_res v)
                                   :: ( f_name
                                      , RFix
                                          ( env
                                          , Some f_name
                                          , x_name
                                          , EHole hole_name
                                          )
                                      )
                                   :: env
                               , ex
                               )

                         | _ ->
                             None
                   )
              |> Option2.sequence
          in
          let new_goal =
            { gamma =
                (f_name, (TArr (tau1, tau2), Rec f_name))
                  :: (x_name, (tau1, Arg f_name))
                  :: gamma
            ; hole_name
            ; goal_type =
                tau2
            ; worlds =
                refined_worlds
            }
          in
          let exp =
            EFix (Some f_name, x_name, EHole hole_name)
          in
            (exp, [new_goal])

      (* Refine-Tuple *)

      | TTuple taus ->
          let* refined_worldss =
            filtered_worlds
              |> List.map
                   ( fun (env, tuple_ex) ->
                       match tuple_ex with
                         | ExTuple exs ->
                             Some (List.map (fun ex -> (env, ex)) exs)

                         | _ ->
                             None
                   )
              |> Option2.sequence
              |> Option2.map List2.transpose
          in
            if List.length refined_worldss <> List.length taus then
              None
            else
              let new_goals =
                List.map2
                  ( fun tau refined_worlds ->
                      { gamma
                      ; hole_name = Fresh.gen_hole ()
                      ; goal_type = tau
                      ; worlds = refined_worlds
                      }
                  )
                  taus
                  refined_worldss
              in
              let exp =
                ETuple
                  ( List.map
                      (fun { hole_name; _ } -> EHole hole_name)
                      new_goals
                  )
              in
                Some (exp, new_goals)

      (* Refine-Ctor *)

      | TData datatype_name ->
          let* datatype_ctors =
            List.assoc_opt datatype_name sigma
          in
          let* (ctor_name, refined_worlds) =
            filtered_worlds
              |> List.map
                   ( fun (env, ctor_ex) ->
                       match ctor_ex with
                         | ExCtor (ctor_name, arg_ex) ->
                             Some (ctor_name, (env, arg_ex))

                         | _ ->
                             None
                   )
              |> Option2.sequence
              |> Option2.and_then
                   ( List.split
                       >> Pair2.map_left List2.collapse_equal
                       >> Option2.sequence_left
                   )
          in
          let* arg_type =
            List.assoc_opt ctor_name datatype_ctors
          in
          let+ new_goal =
            { gamma
            ; hole_name = Fresh.gen_hole ()
            ; goal_type = arg_type
            ; worlds = refined_worlds
            }
          in
          let exp =
            ECtor
              ( ctor_name
              , EHole new_goal.hole_name
              )
          in
            (exp, [new_goal])

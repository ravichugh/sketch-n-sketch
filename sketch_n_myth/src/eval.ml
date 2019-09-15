open Lang

type eval_result =
  (res * resumption_assertions, string) result

type eval_env_result =
  (env * resumption_assertions, string) result

(* Note: fuel gets applied at every application. *)
module FuelLimited = struct
  let rec eval initial_time fuel env exp =
    let open Result2.Syntax in
    Timing.check_cutoff
      ~max_time:Timing_constants.max_eval
      ~initial_time:initial_time;
    let* _ =
      Result2.guard "Ran out of fuel" (fuel > 0)
    in
    match exp with
      | EFix (f, x, body) ->
          Ok
            ( RFix (env, f, x, body)
            , []
            )

      | EVar x ->
          begin match List.assoc_opt x env with
            | None ->
                Error ("Variable not found: " ^ x)

            | Some r ->
                Ok (r, [])
          end

      | EHole name ->
          Ok
            ( RHole (env, name)
            , []
            )

      | ETuple comps ->
          comps
            |> List.map (eval initial_time fuel env)
            |> Result2.sequence
            |> Result2.map
                 begin fun evals -> evals
                   |> List.split
                   |> Pair2.map_fst (fun rs -> RTuple rs)
                   |> Pair2.map_snd List.concat
                 end

      | ECtor (name, arg) ->
          let+ (r, ks) =
            eval initial_time fuel env arg
          in
            ( RCtor (name, r)
            , ks
            )

      | EApp (_, e1, e2) ->
          let* (r1, ks1) =
            eval initial_time fuel env e1
          in
          let* (r2, ks2) =
            eval initial_time fuel env e2
          in
            begin match r1 with
              | RFix (f_env, f, x, body) ->
                  let
                    f_env_extension =
                      begin match f with
                        | Some f_name ->
                            [(f_name, r1)]

                        | None ->
                            []
                      end
                  in
                  let x_env_extension =
                    if Type.ignore_binding x then
                      []
                    else
                      [(x, r2)]
                  in
                  let new_env =
                    x_env_extension @ f_env_extension @ f_env
                  in
                    Result2.map
                      (Pair2.map_snd @@ fun ks3 -> ks1 @ ks2 @ ks3)
                      (eval initial_time (fuel - 1) new_env body)

              | _ ->
                  Ok
                    ( RApp (r1, r2)
                    , ks1 @ ks2
                    )
            end

      | EProj (n, i, arg) ->
          if i <= 0 then
            Error "Non-positive projection index"
          else if i > n then
            Error "Projection index greater than projection length"
          else
            let* (r_arg, ks_arg) =
              eval initial_time fuel env arg
            in
              begin match r_arg with
                | RTuple comps ->
                    if n <> List.length comps then
                      Error "Projection length does not match tuple size"
                    else (* 0 < i <= n = |comps| *)
                      Ok
                        ( List.nth comps (i - 1)
                        , ks_arg
                        )

                | _ ->
                    Ok
                      ( RProj (n, i, r_arg)
                      , ks_arg
                      )
              end

      | ECase (scrutinee, branches) ->
          let* (r0, ks0) =
            eval initial_time fuel env scrutinee
          in
            begin match r0 with
              | RCtor (ctor_name, r_arg) ->
                  begin match List.assoc_opt ctor_name branches with
                    | Some (arg_name, body) ->
                        Result2.map
                          ( Pair2.map_snd @@
                              fun ks_body -> ks0 @ ks_body
                          )
                          ( eval initial_time fuel
                              ((arg_name, r_arg) :: env)
                              body
                          )

                    | None ->
                        Error
                          ( "Non-exhaustive pattern match, "
                          ^ "could not find constructor '"
                          ^ ctor_name
                          ^ "'"
                          )
                  end

              | _ ->
                  Ok
                    ( RCase (env, r0, branches)
                    , ks0
                    )
            end

      | EAssert (e1, e2) ->
          let* (r1, ks1) =
            eval initial_time fuel env e1
          in
          let* (r2, ks2) =
            eval initial_time fuel env e2
          in
            begin match Res.consistent r1 r2 with
              | Some ks3 ->
                  Ok
                    ( RTuple []
                    , ks1 @ ks2 @ ks3
                    )

              | None ->
                  Error "Result consistency failure"
            end

  let rec resume initial_time fuel hf res =
    let open Result2.Syntax in
    match res with
      | RHole (env, name) ->
          begin match Hole_map.find_opt name hf with
            | Some binding ->
                let* (r, ks) =
                  eval initial_time fuel env binding
                in
                let+ (r', ks') =
                  resume initial_time fuel hf r
                in
                  ( r'
                  , ks @ ks'
                  )

            | None ->
                let+ (env', ks) =
                  resume_env initial_time fuel hf env
                in
                  ( RHole (env', name)
                  , ks
                  )
          end

      | RFix (env, f, x, body) ->
          let+ (env', ks) =
            resume_env initial_time fuel hf env
          in
            ( RFix (env', f, x, body)
            , ks
            )

      | RTuple comps ->
          comps
            |> List.map (resume initial_time fuel hf)
            |> Result2.sequence
            |> Result2.map
                 begin fun rs -> rs
                   |> List.split
                   |> Pair2.map_fst (fun rs -> RTuple rs)
                   |> Pair2.map_snd List.concat
                 end

      | RCtor (name, arg) ->
          let+ (arg', ks) =
            resume initial_time fuel hf arg
          in
            ( RCtor (name, arg')
            , ks
            )

      | RApp (r1, r2) ->
          let* (r1', ks1) =
            resume initial_time fuel hf r1
          in
          let* (r2', ks2) =
            resume initial_time fuel hf r2
          in
            begin match r1' with
              | RFix (f_env, f, x, body) ->
                  let
                    f_env_extension =
                      begin match f with
                        | Some f_name ->
                            [(f_name, r1')]

                        | None ->
                            []
                      end
                  in
                  let x_env_extension =
                    if Type.ignore_binding x then
                      []
                    else
                      [(x, r2')]
                  in
                  let new_env =
                    x_env_extension @ f_env_extension @ f_env
                  in
                  let* (r, ks) =
                    eval initial_time (fuel - 1) new_env body
                  in
                  let+ (r', ks') =
                    resume initial_time (fuel - 1) hf r
                  in
                    ( r'
                    , ks1 @ ks2 @ ks @ ks'
                    )

              | _ ->
                  Ok
                    ( RApp (r1', r2')
                    , ks1 @ ks2
                    )
            end

      | RProj (n, i, arg) ->
          if i <= 0 then
            Error "Non-positive projection index"
          else if i > n then
            Error "Projection index greater than projection length"
          else
            let* (arg', ks_arg) =
              resume initial_time fuel hf arg
            in
              begin match arg' with
                | RTuple comps ->
                    if n <> List.length comps then
                      Error "Projection length does not match tuple size"
                    else (* 0 < i <= n = |comps| *)
                      Ok
                        ( List.nth comps (i - 1)
                        , ks_arg
                        )

                | _ ->
                    Ok
                      ( RProj (n, i, arg')
                      , ks_arg
                      )
              end

      | RCase (env, scrutinee, branches) ->
          let* (r0, ks0) =
            resume initial_time fuel hf scrutinee
          in
            begin match r0 with
              | RCtor (ctor_name, r_arg) ->
                  begin match List.assoc_opt ctor_name branches with
                    | Some (arg_name, body) ->
                        Result2.map
                          ( Pair2.map_snd @@
                              fun ks_body -> ks0 @ ks_body
                          )
                          ( resume initial_time fuel hf @@
                              RApp
                                ( RFix (env, None, arg_name, body)
                                , r_arg
                                )
                          )

                    | None ->
                        Error
                          ( "Non-exhaustive pattern match, "
                          ^ "could not find constructor '"
                          ^ ctor_name
                          ^ "'"
                          )
                  end

              | _ ->
                  let+ (env', ks_env) =
                    resume_env initial_time fuel hf env
                  in
                    ( RCase (env', r0, branches)
                    , ks0 @ ks_env
                    )
            end

  and resume_env initial_time fuel hf env : eval_env_result =
    env
      |> List.map
           begin fun binding -> binding
             |> Pair2.map_snd (resume initial_time fuel hf)
             |> Pair2.lift_snd_result
           end
      |> Result2.sequence
      |> Result2.map
           begin fun binding -> binding
             |> List.map (fun (x, (r, ks)) -> ((x, r), ks))
             |> List.split
             |> Pair2.map_snd List.concat
           end
end

let eval env exp =
  try
    FuelLimited.eval (Timing.get ()) 25 env exp
  with
    Timing.Time_exceeded ->
      Log.warn "Evaluation time exceeded";
      Error "Evaluation time exceeded"

let resume hf res =
  try
    FuelLimited.resume (Timing.get ()) 25 hf res
  with
    Timing.Time_exceeded ->
      Log.warn "Resumption time exceeded";
      Error "Resumption time exceeded"

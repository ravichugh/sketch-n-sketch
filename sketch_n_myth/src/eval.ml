open Lang

type eval_result =
  (res * resumption_assertions, string) result

type eval_env_result =
  (env * resumption_assertions, string) result

let ignore_binding (s : string) : bool =
  String.length s > 0 && Char.equal (String.get s 0) '_'

let rec eval env exp =
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
          |> List.map (eval env)
          |> Result2.sequence
          |> Result2.map
               begin fun evals -> evals
                 |> List.split
                 |> Pair2.map_left (fun rs -> RTuple rs)
                 |> Pair2.map_right List.concat
               end

    | ECtor (name, arg) ->
        Result2.pure_bind (eval env arg) @@ fun (r, ks) ->
        ( RCtor (name, r)
        , ks
        )

    | EApp (e1, e2) ->
        Result2.bind (eval env e1) @@ fun (r1, ks1) ->
        Result2.bind (eval env e2) @@ fun (r2, ks2) ->
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
                if ignore_binding x then
                  []
                else
                  [(x, r2)]
              in
              let new_env =
                x_env_extension @ f_env_extension @ f_env
              in
                Result2.map
                  (Pair2.map_right @@ fun ks3 -> ks1 @ ks2 @ ks3)
                  (eval new_env body)

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
          Result2.bind (eval env arg) @@ fun (r_arg, ks_arg) ->
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
        Result2.bind (eval env scrutinee) @@ fun (r0, ks0) ->
        begin match r0 with
          | RCtor (ctor_name, r_arg) ->
              begin match List.assoc_opt ctor_name branches with
                | Some (arg_name, body) ->
                    Result2.map
                      ( Pair2.map_right @@
                          fun ks_body -> ks0 @ ks_body
                      )
                      ( eval
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
        Result2.bind (eval env e1) @@ fun (r1, ks1) ->
        Result2.bind (eval env e2) @@ fun (r2, ks2) ->
        begin match Res.consistent r1 r2 with
          | Some ks3 ->
              Ok
                ( RTuple []
                , ks1 @ ks2 @ ks3
                )

          | None ->
              Error "Result consistency failure"
        end

let rec resume hf res =
  match res with
    | RHole (env, name) ->
        begin match Hole_map.find_opt name hf with
          | Some binding ->
              Result2.bind (eval env binding) @@ fun (r, ks) ->
              Result2.pure_bind (resume hf r) @@ fun (r', ks') ->
              ( r'
              , ks @ ks'
              )

          | None ->
              Result2.pure_bind (resume_env hf env) @@ fun (env', ks) ->
              ( RHole (env', name)
              , ks
              )
        end

    | RFix (env, f, x, body) ->
        Result2.pure_bind (resume_env hf env) @@ fun (env', ks) ->
        ( RFix (env', f, x, body)
        , ks
        )

    | RTuple comps ->
        comps
          |> List.map (resume hf)
          |> Result2.sequence
          |> Result2.map
               begin fun rs -> rs
                 |> List.split
                 |> Pair2.map_left (fun rs -> RTuple rs)
                 |> Pair2.map_right List.concat
               end

    | RCtor (name, arg) ->
        Result2.pure_bind (resume hf arg) @@ fun (arg', ks) ->
        ( RCtor (name, arg')
        , ks
        )

    | RApp (r1, r2) ->
        Result2.bind (resume hf r1) @@ fun (r1', ks1) ->
        Result2.bind (resume hf r2) @@ fun (r2', ks2) ->
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
                if ignore_binding x then
                  []
                else
                  [(x, r2')]
              in
              let new_env =
                x_env_extension @ f_env_extension @ f_env
              in
              Result2.bind (eval new_env body) @@ fun (r, ks) ->
              Result2.pure_bind (resume hf r) @@ fun (r', ks') ->
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
          Result2.bind (resume hf arg) @@ fun (arg', ks_arg) ->
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
        Result2.bind (resume hf scrutinee) @@ fun (r0, ks0) ->
        begin match r0 with
          | RCtor (ctor_name, r_arg) ->
              begin match List.assoc_opt ctor_name branches with
                | Some (arg_name, body) ->
                    Result2.map
                      ( Pair2.map_right @@
                          fun ks_body -> ks0 @ ks_body
                      )
                      ( resume hf @@
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
              Result2.pure_bind (resume_env hf env) @@ fun (env', ks_env) ->
              ( RCase (env', r0, branches)
              , ks0 @ ks_env
              )
        end

and resume_env (hf : hole_filling) (env : env) : eval_env_result =
  env
    |> List.map
         begin fun binding -> binding
           |> Pair2.map_right (resume hf)
           |> Pair2.lift_right_result
         end
    |> Result2.sequence
    |> Result2.map
         begin fun binding -> binding
           |> List.map (fun (x, (r, ks)) -> ((x, r), ks))
           |> List.split
           |> Pair2.map_right List.concat
         end

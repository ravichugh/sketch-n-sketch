open Lang

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
        arg
          |> eval env
          |> Result2.map
               (Pair2.map_left @@ fun x -> RCtor (name, x))

    | EApp (e1, e2) ->
        Result2.bind (eval env e1) @@ fun (r1, ks1) ->
        Result2.bind (eval env e2) @@ fun (r2, ks2) ->
        begin match r1 with
          | RFix (f_env, f, x, body) ->
              Result2.map
                ( Pair2.map_right @@
                    fun ks3 -> ks1 @ ks2 @ ks3
                )
                ( eval
                    ((x, r2) :: (f, r1) :: f_env)
                    body
                )
          | r1 ->
              Ok
                ( RApp (r1, r2)
                , ks1 @ ks2
                )
        end

    | EProj (i, arg) ->
        if i < 0 then
          Error "Negative tuple projection index"

        else
          Result2.bind (eval env arg) @@ fun (r_arg, ks_arg) ->
          begin match r_arg with
            | RTuple comps ->
                begin match List.nth_opt comps i with
                  | Some ri ->
                      Ok (ri, ks_arg)

                  | None ->
                      Error "Projection index greater than tuple size"
                end

            | r ->
                Ok
                  ( RProj (i, r)
                  , ks_arg
                  )
          end

    | ECase (scrutinee, branches) ->
        Result2.bind (eval env scrutinee) @@ fun (r0, ks0) ->
        begin match r0 with
          | RCtor (name, r_arg) ->
              begin match List.assoc_opt name branches with
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
                      ^ name
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
        begin match Res_consistency.check r1 r2 with
          | Some ks3 ->
              Ok
                ( RTuple []
                , ks1 @ ks2 @ ks3
                )

          | None ->
              Error "Result consistency failure"
        end

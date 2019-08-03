open Lang

let eval (env : env) (exp : exp) : (res * res_constraints, string) result =
  match exp with
    | EFix (f, x, body) ->
        Ok
          ( RFix (env, f, x, body)
          , []
          )

    | EVar x ->
        begin match List.assoc_opt x env with
          | None ->
              Error ("variable not found: " ^ x)

          | Some r ->
              Ok (r, [])
        end

    | EHole name ->
        Ok
          ( RHole (env, name)
          , []
          )

    | ETuple comps ->
        Ok
          ( RTuple (List.map (eval env) comps)
          , []
          )

    | _ ->
        Error "unimplemented"

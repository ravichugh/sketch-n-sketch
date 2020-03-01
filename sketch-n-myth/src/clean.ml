open Lang

let propagate (hf : hole_filling) : hole_filling =
  let rec propagate_exp exp =
    match exp with
      (* Main case *)

      | EHole hole_name ->
          begin match Hole_map.find_opt hole_name hf with
            | Some hole_exp ->
                propagate_exp hole_exp

            | None ->
                EHole hole_name
          end

      (* Other cases *)

      | EFix (f, x, body) ->
          EFix (f, x, propagate_exp body)

      | EApp (special, e1, e2) ->
          EApp (special, propagate_exp e1, propagate_exp e2)

      | EVar x ->
          EVar x

      | ETuple components ->
          ETuple (List.map propagate_exp components)

      | EProj (n, i, arg) ->
          EProj (n, i, propagate_exp arg)

      | ECtor (ctor_name, arg) ->
          ECtor (ctor_name, propagate_exp arg)

      | ECase (scrutinee, branches) ->
          ECase
            ( propagate_exp scrutinee
            , List.map (Pair2.map_snd (Pair2.map_snd propagate_exp)) branches
            )

      | EAssert (e1, e2) ->
          EAssert (propagate_exp e1, propagate_exp e2)
  in
    Hole_map.map propagate_exp hf

let restrict
 (delta : hole_ctx) (hf : hole_filling) : (hole_name * exp) list option =
  delta
    |> List.map
         ( fun (hole_name, _) ->
             Option2.sequence_snd (hole_name, Hole_map.find_opt hole_name hf)
         )
    |> Option2.sequence

let clean delta =
  propagate >> restrict delta

open Lang

let val_typ pv =
  match pv with
    | PVInt _ ->
        PTInt

    | PVString _ ->
        PTString

let op_typ po =
  let tint =
    TPrim PTInt
  in
    match po with
      | POPlus ->
          TArr (TTuple [tint; tint], tint)

      | POMinus ->
          TArr (TTuple [tint; tint], tint)

      | POInc ->
          TArr (tint, tint)

      | PODec ->
          TArr (tint, tint)

      | PODiv2 ->
          TArr (TTuple [tint; tint], tint)

let val_equal pv1 pv2 =
  match (pv1, pv2) with
    | (PVInt n1, PVInt n2) ->
        Int.equal n1 n2

    | (PVString s1, PVString s2) ->
        String.equal s1 s2

    | (PVInt _, _)
    | (PVString _, _) ->
        false

let typ_equal pt1 pt2 =
  match (pt1, pt2) with
    | (PTInt, PTInt) ->
        true

    | (PTString, PTString) ->
        true

    | (PTInt, _)
    | (PTString, _) ->
        false

let op_equal po1 po2 =
  match (po1, po2) with
  | (POPlus, POPlus) ->
      true

  | (POMinus, POMinus) ->
      true

  | (POInc, POInc) ->
      true

  | (PODec, PODec) ->
      true

  | (PODiv2, PODiv2) ->
      true

  | _ ->
      false

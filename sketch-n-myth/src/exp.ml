open Lang

let rec syntactically_equal e1 e2 =
  match (e1, e2) with
    | (EFix (mf1, x1, body1), EFix (mf2, x2, body2)) ->
        let mf_equal =
          begin match (mf1, mf2) with
            | (Some f1, Some f2) ->
                String.equal f1 f2

            | (None, None) ->
                true

            | _ ->
                false
          end
        in
          mf_equal
            && String.equal x1 x2
            && syntactically_equal body1 body2

    | (EApp (b1, head1, arg1), EApp (b2, head2, arg2)) ->
        Bool.equal b1 b2
          && syntactically_equal head1 head2
          && syntactically_equal arg1 arg2

    | (EVar x1, EVar x2) ->
        String.equal x1 x2

    | (ETuple es1, ETuple es2) ->
        Int.equal (List.length es1) (List.length es2)
          && List.for_all2 syntactically_equal es1 es2

    | (EProj (n1, i1, arg1), EProj (n2, i2, arg2)) ->
        Int.equal n1 n2
          && Int.equal i1 i2
          && syntactically_equal arg1 arg2

    | (ECtor (name1, arg1), ECtor (name2, arg2)) ->
        String.equal name1 name2
          && syntactically_equal arg1 arg2

    | (ECase (s1, branches1), ECase (s2, branches2)) ->
        syntactically_equal s1 s2
          && Int.equal (List.length branches1) (List.length branches2)
          && List.for_all2
               ( fun (ctor1, (arg1, body1)) (ctor2, (arg2, body2)) ->
                   String.equal ctor1 ctor2
                     && String.equal arg1 arg2
                     && syntactically_equal body1 body2
               )
               branches1
               branches2

    | (EHole name1, EHole name2) ->
        Int.equal name1 name2

    | (EAssert (left1, right1), EAssert (left2, right2)) ->
        syntactically_equal left1 left2
          && syntactically_equal right1 right2

    | _ ->
        false

listAppendSimple xs ys =
  case xs of
    []    -> ys
    x::xs -> x :: listAppendSimple xs ys

listAppendLens = {
  apply (xs, ys) =
    Update.freeze (listAppendSimple xs ys)

  update {input = (xs,ys), outputOld, outputNew} =

    -- consLefts, consRights : a -> List (List a, List a) -> List (List a, List a)
    let consLefts v list = List.simpleMap (Tuple.mapFirst (List.cons v)) list in
    let consRights v list = List.simpleMap (Tuple.mapSecond (List.cons v)) list in

    letrec walk insertLeft diffOps xs ys acc =
      case diffOps of
        [] ->
          case (xs, ys) of
            ([], []) -> acc

        KeepValue::diffOps ->
          case (xs, ys) of
            (x::xs, _) -> consLefts x (walk True diffOps xs ys acc)
            ([], y::ys) -> consRights y (walk False diffOps xs ys acc)

        DeleteValue::diffOps ->
          case (xs, ys) of
            (_::xs, _) -> walk True diffOps xs ys acc
            ([], _::ys) -> walk False diffOps [] ys acc

        (UpdateValue v)::diffOps ->
          case (xs, ys) of
            (_::xs, _) -> consLefts v (walk True diffOps xs ys acc)
            ([], _::ys) -> consRights v (walk False diffOps [] ys acc)

        (InsertValue v)::diffOps ->
          case (xs, ys) of
            (_::_, _) -> consLefts v (walk True diffOps xs ys acc)
            ([], _) ->
              if insertLeft then
                listAppendSimple
                  (consLefts v (walk True diffOps [] ys acc))
                  (consRights v (walk False diffOps [] ys acc))
              else
                consRights v (walk False diffOps [] ys acc)
    in
    let newLists =
      walk True (Update.listDiff outputOld outputNew) xs ys [([],[])]
    in
    { values = newLists }
}

listAppend xs ys =
  Update.applyLens listAppendLens (xs, ys)

-- listConcat xss =
--   case xss of
--     []      -> []
--     ys::yss -> listAppend ys (listConcat yss)

main =
  -- h3 [] [] (toString (listConcat [[0,1], [2,3], [4,5]]))
  -- h3 [] [] (toString (listAppend [0,1] (listAppend [2,3] [4,5])))
  h3 [] [] (toString (listAppend [0,1] [2,3]))

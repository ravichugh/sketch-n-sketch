open Lang

let typeof v =
  match v with
    | PVInt _ ->
        PTInt

    | PVString _ ->
        PTString

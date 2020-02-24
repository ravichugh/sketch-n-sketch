open Lang

let rec equal tau1 tau2 =
  match (tau1, tau2) with
    | (TArr (tau11, tau12), TArr (tau21, tau22)) ->
        equal tau11 tau21 && equal tau12 tau22

    | (TTuple taus1, TTuple taus2) ->
        List.length taus1 = List.length taus2
          && List.for_all2 equal taus1 taus2

    | (TData d1, TData d2) ->
        String.equal d1 d2

    | _ ->
        false

let is_base tau =
  match tau with
    | TArr _ ->
        false

    | TTuple _ ->
        false

    | TData _ ->
        true

let rec domain_of_codomain ~codomain tau =
  match tau with
    | TArr (tau1, tau2) ->
        if equal codomain tau2 then
          Some tau1
        else
          domain_of_codomain ~codomain tau2

    | _ ->
        None

let sub_bind_spec bind_spec =
  match bind_spec with
    | NoSpec | Rec _ ->
        NoSpec

    | Arg name | Dec name ->
        Dec name

let rec bind_spec gamma exp =
  match exp with
    | EProj (_, _, arg) ->
        sub_bind_spec (bind_spec gamma arg)

    | EVar x ->
        List.assoc_opt x gamma
          |> Option2.map snd
          |> Option2.with_default NoSpec

    | _ ->
        NoSpec

let structurally_decreasing_bind_spec ~head_spec ~arg_spec =
  match (head_spec, arg_spec) with
    | (Rec rec_name, Dec dec_name) ->
        String.equal rec_name dec_name

    | (Rec _, _) ->
        false

    | _ ->
        true

let structurally_decreasing gamma ~head ~arg =
  structurally_decreasing_bind_spec
    ~head_spec:(bind_spec gamma head)
    ~arg_spec:(bind_spec gamma arg)

let matches_dec annot bind_spec =
  match annot with
    | Some f ->
        structurally_decreasing_bind_spec
          ~head_spec:(Rec f)
          ~arg_spec:bind_spec

    | None ->
        true

let ignore_binding (s : string) : bool =
  String.length s > 0 && Char.equal (String.get s 0) '_'

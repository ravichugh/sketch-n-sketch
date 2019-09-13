open Lang

module Ctor_map = struct
  include
    Map.Make
      ( struct
          type t = string
          let compare = String.compare
        end
      )

  let from_assoc (xs : (string * 'a) list) : 'a t =
    List.fold_left
      (fun dict (ctor_name, x) -> add ctor_name x dict)
      empty
      xs

  let from_assoc_many (xs : (string * 'a) list) : 'a list t =
    List.fold_left
      ( fun dict (ctor_name, x) ->
          union
            (fun _ k v -> Some (k @ v))
            (singleton ctor_name [x])
            dict
      )
      empty
      xs
end

let filter (ws : worlds) : worlds =
  List.filter (fun (_env, ex) -> ex <> ExTop) ws

let distribute
  (arg_name : string)
  (scrutinee : exp)
  ((env, ex) : world)
  : (string * world) option =
    match Eval.eval env scrutinee with
      | Ok (RCtor (ctor_name, arg), []) ->
          Some (ctor_name, ((arg_name, arg) :: env, ex))

      | _ ->
          None

let branch
 max_scrutinee_size _delta sigma ((gamma, goal_type, goal_dec), worlds) =
  let open Nondet.Syntax in
  let* _ =
    Nondet.guard (Option.is_none goal_dec)
  in
  let
    filtered_worlds =
      filter worlds
  in
  let arg_name =
    Term_gen.fresh_ident gamma Term_gen.match_char
  in
  let* (data_name, data_ctors) =
    Nondet.from_list sigma
  in
  let ctor_info : (string * typ) Ctor_map.t =
    data_ctors
      |> List.map (Pair2.map_snd @@ fun typ -> (arg_name, typ))
      |> Ctor_map.from_assoc
  in
  let* scrutinee =
    Term_gen.up_to_e sigma max_scrutinee_size
      ( gamma
      , TData data_name
      , None
      )
  in
  let top_worlds =
    data_ctors
      |> List.map (Pair2.map_snd @@ fun _ -> ([], ExTop))
      |> Ctor_map.from_assoc_many
  in
  let* distributed_worldss =
    filtered_worlds
      |> List.map (distribute arg_name scrutinee)
      |> Option2.sequence
      |> Option2.map Ctor_map.from_assoc_many
      (* Informativeness Restriction (A) *)
      |> Option2.filter
           ( fun ctor_map ->
               ( List.length data_ctors = 1
                   || Ctor_map.cardinal ctor_map >= 2
               )
           )
      |> Option2.map
           ( Ctor_map.union
               (fun _ _ w -> Some w)
               top_worlds
           )
      |> Nondet.lift_option
  in
  let+ branches_goals =
    Nondet.lift_option @@
      Ctor_map.fold
        ( fun ctor_name distributed_worlds acc_opt ->
            let open! Option2.Syntax in
            let* acc =
              acc_opt
            in
            let+ (arg_name, arg_type) =
              Ctor_map.find_opt ctor_name ctor_info
            in
            let arg_bind_spec =
              scrutinee
                |> Type.bind_spec gamma
                |> Type.sub_bind_spec
            in
            let hole_name =
              Fresh.gen_hole ()
            in
            let goal =
              ( hole_name
              , ( ( (arg_name, (arg_type, arg_bind_spec)) :: gamma
                  , goal_type
                  , None
                  )
                , distributed_worlds
                )
              )
            in
            let branch =
              (ctor_name, (arg_name, EHole hole_name))
            in
              (branch, goal) :: acc
        )
        distributed_worldss
        (Some [])
  in
    branches_goals
      |> List.split
      |> Pair2.map_fst (fun branches -> ECase (scrutinee, branches))

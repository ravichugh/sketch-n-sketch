open Lang

type params =
  { max_scrutinee_size : int
  }

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
  List.filter (fun (env, ex) -> ex <> ExTop) ws

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

let branch params delta sigma { gamma; hole_name; goal_type; worlds } =
  let
    filtered_worlds =
      filter worlds
  in
  let arg_name =
    Term_gen.fresh_ident gamma Term_gen.match_char
  in
  Nondet.bind
    ( Nondet.from_list sigma
    ) @@ fun (data_name, data_ctors) ->
  let ctor_info : (string * typ) Ctor_map.t =
    data_ctors
      |> List.map (fun typ -> (arg_name, typ))
      |> Ctor_map.from_assoc
  in
  Nondet.bind
    ( Term_gen.up_to_e sigma params.max_scrutinee_size
        { gamma
        ; goal_type = TData data_name
        }
    ) @@ fun scrutinee ->
  let top_worlds =
    data_ctors
      |> List.map (Pair2.map_right @@ fun _ -> ([], ExTop))
      |> Ctor_map.from_assoc_many
  in
  let distributed_worldss_nd =
    filtered_worlds
      |> List.map (distribute arg_name scrutinee)
      |> Option2.sequence
      |> Option2.map Ctor_map.from_assoc_many
      |> Option2.map
           ( Ctor_map.union
               (fun _ _ w -> Some w)
               top_worlds
           )
      |> Nondet.lift_option
  in
  Nondet.bind
    distributed_worldss_nd
    @@ fun distributed_worldss ->
  let branches_goals_nd =
    Nondet.lift_option @@
      Ctor_map.fold
        ( fun ctor_name distributed_worlds acc_opt ->
            Option2.bind acc_opt @@ fun acc ->
            Option2.pure_bind
              ( Ctor_map.find_opt ctor_name ctor_info
              ) @@ fun (arg_name, arg_type) ->
            let arg_bind_spec =
              scrutinee
                |> Type.bind_spec gamma
                |> Type.sub_bind_spec
            in
            let goal =
              { gamma = (arg_name, (arg_type, arg_bind_spec)) :: gamma
              ; hole_name = Fresh.gen_hole ()
              ; goal_type
              ; worlds = distributed_worlds
              }
            in
            let branch =
              (ctor_name, (arg_name, EHole goal.hole_name))
            in
              (branch, goal) :: acc
        )
        distributed_worldss
        (Some [])
  in
  Nondet.pure_bind
    branches_goals_nd
    @@ fun branches_goals ->
  branches_goals
    |> List.split
    |> Pair2.map_left (fun branches -> ECase (scrutinee, branches))

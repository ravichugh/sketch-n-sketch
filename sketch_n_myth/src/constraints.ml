open Lang

(* Hole set functions *)

module Hole_set =
  Set.Make(struct type t = hole_name let compare = compare end)

let domain (m : 'a hole_map) : Hole_set.t =
  Hole_set.of_list (List.map fst @@ Hole_map.bindings m)

let hole_maps_disjoint (m1 : 'a hole_map) (m2 : 'b hole_map) =
  Hole_set.is_empty @@
    Hole_set.inter (domain m1) (domain m2)

(* Hole map functions *)

let delete_min map =
  let open Option2.Syntax in
  let+ (k, v) =
    Hole_map.min_binding_opt map
  in
    ((k, v), Hole_map.remove k map)

let empty =
  (Hole_map.empty, Hole_map.empty)

let from_hole_filling hf =
  (hf, Hole_map.empty)

let from_unsolved_constraints us =
  (Hole_map.empty, us)

let solved_singleton h e =
  (Hole_map.singleton h e, Hole_map.empty)

let unsolved_singleton h w =
  (Hole_map.empty, Hole_map.singleton h w)

let satisfies hf (f0, us)  =
  Hole_set.subset (domain f0) (domain hf)
    && Hole_map.for_all
         ( fun hole_name worlds ->
             Example.exp_satisfies hf (EHole hole_name) worlds
         )
         us

let merge_solved fs =
  let exception Merge_failure in
  (* The two maps should be disjoint *)
  let merge =
    Hole_map.union (fun _ _ _ -> raise_notrace Merge_failure)
  in
    try
      Some (List.fold_left merge Hole_map.empty fs)
    with
      Merge_failure ->
        None

let merge_unsolved us =
  let merge =
    Hole_map.union (fun _ v1 v2 -> Some (v1 @ v2))
  in
    List.fold_left merge Hole_map.empty us

let merge ks =
  let open Option2.Syntax in
  let (fs, us) =
    List.split ks
  in
  let* f =
    merge_solved fs
  in
  let u =
    merge_unsolved us
  in
    if hole_maps_disjoint f u then
      Some (f, u)
    else
      None

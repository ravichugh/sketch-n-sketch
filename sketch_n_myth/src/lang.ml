type hole_name =
  int
  [@@deriving yojson]

type exp =
  | EFix of (string option) * string * exp
  | EApp of exp * exp
  | EVar of string
  | ETuple of exp list
  | EProj of int * int * exp
  | ECtor of string * exp
  | ECase of exp * (string * (string * exp)) list
  | EHole of hole_name
  | EAssert of exp * exp
  [@@deriving yojson]

type typ =
  | TArr of typ * typ
  | TTuple of typ list
  | TData of string
  [@@deriving yojson]

type res =
  (* Determinate results *)
  | RFix of env * (string option) * string * exp
  | RTuple of res list
  | RCtor of string * res
  (* Indeterminate results *)
  | RHole of env * hole_name
  | RApp of res * res
  | RProj of int * int * res
  | RCase of env * res * (string * (string * exp)) list
  [@@deriving yojson]

and env =
  (string * res) list
  [@@deriving yojson]

type type_ctx =
  (string * typ) list
  [@@deriving yojson]

type datatype_ctx =
  (string * (string * typ)) list
  [@@deriving yojson]

type hole_ctx =
  (hole_name * (type_ctx * typ)) list
  [@@deriving yojson]

type value =
  | VTuple of value list
  | VCtor of string * value

type res_constraint =
  res * value

type res_constraints =
  res_constraint list

type example =
  | ExTuple of example list
  | ExCtor of string * example
  | ExInputOutput of value * example
  | ExTop

type world =
  env * example

type worlds =
  world list

(* Simple version of "hole filling" data structure *)
type hole_bindings =
  (hole_name * exp) list
  [@@deriving yojson]

module Hole_filling : sig
  type t

  val empty : t
  val find : hole_name -> t -> exp option
  val bindings : t -> hole_bindings
  val extend : t -> t -> t option
end = struct
  module Hole_map =
    Map.Make(struct type t = hole_name let compare = compare end)

  type t =
    exp Hole_map.t

  let empty =
    Hole_map.empty

  let find =
    Hole_map.find_opt

  let bindings =
    Hole_map.bindings

  exception Extension_failure

  let extend f_base f_new =
    try
      Some
        begin
          Hole_map.union
            (* The two maps should be disjoint *)
            (fun _ _ _ -> raise_notrace Extension_failure)
            f_base
            f_new
        end
    with
      Extension_failure ->
        None
end

type hole_filling =
  Hole_filling.t

type hole_constraint_kind =
  | Unsolved of worlds
  | Solved of exp

module Hole_constraints : sig
  type t

  val empty : t
  val singleton : hole_name -> hole_constraint_kind -> t
  val from_filling : hole_filling -> t
  val merge : t -> t -> t option
  val merge_all : t list -> t option
end = struct
  module Hole_map =
    Map.Make(struct type t = hole_name let compare = compare end)

  type t =
    hole_constraint_kind Hole_map.t

  let empty =
    Hole_map.empty

  let singleton =
    Hole_map.singleton

  let from_filling filling =
    let
      add_binding ks (hole_name, exp) =
        Hole_map.add hole_name (Solved exp) ks
    in
      filling
        |> Hole_filling.bindings
        |> List.fold_left add_binding empty

  exception Merge_failure

  let merge ks1 ks2 =
    try
      Some
        begin
          Hole_map.union
            begin fun _hole_name k1 k2 ->
              begin match (k1, k2) with
                | (Unsolved w1, Unsolved w2) ->
                    Some (Unsolved (w1 @ w2))

                | (Solved e1, Solved e2) ->
                    if e1 = e2 then
                      Some (Solved e1)
                    else
                      raise_notrace Merge_failure

                | (Unsolved _w, Solved _e)
                | (Solved _e, Unsolved _w) ->
                    raise_notrace Merge_failure
              end
            end
            ks1
            ks2
        end
    with
      Merge_failure ->
        None

  let merge_all =
    List.fold_left
      (fun maybe_acc ks -> Option2.bind maybe_acc (merge ks))
      (Some empty)
end

type hole_constraints =
  Hole_constraints.t

type hole_name =
  int
  [@@deriving yojson]

module Hole_map =
  Map.Make(struct type t = hole_name let compare = compare end)

type 'a hole_map =
  'a Hole_map.t

type exp =
  | EFix of (string option) * string * exp
  | EApp of exp * exp
  | EVar of string
  | ETuple of exp list
  | EProj of int * int * exp (* (n, i, arg) *)
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

type bind_spec =
  | NoSpec
  | Rec of string
  | Arg of string
  | Dec of string
  [@@deriving yojson]

type type_binding =
  string * (typ * bind_spec)
  [@@deriving yojson]

type type_ctx =
  type_binding list
  [@@deriving yojson]

type datatype_ctx =
  (string * (string * typ) list) list
  [@@deriving yojson]

type hole_ctx =
  (hole_name * (type_ctx * typ * bind_spec)) list
  [@@deriving yojson]

type value =
  | VTuple of value list
  | VCtor of string * value
  [@@deriving yojson]

type example =
  | ExTuple of example list
  | ExCtor of string * example
  | ExInputOutput of value * example
  | ExTop

type world =
  env * example

type worlds =
  world list

type hole_filling =
  exp hole_map

type unsolved_constraints =
  worlds hole_map

type constraints =
  hole_filling * unsolved_constraints

type resumption_assertion =
  res * value
  [@@deriving yojson]

type resumption_assertions =
  resumption_assertion list
  [@@deriving yojson]

type gen_goal =
  { gamma : type_ctx
  ; goal_type : typ
  }

type synthesis_goal =
  { gamma : type_ctx
  ; hole_name : hole_name
  ; goal_type : typ
  ; worlds : worlds
  }

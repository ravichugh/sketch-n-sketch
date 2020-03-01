type hole_name =
  int
  [@@deriving yojson]

module Hole_map =
  Map.Make(struct type t = hole_name let compare = compare end)

type 'a hole_map =
  'a Hole_map.t

type prim_typ =
  | PTInt
  | PTString
  [@@deriving yojson]

type prim_val =
  | PVInt of int
  | PVString of string
  [@@deriving yojson]

type prim_op =
  | POPlus
  | POMinus
  | POInc
  | PODec
  | PODiv2
  [@@deriving yojson]

type exp =
  | EFix of (string option) * string * exp
  (* bool: special recursive call (used only for "recursive window" UI) *)
  | EApp of bool * exp * exp
  | EVar of string
  | ETuple of exp list
  (* (n, i, arg) *)
  | EProj of int * int * exp
  | ECtor of string * exp
  | ECase of exp * (string * (string * exp)) list
  | EHole of hole_name
  | EAssert of exp * exp
  | EPrim of prim_val
  | EPrimOp of prim_op
  [@@deriving yojson]

type typ =
  | TArr of typ * typ
  | TTuple of typ list
  | TData of string
  | TPrim of prim_typ
  [@@deriving yojson]

type res =
  (* Determinate results *)
  | RFix of env * (string option) * string * exp
  | RTuple of res list
  | RCtor of string * res
  | RPrim of prim_val
  (* Indeterminate results *)
  | RHole of env * hole_name
  | RApp of res * res
  | RProj of int * int * res
  | RCase of env * res * (string * (string * exp)) list
  (* Other *)
  | RCtorInverse of string * res
  | RPrimOp of prim_op
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

(* (hole name, (type context, type, function decrease requirement, match depth))
 *)
type hole_ctx =
  (hole_name * (type_ctx * typ * string option * int)) list
  [@@deriving yojson]

type value =
  | VTuple of value list
  | VCtor of string * value
  | VPrim of prim_val
  [@@deriving yojson]

type example =
  | ExTuple of example list
  | ExCtor of string * example
  | ExInputOutput of value * example
  | ExPrim of prim_val
  | ExTop
  [@@deriving yojson]

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
  type_ctx * typ * string option

type synthesis_goal =
  gen_goal * worlds

type fill_goal =
  hole_name * synthesis_goal

type synthesis_params =
  { max_scrutinee_size : int
  ; max_match_depth : int
  ; max_term_size : int
  }

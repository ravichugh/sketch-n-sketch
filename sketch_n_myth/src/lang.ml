type hole_name =
  int
  [@@deriving yojson]

type exp =
  | EFix of (string option) * string * exp
  | EApp of exp * exp
  | EVar of string
  | ETuple of exp list
  | EProj of int * exp
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
  | RProj of int * res
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

type hole_filling =
  (hole_name * exp) list
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
  | ExPartialFunction of value * example
  | ExTop

type world =
  env * example

type worlds =
  world list

type hole_constraint =
  | Unsolved hole_name * worlds
  | Solved hole_name * exp

type hole_constraints =
  hole_constraint list

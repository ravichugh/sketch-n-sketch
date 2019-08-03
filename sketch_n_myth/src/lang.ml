type hole_name =
  int
  [@@deriving yojson]

type exp =
  | EFix of string * string * exp
  | EApp of exp * exp
  | EVar of string
  | ETuple of exp list
  | EProj of int * exp
  | ECtor of string * exp
  | ECase of exp * (string * string * exp) list
  | EHole of hole_name
  [@@deriving yojson]

type typ =
  | TArr of typ * typ
  | TTuple of typ list
  | TData of string
  [@@deriving yojson]

type res =
  (* Determinate results *)
  | RFix of env * string * string * exp
  | RTuple of exp list
  | RCtor of string * exp
  (* Indeterminate results *)
  | RHole of env * hole_name
  | RApp of exp * exp
  | RProj of int * exp
  | RCase of env * exp * (string * string * exp) list

and env =
  (string * res) list

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

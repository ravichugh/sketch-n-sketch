(* Types *)

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
  | RFix of env * string * string * exp
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

(* Functions *)

let rec res_to_value (r : res) : value option =
  match r with
    | RTuple comps ->
        comps
          |> List.map res_to_value
          |> Utils.option_sequence
          |> Utils.option_map (fun vcomps -> VTuple vcomps)

    | RCtor (name, arg) ->
        Utils.option_map
          (fun v -> VCtor (name, v))
          (res_to_value arg)

    | _ ->
      None

let rec results_consistent (r1 : res) (r2 : res) : res_constraints option =
  if r1 == r2 then
    Some []

  else
    match (r1, r2) with
      | (RTuple comps1, RTuple comps2) ->
          if List.length comps1 <> List.length comps2 then
            None
          else
            List.map2 results_consistent comps1 comps2
              |> Utils.option_sequence
              |> Utils.option_map List.concat

      | (RCtor (_, arg1), RCtor (_, arg2)) ->
          results_consistent arg1 arg2

      | _ ->
          begin match res_to_value r1 with
            | Some v1 ->
                Some [(r2, v1)]

            | None ->
                begin match res_to_value r2 with
                  | Some v2 ->
                      Some [(r1, v2)]

                  | None ->
                      None
                end
          end

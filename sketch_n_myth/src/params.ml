(* All timing units are in seconds. *)

let debug_mode : bool =
  false

(* None = infinity *)
let max_solution_count : int option =
  Some 30

let max_total_time : float =
  5.0

let max_eval_time : float =
  0.1

let max_guess_time : float =
  0.25

let initial_fuel : int =
  25

let log_info : bool =
  true

let log_warn : bool =
  true
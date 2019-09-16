(* Information *)

val now : unit -> float

(* Control-Flow Timing *)

type kind =
  | Total
  | Eval

exception Max_time_exceeded of kind

val check_cutoff : kind -> unit
val handle : kind -> (unit -> 'a) -> (unit -> 'a) -> 'a * float

(* Accumulators *)

type accumulator =
  | Guess

val reset_accumulator : accumulator -> unit
val accumulate : accumulator -> (unit -> 'a) -> 'a
val check_accumulator : accumulator -> bool

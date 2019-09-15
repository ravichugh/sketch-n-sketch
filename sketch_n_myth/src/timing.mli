(* All timing units are in seconds. *)

exception Time_exceeded

val get : unit -> float
val check_cutoff : max_time:float -> initial_time:float -> unit

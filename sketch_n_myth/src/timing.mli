(* All timing units are in seconds. *)

exception Time_exceeded
exception Total_time_exceeded

val get : unit -> float
val check_cutoff : max_time:float -> initial_time:float -> unit

val start_total : unit -> unit
val total_elapsed : unit -> float
val check_total_cutoff : unit -> unit

(* All timing units are in seconds. *)

exception Time_exceeded

val max_time : float

val get : unit -> float
val check_cutoff : float -> unit

open Lang

val fresh_ident : char -> type_ctx -> string
val function_char : char
val variable_char : char
val match_char : char

(* Make sure to call clear_cache once synthesis is fully complete for a problem,
 * and not any sooner or later!
 *)
val clear_cache : unit -> unit

(* Generates e-terms up to (and including) some size.
 *)
val up_to_e : int -> gen_goal -> exp Nondet.t

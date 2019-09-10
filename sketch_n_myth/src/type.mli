open Lang

val equal : typ -> typ -> bool

val is_base : typ -> bool

(* Suppose tau = a -> (b -> (c -> d)). Then:
 *   domain_of_codomain ~codomain:d tau = Some c
 *   domain_of_codomain ~codomain:(c -> d) tau = Some b
 *   domain_of_codomain ~codomain:(b -> (c -> d)) tau = Some a
 *   domain_of_codomain ~codomain:(a -> (b -> (c -> d))) tau = None
 *
 * Also:
 *   domain_of_codomain ~codomain:a a = None
 *   domain_of_codomain ~codomain:a (() -> a) = ()
 *)
val domain_of_codomain : codomain:typ -> typ -> typ option

val bind_spec : type_ctx -> exp -> bind_spec

val sub_bind_spec : bind_spec -> bind_spec

val structurally_decreasing_bind_spec :
  head_spec:bind_spec -> arg_spec:bind_spec -> bool

val structurally_decreasing :
  type_ctx -> head:exp -> arg:exp -> bool

val matches_dec :
  string option -> bind_spec -> bool

val ignore_binding : string -> bool

open Lang

val minimal_uneval : bool ref

val check :
  hole_ctx
    -> datatype_ctx
    -> hole_filling
    -> exp
    -> worlds
    -> constraints Nondet.t

val uneval :
  hole_ctx
    -> datatype_ctx
    -> hole_filling
    -> res
    -> example
    -> constraints Nondet.t

val simplify_assertions :
  hole_ctx
    -> datatype_ctx
    -> resumption_assertions
    -> constraints Nondet.t

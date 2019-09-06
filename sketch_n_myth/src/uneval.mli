open Lang

val simplify :
  hole_ctx
    -> datatype_ctx
    -> resumption_assertions
    -> constraints Nondet.t

val uneval :
  hole_ctx
    -> datatype_ctx
    -> hole_filling
    -> res
    -> example
    -> constraints Nondet.t

val check :
  hole_ctx
    -> datatype_ctx
    -> hole_filling
    -> exp
    -> worlds
    -> constraints Nondet.t

val simplify_constraints :
  hole_ctx
    -> datatype_ctx
    -> constraints
    -> constraints Nondet.t

open Lang

val solve_any :
  hole_ctx
    -> datatype_ctx
    -> constraints Nondet.t
    -> (hole_filling * hole_ctx) Nondet.t

open Lang

val solve_any :
  float
    -> hole_ctx
    -> datatype_ctx
    -> constraints Nondet.t
    -> (hole_filling * hole_ctx) Nondet.t

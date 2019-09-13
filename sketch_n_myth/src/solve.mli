open Lang

val solve :
  hole_ctx
    -> datatype_ctx
    -> constraints
    -> (hole_filling * hole_ctx) Nondet.t

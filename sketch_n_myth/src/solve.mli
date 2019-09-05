open Lang

val solve :
  synthesis_params
    -> hole_ctx
    -> datatype_ctx
    -> constraints
    -> (hole_filling * hole_ctx) Nondet.t

val staged_solve :
  hole_ctx
    -> datatype_ctx
    -> constraints
    -> (hole_filling * hole_ctx) Nondet.t

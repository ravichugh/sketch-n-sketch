open Lang

val branch :
  int
    -> hole_ctx
    -> datatype_ctx
    -> hole_filling
    -> synthesis_goal
    -> ((exp * fill_goal list) * constraints) Nondet.t

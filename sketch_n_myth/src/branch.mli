open Lang

val branch :
  int
    -> hole_ctx
    -> datatype_ctx
    -> synthesis_goal
    -> (exp * fill_goal list) Nondet.t

open Lang

type params =
  { max_scrutinee_size : int
  }

val branch :
  params
    -> hole_ctx
    -> datatype_ctx
    -> synthesis_goal
    -> (exp * synthesis_goal list) Nondet.t

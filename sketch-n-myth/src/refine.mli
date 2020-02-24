open Lang

val refine :
  hole_ctx
    -> datatype_ctx
    -> synthesis_goal
    -> (exp * fill_goal list) option

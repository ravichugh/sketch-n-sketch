open Lang

val refine :
  hole_ctx
    -> datatype_ctx
    -> synthesis_goal
    -> (exp * synthesis_goal list) option

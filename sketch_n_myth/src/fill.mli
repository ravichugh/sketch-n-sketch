open Lang

val fill :
  synthesis_params
    -> hole_ctx
    -> datatype_ctx
    -> hole_filling
    -> fill_goal
    (* (assertions, assumptions, context for new holes *)
    -> (constraints * constraints * hole_ctx) Nondet.t

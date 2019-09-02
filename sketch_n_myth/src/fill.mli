open Lang

type params =
  { max_scrutinee_size : int
  ; max_match_depth : int
  ; max_term_size : int
  }

val fill :
  params
    -> hole_ctx
    -> datatype_ctx
    -> hole_filling
    -> fill_goal
    -> (constraints * hole_ctx) Nondet.t

open Lang

type params =
  { max_scrutinee_size : int
  , max_match_depth : int
  , max_term_size : int
  }

val fill :
  params
    -> hole_ctx
    -> datatype_ctx
    -> hole_filling
    -> synthesis_goal
    -> (constraints * datatype_ctx) Nondet.t

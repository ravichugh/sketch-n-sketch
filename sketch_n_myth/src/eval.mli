open Lang

type eval_result =
  (res * res_constraints, string) result

val eval : env -> exp -> eval_result
val resume : hole_filling -> res -> eval_result

open Lang

type eval_result =
  (res * resumption_assertions, string) result

val eval : env -> exp -> eval_result
val resume : hole_filling -> res -> eval_result

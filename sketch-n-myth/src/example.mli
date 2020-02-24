open Lang

val from_value : value -> example

val res_satisfies : hole_filling -> res -> example -> bool
val exp_satisfies : hole_filling -> exp -> worlds -> bool

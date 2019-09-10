open Lang

val final : res -> bool
val determinate : res -> bool
val indeterminate : res -> bool

val res_to_value : res -> value option
val value_to_res : value -> res

val value_to_example : value -> example

val consistent : res -> res -> resumption_assertions option

(* Only possible without higher-order examples *)
val values_equal : value -> value -> bool
val examples_consistent : example -> example -> bool

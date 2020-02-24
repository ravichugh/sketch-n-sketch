open Lang

val final : res -> bool
val determinate : res -> bool
val indeterminate : res -> bool

val to_value : res -> value option
val from_value : value -> res

val consistent : res -> res -> resumption_assertions option

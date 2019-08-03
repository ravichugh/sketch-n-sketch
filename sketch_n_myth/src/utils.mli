(* Function functions *)

val compose : ('b -> 'c) -> ('a -> 'b) -> ('a -> 'c)

(* Pair functions *)

val pair_map2 : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val map_left : ('a -> 'c) -> 'a * 'b -> 'c * 'b
val map_right : ('b -> 'c) -> 'a * 'b -> 'a * 'c

(* Option functions *)

val option_map : ('a -> 'b) -> 'a option -> 'b option
val option_bind : 'a option -> ('a -> 'b option) -> 'b option
val option_sequence : 'a option list -> 'a list option

(* Result functions *)

val result_map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
val result_pure_bind : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
val result_bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val result_sequence : ('a, 'e) result list -> ('a list, 'e) result

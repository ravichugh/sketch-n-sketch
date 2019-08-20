val pure_bind : 'a list -> ('a -> 'b) -> 'b list
val pure : 'a -> 'a list
val bind : 'a list -> ('a -> 'b list) -> 'b list

val concat_map : ('a -> 'b list) -> 'a list -> 'b list
val maximum : 'a list -> 'a option
val repeat : int -> 'a -> 'a list
val sequence : ('a list) list -> ('a list) list
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val filter_somes : 'a option list -> 'a list
val intersperse : 'a -> 'a list -> 'a list
(* Inclusive on both ends *)
val range : low:int -> high:int -> int list
val remove_first : 'a -> 'a list -> 'a list
(* Should only use on comparable types *)
val permutations : 'a list -> 'a list list
val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list

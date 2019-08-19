val pure_bind : 'a list -> ('a -> 'b) -> 'b list
val pure : 'a -> 'a list
val bind : 'a list -> ('a -> 'b list) -> 'b list

val maximum : 'a list -> 'a option
val repeat : int -> 'a -> 'a list
val sequence : ('a list) list -> ('a list) list
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
val filter_somes : 'a option list -> 'a list

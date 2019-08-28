val map : ('a -> 'b) -> 'a option -> 'b option
val pure_bind : 'a option -> ('a -> 'b) -> 'b option
val bind : 'a option -> ('a -> 'b option) -> 'b option
val and_then : ('a -> 'b option) -> 'a option -> 'b option
val sequence : 'a option list -> 'a list option
val with_default : 'a -> 'a option -> 'a
val sequence_left : ('a option * 'b) -> ('a * 'b) option

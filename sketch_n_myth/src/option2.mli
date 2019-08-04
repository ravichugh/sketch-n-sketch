val map : ('a -> 'b) -> 'a option -> 'b option
val bind : 'a option -> ('a -> 'b option) -> 'b option
val sequence : 'a option list -> 'a list option

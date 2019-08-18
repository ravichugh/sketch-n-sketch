val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result
val pure_bind : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
val bind : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
val sequence : ('a, 'e) result list -> ('a list, 'e) result
val to_option : ('a, 'e) result -> 'a option
val with_default : 'a -> ('a, 'e) result -> 'a

val pair : 'a -> 'b -> 'a * 'b
val map_left : ('a -> 'c) -> 'a * 'b -> 'c * 'b
val map_right : ('b -> 'c) -> 'a * 'b -> 'a * 'c
val lift_right_result : 'a * ('b, 'e) result -> ('a * 'b, 'e) result

type 'a t

(* Construction *)

val none : 'a t
val from_list : 'a list -> 'a t

(* Collection *)

val to_list : 'a t -> 'a list

(* Core functions *)

val map : ('a -> 'b) -> 'a t -> 'b t
val pure : 'a -> 'a t
val join : ('a t) t -> 'a t

(* Generic library functions *)

val pure_bind : 'a t -> ('a -> 'b) -> 'b t
val bind : 'a t -> ('a -> 'b t) -> 'b t
val and_then : ('a -> 'b t) -> 'a t -> 'b t
val guard : bool -> unit t

(* Specific library functions *)

(* "Sum" *)
val union : ('a t) list -> 'a t

(* "Product" *)
val one_of_each : ('a t) list -> ('a list) t

val is_empty : 'a t -> bool
val filter : ('a -> bool) -> 'a t -> 'a t
val dedup : 'a t -> 'a t
val collapse_option : ('a option) t -> 'a t

(* Lifting *)

val lift_option : 'a option -> 'a t
val lift_result : ('a, 'e) result -> 'a t

(* Syntax *)

module Syntax : sig
  val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

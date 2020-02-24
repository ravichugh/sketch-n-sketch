val now : unit -> float

module Single : sig
  type t =
    | Total
    | Eval

  val start : t -> unit
  val elapsed : t -> float
  val check : t -> bool
end

module Multi : sig
  type t =
    | Guess

  val reset : t -> unit
  val accumulate : t -> (unit -> 'a) -> 'a
  val check : t -> bool
end

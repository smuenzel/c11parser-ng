
module type S = sig
  type t

  val to_lexing_position : t -> Lexing.position
  val is_beginning_of_line : t -> bool

  val dummy : t
  val incr : t -> t
  val min : t -> t -> t
end

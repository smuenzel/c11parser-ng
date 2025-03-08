
module type Position = sig
  type t

  val to_lexing_position : t -> Lexing.position
  val is_beginning_of_line : t -> bool

  val dummy : t
  val incr : t -> t
end

module type S = sig
  module Position : Position
  type lexbuf

  val start : lexbuf -> unit
  val __private__next_int : lexbuf -> int
  val mark : lexbuf -> int -> unit
  val backtrack : lexbuf -> int

  val override_start_position : lexbuf -> Position.t -> unit

  val lexing_position_start : lexbuf -> Position.t
  val lexing_position_curr : lexbuf -> Position.t
  val lexing_positions : lexbuf -> Position.t * Position.t

  val lexeme_char : lexbuf -> int -> Uchar.t

  module Utf8 : sig
    val lexeme : lexbuf -> string
    val sub_lexeme : lexbuf -> int -> int -> string
  end

  module Latin1 : sig
    val lexeme_char : lexbuf -> int -> char

  end
end

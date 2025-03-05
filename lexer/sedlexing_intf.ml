
module type S = sig
  type lexbuf

  val start : lexbuf -> unit
  val __private__next_int : lexbuf -> int
  val mark : lexbuf -> int -> unit
  val backtrack : lexbuf -> int

  val override_start_position : lexbuf -> Lexing.position -> unit

  val lexing_position_start : lexbuf -> Lexing.position
  val lexing_position_curr : lexbuf -> Lexing.position
  val lexing_positions : lexbuf -> Lexing.position * Lexing.position

  val lexeme_char : lexbuf -> int -> Uchar.t

  module Utf8 : sig
    val lexeme : lexbuf -> string
    val sub_lexeme : lexbuf -> int -> int -> string
  end

  module Latin1 : sig
    val lexeme_char : lexbuf -> int -> char

  end
end

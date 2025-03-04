
module type S = sig
  type position
  type lexbuf

  val lexeme_start_p : lexbuf -> position
  val lexeme_end_p : lexbuf -> position
end

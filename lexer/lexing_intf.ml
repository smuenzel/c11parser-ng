
module type S = sig
  type position
  type lexbuf

  val lexeme_start_p : lexbuf -> position
  val lexeme_end_p : lexbuf -> position
end

module type S_with_token = sig
  include S
  type token
  val lexer : lexbuf -> token
end


module type T = sig
  type t
  val eof : t
end

module From_list(Token : T) = struct
  type position = unit
  type token = Token.t
  type lexbuf = token list ref

  let lexeme_start_p _ = ()
  let lexeme_end_p _ = ()

  let lexer (lexbuf : lexbuf) =
    match !lexbuf with
    | [] -> Token.eof
    | hd :: tl -> lexbuf := tl; hd

  let create l : lexbuf = ref l
end

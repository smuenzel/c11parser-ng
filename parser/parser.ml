
module Make
    (Gen : Astgen_intf.S)
    (Context : Context.Packed)
= struct
  module Raw = Parser_raw.Make (Gen) (Context)

  type token = Raw.token

  exception Error = Raw.Error

  let wrap f =
    fun (lexbuf : Sedlexing.lexbuf) ->
    let state =
      C11lexer.State.create_default
        ~is_typedefname:Context.is_typedefname
        ()
    in
    let old_lexbuf = Lexing.from_string "" in
    let lexer (dummy_lexbuf : Lexing.lexbuf) =
      let token = C11lexer.lexer state lexbuf in
      let start_p, end_p = Sedlexing.lexing_positions lexbuf in
      dummy_lexbuf.lex_start_p <- start_p;
      dummy_lexbuf.lex_curr_p <- end_p;
      token
    in
    f lexer old_lexbuf

  let translation_unit_file =
    wrap Raw.translation_unit_file

  let single_expression = wrap Raw.single_expression
end

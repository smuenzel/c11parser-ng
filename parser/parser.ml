
module Make
    (Gen : Astgen_intf.S)
    (Context : Context.Packed)
= struct
  module Raw = Parser_raw.Make (Gen) (Context)

  type token = Raw.token

  exception Syntax_error of int * int * C11lexer.Token.t * C11lexer.Token.t * string * int

  let token_string token =
    C11lexer.Token.sexp_of_t token
    |> Sexplib0.Sexp.to_string

  let syntax_error_printer = function
    | Syntax_error (line, col, token_last, token_now, msg, state) ->
      Some (Printf.sprintf
              "Syntax error at line %d, column %d: '%s' (last token: %s, current token: %s, state: %d)"
              line
              col
              (String.trim msg)
              (token_string token_last)
              (token_string token_now)
              state)
    | _ -> None

  let () =
    Printexc.register_printer syntax_error_printer

  let wrap f =
    fun (lexbuf : Sedlexing.lexbuf) ->
    let state =
      C11lexer.State.create_default
        ~is_typedefname:Context.is_typedefname
        ()
    in
    let old_lexbuf = Lexing.from_string "" in
    let last_token = ref C11lexer.Token.EOF in
    let current_token = ref C11lexer.Token.EOF in
    let lexer (dummy_lexbuf : Lexing.lexbuf) =
      let token = C11lexer.lexer state lexbuf in
      let start_p, end_p = Sedlexing.lexing_positions lexbuf in
      dummy_lexbuf.lex_start_p <- start_p;
      dummy_lexbuf.lex_curr_p <- end_p;
      last_token := !current_token;
      current_token := token;
      token
    in
    try
      f lexer old_lexbuf
    with
    | Raw.Error e ->
      let msg = 
        try Error_messages.message e 
        with
        | Not_found -> "Unknown error"
      in
      let p = Sedlexing.lexing_position_curr lexbuf in
      raise (Syntax_error (p.pos_lnum, p.pos_cnum - p.pos_bol, !last_token, !current_token, msg, e))
    | C11lexer.Lexer_error (_, l0, c0, _, _) as error ->
      let msg =
        Printf.sprintf
          "Lexer error: %s" (Option.value ~default:"" (C11lexer.print_lexer_error error))
      in
      raise (Syntax_error (l0, c0, !last_token, !current_token, msg, 0))


  let translation_unit_file =
    wrap Raw.translation_unit_file

  let single_expression = wrap Raw.single_expression
end

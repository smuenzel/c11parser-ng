(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2024-2025 Stefan Muenzel
 *)

module Make
    (Gen : Astgen_intf.S with type Located.position = Lexing.position)
    (Context : Context.Packed)
= struct
  module Raw = Parser_raw.Make (Lexing) (Gen) (Context)

  type token = Raw.token

  exception Syntax_error of 
      { pos : int * int
      ; last_token : C11lexer.Token.t 
      ; current_token : C11lexer.Token.t
      ; msg : string
      ; state : int
      }

  let token_string token =
    C11lexer.Token.sexp_of_t token
    |> Sexplib0.Sexp.to_string

  let syntax_error_printer = function
    | Syntax_error 
        { pos = line, col
        ; last_token
        ; current_token
        ; msg
        ; state
        } ->
      Some (Printf.sprintf
              "Syntax error at line %d, column %d:\n\
               '%s'\n\
               (last token: %s, current token: %s, state: %d)"
              line
              col
              (String.trim msg)
              (token_string last_token)
              (token_string current_token)
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
      raise (Syntax_error 
               { pos = p.pos_lnum, p.pos_cnum - p.pos_bol
               ; last_token = !last_token
               ; current_token = !current_token
               ; msg
               ; state = e
               })
    | C11lexer.Lexer_error { pos_start = (l0, c0);  _ } as error ->
      let msg =
        Printf.sprintf
          "Lexer error:\n%s" (Option.value ~default:"" (C11lexer.print_lexer_error error))
      in
      raise (Syntax_error 
               { pos = l0, c0
               ; last_token = !last_token
               ; current_token = !current_token
               ; msg
               ; state = 0
               })


  let translation_unit_file =
    wrap Raw.translation_unit_file

  let single_expression = wrap Raw.single_expression
end

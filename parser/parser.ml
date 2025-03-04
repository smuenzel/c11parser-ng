(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2024-2025 Stefan Muenzel
 *)

module Make
    (Gen : Astgen_intf.S with type Located.position = Lexing.position)
    (Context : Context.Packed)
= struct
  module Raw = Parser_raw.Make (C11lexer.Wrapped_lexer_default) (Gen) (Context)

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
    fun (lexbuf : C11lexer.Sedlexing.lexbuf) ->
    let module Sedlexing = C11lexer.Sedlexing in
    let state =
      C11lexer.State.create_default
        ~is_typedefname:Context.is_typedefname
        ()
    in
    let wrapped_lexer =
      C11lexer.Wrapped_lexer_default.create ~state ~inner_lexer:lexbuf
    in
    try
      f C11lexer.Wrapped_lexer_default.lexer wrapped_lexer
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
               ; last_token = wrapped_lexer.last_token
               ; current_token = wrapped_lexer.current_token
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
               ; last_token = wrapped_lexer.last_token
               ; current_token = wrapped_lexer.current_token
               ; msg
               ; state = 0
               })


  let translation_unit_file =
    wrap Raw.translation_unit_file

  let single_expression = wrap Raw.single_expression
end

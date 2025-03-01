(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2024-2025 Stefan Muenzel
 *)

open Core

module Ast = Model_ast.Ast.Make(Model_ast.Ast_types.Dummy_located)

module P(Context : C11parser.Context.Packed) =
  C11parser.Parser.Make(Ast)(Context)

let test_parser s =
  let context = C11parser.Context.create_packed () in
  let module P = P(val context) in
  let lexbuf = Sedlexing.Utf8.from_string s in
  let edl = P.translation_unit_file lexbuf in
  [%message.omit_nil ""
      ~_: (edl : Ast.External_declaration.t list)]
  |> print_s

let test_pre_token s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let tokens =
    Preprocessor.Pre_token.all_tokens
      ~produce:Preprocessor.Pre_token.produce_with_position lexbuf
  in
  let token_and_pos =
    List.map tokens ~f:(fun (pos, token) ->
        let pos_s =
          Printf.sprintf "%i:%i-%i:%i"
            pos.start.pos_lnum (pos.start.pos_cnum - pos.start.pos_bol)
            pos.end_.pos_lnum (pos.end_.pos_cnum - pos.end_.pos_bol)
        in
        pos_s, token
      )
  in
  [%message.omit_nil ""
      ~_:(token_and_pos : (string * Preprocessor.Pre_token.t) list)
  ]
  |> print_s



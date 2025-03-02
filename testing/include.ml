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

let convert_to_pos tokens =
  List.map tokens ~f:(fun (pos, token) ->
      let pos_s =
        Printf.sprintf "%i:%i-%i:%i"
          pos.Preprocessor.Pre_token.Token_properties.start.pos_lnum (pos.start.pos_cnum - pos.start.pos_bol)
          pos.end_.pos_lnum (pos.end_.pos_cnum - pos.end_.pos_bol)
      in
      pos_s, token
    )

module No_pos = struct
    module Lex = C11parser.Lexing_adapters.From_list(C11lexer.Token)
    module Located = Model_ast.Ast_types.Dummy_located_raw(struct type t = unit end)
    module Ast = Model_ast.Ast.Make(Located)
end

let expression_from_tokens (l : C11lexer.Token.t list) =
  let open No_pos in
  let context = C11parser.Context.create_packed () in
  let module Raw = C11parser.Parser_raw.Make(Lex)(Ast) (val context) in
  let lexbuf = Lex.create l in
  let state = C11lexer.State.create_default () in
  let lexer = C11lexer.wrap_lexer ~lexer:Lex.lexer state in
  try
    Raw.single_expression lexer lexbuf
  with
  | Raw.Error e ->
    let msg =
      try C11parser.Error_messages.message e
      with
      | _ -> "Unknown error"
    in
    raise_s [%message "error"
        (e : int)
        (msg : string)
    ]

let test_pre_token_line s =
  let lexbuf = Sedlexing.Utf8.from_string s in
  let line_acc = ref [] in
  let decode_acc = ref [] in
  while
    match Preprocessor.Line_processor.getline lexbuf with
    | None -> false
    | Some line ->
      decode_acc :=
        (Preprocessor.Line_processor.process_line line) :: !decode_acc;
      let line = convert_to_pos line in
      line_acc := line :: !line_acc; true
  do
    ()
  done;
  let decode = List.rev !decode_acc in
  let lines = List.rev !line_acc in
  let recode =
    List.map decode
      ~f:(function
          | `Unknown -> `Unknown
          | `If tokens ->
            try
            `If (expression_from_tokens tokens)
            with
            | exn -> `Exn exn

        )
  in
  [%message.omit_nil ""
      ~_:(lines : (string * Preprocessor.Pre_token.t) list list)
      (decode : 
         [`If of C11lexer.Token.t list | `Unknown ] list)
      (recode : 
         [`If of No_pos.Ast.Expr.t | `Unknown | `Exn of Exn.t] list)
  ]
  |> print_s



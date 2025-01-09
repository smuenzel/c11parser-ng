open Core

module Ast = Model_ast.Ast.Make(Model_ast.Ast_types.Dummy_located)

module P(Context : C11parser.Context.Packed) =
  C11parser.Parser.Make(Ast)(Context)

let test s =
  let context = C11parser.Context.create_packed () in
  let module P = P(val context) in
  let lexbuf = Sedlexing.Utf8.from_string s in
  P.translation_unit_file lexbuf
  |> [%sexp_of: Ast.External_declaration.t list]
  |> print_s

let%expect_test "" =
 test 
   {|
struct {
  _Alignas(int) char x;
} s;
   |}

(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2024-2025 Stefan Muenzel
 *)

open Core
open Include

let%expect_test "" =
  Printexc.record_backtrace false;
  test
    {|
typedef int x;

_Bool x { q while
    |};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  ( "Syntax error at line 4, column 17:\
   \n'The last token was expected to be a typedef name, but it is a variable name.'\
   \n(last token: (VARIABLE q), current token: WHILE, state: 425)")
  |}]

let%expect_test "" =
  Printexc.record_backtrace false;
  test
    {|
typedef int x;

_Bool x { q : ; }
    |};
  [%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier x))))))
   (Function
    ((returns (Unique (Eq Bool ()))) (declarator (Identifier x)) (arguments ())
     (body
      ((Statement (Labeled (Label (name q) (statement (Expression ()))))))))))
  |}]

let%expect_test "" =
  Printexc.record_backtrace false;
  test
    {|
typedef int x;

_Bool x() { q : ; }
    |};
  [%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier x))))))
   (Function
    ((returns (Unique (Eq Bool ())))
     (declarator
      (Function (declarator (Identifier x)) (parameters (Second ()))))
     (arguments ())
     (body
      ((Statement (Labeled (Label (name q) (statement (Expression ()))))))))))
  |}]


let%expect_test "" =
  test
    {|
typedef int x;

_Bool _Bool { q while
    |};
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  ( "Syntax error at line 4, column 11:\
   \n'Unknown error'\
   \n(last token: BOOL, current token: BOOL, state: 388)")
  |}]

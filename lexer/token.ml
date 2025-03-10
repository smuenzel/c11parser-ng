(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2024-2025 Stefan Muenzel
 *)

open Sexplib0.Sexp_conv

type t =
  | NAME of string
  | VARIABLE of string
  | TYPE of string
  | STRING_LITERAL of Literal.String.t
  | CONSTANT_CHAR of Literal.Char.t
  | CONSTANT_INTEGER of string
  | CONSTANT_DECIMAL_FLOATING of string
  | CONSTANT_HEXADECIMAL_FLOATING of string
  | ALIGNAS
  | ALIGNOF
  | ATOMIC
  | AUTO
  | BOOL
  | BREAK
  | CASE
  | CHAR
  | COMPLEX
  | CONST
  | CONTINUE
  | DEFAULT
  | DO
  | DOUBLE
  | ELSE
  | ENUM
  | EXTERN
  | FLOAT
  | FOR
  | GENERIC
  | GOTO
  | IF
  | IMAGINARY
  | INLINE
  | INT
  | LONG
  | NORETURN
  | REGISTER
  | RESTRICT
  | RETURN
  | SHORT
  | SIGNED
  | SIZEOF
  | STATIC
  | STATIC_ASSERT
  | STRUCT
  | SWITCH
  | THREAD_LOCAL
  | TYPEDEF
  | UNION
  | UNSIGNED
  | VOID
  | VOLATILE
  | WHILE
  | ELLIPSIS
  | ADD_ASSIGN
  | SUB_ASSIGN
  | MUL_ASSIGN
  | DIV_ASSIGN
  | MOD_ASSIGN
  | OR_ASSIGN
  | AND_ASSIGN
  | XOR_ASSIGN
  | LEFT_ASSIGN
  | RIGHT_ASSIGN
  | LEFT
  | RIGHT
  | EQEQ
  | NEQ
  | LEQ
  | GEQ
  | EQ
  | LT
  | GT
  | INC
  | DEC
  | PTR
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | BANG
  | ANDAND
  | BARBAR
  | AND
  | BAR
  | HAT
  | QUESTION
  | COLON
  | TILDE
  | LBRACE
  | RBRACE
  | LBRACK
  | RBRACK
  | LPAREN
  | RPAREN
  | SEMICOLON
  | COMMA
  | DOT
  | ATOMIC_LPAREN
  | EOF
[@@deriving sexp]

type token = t [@@deriving sexp]

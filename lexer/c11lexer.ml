(*
Jacques-Henri Jourdan, Inria Paris
François Pottier, Inria Paris

Copyright (c) 2016-2017, Inria
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Inria nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL INRIA BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(* Identifiers *)
let digit = [%sedlex.regexp? '0' .. '9']
let hexadecimal_digit = [%sedlex.regexp? digit | 'A' .. 'F' | 'a' .. 'f']
let nondigit = [%sedlex.regexp? '_' | 'a' .. 'z' | 'A' .. 'Z']
let digit_or_nondigit = [%sedlex.regexp? nondigit | digit]

let hex_quad = [%sedlex.regexp? Rep(hexadecimal_digit, 4)]
let universal_character_name =
  [%sedlex.regexp? ("\\u", hex_quad) | ("\\U", hex_quad, hex_quad)]

let identifier_nondigit =
  [%sedlex.regexp? nondigit | universal_character_name]

let identifier =
  [%sedlex.regexp? identifier_nondigit, Star (identifier_nondigit|digit)]

(* Whitespaces. '\r' is not considered as white-space by the standard,
   but we include it in order to accept files encoded with DOS-style
   line endings.  Beware : \011 and \012 are DECIMAL escape
   codes. They correspond to vertical tab and form feed,
   respectively. *)
let whitespace_char_no_newline = [%sedlex.regexp? Chars " \t\011\012\r"]

(* Integer constants *)
let nonzero_digit = [%sedlex.regexp? '1'..'9']
let decimal_constant = [%sedlex.regexp? nonzero_digit, Star digit]

let octal_digit = [%sedlex.regexp? '0' .. '7']
let octal_constant = [%sedlex.regexp? '0', Star octal_digit]

let hexadecimal_prefix = [%sedlex.regexp? "0x" | "0X"]
let hexadecimal_constant =
  [%sedlex.regexp? hexadecimal_prefix, Plus hexadecimal_digit]

let unsigned_suffix = [%sedlex.regexp? 'u' | 'U']
let long_suffix = [%sedlex.regexp? 'l' | 'L']
let long_long_suffix = [%sedlex.regexp? "ll" | "LL" ]
let integer_suffix =
  [%sedlex.regexp?
      ( (unsigned_suffix, Opt long_suffix)
      | (unsigned_suffix, long_long_suffix)
      | (long_suffix, Opt unsigned_suffix)
      | (long_long_suffix, Opt unsigned_suffix)
      )]

let integer_constant =
  [%sedlex.regexp?
      ( (decimal_constant, Opt integer_suffix)
      | (octal_constant, Opt integer_suffix)
      | (hexadecimal_constant, Opt integer_suffix)
      )]

(* Floating constants *)
let sign = [%sedlex.regexp? '-' | '+']
let digit_sequence = [%sedlex.regexp? Plus digit ]
let floating_suffix = [%sedlex.regexp? Chars "flFL"]

let fractional_constant =
  [%sedlex.regexp?
      ( (Opt digit_sequence, '.', digit_sequence)
      | (digit_sequence, '.')
      )]
let exponent_part =
  [%sedlex.regexp?  Chars "eE", Opt sign, digit_sequence]
let decimal_floating_constant =
  [%sedlex.regexp?
      ( (fractional_constant, Opt exponent_part, Opt floating_suffix)
      | digit_sequence, exponent_part, Opt floating_suffix
      )]

let hexadecimal_digit_sequence = [%sedlex.regexp? Plus hexadecimal_digit]
let hexadecimal_fractional_constant =
  [%sedlex.regexp?
      ( (Opt hexadecimal_digit_sequence, '.', hexadecimal_digit_sequence)
      | (hexadecimal_digit_sequence, '.')
      )]
let binary_exponent_part =
  [%sedlex.regexp? Chars "pP", Opt sign, digit_sequence]
let hexadecimal_floating_constant =
  [%sedlex.regexp?
      ( (hexadecimal_prefix, hexadecimal_fractional_constant,
         binary_exponent_part, Opt floating_suffix)
      | (hexadecimal_prefix, hexadecimal_digit_sequence,
         binary_exponent_part, Opt floating_suffix)
      )]

(* Preprocessing numbers *)
let preprocessing_exponent = [%sedlex.regexp? Chars "eEpP"]
let preprocessing_number =
  [%sedlex.regexp?
      Opt '.', digit, Star (digit_or_nondigit | (preprocessing_exponent, sign))
  ]

(* Character and string constants *)
let simple_escape_sequence =
  [%sedlex.regexp? '\\', Chars "'.\"?\\abfnrtv"]
let octal_escape_sequence =
  [%sedlex.regexp?
      '\\', (octal_digit | Rep(octal_digit, 2) | Rep(octal_digit, 3))]
let hexadecimal_escape_sequence = 
  [%sedlex.regexp? "\\x", Plus hexadecimal_digit]
let escape_sequence =
  [%sedlex.regexp?
      ( simple_escape_sequence
      | octal_escape_sequence
      | hexadecimal_escape_sequence
      | universal_character_name
      )]

let c lexbuf = Sedlexing.Utf8.lexeme lexbuf
let new_line _lexbuf = ()

let rec initial lexbuf : Token.t =
  match%sedlex lexbuf with
  | Plus whitespace_char_no_newline   ->  initial lexbuf 
  | '\n'                          ->  new_line lexbuf; initial_linebegin lexbuf 
  | "/*"                          ->  multiline_comment lexbuf; initial lexbuf 
  | "//"                          ->  singleline_comment lexbuf; initial_linebegin lexbuf 
  | integer_constant              ->  CONSTANT 
  | decimal_floating_constant     ->  CONSTANT 
  | hexadecimal_floating_constant ->  CONSTANT 
  | preprocessing_number          ->  failwith "These characters form a preprocessor number, but not a constant" 
  | (Chars "LuU" | ""), "'"       ->  char lexbuf; char_literal_end lexbuf; CONSTANT 
  | (Chars "LuU" | "" | "u8"), "\"" ->  string_literal lexbuf; STRING_LITERAL
  | "..."                         ->  ELLIPSIS 
  | "+="                          ->  ADD_ASSIGN 
  | "-="                          ->  SUB_ASSIGN 
  | "*="                          ->  MUL_ASSIGN 
  | "/="                          ->  DIV_ASSIGN 
  | "%="                          ->  MOD_ASSIGN 
  | "|="                          ->  OR_ASSIGN 
  | "&="                          ->  AND_ASSIGN 
  | "^="                          ->  XOR_ASSIGN 
  | "<<="                         ->  LEFT_ASSIGN 
  | ">>="                         ->  RIGHT_ASSIGN 
  | "<<"                          ->  LEFT 
  | ">>"                          ->  RIGHT 
  | "=="                          ->  EQEQ 
  | "!="                          ->  NEQ 
  | "<="                          ->  LEQ 
  | ">="                          ->  GEQ 
  | "="                           ->  EQ 
  | "<"                           ->  LT 
  | ">"                           ->  GT 
  | "++"                          ->  INC 
  | "--"                          ->  DEC 
  | "->"                          ->  PTR 
  | "+"                           ->  PLUS 
  | "-"                           ->  MINUS 
  | "*"                           ->  STAR 
  | "/"                           ->  SLASH 
  | "%"                           ->  PERCENT 
  | "!"                           ->  BANG 
  | "&&"                          ->  ANDAND 
  | "||"                          ->  BARBAR 
  | "&"                           ->  AND 
  | "|"                           ->  BAR 
  | "^"                           ->  HAT 
  | "?"                           ->  QUESTION 
  | ":"                           ->  COLON 
  | "~"                           ->  TILDE 
  | "-> "|"<%"                    ->  LBRACE 
  | "}"|"%>"                      ->  RBRACE 
  | "["|"<:"                      ->  LBRACK 
  | "]"|":>"                      ->  RBRACK 
  | "("                           ->  LPAREN 
  | ")"                           ->  RPAREN 
  | ";"                           ->  SEMICOLON 
  | ","                           ->  COMMA 
  | "."                           ->  DOT 
  | "_Alignas"                    ->  ALIGNAS 
  | "_Alignof"                    ->  ALIGNOF 
  | "_Atomic"                     ->  ATOMIC 
  | "_Bool"                       ->  BOOL 
  | "_Complex"                    ->  COMPLEX 
  | "_Generic"                    ->  GENERIC 
  | "_Imaginary"                  ->  IMAGINARY 
  | "_Noreturn"                   ->  NORETURN 
  | "_Static_assert"              ->  STATIC_ASSERT 
  | "_Thread_local"               ->  THREAD_LOCAL 
  | "auto"                        ->  AUTO 
  | "break"                       ->  BREAK 
  | "case"                        ->  CASE 
  | "char"                        ->  CHAR 
  | "const"                       ->  CONST 
  | "continue"                    ->  CONTINUE 
  | "default"                     ->  DEFAULT 
  | "do"                          ->  DO 
  | "double"                      ->  DOUBLE 
  | "else"                        ->  ELSE 
  | "enum"                        ->  ENUM 
  | "extern"                      ->  EXTERN 
  | "float"                       ->  FLOAT 
  | "for"                         ->  FOR 
  | "goto"                        ->  GOTO 
  | "if"                          ->  IF 
  | "inline"                      ->  INLINE 
  | "int"                         ->  INT 
  | "long"                        ->  LONG 
  | "register"                    ->  REGISTER 
  | "restrict"                    ->  RESTRICT 
  | "return"                      ->  RETURN 
  | "short"                       ->  SHORT 
  | "signed"                      ->  SIGNED 
  | "sizeof"                      ->  SIZEOF 
  | "static"                      ->  STATIC 
  | "struct"                      ->  STRUCT 
  | "switch"                      ->  SWITCH 
  | "typedef"                     ->  TYPEDEF 
  | "union"                       ->  UNION 
  | "unsigned"                    ->  UNSIGNED 
  | "void"                        ->  VOID 
  | "volatile"                    ->  VOLATILE 
  | "while"                       ->  WHILE 
  | identifier                    ->  NAME (c lexbuf)
  | eof                           ->  EOF 
  | _                             ->  failwith "Lexer error" 
  
and initial_linebegin lexbuf =
  match%sedlex lexbuf with
  | '\n'                          ->  new_line lexbuf; initial_linebegin lexbuf 
  | whitespace_char_no_newline    ->  initial_linebegin lexbuf 
  | '#' | "%:"                    ->  hash lexbuf 
  | ""                            ->  initial lexbuf 
  | _                             ->  failwith "Lexer error" 

and char lexbuf =
  match%sedlex lexbuf with
  | simple_escape_sequence        -> ()
  | octal_escape_sequence         -> ()
  | hexadecimal_escape_sequence   -> () 
  | universal_character_name      -> ()
  | '\\'                          -> failwith "incorrect escape sequence" 
  | _                             -> () 

and char_literal_end lexbuf =
  match%sedlex lexbuf with
  | '\''       ->  ()
  | '\n' | eof ->  failwith "missing terminating \"'\" character" 
  | ""         ->  char lexbuf; char_literal_end lexbuf 
  | _          ->  failwith "Lexer error" 

and string_literal lexbuf =
  match%sedlex lexbuf with
  | '\"'       ->  () 
  | '\n' | eof ->  failwith "missing terminating '\"' character" 
  | ""         ->  char lexbuf; string_literal lexbuf 
  | _          ->  failwith "Lexer error" 

(* We assume gcc -E syntax but try to tolerate variations. *)
and hash lexbuf =
  match%sedlex lexbuf with
  | (Plus whitespace_char_no_newline, Star digit, Star whitespace_char_no_newline,
     "\"", Star (Compl (Chars "\n,\"")), "\"", Star (Compl '\n'), '\n'
    )
  | Star whitespace_char_no_newline, "pragma", Plus whitespace_char_no_newline,
    Star (Compl '\n'), '\n'
      ->  new_line lexbuf; initial_linebegin lexbuf 
  | Compl '\n', eof
      ->  failwith "unexpected end of file" 
  | _
      ->  failwith "Lexer error" 

(* Multi-line comment terminated by "*/" *)
and multiline_comment lexbuf =
  match%sedlex lexbuf with
  | "*/"   ->  () 
  | eof    ->  failwith "unterminated comment" 
  | '\n'   ->  new_line lexbuf; multiline_comment lexbuf 
  | _      ->  multiline_comment lexbuf 

(* Single-line comment terminated by a newline *)
and singleline_comment lexbuf =
  match%sedlex lexbuf with
  | '\n'   ->  new_line lexbuf 
  | eof    ->  () 
  | _      ->  singleline_comment lexbuf 


(* This lexer chooses between [inital] or [initial_linebegin],
     depending on whether we are at the beginning of the line or
     not. *)

let lexer (lexbuf : Sedlexing.lexbuf) : Token.t =
  let pos = Sedlexing.lexing_position_curr lexbuf in
  if pos.pos_cnum = pos.pos_bol then
    initial_linebegin lexbuf
  else
    initial lexbuf

(* In the following, we define a new lexer, which wraps [lexer], and applies
     the following two transformations to the token stream:

   - A [NAME] token is replaced with a sequence of either [NAME VARIABLE] or
       [NAME TYPE]. The decision is made via a call to [Context.is_typedefname].
       The call takes place only when the second element of the sequence is
       demanded.

   - When [Options.atomic_strict_syntax] is [true] and an opening parenthesis
       [LPAREN] follows an [ATOMIC] keyword, the parenthesis is replaced by a
       special token, [ATOMIC_LPAREN], so as to allow the parser to treat it
       specially. *)

(* This second lexer is implemented using a 3-state state machine, whose
   states are as follows. *)

module State = struct
  module Kind = struct
    type t =
      | Regular          (* Nothing to recall from the previous tokens. *)
      | Atomic           (* The previous token was [ATOMIC]. If an opening
                            parenthesis follows, then it needs special care. *)
      | Ident of string  (* We have seen an identifier: we have just
                             emitted a [NAME] token. The next token will be
                             either [VARIABLE] or [TYPE], depending on
                             what kind of identifier this is. *)
  end

  type t =
    { mutable kind : Kind.t
    ; is_typedefname : string -> bool
    ; atomic_strict_syntax : bool
    }

  let create_default
      ?(is_typedefname = fun _ -> false) ?(atomic_strict_syntax=false) () =
    { kind = Regular
    ; is_typedefname
    ; atomic_strict_syntax
    }

  let wrap t lexer =
    fun lexbuf -> lexer t lexbuf
end

let lexer (state : State.t) (lexbuf : Sedlexing.lexbuf) : Token.t =
  match state.kind with
  | Ident id ->
    state.kind <- Regular;
    if state.is_typedefname id then TYPE else VARIABLE
  | Atomic
  | Regular ->
    let token = lexer lexbuf in
    match state.kind, token with
    | _, NAME id ->
      state.kind <- Ident id;
      token

    | Atomic, LPAREN ->
      state.kind <- Regular;
      ATOMIC_LPAREN

    | _, ATOMIC ->
      state.kind <- (if state.atomic_strict_syntax then Atomic else Regular);
      token

    | _, _ ->
      state.kind <- Regular;
      token

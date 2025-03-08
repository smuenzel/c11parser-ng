(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2016-2017, Inria
 * SPDX-FileCopyrightText: (c) 2024-2025, Stefan Muenzel
 * SPDX-FileContributor: Jacques-Henri Jourdan, Inria Paris
 * SPDX-FileContributor: Francois Pottier, Inria Paris
 * SPDX-FileContributor: Stefan Muenzel
 *)

module Token = Token
module Literal = Literal
module Lexing_intf = Lexing_intf
module Sedlexing_intf = Sedlexing_intf
module Sedlexing_with_position = Sedlexing_with_position

module Sedlexing = struct
  module T = Sedlexing

  module Position = Util.Lexing_position

  type lexbuf = 
    { sedlexing: T.lexbuf
    ; mutable override_start_position : Lexing.position option
    }

  let create f = 
    { sedlexing = Sedlexing.create ~bytes_per_char:Uchar.utf_8_byte_length f
    ; override_start_position = None
    }

  let set_filename t f = T.set_filename t.sedlexing f

  let override_start_position t pos =
    t.override_start_position <- Some pos

  let start t =
    t.override_start_position <- None;
    T.start t.sedlexing

  let __private__next_int t = T.__private__next_int t.sedlexing
  let backtrack t = T.backtrack t.sedlexing
  let mark t = T.mark t.sedlexing

  let lexing_position_start t =
    match t.override_start_position with
    | Some p -> p
    | None ->
      T.lexing_position_start t.sedlexing

  let lexing_position_curr t = T.lexing_position_curr t.sedlexing

  let lexing_positions lexbuf =
    let start_p = lexing_position_start lexbuf
    and curr_p = lexing_position_curr lexbuf
    in
    (start_p, curr_p)

  let lexeme_length t = T.lexeme_length t.sedlexing
  let lexeme_char t = T.lexeme_char t.sedlexing

  let next_f1 lexbuf ~on_some ~on_none param =
    let ui = __private__next_int lexbuf in
    if ui == -1
    then on_none param
    else on_some param (Uchar.unsafe_of_int ui)

  let [@inline always] next_f2 lexbuf ~on_some ~on_none p1 p2 =
    let ui = __private__next_int lexbuf in
    if ui == -1
    then on_none p1 p2
    else on_some p1 p2 (Uchar.unsafe_of_int ui)

  let [@inline always] next_f3 lexbuf ~on_some ~on_none p1 p2 p3 =
    let ui = __private__next_int lexbuf in
    if ui == -1
    then on_none p1 p2 p3
    else on_some p1 p2 p3 (Uchar.unsafe_of_int ui)


  module Utf8 = struct
    let lexeme t = T.Utf8.lexeme t.sedlexing
    let sub_lexeme t = T.Utf8.sub_lexeme t.sedlexing

    let from_string s =
      { sedlexing =
          (T.Utf8.from_string s)
      ; override_start_position = None
      }
  end

  module Latin1 = struct
    let lexeme_char t = T.Latin1.lexeme_char t.sedlexing

  end

end

exception Lexer_error of 
    { state : string
    ; pos_start : int * int
    ; pos_end : int * int
    }

let print_lexer_error = function
  | Lexer_error { state; pos_start = (l1, c1); pos_end = (l2, c2)} ->
    Some (Printf.sprintf "%s: %d:%d-%d:%d\n" state l1 c1 l2 c2)
  | _ -> None

let () =
  Printexc.register_printer print_lexer_error

module Make(Sedlexing : Sedlexing_intf.S) = struct
  let raise_lexer_error lexbuf state =
    let pos_start, pos_end = Sedlexing.lexing_positions lexbuf in
    let pos_start = Sedlexing.Position.to_lexing_position pos_start in
    let pos_end = Sedlexing.Position.to_lexing_position pos_end in
    raise (Lexer_error
             { state
             ; pos_start = pos_start.pos_lnum, pos_start.pos_cnum - pos_start.pos_bol
             ; pos_end = pos_end.pos_lnum, pos_end.pos_cnum - pos_end.pos_bol
             })

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

  let literal_char_prefix lexbuf =
    match Sedlexing.Latin1.lexeme_char lexbuf 0 with
    | 'L' -> `Wide
    | 'u' -> `U16
    | 'U' -> `U32
    | '\'' -> `Plain
    | _ -> raise_lexer_error lexbuf "invalid literal character prefix"

  let literal_string_prefix lexbuf =
    match Sedlexing.Latin1.lexeme_char lexbuf 0 with
    | '"' -> `Plain
    | _ ->
      match Sedlexing.Utf8.sub_lexeme lexbuf 0 2 with
      | "L\"" -> `Wide
      | "u\"" -> `U16
      | "U\"" -> `U32
      | "u8" -> `Utf8
      | _ -> raise_lexer_error lexbuf "invalid literal string prefix"

  let rec initial lexbuf : Token.t =
    match%sedlex lexbuf with
    | Plus whitespace_char_no_newline   ->  initial lexbuf 
    | '\n'                          ->  new_line lexbuf; initial_linebegin lexbuf 
    | "/*"                          ->  multiline_comment lexbuf; initial lexbuf 
    | "//"                          ->  singleline_comment lexbuf; initial_linebegin lexbuf 
    | integer_constant              ->  CONSTANT_INTEGER (c lexbuf)
    | decimal_floating_constant     ->  CONSTANT_DECIMAL_FLOATING (c lexbuf)
    | hexadecimal_floating_constant ->  CONSTANT_HEXADECIMAL_FLOATING (c lexbuf)
    | preprocessing_number          ->  
      raise_lexer_error lexbuf "These characters form a preprocessor number, but not a constant" 
    | (Chars "LuU" | ""), "'"       ->
      let start_pos = Sedlexing.lexing_position_start lexbuf in
      let kind = literal_char_prefix lexbuf in
      let element = char lexbuf in
      let excess_elements = char_literal_end lexbuf in
      Sedlexing.override_start_position lexbuf start_pos;
      CONSTANT_CHAR { kind; value = element :: excess_elements }
    | (Chars "LuU" | "" | "u8"), "\"" ->
      let start_pos = Sedlexing.lexing_position_start lexbuf in
      let kind = literal_string_prefix lexbuf in
      let value = string_literal lexbuf in
      Sedlexing.override_start_position lexbuf start_pos;
      STRING_LITERAL [ { kind; value } ]
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
    | "{"|"<%"                      ->  LBRACE 
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
    | _                             ->  
      raise_lexer_error lexbuf "Initial"

  and initial_linebegin lexbuf =
    match%sedlex lexbuf with
    | '\n'                          ->  new_line lexbuf; initial_linebegin lexbuf 
    | whitespace_char_no_newline    ->  initial_linebegin lexbuf 
    | '#' | "%:"                    ->  hash lexbuf 
    | ""                            ->  initial lexbuf 
    | _                             ->  
      raise_lexer_error lexbuf "Initial_linebegin"

  and char lexbuf : Literal.Char.Element.t =
    match%sedlex lexbuf with
    | simple_escape_sequence        ->
      Literal.Char.Element.Escape (Sedlexing.Latin1.lexeme_char lexbuf 1)
    | octal_escape_sequence         ->
      Literal.Char.Element.Octal (c lexbuf)
    | hexadecimal_escape_sequence   ->
      Literal.Char.Element.Hex (c lexbuf)
    | universal_character_name      ->
      Literal.Char.Element.Universal (c lexbuf)
    | '\\'                          -> failwith "incorrect escape sequence" 
    | Compl (Chars "")              ->
      Literal.Char.Element.Plain (Sedlexing.lexeme_char lexbuf 0)
    | _                             -> 
      raise_lexer_error lexbuf "Char"

  and [@ocaml.tail_mod_cons] char_literal_end lexbuf =
    match%sedlex lexbuf with
    | '\''       ->  []
    | '\n' | eof ->  failwith "missing terminating \"'\" character" 
    | ""         ->
      let c = char lexbuf in
      c :: char_literal_end lexbuf 
    | _          ->  
      (raise_lexer_error [@tailcall false]) lexbuf "Char_literal_end"

  and [@ocaml.tail_mod_cons] string_literal lexbuf : Literal.Char.Element.t list =
    match%sedlex lexbuf with
    | '\"'       ->  []
    | '\n' | eof ->  failwith "missing terminating '\"' character" 
    | ""         ->
      let c = char lexbuf in 
      c :: string_literal lexbuf 
    | _          ->  
      (raise_lexer_error [@tailcall false]) lexbuf "String_literal"

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
      ->
      raise_lexer_error lexbuf "Hash"

  (* Multi-line comment terminated by "*/" *)
  and multiline_comment lexbuf =
    match%sedlex lexbuf with
    | "*/"   ->  () 
    | eof    ->  failwith "unterminated comment" 
    | '\n'   ->  new_line lexbuf; multiline_comment lexbuf 
    | Compl (Chars "") -> multiline_comment lexbuf 
    | _      ->
      raise_lexer_error lexbuf "Multiline_comment"

  (* Single-line comment terminated by a newline *)
  and singleline_comment lexbuf =
    match%sedlex lexbuf with
    | '\n'   ->  new_line lexbuf 
    | eof    ->  () 
    | Compl (Chars "") ->  singleline_comment lexbuf 
    | _
      ->
      raise_lexer_error lexbuf "Singleline_comment"


  (* This lexer chooses between [inital] or [initial_linebegin],
       depending on whether we are at the beginning of the line or
       not. *)

  module Raw_lexer = struct
    type token = Token.t
    type position = Sedlexing.Position.t
    type lexbuf = Sedlexing.lexbuf

    let lexeme_start_p lexbuf = Sedlexing.lexing_position_start lexbuf
    let lexeme_end_p lexbuf = Sedlexing.lexing_position_curr lexbuf

    let lexer (lexbuf : Sedlexing.lexbuf) : Token.t =
      let pos = Sedlexing.lexing_position_curr lexbuf in
      if Sedlexing.Position.is_beginning_of_line pos
      then initial_linebegin lexbuf
      else initial lexbuf
  end

  module _ : Lexing_intf.S_with_token = struct
    include Raw_lexer
  end

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

  module Wrapped_lexer(Inner_lexer : Lexing_intf.S_with_token with type token = Token.t) = struct
    type lexbuf = 
      { state : State.t
      ; inner_lexer : Inner_lexer.lexbuf
      ; mutable last_token : Token.t
      ; mutable current_token : Token.t
      }

    let create ~state ~inner_lexer =
      { state
      ; inner_lexer
      ; last_token = Token.EOF
      ; current_token = Token.EOF
      }

    type position = Inner_lexer.position
    type token = Token.t

    let lexeme_start_p lexbuf = Inner_lexer.lexeme_start_p lexbuf.inner_lexer
    let lexeme_end_p lexbuf = Inner_lexer.lexeme_end_p lexbuf.inner_lexer

    let lexer lexbuf : Token.t =
      let state = lexbuf.state in
      let result : Token.t =
        match state.kind with
        | Ident id ->
          state.kind <- Regular;
          if state.is_typedefname id then TYPE id else VARIABLE id
        | Atomic
        | Regular ->
          let token = Inner_lexer.lexer lexbuf.inner_lexer in
          match state.kind, (token : Token.t) with
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
      in
      lexbuf.last_token <- lexbuf.current_token;
      lexbuf.current_token <- result;
      result
  end

  module Wrapped_lexer_default = Wrapped_lexer(Raw_lexer)

end

include Make(Sedlexing)

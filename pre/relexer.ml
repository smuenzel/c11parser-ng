
module Utf8_stream =
  C11util.List_stream.Utf8(C11util.Lexing_position)
module Sedlexing_with_position =
  C11util.Sedlexing_with_position.Make(C11util.Lexing_position)
module Lexer_internal =
  C11lexer.Make(Sedlexing_with_position)

let relex_token
    ?(emit_defined=true)
    ~start_pos
    ~end_pos
    (token : Pre_token.t)
    ~f_none
    ~f_token
    ~f_string
    arg0
  =
  match token with
  | Newline ->
    f_none ~start_pos ~end_pos arg0
  | Eof ->
    f_token ~start_pos ~end_pos arg0 C11lexer.Token.EOF
  | Character_constant c ->
    f_token ~start_pos ~end_pos arg0 (C11lexer.Token.CONSTANT_CHAR c)
  | String_literal s ->
    f_token ~start_pos ~end_pos arg0 (C11lexer.Token.STRING_LITERAL s)
  | Identifier "defined" when emit_defined ->
    f_token ~start_pos ~end_pos arg0 C11lexer.Token.DEFINED
  | Header_name _
  | Preprocessing_number _
  | Punctuator _
  | Single_char _
  | Identifier _ ->
    let s = Pre_token.stringify token in
    f_string ~start_pos ~end_pos arg0 s

module Lexer_input = struct
  type t =
    | Token of C11lexer.Token.t
    | String of string
    | Empty
end

type 'inner_state t =
  { inner_state : 'inner_state
  ; get : 'inner_state -> Pre_token.t
  ; lexer_state : C11lexer.State.t
  ; queue : Lexer_input.t Queue.t
  ; mutable current_input : Lexer_input.t
  }

let create 
    ~inner_state
    ~get
    ~lexer_state
  =
  { inner_state
  ; get
  ; lexer_state
  ; queue = Queue.create ()
  ; current_input = Lexer_input.Empty
  }

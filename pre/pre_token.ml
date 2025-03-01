open Sexplib0.Sexp_conv
(* Should be added to sedlex *)

let sedlexing_next_f1 lexbuf ~on_some ~on_none param =
  let ui = Sedlexing.__private__next_int lexbuf in
  if ui == -1
  then on_none param
  else on_some param (Uchar.unsafe_of_int ui)

let [@inline always] sedlexing_next_f2 lexbuf ~on_some ~on_none p1 p2 =
  let ui = Sedlexing.__private__next_int lexbuf in
  if ui == -1
  then on_none p1 p2
  else on_some p1 p2 (Uchar.unsafe_of_int ui)

let [@inline always] sedlexing_next_f3 lexbuf ~on_some ~on_none p1 p2 p3 =
  let ui = Sedlexing.__private__next_int lexbuf in
  if ui == -1
  then on_none p1 p2 p3
  else on_some p1 p2 p3 (Uchar.unsafe_of_int ui)


let backslash = Uchar.of_char '\\'
let newline = Uchar.of_char '\n'

module Process_line_escape = struct
  module State = struct
    type t =
      | Normal
      | Backslash
      | Extra
  end

  type t =
    { mutable state : State.t
    ; mutable pos : int
    ; mutable count : int
    ; mutable max_pos : int
    ; mutable extra : Uchar.t
    }

  let [@inline always] incr t state =
    t.state <- state;
    t.count <- t.count + 1;
    t.pos <- t.pos + 1

  let make inner_lexbuf =
    let t =
      { state = Normal
      ; pos = 0
      ; max_pos = 0
      ; count = 0
      ; extra = Uchar.min
      }
    in
    fun (a : Uchar.t array) pos count ->
      t.pos <- pos;
      t.max_pos <- pos + count;
      t.count <- 0;
      begin match t.state with
        | Extra ->
          Array.unsafe_set a t.pos t.extra;
          incr t Normal
        | _ -> ()
      end;
      while (t.pos < t.max_pos)
            &&
            (sedlexing_next_f2 inner_lexbuf
               ~on_some:(fun t a uchar ->
                   match t.state
                       , Uchar.equal uchar backslash
                       , Uchar.equal uchar newline
                   with
                   | Normal, true, _ -> 
                     t.state <- Backslash;
                     true
                   | Normal, false, _ ->
                     Array.unsafe_set a t.pos uchar; 
                     incr t Normal;
                     true
                   | Backslash, bs, false ->
                     Array.unsafe_set a t.pos backslash;
                     incr t (if bs then Backslash else Normal);
                     if (not bs)
                     then begin
                       if t.pos < t.max_pos
                       then begin
                         Array.unsafe_set a t.pos uchar;
                         incr t Normal;
                         true
                       end else begin 
                         t.state <- Extra;
                         t.extra <- uchar;
                         false
                       end
                     end
                     else true
                   | Backslash, _, true ->
                     t.state <- Normal;
                     true
                   | Extra, _, _ -> assert false
                 )
               ~on_none:(fun t a ->
                   match t.state with
                   | Normal -> false
                   | Backslash ->
                     Array.unsafe_set a t.pos backslash;
                     incr t Normal;
                     false
                   | Extra -> assert false
                 )
               t a
            )
      do
        ()
      done;
      t.count

  let make_lexbuf inner_lexbuf =
    let f = make inner_lexbuf in
    let result = Sedlexing.create f in
    Sedlexing.set_filename result
      (Sedlexing.lexing_position_start inner_lexbuf).pos_fname;
    result

end

module Position = struct
  type t = Lexing.position =
    { pos_fname : string
    ; pos_lnum : int
    ; pos_bol : int
    ; pos_cnum : int
    } [@@deriving sexp]
end

module Token_properties = struct
  type t =
    { start : Position.t; end_ : Position.t }
  [@@deriving sexp]
end

type header_name_kind =
  | System | Local
[@@deriving sexp]

type header_name = header_name_kind * string
[@@deriving sexp]

type t =
  | Header_name of header_name
  | Identifier of string
  | Preprocessing_number of string
  | Character_constant
  | String_literal of string
  | Punctuator of { preceeded_by_whitespace : bool; value : string }
  | Single_char
  | Newline
  | Eof
[@@deriving sexp]

type token = t

let nondigit = [%sedlex.regexp? 'a' .. 'z' | 'A' .. 'Z' | '_']
let digit = [%sedlex.regexp? '0' .. '9']

let digit_or_nondigit = [%sedlex.regexp? digit | nondigit]

let identifier =
  [%sedlex.regexp? nondigit, Star digit_or_nondigit]

let s_char_noescape =
  [%sedlex.regexp? Compl ('\\' | '\n' | '"')]

let preprocessing_exponent =
  [%sedlex.regexp? Chars "eEpP"]
let plus_minus =
  [%sedlex.regexp? '+' | '-']

let preprocessing_number =
  [%sedlex.regexp?
      Opt '.', digit, Star (digit_or_nondigit | ( preprocessing_exponent, plus_minus))
  ]

let h_char = [%sedlex.regexp? Compl('\n' | '>')]
let q_char = [%sedlex.regexp? Compl('\n' | '"')]

let h_char_sequence = [%sedlex.regexp? Plus h_char]
let q_char_sequence = [%sedlex.regexp? Plus q_char]

let header_name_h =
  [%sedlex.regexp? '<', h_char_sequence, '>']

let header_name_q =
  [%sedlex.regexp? '"', q_char_sequence, '"']

let header_name =
  [%sedlex.regexp? header_name_h | header_name_q]

let multi_punctator =
  [%sedlex.regexp?
      "->" | "++" | "--" | "<<" | ">>" | "<=" | ">=" | "==" | "!=" | "&&" | "||"
                 | "..." | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&="
                 | "^=" | "|=" | "##" | "<:" | ":>" | "<%" | "%>" | "%:" | "%:%:"
  ]

let punctuator =
  [%sedlex.regexp?
      multi_punctator | Chars "[](){}.&*+-~!/%<>^|?:;=,#"
  ]

let white_space_no_newline =
  [%sedlex.regexp? Sub(white_space, '\n')]

let c lexbuf =
  Sedlexing.Utf8.lexeme lexbuf

let cn lexbuf a b =
  let len = if b < 0 then Sedlexing.lexeme_length lexbuf + b - a else b - a in
  Sedlexing.Utf8.sub_lexeme lexbuf a len

let produce_with_position lexbuf token =
  let start, end_ = Sedlexing.lexing_positions lexbuf in
  Token_properties.{ start; end_ }, token

let produce_plain _ token = token

let rec next_token ~produce lexbuf =
  match%sedlex lexbuf with
  | "//", Star (Compl '\n'), '\n' -> produce lexbuf Newline
  | "//", Star (Compl '\n'), eof -> produce lexbuf Eof
  | "/*" -> skip_comment ~produce lexbuf
  | identifier -> produce lexbuf (Identifier (c lexbuf))
  | preprocessing_number -> produce lexbuf (Preprocessing_number (c lexbuf))
  | header_name_h -> produce lexbuf (Header_name (System, cn lexbuf 1 (-1)))
  | header_name_q -> produce lexbuf (Header_name (Local, cn lexbuf 1 (-1)))
  | Plus white_space_no_newline, punctuator ->
    produce lexbuf (Punctuator { preceeded_by_whitespace = true; value = c lexbuf })
  | punctuator ->
    (* CR smuenzel: need to consider comment for whitespace, or could make this position-based? *)
    produce lexbuf (Punctuator { preceeded_by_whitespace = false; value = c lexbuf })
  | '\n' -> produce lexbuf Newline
  | Plus white_space_no_newline -> next_token ~produce lexbuf
  | eof -> produce lexbuf Eof
  | _ -> assert false
and skip_comment ~produce lexbuf =
  match%sedlex lexbuf with
  | "*/" -> next_token ~produce lexbuf
  | any -> skip_comment ~produce lexbuf
  | _ -> assert false

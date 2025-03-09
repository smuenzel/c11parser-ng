
(*
let directive lexbuf =
  let token = Pre_token.next_token ~produce:Pre_token.produce_plain lexbuf in
  match token with
  | Identifier "if"
  | Identifier "ifdef"
  | Identifier "ifndef"
  | Identifier "elif"
  | Identifier "else"
  | Identifier "endif" -> true
  | Identifier "include"
  | Identifier "define"
  | Identifier "undef" -> true
  | Identifier "line" -> true
  | Identifier "error"
  | Identifier "pragma" -> true
  | Newline
  | Eof
  | _ -> false
   *)

(*
let line lexbuf =
  let token = Pre_token.next_token ~produce:Pre_token.produce_plain lexbuf in
  match token with
  | Punctuator { value = "#"; _} ->
    ()
  | _ -> ()

   *)

(*
module Reparser = struct
  type t = 
    { mutable tokens : (Pre_token.Token_properties.t * Pre_token.t) list
    ; mutable properties : Pre_token.Token_properties.t
    }

  module As_sedlex = struct
    type lexbuf = t

  end

  module As_lexing = struct
    type position = Lexing.position
    type lexbuf = t

    let lexeme_start_p _ = assert false
    let lexeme_end_p _ = assert false

    let lexer t =
      match t.tokens with
      | [] ->
        t.properties <- Pre_token.Token_properties.dummy;
        C11lexer.Token.EOF
      | (properties, token) :: rest ->
        t.properties <- properties;
        t.tokens <- rest;
        ()
  end

end
   *)

module Utf8_stream = C11util.List_stream.Utf8(C11util.Lexing_position)
module Sedlexing_with_position = C11util.Sedlexing_with_position.Make(C11util.Lexing_position)
module Lexer_x = C11lexer.Make(Sedlexing_with_position)

let rec finish_stream
    ~lexing_state
    stream
    (list : (Lexing.position * Lexing.position * C11lexer.Token.t) list)
    rest
  =
  let stream = Utf8_stream.create (List.rev stream) in
  let lexbuf = Sedlexing_with_position.create stream in
  let lexbuf =
    Lexer_x.Wrapped_lexer_default.create
      ~state:lexing_state
      ~inner_lexer:lexbuf
  in
  [ Lexer_x.Wrapped_lexer_default.all_with_position lexbuf ]
  :: [ list ]
  :: convert_tokens ~lexing_state ~stream:[] rest

and convert_tokens ~lexing_state ~stream list =
  match (list : (Lexing.position * Lexing.position * Pre_token.t) list) with
  | [] ->
    begin match stream with
      | [] -> []
      | stream ->
        finish_stream ~lexing_state stream [] []
    end
  | (p0, p1, token) :: rest ->
    match token with
    | Eof -> 
      finish_stream ~lexing_state stream [p0, p1, C11lexer.Token.EOF] rest
    | Character_constant c ->
      finish_stream ~lexing_state stream [p0, p1, CONSTANT_CHAR c] rest
    | String_literal s ->
      finish_stream ~lexing_state stream [p0, p1, STRING_LITERAL s] rest
    | Newline
    | Header_name _
    | Preprocessing_number _
    | Punctuator _
    | Single_char _
    | Identifier _ ->
      let stream = (p0, p1, Pre_token.stringify token) :: stream in
      convert_tokens ~lexing_state ~stream rest


let rec getline ~acc lexbuf =
  let (_, token) as result =
    Pre_token.next_token ~produce:Pre_token.produce_with_position lexbuf
  in
  match token with
  | Newline -> Some (List.rev acc)
  | Eof -> 
    begin match acc with
      | [] -> None
      | _ -> Some (List.rev acc)
    end
  | _ -> getline ~acc:(result :: acc) lexbuf

let getline lexbuf = getline ~acc:[] lexbuf

let convert_token token : [`T of C11lexer.Token.t | `S of string ] list =
  Relexer.relex_token
    ~start_pos:()
    ~end_pos:()
    token
    ~f_none:(fun ~start_pos:_ ~end_pos:_ () -> [])
    ~f_token:(fun ~start_pos:_ ~end_pos:_ () token -> [ `T token ])
    ~f_string:(fun ~start_pos:_ ~end_pos:_ () s -> [ `S s ])
    ()

let [@ocaml.tail_mod_cons] rec convert_lexbuf lexbuf =
  match C11lexer.initial lexbuf with
  | EOF -> [ ]
  | token -> token :: convert_lexbuf lexbuf

let convert_string_rev sr =
  match sr with
  | [] -> []
  | _ ->
    let str = String.concat " " (List.rev sr) in
    let lexbuf = C11lexer.Sedlexing.Utf8.from_string str in
    convert_lexbuf lexbuf

let rec consolidate_s ~acc =
  function 
  | [] -> 
    convert_string_rev acc
  | `S a :: rest -> 
    consolidate_s ~acc:(a :: acc) rest
  | (`T t) :: rest ->
    let res = convert_string_rev acc in
    List.concat 
      [ res; [ t ]; consolidate_s ~acc:[] rest]


let convert_token_sequence seq =
  let seq' = List.concat_map convert_token seq in
  consolidate_s ~acc:[] seq'

let process_if tokens =
  `If (convert_token_sequence (List.map snd tokens))

let process_line = function
  | (_, Pre_token.Punctuator { value = "#"; _}) :: (_, Identifier "if") :: rest ->
    process_if rest
  | _ -> `Unknown



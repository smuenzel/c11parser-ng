
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

module Utf8_list_stream(P : Util.Position_intf.S) = struct
  let create (strings : (P.t * P.t * string) list) : (P.t * Uchar.t) Seq.t =
    let rec aux ((_, p1, s) as curr) i pcurr rest () =
      if i = String.length s
      then begin
        match rest with
        | [] -> Seq.Nil
        | (p0, _,_) as hd :: tl -> aux hd 0 p0 tl ()
      end
      else begin
        let result = String.get_utf_8_uchar s i in
        if Uchar.utf_decode_is_valid result
        then begin
          let p = P.incr pcurr in
          let p = P.min p p1 in
          Seq.Cons ((p, Uchar.utf_decode_uchar result), aux curr (i + 1) p rest)
        end
        else assert false
      end
    in
    match strings with
    | [] -> (fun () -> Seq.Nil)
    | (p0, _, _) as hd :: tl -> aux hd 0 p0 tl
end

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
  match (token : Pre_token.t) with
  | Eof -> [ `T EOF ]
  | Newline -> [ ]
  | Character_constant c ->
    [ `T (CONSTANT_CHAR c) ]
  | String_literal s ->
    [ `T (STRING_LITERAL s) ]
  | Identifier "defined" ->
    [ `T DEFINED ]
  | Header_name _
  | Preprocessing_number _
  | Punctuator _
  | Single_char _
  | Identifier _ ->
    [ `S (Pre_token.stringify token) ]

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



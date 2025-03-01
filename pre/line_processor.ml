
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

let line lexbuf =
  let token = Pre_token.next_token ~produce:Pre_token.produce_plain lexbuf in
  match token with
  | Punctuator { value = "#"; _} ->
    ()
  | _ -> ()




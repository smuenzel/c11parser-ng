
type t = Lexing.position

let to_lexing_position p = p

let is_beginning_of_line (p : t) = p.pos_cnum = p.pos_bol

let incr (p : t) = { p with pos_cnum = p.pos_cnum + 1 }

let dummy = Lexing.dummy_pos

let min (a :t) (b : t) =
  if a.pos_fname == b.pos_fname || String.equal a.pos_fname b.pos_fname
  then begin
    if a.pos_lnum < b.pos_lnum
    then a
    else if a.pos_lnum > b.pos_lnum
    then b
    else if a.pos_cnum < b.pos_cnum
    then a
    else b
  end else a


type t = Lexing.position

let to_lexing_position p = p

let is_beginning_of_line (p : t) = p.pos_cnum = p.pos_bol

let incr (p : t) = { p with pos_cnum = p.pos_cnum + 1 }

let dummy = Lexing.dummy_pos

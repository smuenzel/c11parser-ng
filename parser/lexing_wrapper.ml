
module Lexing = struct
  include Sedlexing
  type lexbuf = Sedlexing.lexbuf
  type position = Lexing.position
end

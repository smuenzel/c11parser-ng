(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2024-2025 Stefan Muenzel
 *)

open Sexplib0.Sexp_conv

module Uchar = struct
  include Uchar

  let sexp_of_t t = 
    if Uchar.is_char t
    then sexp_of_char (Uchar.to_char t)
    else sexp_of_int (Uchar.to_int t)

  let t_of_sexp sexp =
    match (sexp : Sexplib0.Sexp.t) with
    | Atom s ->
      if String.length s = 1
      then Uchar.of_char (char_of_sexp sexp)
      else Uchar.of_int (int_of_sexp sexp)
    | _ -> failwith "Uchar.t_of_sexp"

end

module Char = struct
  module Kind = struct
    type t =
      [ `Plain
      | `Wide
      | `U16
      | `U32
      ]
    [@@deriving sexp]

    let to_string = function
      | `Plain -> "'"
      | `Wide -> "L'"
      | `U16 -> "u'"
      | `U32 -> "U'"
  end

  module Element = struct
    type t =
      | Universal of string
      | Hex of string
      | Octal of string
      | Escape of char
      | Plain of Uchar.t
    [@@deriving sexp]
  end

  type t =
    { kind : Kind.t
    ; value : Element.t list
    } [@@deriving sexp]
end

module String = struct
  module Kind = struct
    type t =
      [ `Plain
      | `Utf8
      | `Wide
      | `U16
      | `U32
      ]
    [@@deriving sexp]

    let to_string = function
      | `Plain -> "\""
      | `Utf8 -> "u8\""
      | `Wide -> "L\""
      | `U16 -> "u\""
      | `U32 -> "U\""
  end

  module Segment = struct
    type t =
      { kind : Kind.t
      ; value : Char.Element.t list
      } [@@deriving sexp]
  end

  type t = Segment.t list [@@deriving sexp]
end

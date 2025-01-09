open Base
open Core

module type Located = sig
  type 'a t [@@deriving sexp, bin_io, compare, hash]
  val locate : start:Lexing.position -> end_:Lexing.position -> 'a -> 'a t
end

module Var_name = String_id.Make(struct let module_name = "Var_name" end)()
module Typedef_name = String_id.Make(struct let module_name = "Typedef_name" end)()
module General_identifier = String_id.Make(struct let module_name = "General_identifier" end)()

module Type_qualifier = struct
  type t =
    | Const
    | Restrict
    | Volatile
    | Atomic
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Storage_class_specifier = struct
  type t =
    | Static
    | Extern
    | Thread_local
    | Auto
    | Register
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Function_specifier = struct
  type t =
    | Inline
    | Noreturn
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Type_specifier_nonunique = struct
  type t =
    | Char
    | Short
    | Int
    | Long
    | Float
    | Double
    | Signed
    | Unsigned
    | Complex
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Struct_or_union = struct
  type t =
    | Struct
    | Union
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Equality_operator = struct
  type t =
    | Equal
    | Not_equal
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Relational_operator = struct
  type t =
    | Less
    | Greater
    | Less_equal
    | Greater_equal
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Shift_operator = struct
  type t =
    | Left
    | Right
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Additive_operator = struct
  type t =
    | Plus
    | Minus
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Multiplicative_operator = struct
  type t =
    | Multiply
    | Divide
    | Modulo
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Logical_operator = struct
  type t =
    | Logical_and
    | Logical_or
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Bitwise_operator = struct
  type t =
    | Bitwise_and
    | Bitwise_xor
    | Bitwise_or
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Assignment_operator = struct
  type t =
    | Plain
    | Bitwise of Bitwise_operator.t
    | Additive of Additive_operator.t
    | Multiplicative of Multiplicative_operator.t
    | Shift of Shift_operator.t
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Unary_operator = struct
  type t =
    | Address_of
    | Dereference
    | Plus
    | Minus
    | Logical_not
    | Bitwise_not
    | Sizeof
    | Preincrement
    | Predecrement
    | Postincrement
    | Postdecrement
  [@@deriving sexp, bin_io, compare, hash, variants]
end

module Literal = struct
  module Char = struct
    module Kind = struct
      type t =
        [ `Plain
        | `Wide
        | `U16
        | `U32
        ]
      [@@deriving sexp, bin_io, compare, hash]
    end

    module Element = struct
      type t = C11lexer.Literal.Char.Element.t =
        | Universal of string
        | Hex of string
        | Octal of string
        | Escape of char
        | Plain of Uchar.t
      [@@deriving sexp, bin_io, compare, hash, variants]
    end

    type t =
      { kind : Kind.t
      ; value : Element.t list
      } [@@deriving sexp, bin_io, compare, hash]
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
      [@@deriving sexp, bin_io, compare, hash]
    end

    module Segment = struct
      type t = C11lexer.Literal.String.Segment.t =
        { kind : Kind.t
        ; value : Char.Element.t list
        } [@@deriving sexp, bin_io, compare, hash]
    end

    type t = Segment.t list [@@deriving sexp, bin_io, compare, hash]
  end
end

module Constant = struct
  type t =
    | Char of Literal.Char.t
    | String of Literal.String.t
    | Integer of string
    | Decimal_floating of string
    | Hexadecimal_floating of string
  [@@deriving sexp, bin_io, compare, hash]
end

module Make(L : Located) = struct
  type 'a located = 'a L.t [@@deriving sexp, bin_io, compare, hash]
  let locate = L.locate

  module Var_name = Var_name
  module Typedef_name = Typedef_name
  module General_identifier = General_identifier

  module Type_qualifier = Type_qualifier
  module Storage_class_specifier = Storage_class_specifier
  module Function_specifier = Function_specifier
  module Type_specifier_nonunique = Type_specifier_nonunique
  module Struct_or_union = Struct_or_union
  module Equality_operator = Equality_operator
  module Relational_operator = Relational_operator
  module Shift_operator = Shift_operator
  module Additive_operator = Additive_operator
  module Multiplicative_operator = Multiplicative_operator
  module Logical_operator = Logical_operator
  module Bitwise_operator = Bitwise_operator
  module Assignment_operator = Assignment_operator
  module Unary_operator = Unary_operator
  module Constant = Constant

  module%gen rec Enum_constant : sig
    type t =
      { name : General_identifier.t
      ; value : Expr.t located option
      } [@@deriving sexp, bin_io, compare, hash]
  end = Enum_constant

  and Enum : sig
    type t =
      | Named of General_identifier.t
      | Defined of 
          { name : General_identifier.t option
          ; values : Enum_constant.t list
          }
      [@@deriving sexp, bin_io, compare, hash]
  end = Enum

  and Type_specifier_unique : sig
    type t =
      | Void
      | Bool
      | Atomic of Type_name.t
      | Enum of Enum.t
      | Struct_or_union of Struct_or_union_specifier.t
      | Name of Typedef_name.t
    [@@deriving sexp, bin_io, compare, hash]
  end = Type_specifier_unique

  and Struct_or_union_specifier : sig
    type t =
      | Named of { kind : Struct_or_union.t; name : General_identifier.t }
      | Defined of { kind : Struct_or_union.t
                   ; name : General_identifier.t option
                   ; declaration : Struct_declaration.t list
                   }
    [@@deriving sexp, bin_io, compare, hash]
  end = Struct_or_union_specifier

  and Expr : sig
    type t

  end = Expr

  and Type_name : sig
    type t

  end = Type_name

end

(*
open struct
  module _ (L : Located) : C11parser.Astgen_intf.S = struct
    include Make(L)
  end
end
   *)

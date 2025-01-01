
type 'a with_pos = start:Lexing.position -> end_:Lexing.position -> 'a

module Literal = C11lexer.Literal

module type S = sig

  type 'a located

  val locate : ('a -> 'a) with_pos

  module Equality_operator : sig
    type t
    val equal : t
    val not_equal : t
  end

  module Relational_operator : sig
    type t
    val less : t
    val greater : t
    val less_equal : t
    val greater_equal : t
  end

  module Shift_operator : sig
    type t
    val left : t
    val right : t
  end

  module Additive_operator : sig
    type t
    val plus : t
    val minus : t
  end

  module Multiplicative_operator : sig
    type t
    val multiply : t
    val divide : t
    val modulo : t
  end

  module Unary_operator : sig
    type t
    val address_of : t
    val dereference : t
    val plus : t
    val minus : t
    val logical_not : t
    val bitwise_not : t
    val sizeof : t
    val preincrement : t
    val predecrement : t
    val postincrement : t
    val postdecrement : t
  end

  module Var_name : sig
    type t

    val of_string : string -> t
  end

  module Typedef_name : sig
    type t

    val of_string : string -> t
  end

  module General_identifier : sig
    type t

    val var_name : Var_name.t -> t
    val typedef_name : Typedef_name.t -> t
  end

  module Constant : sig
    type t

    val char : Literal.Char.t -> t
    val string : Literal.String.t -> t
    val integer : string -> t
    val decimal_floating : string -> t
    val hexadecimal_floating : string -> t
  end

  module rec Generic_association : sig
    type t

    val default : Expr.t located -> t
    val type_name : unit -> Expr.t located -> t
  end

  and Expr : sig
    type t
    type t' := t located
    type 'op binary = t' * 'op * t' -> t
    type 'op unary = 'op * t' -> t

    val equality : Equality_operator.t binary
    val relational : Relational_operator.t binary
    val shift : Shift_operator.t binary
    val additive : Additive_operator.t binary
    val multiplicative : Multiplicative_operator.t binary
    val unary : Unary_operator.t unary
    val cast : (unit * t') -> t
    val sizeof : unit -> t
    val alignof : unit -> t
    val dot : (t' * General_identifier.t) -> t
    val arrow : (t' * General_identifier.t) -> t
    val var : Var_name.t -> t
    val constant : Constant.t -> t

    val generic : unit -> t
  end
end

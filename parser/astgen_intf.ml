
type 'a with_pos = start:Lexing.position -> end_:Lexing.position -> 'a

module Literal = C11lexer.Literal

module type S = sig

  type 'a located

  val locate : ('a -> 'a located) with_pos

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

    val of_string : string -> t
  end


  module Type_qualifier : sig
    type t
    val const : t
    val restrict : t
    val volatile : t
    val atomic : t
  end

  module Type_specifier_nonunique : sig
    type t
    val char : t
    val short : t
    val int : t
    val long : t
    val float : t
    val double : t
    val signed : t
    val unsigned : t
    val complex : t
  end

  module rec Enum_constant : sig
    type t

    val named : ?value:Expr.t -> General_identifier.t -> t
  end

  and Enum : sig
    type t

    val named : General_identifier.t -> t
    val defined : General_identifier.t option * Enum_constant.t list Util.Stored_reversed.t -> t
  end

  and Type_specifier_unique : sig
    type t
    val void : t
    val bool : t
    val atomic : unit -> t
    val enum : Enum.t -> t
    val struct_or_union : Struct_or_union_specifier.t -> t
    val name : Typedef_name.t -> t
  end

  and Struct_or_union : sig
    type t
    val struct_ : t
    val union : t
  end

  and Struct_or_union_specifier : sig
    type t

    val named : Struct_or_union.t * General_identifier.t -> t

    val defined : Struct_or_union.t * General_identifier.t option * Struct_declaration.t list -> t
  end

  and Struct_declaration : sig
    type t = unit


  end

  and Equality_operator : sig
    type t
    val equal : t
    val not_equal : t
  end

  and Relational_operator : sig
    type t
    val less : t
    val greater : t
    val less_equal : t
    val greater_equal : t
  end

  and Shift_operator : sig
    type t
    val left : t
    val right : t
  end

  and Additive_operator : sig
    type t
    val plus : t
    val minus : t
  end

  and Multiplicative_operator : sig
    type t
    val multiply : t
    val divide : t
    val modulo : t
  end

  and Logical_operator : sig
    type t
    val logical_and : t
    val logical_or : t
  end

  and Bitwise_operator : sig
    type t
    val bitwise_and : t
    val bitwise_xor : t
    val bitwise_or : t
  end

  and Assignment_operator : sig
    type t
    val plain : t
    val bitwise : Bitwise_operator.t -> t
    val multiplicative : Multiplicative_operator.t -> t
    val additive : Additive_operator.t -> t
    val shift : Shift_operator.t -> t
  end

  and Unary_operator : sig
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

  and Constant : sig
    type t

    val char : Literal.Char.t -> t
    val string : Literal.String.t -> t
    val integer : string -> t
    val decimal_floating : string -> t
    val hexadecimal_floating : string -> t
  end

  and Generic_association : sig
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
    val logical : Logical_operator.t binary
    val bitwise : Bitwise_operator.t binary
    val assignment : Assignment_operator.t binary
    val unary : Unary_operator.t unary
    val question : (t' * t' * t') -> t
    val cast : (unit * t') -> t
    val sizeof : unit -> t
    val alignof : unit -> t
    val dot : (t' * General_identifier.t) -> t
    val arrow : (t' * General_identifier.t) -> t
    val var : Var_name.t -> t
    val constant : Constant.t -> t

    val generic : unit -> t

    val comma : t' * t' -> t
  end
   
  and Alignment_specifier : sig
    type t
    val alignas_type : unit -> t
    val alignas_expression : Expr.t located -> t
  end

  and Specifier_qualifier_list : sig
    type t

    type qualifier_or_alignment := (Type_qualifier.t, Alignment_specifier.t) Either.t

    val unique : (Type_specifier_unique.t, qualifier_or_alignment) Util.List_eq1.t -> t
    val nonunique : (Type_specifier_nonunique.t, qualifier_or_alignment) Util.List_ge1.t -> t
  end

  and Declaration_specifier : sig
    type t

    val type_unique : (Type_specifier_unique.t, unit) Util.List_eq1.t -> t
    val type_nonunique : (Type_specifier_nonunique.t, unit) Util.List_ge1.t -> t
  end

  and Declaration_specifier_typedef : sig
    type t

    val type_unique : (Typedef.t, Type_specifier_unique.t, unit) Util.List_eq1_eq1.t -> t
    val type_nonunique : (Typedef.t, Type_specifier_nonunique.t, unit) Util.List_eq1_ge1.t -> t
  end

  and Typedef : sig
    type t

    val typedef : t
  end

  and Abstract_declarator : sig

  end

  and Pointer : sig
    type t

    val make : (Type_qualifier.t list Util.Stored_reversed.t option * t option) -> t

  end

  and Declarator : sig
    type t

    val identifier : General_identifier.t -> t
    val pointer : (Pointer.t option * t) -> t

    type type_qualifier_list := Type_qualifier.t list Util.Stored_reversed.t option

    val array : (t * type_qualifier_list * Expr.t located option) -> t 
    val static_array : (t * type_qualifier_list* Expr.t located) -> t
    val unspecified_size_variable_array : (t * type_qualifier_list) -> t

    val function_ : t * (Parameter_type_list.t, Var_name.t list option) Either.t -> t
  end

  and Parameter_type_list : sig
    type t

    val make : (unit list Util.Stored_reversed.t * bool) -> t
  end

end


type 'a with_pos = start:Lexing.position -> end_:Lexing.position -> 'a

module Literal = C11lexer.Literal

module type S = sig
  type 'a rev := 'a list Util.Stored_reversed.t

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

  module Typedef : sig
    type t

    val typedef : t
  end

  module Type_qualifier : sig
    type t
    val const : t
    val restrict : t
    val volatile : t
    val atomic : t
  end

  module Storage_class_specifier : sig
    type t
    val static : t
    val extern : t
    val thread_local : t
    val auto : t
    val register : t
  end

  module Function_specifier : sig
    type t
    val inline : t
    val noreturn : t
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

  module Struct_or_union : sig
    type t
    val struct_ : t
    val union : t
  end

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

  module Logical_operator : sig
    type t
    val logical_and : t
    val logical_or : t
  end

  module Bitwise_operator : sig
    type t
    val bitwise_and : t
    val bitwise_xor : t
    val bitwise_or : t
  end

  module Assignment_operator : sig
    type t
    val plain : t
    val bitwise : Bitwise_operator.t -> t
    val multiplicative : Multiplicative_operator.t -> t
    val additive : Additive_operator.t -> t
    val shift : Shift_operator.t -> t
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

  module Constant : sig
    type t

    val char : Literal.Char.t -> t
    val string : Literal.String.t -> t
    val integer : string -> t
    val decimal_floating : string -> t
    val hexadecimal_floating : string -> t
  end

  module rec Enum_constant : sig
    type t

    val named : ?value:Expr.t located -> General_identifier.t -> t
  end

  and Enum : sig
    type t

    val named : General_identifier.t -> t
    val defined : General_identifier.t option * Enum_constant.t rev -> t
  end

  and Type_specifier_unique : sig
    type t
    val void : t
    val bool : t
    val atomic : Type_name.t -> t
    val enum : Enum.t -> t
    val struct_or_union : Struct_or_union_specifier.t -> t
    val name : Typedef_name.t -> t
  end

  and Struct_or_union_specifier : sig
    type t

    val named : Struct_or_union.t * General_identifier.t -> t

    val defined : Struct_or_union.t * General_identifier.t option * Struct_declaration.t list -> t
  end

  and Generic_association : sig
    type t

    val default : Expr.t located -> t
    val type_name : Type_name.t * Expr.t located -> t
  end

  and Generic_selection : sig
    type t

    val make : Expr.t located * Generic_association.t rev -> t
  end

  and Struct_initializer : sig
    type t

    val make : Type_name.t * (Designator.t rev option * C_initializer.t) rev -> t
  end

  and Expr : sig
    type t
    type t' := t located
    type 'op binary := t' * 'op * t' -> t
    type 'op unary := 'op * t' -> t

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
    val cast : (Type_name.t * t') -> t
    val sizeof : Type_name.t -> t
    val alignof : Type_name.t -> t
    val dot : (t' * General_identifier.t) -> t
    val arrow : (t' * General_identifier.t) -> t
    val var : Var_name.t -> t
    val constant : Constant.t -> t

    val array_subscript : (t' * t') -> t
    val function_call : (t' * t' rev option) -> t

    val generic : Generic_selection.t -> t

    val comma : t' * t' -> t
    val struct_initializer : Struct_initializer.t -> t
  end
   
  and Alignment_specifier : sig
    type t
    val alignas_type : Type_name.t -> t
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

    val storage_class_specifier : Storage_class_specifier.t -> t
    val type_qualifier : Type_qualifier.t -> t
    val function_specifier : Function_specifier.t -> t
    val alignment_specifier : Alignment_specifier.t -> t

  end

  and Declaration_specifiers : sig
    type t

    val type_unique : (Type_specifier_unique.t, Declaration_specifier.t) Util.List_eq1.t -> t
    val type_nonunique : (Type_specifier_nonunique.t, Declaration_specifier.t) Util.List_ge1.t -> t
  end

  and Declaration_specifiers_typedef : sig
    type t

    val type_unique : (Typedef.t, Type_specifier_unique.t, Declaration_specifier.t) Util.List_eq1_eq1.t -> t
    val type_nonunique : (Typedef.t, Type_specifier_nonunique.t, Declaration_specifier.t) Util.List_eq1_ge1.t -> t
  end

  and Init_declarator : sig
    type 'a t

    val make : 'a -> 'a t
    val with_initializer : ('a * C_initializer.t) -> 'a t
  end

  and Pointer : sig
    type t

    val make : (Type_qualifier.t rev option * t option) -> t

  end

  and Declarator : sig
    type t

    val identifier : General_identifier.t -> t
    val pointer : (Pointer.t option * t) -> t

    type type_qualifier_list := Type_qualifier.t rev option

    val array : (t * type_qualifier_list * Expr.t located option) -> t 
    val static_array : (t * type_qualifier_list* Expr.t located) -> t
    val unspecified_size_variable_array : (t * type_qualifier_list) -> t

    val function_ : t * (Parameter_type_list.t, Var_name.t list option) Either.t -> t
  end

  and Abstract_declarator : sig
    type t

    val pointer : Pointer.t -> t
    val direct : Pointer.t option * Direct_abstract_declarator.t -> t
  end

  and Direct_abstract_declarator : sig
    type t

    val abstract : Abstract_declarator.t -> t

    val array : t option * Type_qualifier.t rev option * Expr.t located option -> t
    val static_array : t option * Type_qualifier.t rev option * Expr.t located -> t
    val unspecified_size_variable_array : t option -> t
    val function_ : t option * Parameter_type_list.t option -> t
  end

  and Parameter_type_list : sig
    type t

    val make : (Parameter_declaration.t rev * bool) -> t
  end

  and Parameter_declaration : sig
    type t

    val declarator : Declaration_specifiers.t * Declarator.t -> t
    val abstract : Declaration_specifiers.t * Abstract_declarator.t option -> t
  end 

  and Struct_declarator : sig
    type t

    val declarator : Declarator.t -> t
    val bit_field : (Declarator.t option * Expr.t located) -> t
  end

  and Struct_declaration : sig
    type t

    val make : (Specifier_qualifier_list.t * Struct_declarator.t rev option) -> t

    val static_assert : unit -> t

  end

  and Compound_statement : sig
    type t
    val make : (Block_item.t rev option) -> t
  end

  and Expression_statement : sig
    type t
    val make : Expr.t located option -> t
  end

  and Selection_statement : sig
    type t
    val if_else : (Expr.t located * Statement.t located * Statement.t located) -> t
    val if_ : (Expr.t located * Statement.t located) -> t
    val switch : (Expr.t located * Statement.t located) -> t
  end

  and Labeled_statement : sig
    type t

    val label : (General_identifier.t * Statement.t located) -> t
    val case : (Expr.t located * Statement.t located) -> t
    val default : Statement.t located -> t
  end

  and Iteration_statement : sig
    type t
    val while_ : (Expr.t located * Statement.t located) -> t
    val do_ : (Statement.t located * Expr.t located) -> t
    val for_expr : (Expr.t located option * Expr.t located option * Expr.t located option * Statement.t located) -> t
    val for_decl : (Declaration.t located * Expr.t located option * Expr.t located option * Statement.t located) -> t
  end

  and Jump_statement : sig
    type t
    val goto : General_identifier.t -> t
    val continue : unit -> t
    val break : unit -> t
    val return : Expr.t located option -> t
  end

  and Block_item : sig
    type t
    val statement : Statement.t located -> t
    val declaration : Declaration.t located -> t
  end

  and Statement : sig
    type t

    val compound : Compound_statement.t -> t
    val expression : Expression_statement.t -> t
    val selection : Selection_statement.t -> t
    val labeled : Labeled_statement.t -> t
    val iteration : Iteration_statement.t -> t
    val jump : Jump_statement.t -> t
  end

  and Declaration : sig
    type t
    val normal : Declaration_specifiers.t * Declarator.t Init_declarator.t list option -> t
    val typedef : Declaration_specifiers_typedef.t * Declarator.t Init_declarator.t list option -> t
    val static_assert : Static_assert_declaration.t -> t
  end

  and Type_name : sig
    type t
    val make : Specifier_qualifier_list.t * Abstract_declarator.t option -> t

  end

  and Static_assert_declaration : sig
    type t
    val make : Expr.t located * C11lexer.Literal.String.t -> t
  end

  and C_initializer : sig
    type t

    val expression : Expr.t located -> t
    val initializer_list : (Designator.t rev option * C_initializer.t) rev -> t

  end

  and Designator : sig
    type t
    val field : General_identifier.t -> t
    val subscript : Expr.t located -> t
  end

  and Function_definition : sig
    type t
    val make : Declaration_specifiers.t * Declarator.t * Declaration.t rev option * Compound_statement.t -> t
  end

  and External_declaration : sig
    type t
    val function_definition : Function_definition.t -> t
    val declaration : Declaration.t -> t
  end

end

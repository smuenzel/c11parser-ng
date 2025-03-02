(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2024-2025 Stefan Muenzel
 *)

open Base
open Core

module type Located = sig
  type 'a t [@@deriving sexp, compare, hash]
  type position = Lexing.position
  val locate : start:position -> end_:position -> 'a -> 'a t
end

module Dummy_located = struct
  type 'a t = 'a [@@deriving sexp, compare, hash]
  type position = Lexing.position
  let locate ~start:_ ~end_:_ x = x
end

module Var_name = String_id.Make(struct let module_name = "Var_name" end)()
module Typedef_name = String_id.Make(struct let module_name = "Typedef_name" end)()
module General_identifier = String_id.Make(struct let module_name = "General_identifier" end)()

module Typedef = struct
  type t =
    | Typedef
  [@@deriving sexp, compare, hash, variants]
end

module Type_qualifier = struct
  type t =
    | Const
    | Restrict
    | Volatile
    | Atomic
  [@@deriving sexp, compare, hash, variants]
end

module Storage_class_specifier = struct
  type t =
    | Static
    | Extern
    | Thread_local
    | Auto
    | Register
  [@@deriving sexp, compare, hash, variants]
end

module Function_specifier = struct
  type t =
    | Inline
    | Noreturn
  [@@deriving sexp, compare, hash, variants]
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
  [@@deriving sexp, compare, hash, variants]
end

module Struct_or_union = struct
  type t =
    | Struct
    | Union
  [@@deriving sexp, compare, hash, variants]
end

module Equality_operator = struct
  type t =
    | Equal
    | Not_equal
  [@@deriving sexp, compare, hash, variants]
end

module Relational_operator = struct
  type t =
    | Less
    | Greater
    | Less_equal
    | Greater_equal
  [@@deriving sexp, compare, hash, variants]
end

module Shift_operator = struct
  type t =
    | Left
    | Right
  [@@deriving sexp, compare, hash, variants]
end

module Additive_operator = struct
  type t =
    | Plus
    | Minus
  [@@deriving sexp, compare, hash, variants]
end

module Multiplicative_operator = struct
  type t =
    | Multiply
    | Divide
    | Modulo
  [@@deriving sexp, compare, hash, variants]
end

module Logical_operator = struct
  type t =
    | Logical_and
    | Logical_or
  [@@deriving sexp, compare, hash, variants]
end

module Bitwise_operator = struct
  type t =
    | Bitwise_and
    | Bitwise_xor
    | Bitwise_or
  [@@deriving sexp, compare, hash, variants]
end

module Assignment_operator = struct
  type t =
    | Plain
    | Bitwise of Bitwise_operator.t
    | Additive of Additive_operator.t
    | Multiplicative of Multiplicative_operator.t
    | Shift of Shift_operator.t
  [@@deriving sexp, compare, hash, variants]
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
    | Defined
    | Preincrement
    | Predecrement
    | Postincrement
    | Postdecrement
  [@@deriving sexp, compare, hash, variants]
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
      [@@deriving sexp, compare, hash]
    end

    module Element = struct
      type t = C11lexer.Literal.Char.Element.t =
        | Universal of string
        | Hex of string
        | Octal of string
        | Escape of char
        | Plain of Uchar.t
      [@@deriving sexp, compare, hash, variants]
    end

    type t = C11lexer.Literal.Char.t =
      { kind : Kind.t
      ; value : Element.t list
      } [@@deriving sexp, compare, hash]
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
      [@@deriving sexp, compare, hash]
    end

    module Segment = struct
      type t = C11lexer.Literal.String.Segment.t =
        { kind : Kind.t
        ; value : Char.Element.t list
        } [@@deriving sexp, compare, hash]
    end

    type t = Segment.t list [@@deriving sexp, compare, hash]
  end
end

module Constant = struct
  type t =
    | Char of Literal.Char.t
    | String of Literal.String.t
    | Integer of string
    | Decimal_floating of string
    | Hexadecimal_floating of string
  [@@deriving sexp, compare, hash, variants]
end

module Make(L : Located) = struct
  type 'a located = 'a L.t [@@deriving sexp, compare, hash]
  type position = L.position
  let locate = L.locate

  module Var_name = Var_name
  module Typedef_name = Typedef_name
  module General_identifier = General_identifier

  module Typedef = Typedef
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
      } [@@deriving sexp, compare, hash]
  end = Enum_constant

  and Enum : sig
    type t =
      | Named of General_identifier.t
      | Defined of 
          { name : General_identifier.t option
          ; values : Enum_constant.t list
          }
    [@@deriving sexp, compare, hash, variants]
  end = Enum

  and Type_specifier_unique : sig
    type t =
      | Void
      | Bool
      | Atomic of Type_name.t
      | Enum of Enum.t
      | Struct_or_union of Struct_or_union_specifier.t
      | Name of Typedef_name.t
    [@@deriving sexp, compare, hash]
  end = Type_specifier_unique

  and Struct_or_union_specifier : sig
    type t =
      | Named of { kind : Struct_or_union.t; name : General_identifier.t }
      | Defined of { kind : Struct_or_union.t
                   ; name : General_identifier.t option
                   ; declaration : Struct_declaration.t list
                   }
    [@@deriving sexp, compare, hash, variants]
  end = Struct_or_union_specifier

  and Generic_association : sig
    type t =
      | Default of Expr.t located
      | Named of { name : Type_name.t; value : Expr.t located }
    [@@deriving sexp, compare, hash, variants]
  end = Generic_association

  and Generic_selection : sig
    type t =
      { discriminant : Expr.t located
      ; associations : Generic_association.t list
      } [@@deriving sexp, compare, hash]
  end = Generic_selection

  and Expr : sig
    module Binary_operator : sig
      type t =
        | Equality of Equality_operator.t
        | Relational of Relational_operator.t
        | Shift of Shift_operator.t
        | Additive of Additive_operator.t
        | Multiplicative of Multiplicative_operator.t
        | Logical of Logical_operator.t
        | Bitwise of Bitwise_operator.t
        | Assignment of Assignment_operator.t
      [@@deriving sexp, compare, hash, variants]
    end
    type t =
      | Binary of
          { operator : Binary_operator.t
          ; left : t located
          ; right : t located
          }
      | Unary of 
          { operator : Unary_operator.t
          ; operand : t located
          }
      | Question of 
          { condition : t located
          ; if_true : t located
          ; if_false : t located
          }
      | Cast of
          { type_name : Type_name.t
          ; operand : t located
          }
      | Sizeof of Type_name.t
      | Alignof of Type_name.t
      | Dot of 
          { receiver : t located
          ; field : General_identifier.t
          }
      | Arrow of 
          { receiver : t located
          ; field : General_identifier.t
          }
      | Var of Var_name.t
      | Constant of Constant.t

      | Array_subscript of 
          { array : t located
          ; index : t located
          }
      | Function_call of
          { callee : t located
          ; arguments : t located list
          }

      | Generic of Generic_selection.t

      | Comma of t located * t located
      | Struct_initializer of Struct_initializer.t
    [@@deriving sexp, compare, hash, variants]
  end = Expr

  and Alignment_specifier : sig
    type t =
      | Type of Type_name.t
      | Expr of Expr.t located
    [@@deriving sexp, compare, hash, variants]
  end = Alignment_specifier

  and Qualifier_or_alignment : sig
    type t = (Type_qualifier.t, Alignment_specifier.t) Base.Either.t
    [@@deriving sexp, compare, hash]
  end = Qualifier_or_alignment

  and Specifier_qualifier_list : sig
    type t =
      | Unique of (Type_specifier_unique.t, Qualifier_or_alignment.t) Util.List_eq1.t
      | Nonunique of (Type_specifier_nonunique.t, Qualifier_or_alignment.t) Util.List_ge1.t
    [@@deriving sexp, compare, hash, variants]
  end = Specifier_qualifier_list

  and Declaration_specifier : sig
    type t =
      | Storage_class_specifier of Storage_class_specifier.t
      | Type_qualifier of Type_qualifier.t
      | Function_specifier of Function_specifier.t
      | Alignment_specifier of Alignment_specifier.t
    [@@deriving sexp, compare, hash, variants]
  end = Declaration_specifier

  and Declaration_specifiers : sig
    type t =
      | Unique of (Type_specifier_unique.t, Declaration_specifier.t) Util.List_eq1.t
      | Nonunique of (Type_specifier_nonunique.t, Declaration_specifier.t) Util.List_ge1.t
    [@@deriving sexp, compare, hash, variants]
  end = Declaration_specifiers

  and Declaration_specifiers_typedef : sig
    type t =
      | Unique of (Typedef.t, Type_specifier_unique.t, Declaration_specifier.t) Util.List_eq1_eq1.t
      | Nonunique of (Typedef.t, Type_specifier_nonunique.t, Declaration_specifier.t) Util.List_eq1_ge1.t
    [@@deriving sexp, compare, hash, variants]
  end = Declaration_specifiers_typedef

  and Init_declarator : sig
    type 'a t =
      | Plain of 'a
      | With_initializer of
          { declarator : 'a
          ; initializer_ : C_initializer.t
          } 
    [@@deriving sexp, compare, hash, variants]
  end = Init_declarator

  and Pointer : sig
    type t = 
      | Value
      | Qualified of 
          { qualifiers : Type_qualifier.t list
          ; inner : t
          }
      | Pointer of t
    [@@deriving sexp, compare, hash, variants]
  end = Pointer

  and Declarator : sig
    type t =
      | Identifier of General_identifier.t
      | Pointer of 
          { pointer : Pointer.t
          ; inner : t
          }
      | Array of 
          { declarator : t
          ; qualifiers : Type_qualifier.t list
          ; size : Expr.t located option
          }
      | Static_array of 
          { declarator : t
          ; qualifiers : Type_qualifier.t list
          ; size : Expr.t located
          }
      | Unspecified_size_variable_array of
          { declarator : t
          ; qualifiers : Type_qualifier.t list
          }
      | Function of
          { declarator : t
          ; parameters :
              (Parameter_type_list.t, Var_name.t list) Either.t
          }
    [@@deriving sexp, compare, hash, variants]
  end = Declarator

  and Abstract_declarator : sig
    type t =
      | Pointer of Pointer.t
      | Direct of 
          { pointer : Pointer.t option
          ; direct : Direct_abstract_declarator.t
          }
    [@@deriving sexp, compare, hash]
  end = Abstract_declarator

  and Direct_abstract_declarator : sig
    type t =
      | Abstract of Abstract_declarator.t
      | Array of 
          { declarator : t option
          ; qualifiers : Type_qualifier.t list
          ; size : Expr.t located option
          }
      | Static_array of
          { declarator : t option
          ; qualifiers : Type_qualifier.t list
          ; size : Expr.t located
          }
      | Unspecified_size_variable_array of t option
      | Function of
          { declarator : t option
          ; parameters : Parameter_type_list.t
          }
    [@@deriving sexp, compare, hash, variants]
  end = Direct_abstract_declarator

  and Parameter_type_list : sig
    type t =
      { declarations : Parameter_declaration.t list
      ; ellipsis : bool
      } [@@deriving sexp, compare, hash]
  end = Parameter_type_list

  and Parameter_declaration : sig
    type t =
      | Declarator of 
          { specifiers : Declaration_specifiers.t
          ; declarator : Declarator.t
          }
      | Abstract of 
          { specifiers : Declaration_specifiers.t
          ; declarator : Abstract_declarator.t option
          }
    [@@deriving sexp, compare, hash, variants]
  end = Parameter_declaration

  and Struct_declarator : sig
    type t =
      | Declarator of Declarator.t
      | Bit_field of
          { declarator : Declarator.t option
          ; size : Expr.t located
          }
    [@@deriving sexp, compare, hash, variants]
  end = Struct_declarator

  and Struct_declaration : sig
    type t =
      | Declarations of
          { specifier_qualifiers : Specifier_qualifier_list.t
          ; declarators : Struct_declarator.t list
          }
      | Static_assert of unit
    [@@deriving sexp, compare, hash, variants]
  end = Struct_declaration

  and Compound_statement : sig
    type t = Block_item.t list
    [@@deriving sexp, compare, hash]
  end = Compound_statement

  and Expression_statement : sig
    type t = Expr.t located option
    [@@deriving sexp, compare, hash]
  end = Expression_statement

  and Selection_statement : sig
    type t = 
      | If of
          { condition : Expr.t located
          ; then_ : Statement.t located
          ; else_ : Statement.t located option
          }
      | Switch of
          { condition : Expr.t located
          ; body : Statement.t located
          }
    [@@deriving sexp, compare, hash]
  end = Selection_statement

  and Labeled_statement : sig
    type t =
      | Label of
          { name : General_identifier.t
          ; statement : Statement.t located
          }
      | Case of 
          { expression : Expr.t located
          ; statement : Statement.t located
          }
      | Default of
          { statement : Statement.t located
          }
    [@@deriving sexp, compare, hash, variants]
  end = Labeled_statement

  and Iteration_statement : sig
    type t =
      | While of 
          { condition : Expr.t located
          ; body : Statement.t located
          }
      | Do of
          { condition : Expr.t located
          ; body : Statement.t located
          }
      | For_expr of
          { init : Expr.t located option
          ; condition : Expr.t located option
          ; increment : Expr.t located option
          ; body : Statement.t located
          }
      | For_decl of
          { declarator : Declaration.t located
          ; condition : Expr.t located option
          ; increment : Expr.t located option
          ; body : Statement.t located
          }
    [@@deriving sexp, compare, hash, variants]
  end = Iteration_statement

  and Jump_statement : sig
    type t =
      | Break
      | Continue
      | Return of Expr.t located option
      | Goto of General_identifier.t
    [@@deriving sexp, compare, hash, variants]
  end = Jump_statement

  and Block_item : sig
    type t =
      | Declaration of Declaration.t located
      | Statement of Statement.t located
    [@@deriving sexp, compare, hash, variants]
  end = Block_item

  and Statement : sig
    type t =
      | Compound of Compound_statement.t
      | Expression of Expression_statement.t
      | Selection of Selection_statement.t
      | Labeled of Labeled_statement.t
      | Iteration of Iteration_statement.t
      | Jump of Jump_statement.t
    [@@deriving sexp, compare, hash]
  end = Statement

  and Declaration : sig
    type t =
      | Normal of
          { specifiers : Declaration_specifiers.t
          ; init_declarators : Declarator.t Init_declarator.t list
          }
      | Typedef of
          { specifiers : Declaration_specifiers_typedef.t
          ; declarators : Declarator.t Init_declarator.t list
          }
      | Static_assert of Static_assert_declaration.t
    [@@deriving sexp, compare, hash, variants]
  end = Declaration

  and Type_name : sig
    type t =
      { specifier_qualifiers : Specifier_qualifier_list.t
      ; abstract_declarator : Abstract_declarator.t option
      } [@@deriving sexp, compare, hash]
  end = Type_name

  and Static_assert_declaration : sig
    type t =
      { condition : Expr.t located
      ; message : Literal.String.t
      } [@@deriving sexp, compare, hash]
  end = Static_assert_declaration

  and C_initializer : sig
    type t =
      | Expression of Expr.t located
      | Initializer_list of (Designator.t list * C_initializer.t) list
    [@@deriving sexp, compare, hash]
  end = C_initializer

  and Struct_initializer : sig
    type t =
      { type_name : Type_name.t
      ; initializer_list : (Designator.t list * C_initializer.t) list
      } [@@deriving sexp, compare, hash]
  end = Struct_initializer

  and Designator : sig
    type t =
      | Field of General_identifier.t
      | Subscript of Expr.t located
    [@@deriving sexp, compare, hash, variants]
  end = Designator

  and Function_definition : sig
    type t =
      { returns : Declaration_specifiers.t
      ; declarator : Declarator.t
      ; arguments : Declaration.t list
      ; body : Compound_statement.t
      } [@@deriving sexp, compare, hash]
  end = Function_definition

  and External_declaration : sig
    type t =
      | Function of Function_definition.t
      | Declaration of Declaration.t
    [@@deriving sexp, compare, hash, variants]
  end = External_declaration
end

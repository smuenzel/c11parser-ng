
module type Located = Ast_types.Located

module Make(L : Located) = struct
  module T = Ast_types.Make(L)

  type 'a located = 'a T.located [@@deriving sexp, compare, hash]
  let locate = T.locate

  module Var_name = T.Var_name
  module Typedef_name = T.Typedef_name
  module General_identifier = T.General_identifier
  module Typedef = T.Typedef
  module Type_qualifier = T.Type_qualifier
  module Storage_class_specifier = T.Storage_class_specifier
  module Function_specifier = T.Function_specifier
  module Type_specifier_nonunique = T.Type_specifier_nonunique
  module Struct_or_union = T.Struct_or_union
  module Equality_operator = T.Equality_operator
  module Relational_operator = T.Relational_operator
  module Shift_operator = T.Shift_operator
  module Additive_operator = T.Additive_operator
  module Multiplicative_operator = T.Multiplicative_operator
  module Logical_operator = T.Logical_operator
  module Bitwise_operator = T.Bitwise_operator
  module Assignment_operator = T.Assignment_operator
  module Unary_operator = T.Unary_operator
  module Constant = T.Constant

  module Enum_constant = struct 
    include T.Enum_constant

    let named ?value name = { name; value }
  end

  module Enum = struct
    include T.Enum

    let defined (name, values) =
      defined ~name ~values:(Util.Stored_reversed.to_list values)
  end

  module Type_specifier_unique = struct
    include T.Type_specifier_unique

    let void = Void
    let bool = Bool
    let atomic tn = Atomic tn
    let enum e = Enum e
    let struct_or_union su = Struct_or_union su
    let name tdn = Name tdn
  end

  module Struct_or_union_specifier = struct
    include T.Struct_or_union_specifier

    let named (su, name) = Named { kind = su; name }
    let defined (su, name, decls) = Defined { kind = su; name; declaration = decls }
  end

  module Generic_association = struct
    include T.Generic_association

    let type_name (name, value) = Named { name; value } 
    let make value = Default value
  end

  module Generic_selection = struct
    include T.Generic_selection

    let make (discriminant, associations) =
      let associations = Util.Stored_reversed.to_list associations in
      { discriminant; associations }
  end

  module Expr = struct 
    include T.Expr

    let equality (left, op, right) = Binary { operator = Equality op; left; right }
    let relational (left, op, right) = Binary { operator = Relational op; left; right }
    let shift (left, op, right) = Binary { operator = Shift op; left; right }
    let additive (left, op, right) = Binary { operator = Additive op; left; right }
    let multiplicative (left, op, right) = Binary { operator = Multiplicative op; left; right }
    let logical (left, op, right) = Binary { operator = Logical op; left; right }
    let bitwise (left, op, right) = Binary { operator = Bitwise op; left; right }
    let assignment (left, op, right) = Binary { operator = Assignment op; left; right }

    let unary (op, operand) = Unary { operator = op; operand }
    let question (condition, if_true, if_false) =
      Question { condition; if_true; if_false }

    let cast (type_name, operand) = Cast { type_name; operand }
    let dot (receiver, field) = Dot { receiver; field }
    let arrow (receiver, field) = Arrow { receiver; field }
    let array_subscript (array, index) = Array_subscript { array; index }
    let function_call (callee, arguments) =
      let arguments = Option.value ~default:Util.Stored_reversed.empty arguments in
      let arguments = Util.Stored_reversed.to_list arguments in
      Function_call { callee; arguments }
    let comma (a, b) = Comma (a,b)
  end

  module Alignment_specifier = T.Alignment_specifier
  module Specifier_qualifier_list = T.Specifier_qualifier_list
  module Declaration_specifier = T.Declaration_specifier
  module Declaration_specifiers = T.Declaration_specifiers
  module Declaration_specifiers_typedef = T.Declaration_specifiers_typedef
  module Init_declarator = T.Init_declarator
  module Pointer = T.Pointer
  module Declarator = T.Declarator
  module Abstract_declarator = T.Abstract_declarator
  module Direct_abstract_declarator = T.Direct_abstract_declarator
  module Parameter_type_list = T.Parameter_type_list
  module Parameter_declaration = T.Parameter_declaration
  module Struct_declarator = T.Struct_declarator
  module Struct_declaration = T.Struct_declaration
  module Compound_statement = T.Compound_statement
  module Expression_statement = T.Expression_statement
  module Selection_statement = T.Selection_statement
  module Labeled_statement = T.Labeled_statement
  module Iteration_statement = T.Iteration_statement
  module Jump_statement = T.Jump_statement
  module Block_item = T.Block_item
  module Statement = T.Statement
  module Declaration = T.Declaration
  module Type_name = T.Type_name
  module Static_assert_declaration = T.Static_assert_declaration
  module C_initializer = T.C_initializer
  module Designator = T.Designator
  module Function_definition = T.Function_definition
  module External_declaration = T.External_declaration
end

open struct
  module _ (L : Located) : C11parser.Astgen_intf.S = struct
    include Make(L)
  end
end

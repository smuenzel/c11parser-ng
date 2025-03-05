(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2024-2025 Stefan Muenzel
 *)

module type Located = Ast_types.Located

module Make(L : Located) = struct
  module T = Ast_types.Make(L)

  module Located = L

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
      let arguments = Util.Stored_reversed.opt_to_list arguments in
      Function_call { callee; arguments }
    let comma (a, b) = Comma (a,b)
  end

  module Qualifier_or_alignment = struct
    include T.Qualifier_or_alignment

    let make = Util.Either.of_caml
  end

  module Alignment_specifier = struct
    include T.Alignment_specifier
    let alignas_type type_name = Type type_name
    let alignas_expression expr = Expr expr
  end

  module Specifier_qualifier_list = struct
    include T.Specifier_qualifier_list

    let unique l = unique (Util.List_eq1.map_unbounded ~f:Qualifier_or_alignment.make l)
    let nonunique l = nonunique (Util.List_ge1.map_unbounded ~f:Qualifier_or_alignment.make l)
  end

  module Declaration_specifier = T.Declaration_specifier

  module Declaration_specifiers = struct
    include T.Declaration_specifiers

    let type_unique l = Unique l
    let type_nonunique l = Nonunique l
  end

  module Declaration_specifiers_typedef = struct 
    include T.Declaration_specifiers_typedef

    let type_unique l = Unique l
    let type_nonunique l = Nonunique l
  end

  module Init_declarator = struct
    include T.Init_declarator

    let make a = Plain a
    let with_initializer (declarator, initializer_) = with_initializer ~declarator ~initializer_
  end

  module Pointer = struct
    include T.Pointer

    let rec make = function
      | None, None -> Value
      | None, Some t -> Pointer t
      | Some qual, p ->
        Qualified { qualifiers = Util.Stored_reversed.to_list qual
                  ; inner = make (None, p)
                  }
  end

  module Declarator = struct
    include T.Declarator

    let pointer (p, t) = Pointer { pointer = p; inner = t }

    let array (declarator, qualifiers, size) =
      let qualifiers = Util.Stored_reversed.opt_to_list qualifiers in
      array ~declarator ~qualifiers ~size

    let static_array (declarator, qualifiers, size) = 
      let qualifiers = Util.Stored_reversed.opt_to_list qualifiers in
      static_array ~declarator ~qualifiers ~size

    let unspecified_size_variable_array (declarator, qualifiers) =
      let qualifiers = Util.Stored_reversed.opt_to_list qualifiers in
      unspecified_size_variable_array ~declarator ~qualifiers

    let function_ (declarator, parameters) =
      let parameters = Util.Either.of_caml parameters in
      let parameters =
        Base.Either.map  ~first:Base.Fn.id ~second:(Option.value ~default:[]) parameters
      in
      function_ ~declarator ~parameters
  end

  module Abstract_declarator = struct
    include T.Abstract_declarator

    let pointer p = Pointer p
    let direct (p, d) = Direct { pointer = p; direct = d }
  end

  module Direct_abstract_declarator = struct
    include T.Direct_abstract_declarator

    let array (declarator, qualifiers, size) =
      let qualifiers = Util.Stored_reversed.opt_to_list qualifiers in
      array ~declarator ~qualifiers ~size

    let static_array (declarator, qualifiers, size) =
      let qualifiers = Util.Stored_reversed.opt_to_list qualifiers in
      static_array ~declarator ~qualifiers ~size

    let function_ (declarator, parameters) =
      let parameters =
        match parameters with
        | None -> { T.Parameter_type_list.declarations = []; ellipsis = false }
        | Some ptl -> ptl
      in
      function_  ~declarator ~parameters
  end

  module Parameter_type_list = struct
    include T.Parameter_type_list

    let make (declarations, ellipsis) =
      let declarations = Util.Stored_reversed.to_list declarations in
      { declarations; ellipsis }
  end

  module Parameter_declaration = struct
    include T.Parameter_declaration

    let declarator (specifiers, declarator_) =
      declarator ~specifiers ~declarator:declarator_

    let abstract (specifiers, declarator) =
      abstract ~specifiers ~declarator
  end

  module Struct_declarator = struct
    include T.Struct_declarator

    let bit_field (declarator, size) =
      bit_field ~declarator ~size

    let make declarator = Declarator declarator
  end

  module Struct_declaration = struct
    include T.Struct_declaration

    let make (specifier_qualifiers, declarator) =
      Declarations { specifier_qualifiers
                   ; declarators = Util.Stored_reversed.opt_to_list declarator
                   }
  end

  module Compound_statement = struct
    include T.Compound_statement

    let make l = Util.Stored_reversed.opt_to_list l
  end

  module Expression_statement = struct 
    include T.Expression_statement

    let make expr = expr
  end

  module Selection_statement = struct
    include T.Selection_statement

    let if_ (condition, then_) = If { condition; then_; else_ = None }
    let if_else (condition, then_, else_) = If { condition; then_; else_ = Some else_ }
    let switch (condition, body) = Switch { condition; body }
  end

  module Labeled_statement = struct
    include T.Labeled_statement

    let label (name, statement) = label ~name ~statement
    let case (expression, statement) = case ~expression ~statement
    let default (statement) = default ~statement
  end

  module Iteration_statement = struct
    include T.Iteration_statement

    let while_ (condition, body) = while_ ~condition ~body
    let do_ (body, condition) = do_ ~body ~condition
    let for_expr (init, condition, increment, body) =
      for_expr ~init ~condition ~increment ~body
    let for_decl (declarator, condition, increment, body) =
      for_decl ~declarator ~condition ~increment ~body
  end

  module Jump_statement = struct
    include T.Jump_statement

    let continue () = continue
    let break () = break
  end

  module Block_item = T.Block_item

  module Statement = struct
    include T.Statement

    let compound a = Compound a
    let expression a = Expression a
    let selection a = Selection a
    let labeled a = Labeled a
    let iteration a = Iteration a
    let jump a = Jump a
  end

  module Declaration = struct
    include T.Declaration

    let normal (specifiers, init_declarators) =
      let init_declarators = Option.value ~default:[] init_declarators in
      normal ~specifiers ~init_declarators

    let typedef (specifiers, declarators) =
      let declarators = Option.value ~default:[] declarators in
      typedef ~specifiers ~declarators
  end

  module Type_name = struct
    include T.Type_name

    let make (specifier_qualifiers, abstract_declarator) =
      { specifier_qualifiers; abstract_declarator }
  end

  module Static_assert_declaration = struct
    include T.Static_assert_declaration

    let make (condition, message) = { condition; message }
  end

  module C_initializer = struct
    include T.C_initializer

    let expression e = Expression e
    let initializer_list l = 
      let l = Util.Stored_reversed.to_list l in
      let l = Core.List.map ~f:(Core.Tuple2.map_fst ~f:Util.Stored_reversed.opt_to_list) l in
      Initializer_list l
  end

  module Struct_initializer = struct
    include T.Struct_initializer

    let make (type_name, l) =
      let l = Util.Stored_reversed.to_list l in
      let l = Core.List.map ~f:(Core.Tuple2.map_fst ~f:Util.Stored_reversed.opt_to_list) l in
      { type_name; initializer_list = l }

  end

  module Designator = T.Designator

  module Function_definition = struct
    include T.Function_definition

    let make (returns, declarator, arguments, body) =
      let arguments = Util.Stored_reversed.opt_to_list arguments in
      { returns; declarator; arguments; body }
  end

  module External_declaration = struct
    include T.External_declaration

    let function_definition f = Function f
  end
end

open struct
  module _ (L : Located) : C11parser.Astgen_intf.S = struct
    include Make(L)
  end
end

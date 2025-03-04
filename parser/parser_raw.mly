(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2016-2017, Inria
 * SPDX-FileCopyrightText: (c) 2024-2025, Stefan Muenzel
 * SPDX-FileContributor: Jacques-Henri Jourdan, Inria Paris
 * SPDX-FileContributor: Francois Pottier, Inria Paris
 * SPDX-FileContributor: Stefan Muenzel
 *)

(* WARNING. When processing this grammar, Menhir should announce that
   ONE shift/reduce conflict was silently solved and that ONE state
   has 3 reduce/reduce conflicts on RPAREN, LPAREN, and LBRACK. If you
   modify the grammar, you should check that this is still the case. *)

%parameter<Lexing : Lexing_intf.S>
%parameter<Gen : Astgen_intf.S with type Located.position = Lexing.position>
%parameter<Context : Context.Packed>

%token<string> NAME
(* Also attach the name to VARIABLE and TYPE tokens, for better error messages. *)
%token<string> VARIABLE
%token<string> TYPE
%token<C11lexer.Literal.String.t> STRING_LITERAL

%token<C11lexer.Literal.Char.t> CONSTANT_CHAR
%token<string> CONSTANT_INTEGER
%token<string> CONSTANT_DECIMAL_FLOATING
%token<string> CONSTANT_HEXADECIMAL_FLOATING

%token ALIGNAS "_Alignas"
%token ALIGNOF "_Alignof"
%token ATOMIC "_Atomic"
%token AUTO "auto"
%token BOOL "_Bool"
%token BREAK "break"
%token CASE "case"
%token CHAR "char"
%token COMPLEX "_Complex"
%token CONST "const"
%token CONTINUE "continue"
%token DEFAULT "default"
%token DO "do"
%token DOUBLE "double"
%token ELSE "else"
%token ENUM "enum"
%token EXTERN "extern"
%token FLOAT "float"
%token FOR "for"
%token GENERIC "_Generic"
%token GOTO "goto"
%token IF "if"
%token IMAGINARY "_Imaginary"
%token INLINE "inline"
%token INT "int"
%token LONG "long"
%token NORETURN "_Noreturn"
%token REGISTER "register"
%token RESTRICT "restrict"
%token RETURN "return"
%token SHORT "short"
%token SIGNED "signed"
%token SIZEOF "sizeof"
%token STATIC "static"
%token STATIC_ASSERT "_Static_assert"
%token STRUCT "struct"
%token SWITCH "switch"
%token THREAD_LOCAL "_Thread_local"
%token TYPEDEF "typedef"
%token UNION "union"
%token UNSIGNED "unsigned"
%token VOID "void"
%token VOLATILE "volatile"
%token WHILE "while"

(* For preprocessor only *)
%token DEFINED "defined"

%token ELLIPSIS "..."
%token ADD_ASSIGN "+="
%token SUB_ASSIGN "-="
%token MUL_ASSIGN "*="
%token DIV_ASSIGN "/="
%token MOD_ASSIGN "%="
%token OR_ASSIGN "|="
%token AND_ASSIGN "&="
%token XOR_ASSIGN "^="
%token LEFT_ASSIGN "<<="
%token RIGHT_ASSIGN ">>="
%token LEFT "<<"
%token RIGHT ">>"
%token EQEQ "=="
%token NEQ "!="
%token LEQ "<="
%token GEQ ">="
%token EQ "="
%token LT "<"
%token GT ">"
%token INC "++"
%token DEC "--"
%token PTR "->"
%token PLUS "+"
%token MINUS "-"
%token STAR "*"
%token SLASH "/"
%token PERCENT "%"
%token BANG "!"
%token ANDAND "&&"
%token BARBAR "||"
%token AND "&"
%token BAR "|"
%token HAT "^"
%token QUESTION "?"
%token COLON ":"
%token TILDE "~"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACK "["
%token RBRACK "]"
%token LPAREN "("
%token RPAREN ")"
%token SEMICOLON ";"
%token COMMA ","
%token DOT "."

(* ATOMIC_LPAREN is "special"; it's used for left parentheses that
   follow the ["_Atomic"] keyword. It isn't given a token alias *)
%token ATOMIC_LPAREN

%token EOF

%type<Context.snapshot> save_context
%type<Context.snapshot * Gen.Parameter_type_list.t> parameter_type_list
%type<Context.snapshot Declarator.t * Gen.Declarator.t> declarator direct_declarator
%type<Gen.Expr.t> unary_expression cast_expression postfix_expression additive_expression
%type<Gen.Expr.t> multiplicative_expression shift_expression
%type<Gen.Expr.t> relational_expression equality_expression primary_expression
%type<string*Gen.Typedef_name.t> typedef_name
%type<string*Gen.Var_name.t> var_name
%type<string*Gen.General_identifier.t> general_identifier
%type<Gen.Declaration.t> declaration
%type<Gen.Type_name.t> type_name
%type<Gen.C_initializer.t> c_initializer

(* There is a reduce/reduce conflict in the grammar. It corresponds to the
   conflict in the second declaration in the following snippet:

     typedef int T;
     int f(int(T));

   It is specified by 6.7.6.3 11: 'T' should be taken as the type of the
   parameter of the anonymous function taken as a parameter by f (thus,
   f has type (T -> int) -> int).

   The reduce/reduce conflict is solved by letting menhir reduce the production
   appearing first in this file. This is the reason why we have the
   [typedef_name_spec] proxy: it is here just to make sure the conflicting
   production appears before the other (which concerns [general_identifier]). *)

(* These precedence declarations solve the dangling else conflict. *)
%nonassoc below_ELSE
%nonassoc ELSE

%start<Gen.External_declaration.t list> translation_unit_file
%start<Gen.Expr.t> single_expression

%%

(* Helpers *)

%public %inline located(X):
  x=X { Gen.Located.locate ~start:$startpos(x) ~end_:$endpos(x) x }

%inline get_context(X):
  x=X { fst x }

%inline drop_context(X):
  x=X { snd x }

%inline as_typed(X):
  x=X { snd x }

%inline as_untyped(X):
  x=X { fst x }

(* A list of A's and B's that contains exactly one A: *)

let list_eq1(A, B) :=
| a=A; b=B*;
{ Util.List_eq1.Eq (a, b) }
| b=B; rest=list_eq1(A, B);
{ Util.List_eq1.Neq (b, rest) }

(* A list of A's and B's that contains at least one A: *)

let list_ge1(A, B) :=
| a=A; b=B*;
{ Util.List_ge1.Eq (a, b) }
| a=A; rest=list_ge1(A, B);
{ Util.List_ge1.A (a, rest) }
| b=B; rest=list_ge1(A, B);
{ Util.List_ge1.B (b, rest) }

(* A list of A's, B's and C's that contains exactly one A and exactly one B: *)

let list_eq1_eq1(A, B, C) :=
| a=A; rest=list_eq1(B, C);
{ Util.List_eq1_eq1.A (a, rest) }
| b=B; rest=list_eq1(A, C);
{ Util.List_eq1_eq1.B (b, rest) }
| c=C; rest=list_eq1_eq1(A, B, C);
{ Util.List_eq1_eq1.C (c, rest) }

(* A list of A's, B's and C's that contains exactly one A and at least one B: *)

let list_eq1_ge1(A, B, C) :=
| a=A; rest=list_ge1(B, C);
{ Util.List_eq1_ge1.A (a, rest) }
| b=B; rest=list_eq1(A, C);
{ Util.List_eq1_ge1.B (b, rest) }
| b=B; rest=list_eq1_ge1(A, B, C);
{ Util.List_eq1_ge1.B' (b, rest) }
| c=C; rest=list_eq1_ge1(A, B, C);
{ Util.List_eq1_ge1.C (c, rest) }

(* Upon finding an identifier, the lexer emits two tokens. The first token,
   [NAME], indicates that a name has been found; the second token, either [TYPE]
   or [VARIABLE], tells what kind of name this is. The classification is
   performed only when the second token is demanded by the parser. *)

let typedef_name :=
| name=NAME; TYPE;
    { name, Gen.Typedef_name.of_string name }

let var_name :=
| name=NAME; VARIABLE;
{ name, Gen.Var_name.of_string name }

(* [typedef_name_spec] must be declared before [general_identifier], so that the
   reduce/reduce conflict is solved the right way. *)

let typedef_name_spec :=
| ~=typedef_name;
    <>

let general_identifier :=
| n=typedef_name;
  { (fst n), Gen.General_identifier.of_string (fst n) }
| n=var_name;
  { (fst n), Gen.General_identifier.of_string (fst n) }

save_context:
| (* empty *)
    { Context.save_context () }

scoped(X):
| ctx = save_context x = X
    { Context.restore_context ctx; x }

(* [declarator_varname] and [declarator_typedefname] are like [declarator]. In
   addition, they have the side effect of introducing the declared identifier as
   a new variable or typedef name in the current context. *)

declarator_varname:
| d = declarator
    { Context.declare_varname (Declarator.identifier (fst d)); d }

declarator_typedefname:
| d = declarator
    { Context.declare_typedefname (Declarator.identifier (fst d)); d }

(* Merge source-level string literals. *)
let string_literal :=
| ~=STRING_LITERAL;
    <>
| a=string_literal; b=STRING_LITERAL;
    { a @ b }

(* End of the helpers, and beginning of the grammar proper: *)

let primary_expression :=
| n=var_name;
  { Gen.Expr.var (snd n) }
| c=CONSTANT_CHAR;
  { Gen.Expr.constant (Gen.Constant.char c) }
| c=CONSTANT_INTEGER;
{ Gen.Expr.constant (Gen.Constant.integer c) }
| c=CONSTANT_DECIMAL_FLOATING;
{ Gen.Expr.constant (Gen.Constant.decimal_floating c) }
| c=CONSTANT_HEXADECIMAL_FLOATING;
{ Gen.Expr.constant (Gen.Constant.hexadecimal_floating c) }
| x=string_literal;
  { Gen.Expr.constant (Gen.Constant.string x) }
| "("; ~=expression; ")";
  <>
| ~=generic_selection;
< Gen.Expr.generic >

let generic_selection :=
| "_Generic"; "("; ~=located(assignment_expression); ","; ~=generic_assoc_list; ")";
< Gen.Generic_selection.make >

let generic_assoc_list :=
| ~=generic_association; < Util.Stored_reversed.singleton >
| ~=generic_assoc_list; ","; ~=generic_association; < Util.Stored_reversed.snoc >

let generic_association :=
| ~=type_name; ":"; ~=located(assignment_expression);
< Gen.Generic_association.type_name >
| "default"; ":"; ~=located(assignment_expression);
< Gen.Generic_association.default >

let postfix_expression :=
| ~=primary_expression;
  <>
| ~=located(postfix_expression); "["; ~=located(expression); "]";
< Gen.Expr.array_subscript >
| ~=located(postfix_expression); "("; ~=argument_expression_list?; ")";
< Gen.Expr.function_call >
| ~=located(postfix_expression); "."; ~=as_typed(general_identifier);
  < Gen.Expr.dot >
| ~=located(postfix_expression); "->"; ~=as_typed(general_identifier);
  < Gen.Expr.arrow >
| x=located(postfix_expression); "++";
  { Gen.Expr.unary (Gen.Unary_operator.postincrement, x) }
| x=located(postfix_expression); "--";
  { Gen.Expr.unary (Gen.Unary_operator.postdecrement, x) }
| "("; t=type_name; ")"; "{"; i=initializer_list; ","?; "}";
{ Gen.Expr.struct_initializer (Gen.Struct_initializer.make (t,i)) }

let argument_expression_list :=
| ~=located(assignment_expression); < Util.Stored_reversed.singleton >
| ~=argument_expression_list; ","; ~=located(assignment_expression);
< Util.Stored_reversed.snoc >


let unary_expression :=
| ~=postfix_expression;
<>
| "++"; e=located(unary_expression);
{ Gen.Expr.unary (Gen.Unary_operator.preincrement, e) }
| "--"; e=located(unary_expression);
{ Gen.Expr.unary (Gen.Unary_operator.predecrement, e) }
| ~=unary_operator; ~=located(cast_expression);
< Gen.Expr.unary >
| "sizeof"; e=located(unary_expression);
{ Gen.Expr.unary (Gen.Unary_operator.sizeof, e) }
| "sizeof"; "("; ~=type_name; ")";
< Gen.Expr.sizeof >
| "defined"; e=located(unary_expression);
{ Gen.Expr.unary (Gen.Unary_operator.defined, e) }
| "_Alignof"; "("; ~=type_name; ")";
< Gen.Expr.alignof >

unary_operator:
| "&" { Gen.Unary_operator.address_of }
| "*" { Gen.Unary_operator.dereference }
| "+" { Gen.Unary_operator.plus }
| "-" { Gen.Unary_operator.minus }
| "~" { Gen.Unary_operator.bitwise_not }
| "!" { Gen.Unary_operator.logical_not }

let cast_expression :=
| ~=unary_expression;
<>
| "("; ~=type_name; ")"; ~=located(cast_expression);
< Gen.Expr.cast >

multiplicative_operator:
| "*" { Gen.Multiplicative_operator.multiply }
| "/" { Gen.Multiplicative_operator.divide }
| "%" { Gen.Multiplicative_operator.modulo }

let multiplicative_expression :=
| ~=cast_expression;
< >
| ~=located(multiplicative_expression); ~=multiplicative_operator; ~=located(cast_expression);
< Gen.Expr.multiplicative >

additive_operator:
| "+" { Gen.Additive_operator.plus }
| "-" { Gen.Additive_operator.minus }

let additive_expression :=
| ~=multiplicative_expression;
  <>
| ~=located(additive_expression); ~=additive_operator; ~=located(multiplicative_expression);
  < Gen.Expr.additive >

shift_operator:
| "<<" { Gen.Shift_operator.left }
| ">>" { Gen.Shift_operator.right }

let shift_expression :=
| ~=additive_expression;
 <>
| ~=located(shift_expression); ~=shift_operator; ~=located(additive_expression);
    < Gen.Expr.shift >

relational_operator:
| "<" { Gen.Relational_operator.less }
| ">" { Gen.Relational_operator.greater }
| "<=" { Gen.Relational_operator.less_equal }
| ">=" { Gen.Relational_operator.greater_equal }

let relational_expression :=
| ~=shift_expression;
    <>
| ~=located(relational_expression); ~=relational_operator; ~=located(shift_expression);
    < Gen.Expr.relational >

equality_operator:
| "==" { Gen.Equality_operator.equal }
| "!=" { Gen.Equality_operator.not_equal }

let equality_expression :=
| ~=relational_expression; 
  <>
| ~=located(equality_expression); ~=equality_operator; ~=located(relational_expression);
    < Gen.Expr.equality >

let and_expression :=
| ~=equality_expression; <>
| a=located(and_expression); "&"; b=located(equality_expression);
    { Gen.Expr.bitwise (a, Gen.Bitwise_operator.bitwise_and, b) }

let exclusive_or_expression :=
| ~=and_expression; <>
| a=located(exclusive_or_expression); "^"; b=located(and_expression);
    { Gen.Expr.bitwise (a, Gen.Bitwise_operator.bitwise_xor, b) }

let inclusive_or_expression :=
| ~=exclusive_or_expression; <>
| a=located(inclusive_or_expression); "|"; b=located(exclusive_or_expression);
    { Gen.Expr.bitwise (a, Gen.Bitwise_operator.bitwise_or, b) }

let logical_and_expression :=
| ~=inclusive_or_expression; <>
| a=located(logical_and_expression); "&&"; b=located(inclusive_or_expression);
    { Gen.Expr.logical (a, Gen.Logical_operator.logical_and, b) }

let logical_or_expression :=
| ~=logical_and_expression; <>
| a=located(logical_or_expression); "||"; b=located(logical_and_expression);
    { Gen.Expr.logical (a, Gen.Logical_operator.logical_or, b) }

let conditional_expression :=
| ~=logical_or_expression; <>
| ~=located(logical_or_expression); "?"; ~=located(expression); ":"; ~=located(conditional_expression);
    < Gen.Expr.question >

let assignment_expression :=
| ~=conditional_expression; <>
| ~=located(unary_expression); ~=assignment_operator; ~=located(assignment_expression);
    < Gen.Expr.assignment >

let assignment_operator :=
| "="; { Gen.Assignment_operator.plain }
| "*="; { Gen.Assignment_operator.multiplicative (Gen.Multiplicative_operator.multiply) }
| "/="; { Gen.Assignment_operator.multiplicative (Gen.Multiplicative_operator.divide) }
| "%="; { Gen.Assignment_operator.multiplicative (Gen.Multiplicative_operator.modulo) }
| "+="; { Gen.Assignment_operator.additive (Gen.Additive_operator.plus) }
| "-="; { Gen.Assignment_operator.additive (Gen.Additive_operator.minus) }
| "<<="; { Gen.Assignment_operator.shift (Gen.Shift_operator.left) }
| ">>="; { Gen.Assignment_operator.shift (Gen.Shift_operator.right) }
| "&="; { Gen.Assignment_operator.bitwise (Gen.Bitwise_operator.bitwise_and) }
| "^="; { Gen.Assignment_operator.bitwise (Gen.Bitwise_operator.bitwise_xor) }
| "|="; { Gen.Assignment_operator.bitwise (Gen.Bitwise_operator.bitwise_or) }

let expression :=
| ~=assignment_expression; <>
| ~=located(expression); ","; ~=located(assignment_expression);
    < Gen.Expr.comma >


let constant_expression :=
| ~=conditional_expression;
    <>

(* We separate type declarations, which contain an occurrence of ["typedef"], and
   normal declarations, which do not. This makes it possible to distinguish /in
   the grammar/ whether a declaration introduces typedef names or variables in
   the context. *)

let declaration :=
| ~=declaration_specifiers;         ~=init_declarator_list(as_typed(declarator_varname))?;     ";";
< Gen.Declaration.normal >
| ~=declaration_specifiers_typedef; ~=init_declarator_list(as_typed(declarator_typedefname))?; ";";
< Gen.Declaration.typedef >
| ~=static_assert_declaration;
< Gen.Declaration.static_assert >

(* [declaration_specifier] corresponds to one declaration specifier in the C18
   standard, deprived of "typedef" and of type specifiers. *)

let declaration_specifier :=
| ~=storage_class_specifier (* deprived of "typedef" *);
< Gen.Declaration_specifier.storage_class_specifier >
| ~=type_qualifier;
< Gen.Declaration_specifier.type_qualifier >
| ~=function_specifier;
< Gen.Declaration_specifier.function_specifier >
| ~=alignment_specifier;
< Gen.Declaration_specifier.alignment_specifier >

(* [declaration_specifiers] requires that at least one type specifier be
   present, and, if a unique type specifier is present, then no other type
   specifier be present. In other words, one should have either at least one
   nonunique type specifier, or exactly one unique type specifier.

   This is a weaker condition than 6.7.2 2. Encoding this condition in the
   grammar is necessary to disambiguate the example in 6.7.7 6:

     typedef signed int t;
     struct tag {
     unsigned t:4;
     const t:5;
     };

   The first field is a named t, while the second is unnamed of type t.

   [declaration_specifiers] forbids the ["typedef"] keyword. *)

let declaration_specifiers :=
| ~=list_eq1(type_specifier_unique,    declaration_specifier);
< Gen.Declaration_specifiers.type_unique >
| ~=list_ge1(type_specifier_nonunique, declaration_specifier);
< Gen.Declaration_specifiers.type_nonunique >

(* [declaration_specifiers_typedef] is analogous to [declaration_specifiers],
   but requires the ["typedef"] keyword to be present (exactly once). *)

let typedef :=
| "typedef"; { Gen.Typedef.typedef }

let declaration_specifiers_typedef :=
| ~=list_eq1_eq1(typedef, type_specifier_unique,    declaration_specifier);
<Gen.Declaration_specifiers_typedef.type_unique>
| ~=list_eq1_ge1(typedef, type_specifier_nonunique, declaration_specifier);
<Gen.Declaration_specifiers_typedef.type_nonunique>

(* The parameter [declarator] in [init_declarator_list] and [init_declarator]
   is instantiated with [declarator_varname] or [declarator_typedefname]. *)

let init_declarator_list(declarator) :=
  ~=separated_nonempty_list(",", init_declarator(declarator)); <>

let init_declarator(declarator) :=
| ~=declarator; < Gen.Init_declarator.make >
| ~=declarator; "="; ~=c_initializer; < Gen.Init_declarator.with_initializer >

(* [storage_class_specifier] corresponds to storage-class-specifier in the
   C18 standard, deprived of ["typedef"] (which receives special treatment). *)

let storage_class_specifier :=
| "extern"; { Gen.Storage_class_specifier.extern }
| "static"; { Gen.Storage_class_specifier.static }
| "_Thread_local"; { Gen.Storage_class_specifier.thread_local }
| "auto"; { Gen.Storage_class_specifier.auto }
| "register"; { Gen.Storage_class_specifier.register }

(* A type specifier which can appear together with other type specifiers. *)

let type_specifier_nonunique :=
| "char"; { Gen.Type_specifier_nonunique.char }
| "short"; { Gen.Type_specifier_nonunique.short }
| "int"; { Gen.Type_specifier_nonunique.int }
| "long"; { Gen.Type_specifier_nonunique.long }
| "float"; { Gen.Type_specifier_nonunique.float }
| "double"; { Gen.Type_specifier_nonunique.double }
| "signed"; { Gen.Type_specifier_nonunique.signed }
| "unsigned"; { Gen.Type_specifier_nonunique.unsigned }
| "_Complex"; { Gen.Type_specifier_nonunique.complex }

(* A type specifier which cannot appear together with other type specifiers. *)

let type_specifier_unique :=
| "void"; { Gen.Type_specifier_unique.void }
| "_Bool"; { Gen.Type_specifier_unique.bool }
| ~=atomic_type_specifier; <>
| ~=struct_or_union_specifier; < Gen.Type_specifier_unique.struct_or_union >
| ~=enum_specifier; < Gen.Type_specifier_unique.enum >
| ~=as_typed(typedef_name_spec); < Gen.Type_specifier_unique.name >

let struct_or_union_specifier :=
| ~=struct_or_union; ~=as_typed(general_identifier)?; "{"; ~=struct_declaration_list; "}";
<Gen.Struct_or_union_specifier.defined>
| ~=struct_or_union; ~=as_typed(general_identifier);
<Gen.Struct_or_union_specifier.named>

let struct_or_union :=
| "struct"; { Gen.Struct_or_union.struct_ }
| "union"; { Gen.Struct_or_union.union }

let struct_declaration_list :=
| ~=nonempty_list(struct_declaration); <>

let struct_declaration :=
| ~=specifier_qualifier_list; ~=struct_declarator_list?; ";";
  < Gen.Struct_declaration.make >
| static_assert_declaration;
  < Gen.Struct_declaration.static_assert >

let qualifier_or_alignment :=
| ~=type_qualifier;
< Either.Left >
| ~=alignment_specifier;
< Either.Right >

(* [specifier_qualifier_list] is as in the standard, except it also encodes the
   same constraint as [declaration_specifiers] (see above). *)

let specifier_qualifier_list :=
| ~=list_eq1(type_specifier_unique,    qualifier_or_alignment);
    <Gen.Specifier_qualifier_list.unique>
| ~=list_ge1(type_specifier_nonunique, qualifier_or_alignment);
    <Gen.Specifier_qualifier_list.nonunique>

let struct_declarator_list :=
| ~=struct_declarator; < Util.Stored_reversed.singleton >
| ~=struct_declarator_list; ","; ~=struct_declarator;
    < Util.Stored_reversed.snoc >

let struct_declarator :=
| ~=drop_context(declarator);
  < Gen.Struct_declarator.declarator >
| d=declarator?; ":"; x=located(constant_expression);
  { Gen.Struct_declarator.bit_field (Option.map snd d, x) }

let enum_specifier :=
| "enum"; ~=as_typed(general_identifier)?; "{"; ~=enumerator_list; ","?; "}";
< Gen.Enum.defined >
| "enum"; ~=as_typed(general_identifier);
< Gen.Enum.named >

let enumerator_list :=
| ~=enumerator; < Util.Stored_reversed.singleton >
| ~=enumerator_list; ","; ~=enumerator; < Util.Stored_reversed.snoc >

let enumerator :=
| i = enumeration_constant;
{ Context.declare_varname (fst i); Gen.Enum_constant.named (snd i) }
| i = enumeration_constant; "="; value=located(constant_expression);
    { Context.declare_varname (fst i); Gen.Enum_constant.named ~value (snd i) }

enumeration_constant:
| i = general_identifier
    { i }

let atomic_type_specifier :=
| "_Atomic"; "("; ~=type_name; ")";
< Gen.Type_specifier_unique.atomic >
| "_Atomic"; ATOMIC_LPAREN; ~=type_name; ")";
< Gen.Type_specifier_unique.atomic >
    

let type_qualifier :=
| "const"; { Gen.Type_qualifier.const }
| "restrict"; { Gen.Type_qualifier.restrict }
| "volatile"; { Gen.Type_qualifier.volatile }
| "_Atomic"; { Gen.Type_qualifier.atomic }

let function_specifier :=
| "inline"; { Gen.Function_specifier.inline }
| "_Noreturn"; { Gen.Function_specifier.noreturn }

let alignment_specifier :=
| "_Alignas"; "("; ~=type_name; ")";
  < Gen.Alignment_specifier.alignas_type >
| "_Alignas"; "("; ~=located(constant_expression); ")";
    < Gen.Alignment_specifier.alignas_expression >

let declarator :=
| p=ioption(pointer); d = direct_declarator;
    { let parser_d, result_d = d in
      let result = 
        match p with
        | None -> result_d
        | Some p ->
            Gen.Declarator.pointer (p, result_d)
      in
      Declarator.other_declarator parser_d, result }

(* The occurrences of [save_context] inside [direct_declarator] and
   [direct_abstract_declarator] seem to serve no purpose. In fact, they are
   required in order to avoid certain conflicts. In other words, we must save
   the context at this point because the LR automaton is exploring multiple
   avenues in parallel and some of them do require saving the context. *)

let direct_declarator :=
| i = general_identifier;
    { Declarator.identifier_declarator (fst i)
    , Gen.Declarator.identifier (snd i)
    }
| "("; save_context; d = declarator; ")";
    { d }
| d = direct_declarator; "["; t=type_qualifier_list?; e=located(assignment_expression)?; "]";
    { Declarator.other_declarator (fst d)
    , Gen.Declarator.array (snd d, t, e)
    }
| d = direct_declarator; "["; "static"; t=type_qualifier_list?; e=located(assignment_expression); "]";
    { Declarator.other_declarator (fst d)
    , Gen.Declarator.static_array (snd d, t, e)
    }
| d = direct_declarator; "["; t=type_qualifier_list; "static"; e=located(assignment_expression); "]";
    { Declarator.other_declarator (fst d)
    , Gen.Declarator.static_array (snd d, Some t, e)
    }
| d = direct_declarator; "["; t=type_qualifier_list?; "*"; "]";
    { Declarator.other_declarator (fst d)
    , Gen.Declarator.unspecified_size_variable_array (snd d, t)
    }
| d = direct_declarator; "("; p = scoped(parameter_type_list); ")";
    { Declarator.function_declarator (fst d) (fst p)
    , Gen.Declarator.function_ (snd d, Left (snd p))
    }
| d = direct_declarator; "("; save_context; i=identifier_list?; ")";
    { Declarator.other_declarator (fst d)
    , Gen.Declarator.function_ (snd d, Right i)
    }

let pointer :=
| "*"; ~=type_qualifier_list?; ~=pointer?;
    < Gen.Pointer.make >

let type_qualifier_list :=
| ~=type_qualifier_list?; ~=type_qualifier;
    < Util.Stored_reversed.snoc_opt >

let parameter_type_list :=
| p=parameter_list; d=option(","; "..."; {}); ctx = save_context;
    { ctx
    , Gen.Parameter_type_list.make (p, Option.is_some d)
    }

let parameter_list :=
| ~=parameter_declaration; < Util.Stored_reversed.singleton >
| ~=parameter_list; ","; ~=parameter_declaration; < Util.Stored_reversed.snoc >

let parameter_declaration :=
| ~=declaration_specifiers; ~=drop_context(declarator_varname); 
< Gen.Parameter_declaration.declarator >
| ~=declaration_specifiers; ~=abstract_declarator?;
< Gen.Parameter_declaration.abstract >

let identifier_list := ~=separated_nonempty_list(",", as_typed(var_name)); <>

let type_name :=
| ~=specifier_qualifier_list; ~=abstract_declarator?;
< Gen.Type_name.make >

let abstract_declarator :=
| ~=pointer;
< Gen.Abstract_declarator.pointer >
| ~=ioption(pointer); ~=direct_abstract_declarator;
< Gen.Abstract_declarator.direct >

let direct_abstract_declarator :=
| "("; save_context; ~=abstract_declarator; ")";
< Gen.Direct_abstract_declarator.abstract >
| ~=direct_abstract_declarator?; "["; ~=ioption(type_qualifier_list); ~=located(assignment_expression)?; "]";
< Gen.Direct_abstract_declarator.array >
| ~=direct_abstract_declarator?; "["; "static"; ~=type_qualifier_list?; ~=located(assignment_expression); "]";
< Gen.Direct_abstract_declarator.static_array >
| d=direct_abstract_declarator?; "["; t=type_qualifier_list; "static"; e=located(assignment_expression); "]";
{ Gen.Direct_abstract_declarator.static_array (d, Some t, e) }
| ~=direct_abstract_declarator?; "["; "*"; "]";
< Gen.Direct_abstract_declarator.unspecified_size_variable_array >
| ~=ioption(direct_abstract_declarator); "("; ~=drop_context(scoped(parameter_type_list))?; ")";
< Gen.Direct_abstract_declarator.function_ >

let c_initializer :=
| ~=located(assignment_expression);
< Gen.C_initializer.expression >
| "{"; ~=initializer_list; ","?; "}";
< Gen.C_initializer.initializer_list >

let initializer_list :=
| d=designation?; c=c_initializer;
{ Util.Stored_reversed.singleton (d, c) }
| l=initializer_list; ","; d=designation?; c=c_initializer;
{ Util.Stored_reversed.snoc (l, (d, c)) }

let designation :=
| ~=designator_list; "="; <>

let designator_list :=
| ~=designator_list?; ~=designator; < Util.Stored_reversed.snoc_opt >

let designator :=
| "["; ~=located(constant_expression); "]";
< Gen.Designator.subscript >
| "."; ~=as_typed(general_identifier);
< Gen.Designator.field >

let static_assert_declaration :=
| "_Static_assert"; "("; ~=located(constant_expression); ","; ~=string_literal; ")"; ";";
< Gen.Static_assert_declaration.make >

let statement :=
| ~=labeled_statement; < Gen.Statement.labeled >
| ~=scoped(compound_statement); < Gen.Statement.compound >
| ~=expression_statement; < Gen.Statement.expression >
| ~=scoped(selection_statement); < Gen.Statement.selection >
| ~=scoped(iteration_statement); < Gen.Statement.iteration >
| ~=jump_statement; < Gen.Statement.jump >

let labeled_statement :=
| ~=as_typed(general_identifier); ":"; ~=located(statement);
< Gen.Labeled_statement.label >
| "case"; ~=located(constant_expression); ":"; ~=located(statement);
< Gen.Labeled_statement.case >
| "default"; ":"; ~=located(statement);
< Gen.Labeled_statement.default >

let compound_statement :=
| "{"; ~=block_item_list?; "}";
    < Gen.Compound_statement.make >

let block_item_list :=
| ~=block_item_list?; ~=block_item;
    < Util.Stored_reversed.snoc_opt >

let block_item :=
| ~=located(declaration);
< Gen.Block_item.declaration >
| ~=located(statement);
< Gen.Block_item.statement >

let expression_statement :=
| ~=located(expression)?; ";";
< Gen.Expression_statement.make >

let selection_statement :=
| "if"; "("; ~=located(expression); ")"; ~=located(scoped(statement)); "else"; ~=located(scoped(statement));
    < Gen.Selection_statement.if_else >
| "if"; "("; ~=located(expression); ")"; ~=located(scoped(statement)); %prec below_ELSE
< Gen.Selection_statement.if_ >
| "switch"; "("; ~=located(expression); ")"; ~=located(scoped(statement));
< Gen.Selection_statement.switch >

let iteration_statement :=
| "while"; "("; ~=located(expression); ")"; ~=located(scoped(statement));
< Gen.Iteration_statement.while_ >
| "do"; ~=located(scoped(statement)); "while"; "("; ~=located(expression); ")"; ";";
< Gen.Iteration_statement.do_ >
| "for"; "("; ~=located(expression)?; ";"; ~=located(expression)?; ";"; ~=located(expression)?; ")"; ~=located(scoped(statement));
< Gen.Iteration_statement.for_expr >
| "for"; "("; ~=located(declaration); ~=located(expression)?; ";"; ~=located(expression)?; ")"; ~=located(scoped(statement));
< Gen.Iteration_statement.for_decl >

let jump_statement :=
| "goto"; ~=as_typed(general_identifier); ";";
< Gen.Jump_statement.goto >
| "continue"; ";";
< Gen.Jump_statement.continue >
| "break"; ";";
< Gen.Jump_statement.break >
| "return"; ~=located(expression)?; ";";
< Gen.Jump_statement.return >

let translation_unit_file :=
| e=external_declaration; t=translation_unit_file;
{ e :: t }
| e=external_declaration; EOF;
{ [ e ] }

let external_declaration :=
| ~=function_definition; < Gen.External_declaration.function_definition >
| ~=declaration; < Gen.External_declaration.declaration >

let function_definition1 :=
| s = declaration_specifiers; d = declarator_varname;
    { let ctx = Context.save_context () in
      Declarator.reinstall_function_context (module Context) (fst d);
      ctx, (s, (snd d)) }

let function_definition :=
| fd1 = function_definition1; dl=declaration_list?; cs=compound_statement;
    { let ctx, (s, d) = fd1 in
    Context.restore_context ctx;
    Gen.Function_definition.make (s, d, dl, cs)
    }

let declaration_list :=
| ~=declaration;
    < Util.Stored_reversed.singleton >
| ~=declaration_list; ~=declaration;
    < Util.Stored_reversed.snoc >

single_expression:
| e=expression ";"? EOF
{ e }


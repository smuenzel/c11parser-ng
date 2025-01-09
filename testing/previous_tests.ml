open Core
open Include

let%expect_test "aligned_struct_c18" =
  test
    {|
struct {
  _Alignas(int) char x;
} s;
    |};
[%expect {|
  ((Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name ())
          (declaration
           ((Declarations
             (specifier_qualifiers
              (Nonunique
               (B
                (Second
                 (Type
                  ((specifier_qualifiers (Nonunique (Eq Int ())))
                   (abstract_declarator ()))))
                (Eq Char ()))))
             (declarators ((Declarator (Identifier x)))))))))
        ())))
     (init_declarators ((Plain (Identifier s)))))))
  |}]

let%expect_test "argument_scope" =
  test
    {|
typedef struct foo foo;

void blah(int foo) {
  foo = 1;
}
    |};
[%expect {|
  ((Declaration
    (Typedef
     (specifiers
      (Unique
       (A Typedef (Eq (Struct_or_union (Named (kind Struct) (name foo))) ()))))
     (declarators ((Plain (Identifier foo))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier blah))
       (parameters
        (First
         ((declarations
           ((Declarator (specifiers (Nonunique (Eq Int ())))
             (declarator (Identifier foo)))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var foo))
           (right (Constant (Integer 1))))))))))))
  |}]

let%expect_test "atomic" =
  test
    {|
typedef _Atomic(int) atomic_int;
typedef _Atomic int atomic_int;
typedef _Atomic _Atomic _Atomic(int) atomic_int;

typedef const int const_int;

typedef const atomic_int const_atomic_int;
typedef _Atomic const int const_atomic_int;
typedef const _Atomic int const_atomic_int;
typedef const _Atomic(int) const_atomic_int;
typedef _Atomic const_int const_atomic_int;

typedef int *_Atomic atomic_int_ptr;
typedef _Atomic(int *) atomic_int_ptr;

typedef int _Atomic *int_atomic_ptr;
typedef _Atomic(int) *int_atomic_ptr;

typedef int int_fn();
typedef _Atomic int atomic_int_array[3];

_Atomic struct S { int n; };

struct S
_Atomic atomic_s_no_missing_semicolon;

int *const _Atomic atomic_return_type();
    |};
[%expect {|
  ((Declaration
    (Typedef
     (specifiers
      (Unique
       (A Typedef
        (Eq
         (Atomic
          ((specifier_qualifiers (Nonunique (Eq Int ())))
           (abstract_declarator ())))
         ()))))
     (declarators ((Plain (Identifier atomic_int))))))
   (Declaration
    (Typedef
     (specifiers
      (Nonunique (A Typedef (B (Type_qualifier Atomic) (Eq Int ())))))
     (declarators ((Plain (Identifier atomic_int))))))
   (Declaration
    (Typedef
     (specifiers
      (Unique
       (A Typedef
        (Neq (Type_qualifier Atomic)
         (Neq (Type_qualifier Atomic)
          (Eq
           (Atomic
            ((specifier_qualifiers (Nonunique (Eq Int ())))
             (abstract_declarator ())))
           ()))))))
     (declarators ((Plain (Identifier atomic_int))))))
   (Declaration
    (Typedef
     (specifiers
      (Nonunique (A Typedef (B (Type_qualifier Const) (Eq Int ())))))
     (declarators ((Plain (Identifier const_int))))))
   (Declaration
    (Typedef
     (specifiers
      (Unique
       (A Typedef (Neq (Type_qualifier Const) (Eq (Name atomic_int) ())))))
     (declarators ((Plain (Identifier const_atomic_int))))))
   (Declaration
    (Typedef
     (specifiers
      (Nonunique
       (A Typedef
        (B (Type_qualifier Atomic) (B (Type_qualifier Const) (Eq Int ()))))))
     (declarators ((Plain (Identifier const_atomic_int))))))
   (Declaration
    (Typedef
     (specifiers
      (Nonunique
       (A Typedef
        (B (Type_qualifier Const) (B (Type_qualifier Atomic) (Eq Int ()))))))
     (declarators ((Plain (Identifier const_atomic_int))))))
   (Declaration
    (Typedef
     (specifiers
      (Unique
       (A Typedef
        (Neq (Type_qualifier Const)
         (Eq
          (Atomic
           ((specifier_qualifiers (Nonunique (Eq Int ())))
            (abstract_declarator ())))
          ())))))
     (declarators ((Plain (Identifier const_atomic_int))))))
   (Declaration
    (Typedef
     (specifiers
      (Unique
       (A Typedef (Neq (Type_qualifier Atomic) (Eq (Name const_int) ())))))
     (declarators ((Plain (Identifier const_atomic_int))))))
   (Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators
      ((Plain
        (Pointer (pointer (Qualified (qualifiers (Atomic)) (inner Value)))
         (inner (Identifier atomic_int_ptr))))))))
   (Declaration
    (Typedef
     (specifiers
      (Unique
       (A Typedef
        (Eq
         (Atomic
          ((specifier_qualifiers (Nonunique (Eq Int ())))
           (abstract_declarator ((Pointer Value)))))
         ()))))
     (declarators ((Plain (Identifier atomic_int_ptr))))))
   (Declaration
    (Typedef
     (specifiers (Nonunique (A Typedef (Eq Int ((Type_qualifier Atomic))))))
     (declarators
      ((Plain (Pointer (pointer Value) (inner (Identifier int_atomic_ptr))))))))
   (Declaration
    (Typedef
     (specifiers
      (Unique
       (A Typedef
        (Eq
         (Atomic
          ((specifier_qualifiers (Nonunique (Eq Int ())))
           (abstract_declarator ())))
         ()))))
     (declarators
      ((Plain (Pointer (pointer Value) (inner (Identifier int_atomic_ptr))))))))
   (Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators
      ((Plain
        (Function (declarator (Identifier int_fn)) (parameters (Second ()))))))))
   (Declaration
    (Typedef
     (specifiers
      (Nonunique (A Typedef (B (Type_qualifier Atomic) (Eq Int ())))))
     (declarators
      ((Plain
        (Array (declarator (Identifier atomic_int_array)) (qualifiers ())
         (size ((Constant (Integer 3))))))))))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Neq (Type_qualifier Atomic)
        (Eq
         (Struct_or_union
          (Defined (kind Struct) (name (S))
           (declaration
            ((Declarations (specifier_qualifiers (Nonunique (Eq Int ())))
              (declarators ((Declarator (Identifier n)))))))))
         ()))))
     (init_declarators ())))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq (Struct_or_union (Named (kind Struct) (name S)))
        ((Type_qualifier Atomic)))))
     (init_declarators ((Plain (Identifier atomic_s_no_missing_semicolon))))))
   (Declaration
    (Normal (specifiers (Nonunique (Eq Int ())))
     (init_declarators
      ((Plain
        (Pointer
         (pointer (Qualified (qualifiers (Const Atomic)) (inner Value)))
         (inner
          (Function (declarator (Identifier atomic_return_type))
           (parameters (Second ())))))))))))
  |}]

let%expect_test "atomic_parenthesis" =
  test
    {|
// atomic_parenthesis.c
int _Atomic (x);
    |};
[%expect {|
  ((Declaration
    (Normal (specifiers (Nonunique (Eq Int ((Type_qualifier Atomic)))))
     (init_declarators ((Plain (Identifier x)))))))
  |}]

let%expect_test "bitfield_declaration_ambiguity" =
  test
    {|
// bitfield_declaration_ambiguity.c
typedef signed int T;
struct S {
  unsigned T:3; // bit-field named T with type unsigned
  const T:3;    // anonymous bit-field with type const T
};
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (A Signed (Eq Int ())))))
     (declarators ((Plain (Identifier T))))))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (S))
          (declaration
           ((Declarations (specifier_qualifiers (Nonunique (Eq Unsigned ())))
             (declarators
              ((Bit_field (declarator ((Identifier T)))
                (size (Constant (Integer 3)))))))
            (Declarations
             (specifier_qualifiers
              (Unique (Neq (First Const) (Eq (Name T) ()))))
             (declarators
              ((Bit_field (declarator ()) (size (Constant (Integer 3)))))))))))
        ())))
     (init_declarators ()))))
  |}]


let%expect_test "bitfield_declaration_ambiguity.fail" =
  test
    {|
typedef signed int T;
struct S {
  const T:3;
};


int f(struct S s) {
  return s.T;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (A Signed (Eq Int ())))))
     (declarators ((Plain (Identifier T))))))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (S))
          (declaration
           ((Declarations
             (specifier_qualifiers
              (Unique (Neq (First Const) (Eq (Name T) ()))))
             (declarators
              ((Bit_field (declarator ()) (size (Constant (Integer 3)))))))))))
        ())))
     (init_declarators ())))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Declarator
             (specifiers
              (Unique (Eq (Struct_or_union (Named (kind Struct) (name S))) ())))
             (declarator (Identifier s)))))
          (ellipsis false))))))
     (arguments ())
     (body ((Statement (Jump (Return ((Dot (receiver (Var s)) (field T)))))))))))
  |}]

let%expect_test "bitfield_declaration_ambiguity.ok" =
  test
    {|
typedef signed int T;
struct S {
  unsigned T:3;
};


int f(struct S s) {
  return s.T;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (A Signed (Eq Int ())))))
     (declarators ((Plain (Identifier T))))))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (S))
          (declaration
           ((Declarations (specifier_qualifiers (Nonunique (Eq Unsigned ())))
             (declarators
              ((Bit_field (declarator ((Identifier T)))
                (size (Constant (Integer 3)))))))))))
        ())))
     (init_declarators ())))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Declarator
             (specifiers
              (Unique (Eq (Struct_or_union (Named (kind Struct) (name S))) ())))
             (declarator (Identifier s)))))
          (ellipsis false))))))
     (arguments ())
     (body ((Statement (Jump (Return ((Dot (receiver (Var s)) (field T)))))))))))
  |}]


let%expect_test "block_scope" =
  test
    {|
// block_scope.c
typedef int T;
int x;
void f(void) {
  { T T;
    T = 1;
    typedef int x;
  }
  x = 1; // x as a type is no longer visible
  T u;   // T as a variable is no longer visible
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T))))))
   (Declaration
    (Normal (specifiers (Nonunique (Eq Int ())))
     (init_declarators ((Plain (Identifier x))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Compound
         ((Declaration
           (Normal (specifiers (Unique (Eq (Name T) ())))
            (init_declarators ((Plain (Identifier T))))))
          (Statement
           (Expression
            ((Binary (operator (Assignment Plain)) (left (Var T))
              (right (Constant (Integer 1)))))))
          (Declaration
           (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
            (declarators ((Plain (Identifier x)))))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var x))
           (right (Constant (Integer 1)))))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name T) ())))
         (init_declarators ((Plain (Identifier u)))))))))))
  |}]

let%expect_test "c11-noreturn" =
  test
    {|
_Noreturn int f();
int _Noreturn f();
    |};
[%expect {|
  ((Declaration
    (Normal
     (specifiers (Nonunique (B (Function_specifier Noreturn) (Eq Int ()))))
     (init_declarators
      ((Plain (Function (declarator (Identifier f)) (parameters (Second ()))))))))
   (Declaration
    (Normal (specifiers (Nonunique (Eq Int ((Function_specifier Noreturn)))))
     (init_declarators
      ((Plain (Function (declarator (Identifier f)) (parameters (Second ())))))))))
  |}]


let%expect_test "c1x-alignas" =
  test
    {|
_Alignas(4) char c1;
unsigned _Alignas(long) char c2;
char _Alignas(16) c3;
char _Alignas(_Alignof(int)) c5;
    |};
[%expect {|
  ((Declaration
    (Normal
     (specifiers
      (Nonunique
       (B (Alignment_specifier (Expr (Constant (Integer 4)))) (Eq Char ()))))
     (init_declarators ((Plain (Identifier c1))))))
   (Declaration
    (Normal
     (specifiers
      (Nonunique
       (A Unsigned
        (B
         (Alignment_specifier
          (Type
           ((specifier_qualifiers (Nonunique (Eq Long ())))
            (abstract_declarator ()))))
         (Eq Char ())))))
     (init_declarators ((Plain (Identifier c2))))))
   (Declaration
    (Normal
     (specifiers
      (Nonunique
       (Eq Char ((Alignment_specifier (Expr (Constant (Integer 16))))))))
     (init_declarators ((Plain (Identifier c3))))))
   (Declaration
    (Normal
     (specifiers
      (Nonunique
       (Eq Char
        ((Alignment_specifier
          (Expr
           (Alignof
            ((specifier_qualifiers (Nonunique (Eq Int ())))
             (abstract_declarator ())))))))))
     (init_declarators ((Plain (Identifier c5)))))))
  |}]

let%expect_test "char-literal-printing" =
  test
    {|
int    test1(void) { return '\\'; }
int test2(void) { return L'\\'; }
int    test3(void) { return '\''; }
int test4(void) { return L'\''; }
int    test5(void) { return '\a'; }
int test6(void) { return L'\a'; }
int    test7(void) { return '\b'; }
int test8(void) { return L'\b'; }
int    test11(void) { return '\f'; }
int test12(void) { return L'\f'; }
int    test13(void) { return '\n'; }
int test14(void) { return L'\n'; }
int    test15(void) { return '\r'; }
int test16(void) { return L'\r'; }
int    test17(void) { return '\t'; }
int test18(void) { return L'\t'; }
int    test19(void) { return '\v'; }
int test20(void) { return L'\v'; }
int    test21(void) { return 'c'; }
int test22(void) { return L'c'; }
int    test23(void) { return '\x3'; }
int test24(void) { return L'\x3'; }
int test25(void) { return L'\x333'; }
    |};
[%expect {|
  ((Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test1))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump
         (Return ((Constant (Char ((kind Plain) (value ((Escape "\\"))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test2))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump
         (Return ((Constant (Char ((kind Wide) (value ((Escape "\\"))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test3))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Plain) (value ((Escape '))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test4))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Wide) (value ((Escape '))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test5))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Plain) (value ((Escape a))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test6))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Wide) (value ((Escape a))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test7))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Plain) (value ((Escape b))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test8))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Wide) (value ((Escape b))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test11))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Plain) (value ((Escape f))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test12))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Wide) (value ((Escape f))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test13))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Plain) (value ((Escape n))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test14))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Wide) (value ((Escape n))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test15))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Plain) (value ((Escape r))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test16))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Wide) (value ((Escape r))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test17))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Plain) (value ((Escape t))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test18))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Wide) (value ((Escape t))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test19))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Plain) (value ((Escape v))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test20))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump (Return ((Constant (Char ((kind Wide) (value ((Escape v))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test21))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump
         (Return ((Constant (Char ((kind Plain) (value ((Plain U+0063))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test22))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump
         (Return ((Constant (Char ((kind Wide) (value ((Plain U+0063))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test23))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump
         (Return ((Constant (Char ((kind Plain) (value ((Hex "\\x3"))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test24))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump
         (Return ((Constant (Char ((kind Wide) (value ((Hex "\\x3"))))))))))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier test25))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Jump
         (Return ((Constant (Char ((kind Wide) (value ((Hex "\\x333")))))))))))))))
  |}]

let%expect_test "c-namespace" =
  test
    {|
void bla1() {
  struct XXX;
  int XXX;
}

    |};
[%expect {|
  ((Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier bla1)) (parameters (Second ()))))
     (arguments ())
     (body
      ((Declaration
        (Normal
         (specifiers
          (Unique (Eq (Struct_or_union (Named (kind Struct) (name XXX))) ())))
         (init_declarators ())))
       (Declaration
        (Normal (specifiers (Nonunique (Eq Int ())))
         (init_declarators ((Plain (Identifier XXX)))))))))))
  |}]

let%expect_test "control-scope" =
  test
    {|
int f (int z) {
  if (z + sizeof (enum {a}))
    return 1 + sizeof (enum {a});
  return 0;
}
    |};
[%expect {|
  ((Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Declarator (specifiers (Nonunique (Eq Int ())))
             (declarator (Identifier z)))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Selection
         (If
          (condition
           (Binary (operator (Additive Plus)) (left (Var z))
            (right
             (Sizeof
              ((specifier_qualifiers
                (Unique
                 (Eq
                  (Enum (Defined (name ()) (values (((name a) (value ()))))))
                  ())))
               (abstract_declarator ()))))))
          (then_
           (Jump
            (Return
             ((Binary (operator (Additive Plus)) (left (Constant (Integer 1)))
               (right
                (Sizeof
                 ((specifier_qualifiers
                   (Unique
                    (Eq
                     (Enum
                      (Defined (name ()) (values (((name a) (value ()))))))
                     ())))
                  (abstract_declarator ())))))))))
          (else_ ()))))
       (Statement (Jump (Return ((Constant (Integer 0)))))))))))
  |}]

let%expect_test "dangling_else" =
  test
    {|
// dangling_else.c
int f(void) {
  if(0)
    if(1) return 1;
   else return 0;
  return 1;
}
    |};
[%expect {|
  ((Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Selection
         (If (condition (Constant (Integer 0)))
          (then_
           (Selection
            (If (condition (Constant (Integer 1)))
             (then_ (Jump (Return ((Constant (Integer 1))))))
             (else_ ((Jump (Return ((Constant (Integer 0))))))))))
          (else_ ()))))
       (Statement (Jump (Return ((Constant (Integer 1)))))))))))
  |}]


let%expect_test "dangling_else_lookahead" =
  test
    {|
// dangling_else_lookahead.c
typedef int T;
void f(void) {
  for(int T; ;)
    if(1);
  // T must be resolved outside of the scope of the
  // "for" statement, hence denotes a typedef name:
  T x;
  x = 0;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Iteration
         (For_decl
          (declarator
           (Normal (specifiers (Nonunique (Eq Int ())))
            (init_declarators ((Plain (Identifier T))))))
          (condition ()) (increment ())
          (body
           (Selection
            (If (condition (Constant (Integer 1))) (then_ (Expression ()))
             (else_ ())))))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name T) ())))
         (init_declarators ((Plain (Identifier x))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var x))
           (right (Constant (Integer 0))))))))))))
  |}]

let%expect_test "dangling_else_lookahead.if" =
  test
    {|
// dangling_else_lookahead.c
typedef int T;
void f(void) {
  if(sizeof(enum { T }) == 0);
  T x; // T should be resolved outside of the scope of "if"
  x = 0;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Selection
         (If
          (condition
           (Binary (operator (Equality Equal))
            (left
             (Sizeof
              ((specifier_qualifiers
                (Unique
                 (Eq
                  (Enum (Defined (name ()) (values (((name T) (value ()))))))
                  ())))
               (abstract_declarator ()))))
            (right (Constant (Integer 0)))))
          (then_ (Expression ())) (else_ ()))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name T) ())))
         (init_declarators ((Plain (Identifier x))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var x))
           (right (Constant (Integer 0))))))))))))
  |}]

let%expect_test "dangling_else_misleading.fail" =
  test
    {|
// dangling_else_misleading.fail.c
typedef int T;
void f(void) {
  if(1)
    for(int T; ;)
      if(1) {}
      else {
        T x;
      }
}
    |};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  ("C11parser.Parser_raw.Make(Gen)(Context).MenhirBasics.Error")
  Raised at C11parser__Parser_raw.Make.MenhirBasics._eRR in file "parser/parser_raw.ml" (inlined), line 21, characters 8-19
  Called from C11parser__Parser_raw.Make._menhir_run_425 in file "parser/parser_raw.ml", line 5921, characters 12-19
  Called from Testing__Include.test in file "testing/include.ml", line 12, characters 12-42
  Called from Testing__Previous_tests.(fun) in file "testing/previous_tests.ml", lines 1063-1075, characters 2-6
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]


let%expect_test "declaration_ambiguity" =
  test
    {|
// declaration_ambiguity.c
typedef int T;
void f (void) {
  unsigned int;   // declares zero variables of type "unsigned int"
  const T;        // declares zero variables of type "const T"
  T x;            // T is still visible as a typedef name
  unsigned T;     // declares a variable "T" of type "unsigned"
  T = 1;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Declaration
        (Normal (specifiers (Nonunique (A Unsigned (Eq Int ()))))
         (init_declarators ())))
       (Declaration
        (Normal
         (specifiers (Unique (Neq (Type_qualifier Const) (Eq (Name T) ()))))
         (init_declarators ())))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name T) ())))
         (init_declarators ((Plain (Identifier x))))))
       (Declaration
        (Normal (specifiers (Nonunique (Eq Unsigned ())))
         (init_declarators ((Plain (Identifier T))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var T))
           (right (Constant (Integer 1))))))))))))
  |}]

let%expect_test "declarators" =
  test
    {|
extern int a1[];

void f1(int [*]);

char ((((*X))));

void (*signal(int, void (*)(int)))(int);

int aaaa, ***C, * const D, B(int);

int *A;

struct str;

void test2(int *P, int A) {
  struct str;
  int Array[*(int*)P+A];
}

struct xyz { int y; };
enum myenum { ASDFAS };
struct test10 { int a; } static test10x;
struct test11 { int a; } const test11x;
struct test13 { int a; } (test13x);


struct EnumBitfield {
  enum E2 { e2 } : 4; // ok
};

enum E11 { A1 = 1,};

int PR20634 = sizeof(struct { int n; } [5]);
    |};
[%expect {|
  ((Declaration
    (Normal
     (specifiers (Nonunique (B (Storage_class_specifier Extern) (Eq Int ()))))
     (init_declarators
      ((Plain (Array (declarator (Identifier a1)) (qualifiers ()) (size ())))))))
   (Declaration
    (Normal (specifiers (Unique (Eq Void ())))
     (init_declarators
      ((Plain
        (Function (declarator (Identifier f1))
         (parameters
          (First
           ((declarations
             ((Abstract (specifiers (Nonunique (Eq Int ())))
               (declarator
                ((Direct (pointer ())
                  (direct (Unspecified_size_variable_array ()))))))))
            (ellipsis false))))))))))
   (Declaration
    (Normal (specifiers (Nonunique (Eq Char ())))
     (init_declarators
      ((Plain (Pointer (pointer Value) (inner (Identifier X))))))))
   (Declaration
    (Normal (specifiers (Unique (Eq Void ())))
     (init_declarators
      ((Plain
        (Function
         (declarator
          (Pointer (pointer Value)
           (inner
            (Function (declarator (Identifier signal))
             (parameters
              (First
               ((declarations
                 ((Abstract (specifiers (Nonunique (Eq Int ())))
                   (declarator ()))
                  (Abstract (specifiers (Unique (Eq Void ())))
                   (declarator
                    ((Direct (pointer ())
                      (direct
                       (Function (declarator ((Abstract (Pointer Value))))
                        (parameters
                         ((declarations
                           ((Abstract (specifiers (Nonunique (Eq Int ())))
                             (declarator ()))))
                          (ellipsis false)))))))))))
                (ellipsis false))))))))
         (parameters
          (First
           ((declarations
             ((Abstract (specifiers (Nonunique (Eq Int ()))) (declarator ()))))
            (ellipsis false))))))))))
   (Declaration
    (Normal (specifiers (Nonunique (Eq Int ())))
     (init_declarators
      ((Plain (Identifier aaaa))
       (Plain
        (Pointer (pointer (Pointer (Pointer Value))) (inner (Identifier C))))
       (Plain
        (Pointer (pointer (Qualified (qualifiers (Const)) (inner Value)))
         (inner (Identifier D))))
       (Plain
        (Function (declarator (Identifier B))
         (parameters
          (First
           ((declarations
             ((Abstract (specifiers (Nonunique (Eq Int ()))) (declarator ()))))
            (ellipsis false))))))))))
   (Declaration
    (Normal (specifiers (Nonunique (Eq Int ())))
     (init_declarators
      ((Plain (Pointer (pointer Value) (inner (Identifier A))))))))
   (Declaration
    (Normal
     (specifiers
      (Unique (Eq (Struct_or_union (Named (kind Struct) (name str))) ())))
     (init_declarators ())))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier test2))
       (parameters
        (First
         ((declarations
           ((Declarator (specifiers (Nonunique (Eq Int ())))
             (declarator (Pointer (pointer Value) (inner (Identifier P)))))
            (Declarator (specifiers (Nonunique (Eq Int ())))
             (declarator (Identifier A)))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Declaration
        (Normal
         (specifiers
          (Unique (Eq (Struct_or_union (Named (kind Struct) (name str))) ())))
         (init_declarators ())))
       (Declaration
        (Normal (specifiers (Nonunique (Eq Int ())))
         (init_declarators
          ((Plain
            (Array (declarator (Identifier Array)) (qualifiers ())
             (size
              ((Binary (operator (Additive Plus))
                (left
                 (Unary (operator Dereference)
                  (operand
                   (Cast
                    (type_name
                     ((specifier_qualifiers (Nonunique (Eq Int ())))
                      (abstract_declarator ((Pointer Value)))))
                    (operand (Var P))))))
                (right (Var A)))))))))))))))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (xyz))
          (declaration
           ((Declarations (specifier_qualifiers (Nonunique (Eq Int ())))
             (declarators ((Declarator (Identifier y)))))))))
        ())))
     (init_declarators ())))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Enum (Defined (name (myenum)) (values (((name ASDFAS) (value ()))))))
        ())))
     (init_declarators ())))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (test10))
          (declaration
           ((Declarations (specifier_qualifiers (Nonunique (Eq Int ())))
             (declarators ((Declarator (Identifier a)))))))))
        ((Storage_class_specifier Static)))))
     (init_declarators ((Plain (Identifier test10x))))))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (test11))
          (declaration
           ((Declarations (specifier_qualifiers (Nonunique (Eq Int ())))
             (declarators ((Declarator (Identifier a)))))))))
        ((Type_qualifier Const)))))
     (init_declarators ((Plain (Identifier test11x))))))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (test13))
          (declaration
           ((Declarations (specifier_qualifiers (Nonunique (Eq Int ())))
             (declarators ((Declarator (Identifier a)))))))))
        ())))
     (init_declarators ((Plain (Identifier test13x))))))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (EnumBitfield))
          (declaration
           ((Declarations
             (specifier_qualifiers
              (Unique
               (Eq
                (Enum (Defined (name (E2)) (values (((name e2) (value ()))))))
                ())))
             (declarators
              ((Bit_field (declarator ()) (size (Constant (Integer 4)))))))))))
        ())))
     (init_declarators ())))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Enum
         (Defined (name (E11))
          (values (((name A1) (value ((Constant (Integer 1)))))))))
        ())))
     (init_declarators ())))
   (Declaration
    (Normal (specifiers (Nonunique (Eq Int ())))
     (init_declarators
      ((With_initializer (declarator (Identifier PR20634))
        (initializer_
         (Expression
          (Sizeof
           ((specifier_qualifiers
             (Unique
              (Eq
               (Struct_or_union
                (Defined (kind Struct) (name ())
                 (declaration
                  ((Declarations (specifier_qualifiers (Nonunique (Eq Int ())))
                    (declarators ((Declarator (Identifier n)))))))))
               ())))
            (abstract_declarator
             ((Direct (pointer ())
               (direct
                (Array (declarator ()) (qualifiers ())
                 (size ((Constant (Integer 5)))))))))))))))))))
  |}]


let%expect_test "declarator_visibility" =
  test
    {|
// declarator_visibility.c
typedef int T, T1(T);   // T is visible when declaring T1.
void f(void) {
  int (*T)(T x) = 0;
    // This declaration declares T as being a pointer to a
    // function taking one parameter, x, of type T, and
    // returning an integer. It initializes T to the null pointer.
    // The declaration is valid, since in the declarator of the
    // parameter x, T is still a typedef name, as the declarator
    // of T has not yet ended.

  int T1 = sizeof((int)T1);
    // In the initializer sizeof((int)T1), the declarator of T1 has
    // ended (it is constituted solely by the identifier T1), so T1
    // denotes a variable.
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators
      ((Plain (Identifier T))
       (Plain
        (Function (declarator (Identifier T1))
         (parameters
          (First
           ((declarations
             ((Abstract (specifiers (Unique (Eq (Name T) ()))) (declarator ()))))
            (ellipsis false))))))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Declaration
        (Normal (specifiers (Nonunique (Eq Int ())))
         (init_declarators
          ((With_initializer
            (declarator
             (Function
              (declarator (Pointer (pointer Value) (inner (Identifier T))))
              (parameters
               (First
                ((declarations
                  ((Declarator (specifiers (Unique (Eq (Name T) ())))
                    (declarator (Identifier x)))))
                 (ellipsis false))))))
            (initializer_ (Expression (Constant (Integer 0)))))))))
       (Declaration
        (Normal (specifiers (Nonunique (Eq Int ())))
         (init_declarators
          ((With_initializer (declarator (Identifier T1))
            (initializer_
             (Expression
              (Unary (operator Sizeof)
               (operand
                (Cast
                 (type_name
                  ((specifier_qualifiers (Nonunique (Eq Int ())))
                   (abstract_declarator ())))
                 (operand (Var T1)))))))))))))))))
  |}]

let%expect_test "designator" =
  test
    {|
// RUN: %clang_cc1 -fsyntax-only %s -verify -pedantic

int X[] = {
  /* [4]4,       // expected-warning {{use of GNU 'missing =' extension in designator}} */
  [5] = 7
};

struct foo {
  int arr[10];
};

struct foo Y[10] = {
  [4] .arr [2] = 4,

  // This is not the GNU array init designator extension.
  /* [4] .arr [2] 4  // expected-error {{expected '=' or another designator}} */
};
    |};
[%expect {|
  ((Declaration
    (Normal (specifiers (Nonunique (Eq Int ())))
     (init_declarators
      ((With_initializer
        (declarator
         (Array (declarator (Identifier X)) (qualifiers ()) (size ())))
        (initializer_
         (Initializer_list
          ((((Subscript (Constant (Integer 5))))
            (Expression (Constant (Integer 7))))))))))))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (foo))
          (declaration
           ((Declarations (specifier_qualifiers (Nonunique (Eq Int ())))
             (declarators
              ((Declarator
                (Array (declarator (Identifier arr)) (qualifiers ())
                 (size ((Constant (Integer 10)))))))))))))
        ())))
     (init_declarators ())))
   (Declaration
    (Normal
     (specifiers
      (Unique (Eq (Struct_or_union (Named (kind Struct) (name foo))) ())))
     (init_declarators
      ((With_initializer
        (declarator
         (Array (declarator (Identifier Y)) (qualifiers ())
          (size ((Constant (Integer 10))))))
        (initializer_
         (Initializer_list
          ((((Subscript (Constant (Integer 4))) (Field arr)
             (Subscript (Constant (Integer 2))))
            (Expression (Constant (Integer 4)))))))))))))
  |}]


let%expect_test "enum" =
  test
    {|
// enum.c
typedef enum { a, b = a } foo;
// Each enumeration constant has scope that begins just after the
// appearance of its defining enumerator in an enumerator list.
    |};
[%expect {|
  ((Declaration
    (Typedef
     (specifiers
      (Unique
       (A Typedef
        (Eq
         (Enum
          (Defined (name ())
           (values (((name a) (value ())) ((name b) (value ((Var a))))))))
         ()))))
     (declarators ((Plain (Identifier foo)))))))
  |}]

let%expect_test "enum_constant_visibility" =
  test
    {|
// enum_constant_visibility.c
typedef int T;
void f(void) {
  int x;
  x = (enum {T, U = T+1})1 + T;
  int y = U - T;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Declaration
        (Normal (specifiers (Nonunique (Eq Int ())))
         (init_declarators ((Plain (Identifier x))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var x))
           (right
            (Binary (operator (Additive Plus))
             (left
              (Cast
               (type_name
                ((specifier_qualifiers
                  (Unique
                   (Eq
                    (Enum
                     (Defined (name ())
                      (values
                       (((name T) (value ()))
                        ((name U)
                         (value
                          ((Binary (operator (Additive Plus)) (left (Var T))
                            (right (Constant (Integer 1)))))))))))
                    ())))
                 (abstract_declarator ())))
               (operand (Constant (Integer 1)))))
             (right (Var T))))))))
       (Declaration
        (Normal (specifiers (Nonunique (Eq Int ())))
         (init_declarators
          ((With_initializer (declarator (Identifier y))
            (initializer_
             (Expression
              (Binary (operator (Additive Minus)) (left (Var U))
               (right (Var T)))))))))))))))
  |}]

let%expect_test "enum_shadows_typedef" =
  test
    {|
// enum_shadows_typedef.c
typedef int T;
void f(void) {
  int x = (int)(enum {T})1;
  // T now denotes an enumeration constant,
  // and behaves syntactically like a variable:
  x = (int)T;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Declaration
        (Normal (specifiers (Nonunique (Eq Int ())))
         (init_declarators
          ((With_initializer (declarator (Identifier x))
            (initializer_
             (Expression
              (Cast
               (type_name
                ((specifier_qualifiers (Nonunique (Eq Int ())))
                 (abstract_declarator ())))
               (operand
                (Cast
                 (type_name
                  ((specifier_qualifiers
                    (Unique
                     (Eq
                      (Enum
                       (Defined (name ()) (values (((name T) (value ()))))))
                      ())))
                   (abstract_declarator ())))
                 (operand (Constant (Integer 1)))))))))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var x))
           (right
            (Cast
             (type_name
              ((specifier_qualifiers (Nonunique (Eq Int ())))
               (abstract_declarator ())))
             (operand (Var T)))))))))))))
  |}]

let%expect_test "enum-trick" =
  test
    {|
// enum-trick.c
#include <stdio.h>
enum { a = 42 } x = a;
int main(int argc, char *argv[]) {
  enum { a = a + 1 } y = a;
  printf("%d, %d\n", x, y); // prints: 42, 43
}
// Each enumeration constant has scope that begins just after the
// appearance of its defining enumerator in an enumerator list.
    |};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "Lexer error")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from C11lexer.lexer in file "lexer/c11lexer.ml", line 418, characters 16-28
  Called from C11parser__Parser.Make.wrap.(fun).lexer in file "parser/parser.ml", line 21, characters 18-45
  Called from C11parser__Parser_raw.Make._menhir_run_383 in file "parser/parser_raw.ml", line 16026, characters 19-47
  Called from Testing__Include.test in file "testing/include.ml", line 12, characters 12-42
  Called from Testing__Previous_tests.(fun) in file "testing/previous_tests.ml", lines 1675-1686, characters 2-6
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]

let%expect_test "expressions" =
  test
    {|
void test1() {
  if (sizeof (int){ 1}) {}   // sizeof compound literal
  if (sizeof (int)) {}       // sizeof type

  (void)(int)4;   // cast.
  (void)(int){4}; // compound literal.

  int A = (struct{ int a;}){ 1}.a;
}

int test2(int a, int b) {
  return a ? (void)a,b : a;
}

int test3(int a, int b, int c) {
  return a = b = c;
}

int test4() {
  test4();
  return 0;
}

struct X0 { struct { struct { int c[10][9]; } b; } a; };

void test_sizeof(){
  int arr[10];
  (void)sizeof arr[0];
  (void)sizeof(arr[0]);
  (void)sizeof(arr)[0];
}
    |};
[%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  (Failure "")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from C11parser__Parser_raw.Make._menhir_action_218 in file "parser/parser_raw.mly" (inlined), line 343, characters 4-18
  Called from C11parser__Parser_raw.Make._menhir_run_367 in file "parser/parser_raw.ml", line 13530, characters 17-56
  Called from Testing__Include.test in file "testing/include.ml", line 12, characters 12-42
  Called from Testing__Previous_tests.(fun) in file "testing/previous_tests.ml", lines 1703-1736, characters 2-6
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]

let%expect_test "function-decls" =
  test
    {|
void foo() {
  int X;
  X = sizeof(void (*(*)())());
  X = sizeof(int(*)(int, float, ...));
  X = sizeof(void (*(*)(int arga, void (*argb)(double Y)))(void* Z));
}
    |};
[%expect {|
  ((Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier foo)) (parameters (Second ()))))
     (arguments ())
     (body
      ((Declaration
        (Normal (specifiers (Nonunique (Eq Int ())))
         (init_declarators ((Plain (Identifier X))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var X))
           (right
            (Sizeof
             ((specifier_qualifiers (Unique (Eq Void ())))
              (abstract_declarator
               ((Direct (pointer ())
                 (direct
                  (Function
                   (declarator
                    ((Abstract
                      (Direct (pointer (Value))
                       (direct
                        (Function (declarator ((Abstract (Pointer Value))))
                         (parameters ((declarations ()) (ellipsis false)))))))))
                   (parameters ((declarations ()) (ellipsis false)))))))))))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var X))
           (right
            (Sizeof
             ((specifier_qualifiers (Nonunique (Eq Int ())))
              (abstract_declarator
               ((Direct (pointer ())
                 (direct
                  (Function (declarator ((Abstract (Pointer Value))))
                   (parameters
                    ((declarations
                      ((Abstract (specifiers (Nonunique (Eq Int ())))
                        (declarator ()))
                       (Abstract (specifiers (Nonunique (Eq Float ())))
                        (declarator ()))))
                     (ellipsis true)))))))))))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var X))
           (right
            (Sizeof
             ((specifier_qualifiers (Unique (Eq Void ())))
              (abstract_declarator
               ((Direct (pointer ())
                 (direct
                  (Function
                   (declarator
                    ((Abstract
                      (Direct (pointer (Value))
                       (direct
                        (Function (declarator ((Abstract (Pointer Value))))
                         (parameters
                          ((declarations
                            ((Declarator (specifiers (Nonunique (Eq Int ())))
                              (declarator (Identifier arga)))
                             (Declarator (specifiers (Unique (Eq Void ())))
                              (declarator
                               (Function
                                (declarator
                                 (Pointer (pointer Value)
                                  (inner (Identifier argb))))
                                (parameters
                                 (First
                                  ((declarations
                                    ((Declarator
                                      (specifiers (Nonunique (Eq Double ())))
                                      (declarator (Identifier Y)))))
                                   (ellipsis false)))))))))
                           (ellipsis false)))))))))
                   (parameters
                    ((declarations
                      ((Declarator (specifiers (Unique (Eq Void ())))
                        (declarator
                         (Pointer (pointer Value) (inner (Identifier Z)))))))
                     (ellipsis false))))))))))))))))))))
  |}]

let%expect_test "function_parameter_scope" =
  test
    {|
// function_parameter_scope.c
typedef long T, U;
enum {V} (*f(T T, enum {U} y, int x[T+U]))(T t);
  // The above declares a function f of type:
  // (long, enum{U}, ptr(int)) -> ptr (long -> enum{V})
T x[(U)V+1]; // T and U again denote types; V remains visible
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Long ()))))
     (declarators ((Plain (Identifier T)) (Plain (Identifier U))))))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq (Enum (Defined (name ()) (values (((name V) (value ())))))) ())))
     (init_declarators
      ((Plain
        (Function
         (declarator
          (Pointer (pointer Value)
           (inner
            (Function (declarator (Identifier f))
             (parameters
              (First
               ((declarations
                 ((Declarator (specifiers (Unique (Eq (Name T) ())))
                   (declarator (Identifier T)))
                  (Declarator
                   (specifiers
                    (Unique
                     (Eq
                      (Enum
                       (Defined (name ()) (values (((name U) (value ()))))))
                      ())))
                   (declarator (Identifier y)))
                  (Declarator (specifiers (Nonunique (Eq Int ())))
                   (declarator
                    (Array (declarator (Identifier x)) (qualifiers ())
                     (size
                      ((Binary (operator (Additive Plus)) (left (Var T))
                        (right (Var U))))))))))
                (ellipsis false))))))))
         (parameters
          (First
           ((declarations
             ((Declarator (specifiers (Unique (Eq (Name T) ())))
               (declarator (Identifier t)))))
            (ellipsis false))))))))))
   (Declaration
    (Normal (specifiers (Unique (Eq (Name T) ())))
     (init_declarators
      ((Plain
        (Array (declarator (Identifier x)) (qualifiers ())
         (size
          ((Binary (operator (Additive Plus))
            (left
             (Cast
              (type_name
               ((specifier_qualifiers (Unique (Eq (Name U) ())))
                (abstract_declarator ())))
              (operand (Var V))))
            (right (Constant (Integer 1)))))))))))))
  |}]

let%expect_test "function_parameter_scope_extends" =
  test
    {|
// function_parameter_scope_extends.c
typedef long T, U;
enum {V} (*f(T T, enum {U} y, int x[T+U]))(T t) {
  // The last T on the previous line denotes a type!
  // Here, V, T, U, y, x denote variables:
  long l = T+U+V+x[0]+y;
  return 0;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Long ()))))
     (declarators ((Plain (Identifier T)) (Plain (Identifier U))))))
   (Function
    ((specifiers
      (Unique
       (Eq (Enum (Defined (name ()) (values (((name V) (value ())))))) ())))
     (declarator
      (Function
       (declarator
        (Pointer (pointer Value)
         (inner
          (Function (declarator (Identifier f))
           (parameters
            (First
             ((declarations
               ((Declarator (specifiers (Unique (Eq (Name T) ())))
                 (declarator (Identifier T)))
                (Declarator
                 (specifiers
                  (Unique
                   (Eq
                    (Enum (Defined (name ()) (values (((name U) (value ()))))))
                    ())))
                 (declarator (Identifier y)))
                (Declarator (specifiers (Nonunique (Eq Int ())))
                 (declarator
                  (Array (declarator (Identifier x)) (qualifiers ())
                   (size
                    ((Binary (operator (Additive Plus)) (left (Var T))
                      (right (Var U))))))))))
              (ellipsis false))))))))
       (parameters
        (First
         ((declarations
           ((Declarator (specifiers (Unique (Eq (Name T) ())))
             (declarator (Identifier t)))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Declaration
        (Normal (specifiers (Nonunique (Eq Long ())))
         (init_declarators
          ((With_initializer (declarator (Identifier l))
            (initializer_
             (Expression
              (Binary (operator (Additive Plus))
               (left
                (Binary (operator (Additive Plus))
                 (left
                  (Binary (operator (Additive Plus))
                   (left
                    (Binary (operator (Additive Plus)) (left (Var T))
                     (right (Var U))))
                   (right (Var V))))
                 (right
                  (Array_subscript (array (Var x))
                   (index (Constant (Integer 0)))))))
               (right (Var y))))))))))
       (Statement (Jump (Return ((Constant (Integer 0)))))))))))
  |}]

let%expect_test "if_scopes" =
  test
    {|
// if_scopes.c
typedef int T, U;
int x;
void f(void) {
  if(sizeof(enum {T}))
    // The declaration of T as an enumeration constant is
    // visible in both branches:
    x = sizeof(enum {U}) + T;
  else {
    // Here, the declaration of U as an enumeration constant
    // is no longer visible, but that of T still is.
    U u = (int)T;
  }
  switch(sizeof(enum {U})) x = U;
  // Here, T and U are typedef names again:
  T t; U u;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T)) (Plain (Identifier U))))))
   (Declaration
    (Normal (specifiers (Nonunique (Eq Int ())))
     (init_declarators ((Plain (Identifier x))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Selection
         (If
          (condition
           (Sizeof
            ((specifier_qualifiers
              (Unique
               (Eq (Enum (Defined (name ()) (values (((name T) (value ()))))))
                ())))
             (abstract_declarator ()))))
          (then_
           (Expression
            ((Binary (operator (Assignment Plain)) (left (Var x))
              (right
               (Binary (operator (Additive Plus))
                (left
                 (Sizeof
                  ((specifier_qualifiers
                    (Unique
                     (Eq
                      (Enum
                       (Defined (name ()) (values (((name U) (value ()))))))
                      ())))
                   (abstract_declarator ()))))
                (right (Var T))))))))
          (else_
           ((Compound
             ((Declaration
               (Normal (specifiers (Unique (Eq (Name U) ())))
                (init_declarators
                 ((With_initializer (declarator (Identifier u))
                   (initializer_
                    (Expression
                     (Cast
                      (type_name
                       ((specifier_qualifiers (Nonunique (Eq Int ())))
                        (abstract_declarator ())))
                      (operand (Var T)))))))))))))))))
       (Statement
        (Selection
         (Switch
          (condition
           (Sizeof
            ((specifier_qualifiers
              (Unique
               (Eq (Enum (Defined (name ()) (values (((name U) (value ()))))))
                ())))
             (abstract_declarator ()))))
          (body
           (Expression
            ((Binary (operator (Assignment Plain)) (left (Var x))
              (right (Var U)))))))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name T) ())))
         (init_declarators ((Plain (Identifier t))))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name U) ())))
         (init_declarators ((Plain (Identifier u)))))))))))
  |}]

let%expect_test "local_scope" =
  test
    {|
// local_scope.c
typedef int T;
void f(void) {
  T y = 1; // T is a type
  if(1) {
    int T;
    T = 1; // T is a variable
  }
  T x = 1; // T is a type again
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Declaration
        (Normal (specifiers (Unique (Eq (Name T) ())))
         (init_declarators
          ((With_initializer (declarator (Identifier y))
            (initializer_ (Expression (Constant (Integer 1)))))))))
       (Statement
        (Selection
         (If (condition (Constant (Integer 1)))
          (then_
           (Compound
            ((Declaration
              (Normal (specifiers (Nonunique (Eq Int ())))
               (init_declarators ((Plain (Identifier T))))))
             (Statement
              (Expression
               ((Binary (operator (Assignment Plain)) (left (Var T))
                 (right (Constant (Integer 1))))))))))
          (else_ ()))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name T) ())))
         (init_declarators
          ((With_initializer (declarator (Identifier x))
            (initializer_ (Expression (Constant (Integer 1))))))))))))))
  |}]

let%expect_test "local_typedef" =
  test
    {|
// local_typedef.c
typedef int T1;      // Declaration of type T1 as int
void f(void) {
  typedef int *T2;   // Declaration of type T2 as pointer to int
  T1 x1;             // Declaration of x1 of type T1
  T2 x2;             // Declaration of x2 of type T2
  x1 = 0;
  x2 = 0;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T1))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Declaration
        (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
         (declarators
          ((Plain (Pointer (pointer Value) (inner (Identifier T2))))))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name T1) ())))
         (init_declarators ((Plain (Identifier x1))))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name T2) ())))
         (init_declarators ((Plain (Identifier x2))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var x1))
           (right (Constant (Integer 0)))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var x2))
           (right (Constant (Integer 0))))))))))))
  |}]

let%expect_test "long-long-struct" =
  test
    {|
typedef struct {
  long long foo;
} mystruct;
    |};
[%expect {|
  ((Declaration
    (Typedef
     (specifiers
      (Unique
       (A Typedef
        (Eq
         (Struct_or_union
          (Defined (kind Struct) (name ())
           (declaration
            ((Declarations
              (specifier_qualifiers (Nonunique (A Long (Eq Long ()))))
              (declarators ((Declarator (Identifier foo)))))))))
         ()))))
     (declarators ((Plain (Identifier mystruct)))))))
  |}]

let%expect_test "loop_scopes" =
  test
    {|
// loop_scopes.c
typedef int T, U;
int x;
void f(void) {
  for(int T = 0; sizeof(enum {U}); ) x = U+T;
  for(sizeof(enum {U}); ; ) x = U + sizeof(enum {T});
  while(sizeof(enum {U})) x = U;
  // A declaration in the body of a do ... while loop
  // is not visible in the loop condition.
  do x = sizeof(enum {U}) + U;
  while((U)1 + sizeof(enum {U}));
  // The above declarations of T and U took place in inner scopes
  // and are no longer visible.
  T u3; U u4;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T)) (Plain (Identifier U))))))
   (Declaration
    (Normal (specifiers (Nonunique (Eq Int ())))
     (init_declarators ((Plain (Identifier x))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Iteration
         (For_decl
          (declarator
           (Normal (specifiers (Nonunique (Eq Int ())))
            (init_declarators
             ((With_initializer (declarator (Identifier T))
               (initializer_ (Expression (Constant (Integer 0)))))))))
          (condition
           ((Sizeof
             ((specifier_qualifiers
               (Unique
                (Eq (Enum (Defined (name ()) (values (((name U) (value ()))))))
                 ())))
              (abstract_declarator ())))))
          (increment ())
          (body
           (Expression
            ((Binary (operator (Assignment Plain)) (left (Var x))
              (right
               (Binary (operator (Additive Plus)) (left (Var U))
                (right (Var T)))))))))))
       (Statement
        (Iteration
         (For_expr
          (init
           ((Sizeof
             ((specifier_qualifiers
               (Unique
                (Eq (Enum (Defined (name ()) (values (((name U) (value ()))))))
                 ())))
              (abstract_declarator ())))))
          (condition ()) (increment ())
          (body
           (Expression
            ((Binary (operator (Assignment Plain)) (left (Var x))
              (right
               (Binary (operator (Additive Plus)) (left (Var U))
                (right
                 (Sizeof
                  ((specifier_qualifiers
                    (Unique
                     (Eq
                      (Enum
                       (Defined (name ()) (values (((name T) (value ()))))))
                      ())))
                   (abstract_declarator ())))))))))))))
       (Statement
        (Iteration
         (While
          (condition
           (Sizeof
            ((specifier_qualifiers
              (Unique
               (Eq (Enum (Defined (name ()) (values (((name U) (value ()))))))
                ())))
             (abstract_declarator ()))))
          (body
           (Expression
            ((Binary (operator (Assignment Plain)) (left (Var x))
              (right (Var U)))))))))
       (Statement
        (Iteration
         (Do
          (condition
           (Binary (operator (Additive Plus))
            (left
             (Cast
              (type_name
               ((specifier_qualifiers (Unique (Eq (Name U) ())))
                (abstract_declarator ())))
              (operand (Constant (Integer 1)))))
            (right
             (Sizeof
              ((specifier_qualifiers
                (Unique
                 (Eq
                  (Enum (Defined (name ()) (values (((name U) (value ()))))))
                  ())))
               (abstract_declarator ()))))))
          (body
           (Expression
            ((Binary (operator (Assignment Plain)) (left (Var x))
              (right
               (Binary (operator (Additive Plus))
                (left
                 (Sizeof
                  ((specifier_qualifiers
                    (Unique
                     (Eq
                      (Enum
                       (Defined (name ()) (values (((name U) (value ()))))))
                      ())))
                   (abstract_declarator ()))))
                (right (Var U)))))))))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name T) ())))
         (init_declarators ((Plain (Identifier u3))))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name U) ())))
         (init_declarators ((Plain (Identifier u4)))))))))))
  |}]

let%expect_test "namespaces" =
  test
    {|
// namespaces.c
typedef int S, T, U;
struct S { int T; };
union U { int x; };
void f(void) {
  // The following uses of S, T, U are correct, and have no
  // effect on the visibility of S, T, U as typedef names.
  struct S s = { .T = 1 };
  T: s.T = 2;
  union U u = { 1 };
  goto T;
  // S, T and U are still typedef names:
  S ss = 1; T tt = 1; U uu = 1;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators
      ((Plain (Identifier S)) (Plain (Identifier T)) (Plain (Identifier U))))))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (S))
          (declaration
           ((Declarations (specifier_qualifiers (Nonunique (Eq Int ())))
             (declarators ((Declarator (Identifier T)))))))))
        ())))
     (init_declarators ())))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Union) (name (U))
          (declaration
           ((Declarations (specifier_qualifiers (Nonunique (Eq Int ())))
             (declarators ((Declarator (Identifier x)))))))))
        ())))
     (init_declarators ())))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Declaration
        (Normal
         (specifiers
          (Unique (Eq (Struct_or_union (Named (kind Struct) (name S))) ())))
         (init_declarators
          ((With_initializer (declarator (Identifier s))
            (initializer_
             (Initializer_list
              ((((Field T)) (Expression (Constant (Integer 1))))))))))))
       (Statement
        (Labeled
         (Label (name T)
          (statement
           (Expression
            ((Binary (operator (Assignment Plain))
              (left (Dot (receiver (Var s)) (field T)))
              (right (Constant (Integer 2))))))))))
       (Declaration
        (Normal
         (specifiers
          (Unique (Eq (Struct_or_union (Named (kind Union) (name U))) ())))
         (init_declarators
          ((With_initializer (declarator (Identifier u))
            (initializer_
             (Initializer_list ((() (Expression (Constant (Integer 1))))))))))))
       (Statement (Jump (Goto T)))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name S) ())))
         (init_declarators
          ((With_initializer (declarator (Identifier ss))
            (initializer_ (Expression (Constant (Integer 1)))))))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name T) ())))
         (init_declarators
          ((With_initializer (declarator (Identifier tt))
            (initializer_ (Expression (Constant (Integer 1)))))))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name U) ())))
         (init_declarators
          ((With_initializer (declarator (Identifier uu))
            (initializer_ (Expression (Constant (Integer 1))))))))))))))
  |}]

let%expect_test "no_local_scope" =
  test
    {|
// no_local_scope.c
typedef int T, U, V;
int x;
int f(void) {
  x = sizeof(enum {T});
  label: x = sizeof(enum {U});
  return sizeof(enum {V});
  // T, U and V now denote enumeration constants:
  x = T + U + V;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators
      ((Plain (Identifier T)) (Plain (Identifier U)) (Plain (Identifier V))))))
   (Declaration
    (Normal (specifiers (Nonunique (Eq Int ())))
     (init_declarators ((Plain (Identifier x))))))
   (Function
    ((specifiers (Nonunique (Eq Int ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var x))
           (right
            (Sizeof
             ((specifier_qualifiers
               (Unique
                (Eq (Enum (Defined (name ()) (values (((name T) (value ()))))))
                 ())))
              (abstract_declarator ()))))))))
       (Statement
        (Labeled
         (Label (name label)
          (statement
           (Expression
            ((Binary (operator (Assignment Plain)) (left (Var x))
              (right
               (Sizeof
                ((specifier_qualifiers
                  (Unique
                   (Eq
                    (Enum (Defined (name ()) (values (((name U) (value ()))))))
                    ())))
                 (abstract_declarator ())))))))))))
       (Statement
        (Jump
         (Return
          ((Sizeof
            ((specifier_qualifiers
              (Unique
               (Eq (Enum (Defined (name ()) (values (((name V) (value ()))))))
                ())))
             (abstract_declarator ())))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain)) (left (Var x))
           (right
            (Binary (operator (Additive Plus))
             (left
              (Binary (operator (Additive Plus)) (left (Var T))
               (right (Var U))))
             (right (Var V)))))))))))))
  |}]

let%expect_test "parameter_declaration_ambiguity" =
  test
    {|
// parameter_declaration_ambiguity.c
typedef int T;
void f(int(x), int(T), int T);
// First parameter: named x, of type int
// Second parameter: anonymous, of type int(T) (i.e., T -> int)
// Third parameter: named T, of type int
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T))))))
   (Declaration
    (Normal (specifiers (Unique (Eq Void ())))
     (init_declarators
      ((Plain
        (Function (declarator (Identifier f))
         (parameters
          (First
           ((declarations
             ((Declarator (specifiers (Nonunique (Eq Int ())))
               (declarator (Identifier x)))
              (Abstract (specifiers (Nonunique (Eq Int ())))
               (declarator
                ((Direct (pointer ())
                  (direct
                   (Function (declarator ())
                    (parameters
                     ((declarations
                       ((Abstract (specifiers (Unique (Eq (Name T) ())))
                         (declarator ()))))
                      (ellipsis false)))))))))
              (Declarator (specifiers (Nonunique (Eq Int ())))
               (declarator (Identifier T)))))
            (ellipsis false)))))))))))
  |}]

let%expect_test "parameter_declaration_ambiguity.test" =
  test
    {|
typedef int T;

void f(int(T), T x);
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T))))))
   (Declaration
    (Normal (specifiers (Unique (Eq Void ())))
     (init_declarators
      ((Plain
        (Function (declarator (Identifier f))
         (parameters
          (First
           ((declarations
             ((Abstract (specifiers (Nonunique (Eq Int ())))
               (declarator
                ((Direct (pointer ())
                  (direct
                   (Function (declarator ())
                    (parameters
                     ((declarations
                       ((Abstract (specifiers (Unique (Eq (Name T) ())))
                         (declarator ()))))
                      (ellipsis false)))))))))
              (Declarator (specifiers (Unique (Eq (Name T) ())))
               (declarator (Identifier x)))))
            (ellipsis false)))))))))))
  |}]

let%expect_test "statements" =
  test
    {|
void test1() {
  { ; {  ;;}} ;;
}

void test2() {
  if (0) { if (1) {} } else { }
  do { } while (0);
  while (0) while(0) do ; while(0);
  for ((void)0;0;(void)0)
    for (;;)
      for ((void)9;0;(void)2)
        ;
  for (int X = 0; 0; (void)0);
}

void test3() {
    switch (0) {
    case 4:
      if (0) {
    case 6: ;
      }
    default:
      ;
  }
}

void test4() {
  if (0);
  int X;
foo:  if (0);
}

typedef int t;
void test5() {
  if (0);
  t x = 0;
  if (0);
}
    |};
[%expect {|
  ((Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier test1)) (parameters (Second ()))))
     (arguments ())
     (body
      ((Statement
        (Compound
         ((Statement (Expression ()))
          (Statement
           (Compound ((Statement (Expression ())) (Statement (Expression ()))))))))
       (Statement (Expression ())) (Statement (Expression ()))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier test2)) (parameters (Second ()))))
     (arguments ())
     (body
      ((Statement
        (Selection
         (If (condition (Constant (Integer 0)))
          (then_
           (Compound
            ((Statement
              (Selection
               (If (condition (Constant (Integer 1))) (then_ (Compound ()))
                (else_ ())))))))
          (else_ ((Compound ()))))))
       (Statement
        (Iteration
         (Do (condition (Constant (Integer 0))) (body (Compound ())))))
       (Statement
        (Iteration
         (While (condition (Constant (Integer 0)))
          (body
           (Iteration
            (While (condition (Constant (Integer 0)))
             (body
              (Iteration
               (Do (condition (Constant (Integer 0))) (body (Expression ())))))))))))
       (Statement
        (Iteration
         (For_expr
          (init
           ((Cast
             (type_name
              ((specifier_qualifiers (Unique (Eq Void ())))
               (abstract_declarator ())))
             (operand (Constant (Integer 0))))))
          (condition ((Constant (Integer 0))))
          (increment
           ((Cast
             (type_name
              ((specifier_qualifiers (Unique (Eq Void ())))
               (abstract_declarator ())))
             (operand (Constant (Integer 0))))))
          (body
           (Iteration
            (For_expr (init ()) (condition ()) (increment ())
             (body
              (Iteration
               (For_expr
                (init
                 ((Cast
                   (type_name
                    ((specifier_qualifiers (Unique (Eq Void ())))
                     (abstract_declarator ())))
                   (operand (Constant (Integer 9))))))
                (condition ((Constant (Integer 0))))
                (increment
                 ((Cast
                   (type_name
                    ((specifier_qualifiers (Unique (Eq Void ())))
                     (abstract_declarator ())))
                   (operand (Constant (Integer 2))))))
                (body (Expression ())))))))))))
       (Statement
        (Iteration
         (For_decl
          (declarator
           (Normal (specifiers (Nonunique (Eq Int ())))
            (init_declarators
             ((With_initializer (declarator (Identifier X))
               (initializer_ (Expression (Constant (Integer 0)))))))))
          (condition ((Constant (Integer 0))))
          (increment
           ((Cast
             (type_name
              ((specifier_qualifiers (Unique (Eq Void ())))
               (abstract_declarator ())))
             (operand (Constant (Integer 0))))))
          (body (Expression ())))))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier test3)) (parameters (Second ()))))
     (arguments ())
     (body
      ((Statement
        (Selection
         (Switch (condition (Constant (Integer 0)))
          (body
           (Compound
            ((Statement
              (Labeled
               (Case (expression (Constant (Integer 4)))
                (statement
                 (Selection
                  (If (condition (Constant (Integer 0)))
                   (then_
                    (Compound
                     ((Statement
                       (Labeled
                        (Case (expression (Constant (Integer 6)))
                         (statement (Expression ()))))))))
                   (else_ ())))))))
             (Statement (Labeled (Default (statement (Expression ())))))))))))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier test4)) (parameters (Second ()))))
     (arguments ())
     (body
      ((Statement
        (Selection
         (If (condition (Constant (Integer 0))) (then_ (Expression ()))
          (else_ ()))))
       (Declaration
        (Normal (specifiers (Nonunique (Eq Int ())))
         (init_declarators ((Plain (Identifier X))))))
       (Statement
        (Labeled
         (Label (name foo)
          (statement
           (Selection
            (If (condition (Constant (Integer 0))) (then_ (Expression ()))
             (else_ ())))))))))))
   (Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier t))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier test5)) (parameters (Second ()))))
     (arguments ())
     (body
      ((Statement
        (Selection
         (If (condition (Constant (Integer 0))) (then_ (Expression ()))
          (else_ ()))))
       (Declaration
        (Normal (specifiers (Unique (Eq (Name t) ())))
         (init_declarators
          ((With_initializer (declarator (Identifier x))
            (initializer_ (Expression (Constant (Integer 0)))))))))
       (Statement
        (Selection
         (If (condition (Constant (Integer 0))) (then_ (Expression ()))
          (else_ ())))))))))
  |}]

let%expect_test "struct-recursion" =
  test
    {|
// RUN: %clang_cc1 %s -fsyntax-only

// C99 6.7.2.3p11

// mutually recursive structs
struct s1 { struct s2 *A; };
struct s2 { struct s1 *B; };

// both types are complete now.
struct s1 a;
struct s2 b;
    |};
[%expect {|
  ((Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (s1))
          (declaration
           ((Declarations
             (specifier_qualifiers
              (Unique
               (Eq (Struct_or_union (Named (kind Struct) (name s2))) ())))
             (declarators
              ((Declarator (Pointer (pointer Value) (inner (Identifier A)))))))))))
        ())))
     (init_declarators ())))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (s2))
          (declaration
           ((Declarations
             (specifier_qualifiers
              (Unique
               (Eq (Struct_or_union (Named (kind Struct) (name s1))) ())))
             (declarators
              ((Declarator (Pointer (pointer Value) (inner (Identifier B)))))))))))
        ())))
     (init_declarators ())))
   (Declaration
    (Normal
     (specifiers
      (Unique (Eq (Struct_or_union (Named (kind Struct) (name s1))) ())))
     (init_declarators ((Plain (Identifier a))))))
   (Declaration
    (Normal
     (specifiers
      (Unique (Eq (Struct_or_union (Named (kind Struct) (name s2))) ())))
     (init_declarators ((Plain (Identifier b)))))))
  |}]

let%expect_test "typedef_star" =
  test
    {|
// typedef_star.c
typedef int T;
void f(void) {
  T * b;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier T))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Declaration
        (Normal (specifiers (Unique (Eq (Name T) ())))
         (init_declarators
          ((Plain (Pointer (pointer Value) (inner (Identifier b)))))))))))))
  |}]

let%expect_test "types" =
  test
    {|
typedef int X;
struct Y { short X; };

typedef struct foo { int x; } foo;
void test() {
   foo *foo;
   foo->x = 0;
}
    |};
[%expect {|
  ((Declaration
    (Typedef (specifiers (Nonunique (A Typedef (Eq Int ()))))
     (declarators ((Plain (Identifier X))))))
   (Declaration
    (Normal
     (specifiers
      (Unique
       (Eq
        (Struct_or_union
         (Defined (kind Struct) (name (Y))
          (declaration
           ((Declarations (specifier_qualifiers (Nonunique (Eq Short ())))
             (declarators ((Declarator (Identifier X)))))))))
        ())))
     (init_declarators ())))
   (Declaration
    (Typedef
     (specifiers
      (Unique
       (A Typedef
        (Eq
         (Struct_or_union
          (Defined (kind Struct) (name (foo))
           (declaration
            ((Declarations (specifier_qualifiers (Nonunique (Eq Int ())))
              (declarators ((Declarator (Identifier x)))))))))
         ()))))
     (declarators ((Plain (Identifier foo))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier test)) (parameters (Second ()))))
     (arguments ())
     (body
      ((Declaration
        (Normal (specifiers (Unique (Eq (Name foo) ())))
         (init_declarators
          ((Plain (Pointer (pointer Value) (inner (Identifier foo))))))))
       (Statement
        (Expression
         ((Binary (operator (Assignment Plain))
           (left (Arrow (receiver (Var foo)) (field x)))
           (right (Constant (Integer 0))))))))))))
  |}]

let%expect_test "variable_star" =
  test
    {|
// variable_star.c
int T, b;
void f(void) {
  T * b;
}
    |};
[%expect {|
  ((Declaration
    (Normal (specifiers (Nonunique (Eq Int ())))
     (init_declarators ((Plain (Identifier T)) (Plain (Identifier b))))))
   (Function
    ((specifiers (Unique (Eq Void ())))
     (declarator
      (Function (declarator (Identifier f))
       (parameters
        (First
         ((declarations
           ((Abstract (specifiers (Unique (Eq Void ()))) (declarator ()))))
          (ellipsis false))))))
     (arguments ())
     (body
      ((Statement
        (Expression
         ((Binary (operator (Multiplicative Multiply)) (left (Var T))
           (right (Var b)))))))))))
  |}]

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

(*
let%expect_test "atomic_parenthesis" =
  test
    {|
// atomic_parenthesis.c
int _Atomic (x);
    |};
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

let%expect_test "c11-noreturn" =
  test
    {|
_Noreturn int f();
int _Noreturn f();
    |};
[%expect {||}]

let%expect_test "c1x-alignas" =
  test
    {|
_Alignas(4) char c1;
unsigned _Alignas(long) char c2;
char _Alignas(16) c3;
char _Alignas(_Alignof(int)) c5;
    |};
[%expect {||}]

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
[%expect {||}]

let%expect_test "c-namespace" =
  test
    {|
void bla1() {
  struct XXX;
  int XXX;
}

    |};
[%expect {||}]

let%expect_test "control-scope" =
  test
    {|
int f (int z) {
  if (z + sizeof (enum {a}))
    return 1 + sizeof (enum {a});
  return 0;
}
    |};
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

let%expect_test "enum" =
  test
    {|
// enum.c
typedef enum { a, b = a } foo;
// Each enumeration constant has scope that begins just after the
// appearance of its defining enumerator in an enumerator list.
    |};
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

let%expect_test "long-long-struct" =
  test
    {|
typedef struct {
  long long foo;
} mystruct;
    |};
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

let%expect_test "parameter_declaration_ambiguity.test" =
  test
    {|
typedef int T;

void f(int(T), T x);
    |};
[%expect {||}]

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
[%expect {||}]

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
[%expect {||}]

let%expect_test "typedef_star" =
  test
    {|
// typedef_star.c
typedef int T;
void f(void) {
  T * b;
}
    |};
[%expect {||}]

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
[%expect {||}]

let%expect_test "variable_star" =
  test
    {|
// variable_star.c
int T, b;
void f(void) {
  T * b;
}
    |};
[%expect {||}]

   *)

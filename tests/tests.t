  $ PARSECMD="$TESTDIR/../parse -std c11 -atomic-permissive-syntax"
  $ $PARSECMD < $TESTDIR/argument_scope.c
  cannot open /argument_scope.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/atomic.c
  cannot open /atomic.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/c11-noreturn.c
  cannot open /c11-noreturn.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/c1x-alignas.c
  cannot open /c1x-alignas.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/char-literal-printing.c
  cannot open /char-literal-printing.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/c-namespace.c
  cannot open /c-namespace.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/control-scope.c
  cannot open /control-scope.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/declarators.c
  cannot open /declarators.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/designator.c
  cannot open /designator.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/expressions.c
  cannot open /expressions.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/long-long-struct.c
  cannot open /long-long-struct.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/function-decls.c
  cannot open /function-decls.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/statements.c
  cannot open /statements.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/struct-recursion.c
  cannot open /struct-recursion.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/types.c
  cannot open /types.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/local_typedef.c
  cannot open /local_typedef.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/declaration_ambiguity.c
  cannot open /declaration_ambiguity.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/declarator_visibility.c
  cannot open /declarator_visibility.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/enum_shadows_typedef.c
  cannot open /enum_shadows_typedef.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/enum_constant_visibility.c
  cannot open /enum_constant_visibility.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/namespaces.c
  cannot open /namespaces.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/local_scope.c
  cannot open /local_scope.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/block_scope.c
  cannot open /block_scope.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/if_scopes.c
  cannot open /if_scopes.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/loop_scopes.c
  cannot open /loop_scopes.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/no_local_scope.c
  cannot open /no_local_scope.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/function_parameter_scope.c
  cannot open /function_parameter_scope.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/function_parameter_scope_extends.c
  cannot open /function_parameter_scope_extends.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/dangling_else.c
  cannot open /dangling_else.c: No such file
  [2]
SHOULD FAIL:
  $ $PARSECMD < $TESTDIR/dangling_else_misleading.fail.c
  cannot open /dangling_else_misleading.fail.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/dangling_else_lookahead.c
  cannot open /dangling_else_lookahead.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/dangling_else_lookahead.if.c
  cannot open /dangling_else_lookahead.if.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/parameter_declaration_ambiguity.c
  cannot open /parameter_declaration_ambiguity.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/parameter_declaration_ambiguity.test.c
  cannot open /parameter_declaration_ambiguity.test.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/bitfield_declaration_ambiguity.c
  cannot open /bitfield_declaration_ambiguity.c: No such file
  [2]
SHOULD FAIL (does not because we do not have a semantic analysis):
  $ $PARSECMD < $TESTDIR/bitfield_declaration_ambiguity.fail.c
  cannot open /bitfield_declaration_ambiguity.fail.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/bitfield_declaration_ambiguity.ok.c
  cannot open /bitfield_declaration_ambiguity.ok.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/atomic_parenthesis.c
  cannot open /atomic_parenthesis.c: No such file
  [2]
  $ $PARSECMD < $TESTDIR/aligned_struct_c18.c
  cannot open /aligned_struct_c18.c: No such file
  [2]
  $ ls $TESTDIR/*.c
  ls: cannot access '/*.c': No such file or directory
  [2]

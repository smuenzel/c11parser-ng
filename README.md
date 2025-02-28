C11parser-ng
============

##### A correct C89/C90/C99/C11/C18 parser written using Menhir, Sedlex and OCaml

Based on the original C11parser by Jacques-Henri Jourdan and François Pottier, see
README.orig.md for details.

This version converts the original lexer to sedlex and changes many of the menhir
rules to use the new syntax.
In addition, an example AST is provided.

The example AST is not yet complete, but the parser should be equivalent to
C11parser, modulo any newly introduced errors.

(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2016-2017, Inria
 * SPDX-FileCopyrightText: (c) 2024-2025, Stefan Muenzel
 * SPDX-FileContributor: Jacques-Henri Jourdan, Inria Paris
 * SPDX-FileContributor: Francois Pottier, Inria Paris
 * SPDX-FileContributor: Stefan Muenzel
 *)

(* We distinguish between three kinds of declarators: 1- identifiers,
   2- function declarators, and 3- everything else. In the case of a
   function declarator, we save a snapshot of the context at the END
   of the parameter-type-list. *)

(* With a declarator, we associate two pieces of information: 1- the
   identifier that is being declared; 2- the declarator's kind, as
   defined above. *)

type 'context t

(* This accessor returns the identifier that is being declared. *)

val identifier: 'context t -> string

(* Three functions for constructing declarators. *)

val identifier_declarator: string -> 'context t
val function_declarator: 'context t -> 'context -> 'context t
val other_declarator: 'context t -> 'context t

(* A function for restoring the context that was saved in a function
   declarator and, on top of that, declaring the function itself as a
   variable. *)

val reinstall_function_context 
  : (module Context.Packed with type snapshot = 'snapshot)
  -> 'snapshot t -> unit

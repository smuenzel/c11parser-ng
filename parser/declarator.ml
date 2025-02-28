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

(* K&R function declarators are considered part of "other" declarators. *)

type 'context declarator_kind =
  | DeclaratorIdentifier
  | DeclaratorFunction of 'context
  | DeclaratorOther

(* With a declarator, we associate two pieces of information: 1- the
   identifier that is being declared; 2- the declarator's kind, as
   defined above. *)

type 'context t = {
  identifier: string;
  kind: 'context declarator_kind
}

(* This accessor returns the identifier that is being declared. *)

let identifier d =
  d.identifier

(* Three functions for constructing declarators. *)

let identifier_declarator i =
  { identifier = i; kind = DeclaratorIdentifier }

let function_declarator d ctx =
  match d.kind with
  | DeclaratorIdentifier -> { d with kind = DeclaratorFunction ctx }
  | _                    ->   d

let other_declarator d =
  match d.kind with
  | DeclaratorIdentifier -> { d with kind = DeclaratorOther }
  | _                    ->   d

(* A function for restoring the context that was saved in a function
   declarator and, on top of that, declaring the function itself as a
   variable. *)

let reinstall_function_context
    (type snapshot)
    (module Context : Context.Packed with type snapshot = snapshot)
    d
  =
  match d.kind with
  | DeclaratorFunction ctx ->
    Context.restore_context ctx;
    Context.declare_varname d.identifier
  | _ ->
    (* If we are here, then we have encountered a declarator that is
       not a function declarator yet is followed by (the first symbol
       of) [declaration_list? compound_statement]. Either this is a
       K&R function declarator (in which case we should do nothing)
       or this is an error. *)
    ()

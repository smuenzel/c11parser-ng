(* SPDX-License-Identifier: BSD-3-Clause *)
(*
Jacques-Henri Jourdan, Inria Paris
François Pottier, Inria Paris
*)

type t 

val create : unit -> t

(* This declares [id] as a typedef name. *)
val declare_typedefname: t -> string -> t

(* This declares [id] as a variable (hence un-declares it as a typedef name). *)
val declare_varname: t -> string -> t

(* This tests whether [id] is known as a typedef name. *)
val is_typedefname: t -> string -> bool

module type Packed = sig
  val declare_typedefname : string -> unit
  val declare_varname : string -> unit
  val is_typedefname : string -> bool

  type snapshot

  (* This takes a snapshot of the current context. *)
  val save_context: unit -> snapshot

  (* This re-installs a snapshot as the current context. *)
  val restore_context: snapshot -> unit
end

val create_packed : ?init:t -> unit -> (module Packed)

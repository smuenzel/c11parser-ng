(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2016-2017, Inria
 * SPDX-FileCopyrightText: (c) 2024-2025, Stefan Muenzel
 * SPDX-FileContributor: Jacques-Henri Jourdan, Inria Paris
 * SPDX-FileContributor: Francois Pottier, Inria Paris
 * SPDX-FileContributor: Stefan Muenzel
 *)

module StringSet = Set.Make(String)

type t =
  { typedefnames : StringSet.t
  }

let create () =
  { typedefnames = StringSet.empty }

(* This declares [id] as a typedef name. *)
let declare_typedefname t id =
  { typedefnames = StringSet.add id t.typedefnames }

(* This declares [id] as a variable (hence un-declares it as a typedef name). *)
let declare_varname t id =
  { typedefnames = StringSet.remove id t.typedefnames }

(* This tests whether [id] is known as a typedef name. *)
let is_typedefname t id =
  StringSet.mem id t.typedefnames 

module type Packed = sig
  val declare_typedefname : string -> unit
  val declare_varname : string -> unit
  val is_typedefname : string -> bool

  type snapshot

  val save_context : unit -> snapshot
  val restore_context : snapshot -> unit
end

let create_packed ?init () : (module Packed) =
  (module struct
    let t =
      ref (
        match init with
        | Some init -> init
        | None -> create ())
    let declare_typedefname id = t := declare_typedefname !t id
    let declare_varname id = t := declare_varname !t id
    let is_typedefname id = is_typedefname !t id

    type snapshot = t
    let save_context () = !t
    let restore_context snapshot = t := snapshot
  end)

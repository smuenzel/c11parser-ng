(* SPDX-License-Identifier: BSD-3-Clause
 * SPDX-FileCopyrightText: (c) 2024-2025 Stefan Muenzel
 *)

module List_eq1 = struct
  type ('a, 'b) t =
    | Eq of 'a * 'b list
    | Neq of 'b * ('a, 'b) t
end

module List_2 = struct
  type ('a, 'b) t =
    | Empty
    | A of 'a * ('a, 'b) t
    | B of 'b * ('a, 'b) t

end

module List_ge1 = struct
  type ('a, 'b) t =
    | Eq of 'a * 'b list
    | A of 'a * ('a, 'b) t
    | B of 'b * ('a, 'b) t
end

module List_eq1_eq1 = struct
  type ('a, 'b, 'c) t =
    | A of 'a * ('b, 'c) List_eq1.t
    | B of 'b * ('a, 'c) List_eq1.t
    | C of 'c * ('a, 'b, 'c) t
end

module List_eq1_ge1 =  struct
  type ('a, 'b, 'c) t =
    | A of 'a * ('b, 'c) List_ge1.t
    | B of 'b * ('a, 'c) List_eq1.t
    | B' of 'b * ('a, 'b, 'c) t
    | C of 'c * ('a, 'b, 'c) t
end

module Stored_reversed : sig
  type +'a t [@@deriving sexp]

  val empty : 'a list t

  val singleton : 'a -> 'a list t

  val snoc : 'a list t * 'a -> 'a list t
  val snoc_opt : 'a list t option * 'a -> 'a list t

  val to_list : 'a list t -> 'a list
  val of_list : 'a list -> 'a list t
end = struct

  type +'a t = 'a [@@deriving sexp]

  let empty = []

  let singleton x = [x]

  let snoc (l, x) = x :: l

  let snoc_opt (l,x) =
    match l with
    | None -> [x]
    | Some l -> x :: l

  let to_list l = List.rev l
  let of_list l = List.rev l

end

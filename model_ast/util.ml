open Core

module List_eq1 = struct
  type ('a, 'b) t = ('a ,'b) C11parser.Util.List_eq1.t =
    | Eq of 'a * 'b list
    | Neq of 'b * ('a, 'b) t
  [@@deriving sexp, bin_io, compare, hash]

  let [@ocaml.tail_modulo_cons] rec map_unbounded : 
    'a 'b 'c . f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t =
    fun ~f t ->
    match t with
    | Eq (a, b) -> Eq (a, List.map ~f b)
    | Neq (b, t') -> Neq (f b, map_unbounded ~f t')
end

module List_2 = struct
  type ('a, 'b) t = ('a, 'b) C11parser.Util.List_2.t =
    | Empty
    | A of 'a * ('a, 'b) t
    | B of 'b * ('a, 'b) t
  [@@deriving sexp, bin_io, compare, hash]
end

module List_ge1 = struct
  type ('a, 'b) t = ('a, 'b) C11parser.Util.List_ge1.t =
    | Eq of 'a * 'b list
    | A of 'a * ('a, 'b) t
    | B of 'b * ('a, 'b) t
  [@@deriving sexp, bin_io, compare, hash]

  let [@ocaml.tail_modulo_cons] rec map_unbounded :
    'a 'b 'c . f:('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t =
    fun ~f t ->
    match t with
    | Eq (a, b) -> Eq (a, List.map ~f b)
    | A (a, t') -> A (a, map_unbounded ~f t')
    | B (b, t') -> B (f b, map_unbounded ~f t')

end

module List_eq1_eq1 = struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) C11parser.Util.List_eq1_eq1.t =
    | A of 'a * ('b, 'c) List_eq1.t
    | B of 'b * ('a, 'c) List_eq1.t
    | C of 'c * ('a, 'b, 'c) t
  [@@deriving sexp, bin_io, compare, hash]
end

module List_eq1_ge1 =  struct
  type ('a, 'b, 'c) t = ('a, 'b, 'c) C11parser.Util.List_eq1_ge1.t =
    | A of 'a * ('b, 'c) List_ge1.t
    | B of 'b * ('a, 'c) List_eq1.t
    | B' of 'b * ('a, 'b, 'c) t
    | C of 'c * ('a, 'b, 'c) t
  [@@deriving sexp, bin_io, compare, hash]
end

module Stored_reversed = struct
  include C11parser.Util.Stored_reversed

  let opt_to_list = function
    | None -> []
    | Some l -> to_list l
end

module Either = struct
  let of_caml : _ Stdlib.Either.t -> _ Either.t = function
    | Stdlib.Either.Left x -> First x
    | Stdlib.Either.Right x -> Second x
end

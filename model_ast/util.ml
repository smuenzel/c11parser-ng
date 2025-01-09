open Core

module List_eq1 = struct
  type ('a, 'b) t = ('a ,'b) C11parser.Util.List_eq1.t =
    | Eq of 'a * 'b list
    | Neq of 'b * ('a, 'b) t
  [@@deriving sexp, bin_io, compare, hash]
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

module Stored_reversed = C11parser.Util.Stored_reversed

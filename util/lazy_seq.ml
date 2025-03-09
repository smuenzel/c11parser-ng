
type 'a node = ('a * 'a t) option
and 'a t = T of ('a node Lazy.t) [@@unboxed]


let of_lazy l = T l

let is_empty (T t : _ t) =
  match t with
  | lazy None -> true
  | _ -> false

let uncons (T t : _ t) =
  Lazy.force t


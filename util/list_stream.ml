
module Utf8(P : Position_intf.S) = struct
  let create (strings : (P.t * P.t * string) list) : (P.t * Uchar.t) Lazy_seq.t =
    let rec aux ((_, p1, s) as curr) i pcurr rest : (P.t * Uchar.t) Lazy_seq.node =
      if i = String.length s
      then begin
        match rest with
        | [] -> None
        | (p0, _,_) as hd :: tl -> aux hd 0 p0 tl
      end
      else begin
        let result = String.get_utf_8_uchar s i in
        if Uchar.utf_decode_is_valid result
        then begin
          let p = P.incr pcurr in
          let p = P.min p p1 in
          let rest =
            Lazy_seq.of_lazy (lazy (aux curr (i + 1) p rest))
          in
          Some ((p, Uchar.utf_decode_uchar result), rest)
        end
        else raise Sedlexing.MalFormed
      end
    in
    match strings with
    | [] -> Lazy_seq.of_lazy (lazy None)
    | (p0, _, _) as hd :: tl -> Lazy_seq.of_lazy (lazy (aux hd 0 p0 tl))
end

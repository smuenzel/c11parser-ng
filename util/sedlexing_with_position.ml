
module Make(Position : Position_intf.S) = struct
  type acc = (Position.t * Uchar.t)

  module Position = Position

  type state =
    { output_buffer : Buffer.t
    ; mutable buffer : acc Lazy_seq.t
    ; mutable lexeme_rev : acc list
    ; mutable lexeme_start : Position.t
    ; mutable lexeme_length : int
    ; mutable override_start_pos : Position.t option
    }

  type t =
    { mutable state : state
    ; mutable mark : (int * state) option
    }

  type lexbuf = t

  (* Public interface *)

  let create (s : acc Lazy_seq.t) =
    let state =
      { output_buffer = Buffer.create 32
      ; buffer = s
      ; lexeme_rev = []
      ; lexeme_start = Position.dummy
      ; lexeme_length = 0
      ; override_start_pos = None
      }
    in
    { state; mark = None }

  let start t =
    t.mark <- None;
    let state = t.state in
    Buffer.clear state.output_buffer;
    state.override_start_pos <- None;
    state.lexeme_rev <- [];
    state.lexeme_length <- 0;
    state.lexeme_start <-
      begin match Lazy_seq.uncons state.buffer with
        | None -> Position.dummy
        | Some ((p,_),_) -> p
      end

  let __private__next_int t =
    let state = t.state in
    match Lazy_seq.uncons state.buffer with
    | None -> -1
    | Some (((_, c) as v), rest) ->
      state.lexeme_rev <- v :: state.lexeme_rev;
      state.buffer <- rest;
      state.lexeme_length <- state.lexeme_length + 1;
      Uchar.to_int c

  let mark t i =
    let state_copy = { t.state with output_buffer = t.state.output_buffer } in
    t.mark <- Some (i, state_copy)

  let backtrack t = 
    match t.mark with
    | None -> -1
    | Some (i, state) ->
      t.state <- state;
      t.mark <- None;
      i

  let override_start_position t pos =
    t.state.override_start_pos <- Some pos

  let lexing_position_start t =
    let state = t.state in
    match state.override_start_pos with
    | None -> state.lexeme_start
    | Some pos -> pos

  let lexing_position_curr t =
    let state = t.state in
    match state.lexeme_rev with
    | [] -> state.lexeme_start
    | (p, _) :: _ -> Position.incr p

  let lexing_positions lexbuf =
    let start_p = lexing_position_start lexbuf
    and curr_p = lexing_position_curr lexbuf
    in
    (start_p, curr_p)

  let lexeme_char lexbuf i =
    let state = lexbuf.state in
    let i = state.lexeme_length - i - 1 in
    let acc = List.nth state.lexeme_rev i in
    snd acc

  module Utf8 = struct

    let lexeme t : string = 
      let state = t.state in
      Buffer.clear state.output_buffer;
      let buffer =
        List.fold_right
          (fun (_, c) buffer ->
             Buffer.add_utf_8_uchar buffer c;
             buffer
          )
          state.lexeme_rev
          state.output_buffer
      in
      Buffer.contents buffer

    let sub_lexeme t (pos : int) (length : int) =
      let state = t.state in
      Buffer.clear state.output_buffer;
      let last_index = pos + length - 1 in
      let _ : int =
        List.fold_right
          (fun (_, c) i ->
             if i >= pos && i <= last_index then
               Buffer.add_utf_8_uchar state.output_buffer c;
             i + 1
          )
          state.lexeme_rev
          0
      in
      Buffer.contents state.output_buffer
  end

  module Latin1 = struct
    let lexeme_char t i =
      Uchar.to_char (lexeme_char t i)
  end

end

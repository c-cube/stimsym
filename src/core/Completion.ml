
(* This file is free software. See file "license" for more details. *)

(** {1 Completion} *)

module E = Expr

type completion = {
  text: string; (* the completed text *)
  summary: string option; (* short description of this particular completion *)
}

type 'a within_ctx = {
  start: int; (* offset at which completion starts *)
  stop: int; (* offset at which completion ends *)
  l : 'a list;
}

(* find if there is a part of [str.[0 .. cursor_pos]]
    that looks like the beginning of an identifier.
   returns [start, chunk, stop] where
   [s[..start] ^ chunk ^ s[end..] = s] *)
let find_suffix_id ~cursor_pos (s:string): (int * string * int) option =
  let is_ok_char =
    function 'A'..'Z' | 'a'..'z' | '0'..'9' -> true | _ -> false
  in
  let len = min cursor_pos (String.length s) in
  let i = ref (len-1) in
  while !i >= 0 && is_ok_char s.[!i] do decr i done;
  incr i; (* revert last choice *)
  if len - !i >= 1
  then (
    let chunk = String.sub s !i (len - !i) in
    Some (!i, chunk, len)
  ) else None

(* completion based on builtins' names *)
let complete_builtin partial_id : E.Cst.t list =
  Builtins.complete_symbol partial_id
  |> CCList.filter_map
    (function
      | E.Const c -> Some c
      | _ -> None)

(* completion based on every symbol but builtins *)
let complete_all partial_id : E.Cst.t list =
  Expr.Cst.complete partial_id
  |> List.filter (fun c -> not (Builtins.const_is_builtin c))

let find_constants ?(exact=false) s ~cursor_pos : _ within_ctx =
  match find_suffix_id ~cursor_pos s with
    | None -> {start=0;stop=0;l=[]}
    | Some (start,partial_id,stop) ->
      let l =
        List.rev_append (complete_builtin partial_id) (complete_all partial_id)
      in
      let l =
        if exact then List.filter (fun e -> e.E.cst_name = partial_id) l
        else l
      in
      {start;stop;l}

let complete s ~cursor_pos : completion within_ctx =
  let wctx = find_constants s ~cursor_pos in
  let l =
    List.rev_map
      (fun c ->
         let text = c.E.cst_name in
         let summary = None in (* TODO? *)
         { text; summary })
    wctx.l
  in
  {wctx with l}

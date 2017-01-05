
(* This file is free software. See file "license" for more details. *)

(** {1 Completion} *)

module E = Expr

type completion = {
  text: string; (* the completed text *)
  summary: string option; (* short description of this particular completion *)
}

(* find if there is a part of [str.[0 .. cursor_pos]]
    that looks like the beginning of an identifier.
   returns [prefix, chunk, suffix] where
   [length (prefix ^ chunk) = cursor_pos]
   and [prefix ^ chunk ^ suffix = s] *)
let find_suffix_id ~cursor_pos (s:string): (string * string * string) option =
  let is_ok_char =
    function 'A'..'Z' | 'a'..'z' | '0'..'9' -> true | _ -> false
  in
  let len = min cursor_pos (String.length s) in
  let i = ref (len-1) in
  while !i >= 0 && is_ok_char s.[!i] do decr i done;
  incr i; (* revert last choice *)
  if len - !i >= 1
  then (
    let prefix = String.sub s 0 !i in
    let chunk = String.sub s !i (len - !i) in
    let suffix = String.sub s len (String.length s-len) in
    Some (prefix, chunk, suffix)
  ) else None

(* completion based on builtins' names *)
let complete_builtin ~(cursor_pos:int) (s:string): completion list =
  match find_suffix_id ~cursor_pos s with
    | None -> []
    | Some (prefix,partial_id,_) ->
      Builtins.complete_symbol partial_id
      |> CCList.filter_map
        (function
          | E.Const c ->
            let text = prefix ^ c.E.cst_name in
            let summary = None in (* TODO *)
            Some { text; summary }
          | _ -> None)

let complete s ~cursor_pos = complete_builtin ~cursor_pos s


(* This file is free software. See file "license" for more details. *)

(** {1 Completion} *)

module E = Expr

type completion = {
  text: string; (* the completed text *)
  summary: string option; (* short description of this particular completion *)
}

(* find if there is a suffix of [str] that looks like the beginning of
     an identifier *)
let find_suffix_id (s:string): (string * string) option =
  let is_ok_char =
    function 'A'..'Z' | 'a'..'z' | '0'..'9' -> true | _ -> false
  in
  let n = String.length s in
  let i = ref (n-1) in
  while !i >= 0 && is_ok_char s.[!i] do decr i done;
  incr i; (* revert last choice *)
  if n - !i >= 1
  then (
    let prefix = String.sub s 0 !i in
    let suffix = String.sub s !i (n - !i) in
    Some (prefix, suffix)
  ) else None

(* completion based on builtins' names *)
let complete_builtin (s:string): completion list =
  match find_suffix_id s with
    | None -> []
    | Some (prefix,partial_id) ->
      Builtins.complete_symbol partial_id
      |> CCList.filter_map
        (function
          | E.Const c ->
            let text = prefix ^ c.E.cst_name in
            let summary = None in (* TODO *)
            Some { text; summary }
          | _ -> None)

let complete s = complete_builtin s

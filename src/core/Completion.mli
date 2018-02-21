
(* This file is free software. See file "license" for more details. *)

(** {1 Completion} *)

type completion = {
  text: string; (* the completed text *)
  summary: string option; (* short description of this particular completion *)
}

type 'a within_ctx = {
  start: int; (* offset at which completion starts *)
  stop: int; (* offset at which completion ends *)
  l : 'a list;
}

(* find constants that correspond to this position *)
val find_constants : string -> cursor_pos:int -> Expr.Cst.t within_ctx

(* completions based on constants that can complete to this position *)
val complete : string -> cursor_pos:int -> completion within_ctx

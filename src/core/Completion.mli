
(* This file is free software. See file "license" for more details. *)

(** {1 Completion} *)

type completion = {
  text: string; (* the completed text *)
  summary: string option; (* short description of this particular completion *)
}

type completions = {
  start: int; (* offset at which completion starts *)
  stop: int; (* offset at which completion ends *)
  l : completion list
}

val complete : string -> cursor_pos:int -> completions


(* This file is free software. See file "license" for more details. *)

(** {1 Completion} *)

type completion = {
  text: string; (* the completed text *)
  summary: string option; (* short description of this particular completion *)
}

val complete : string -> completion list

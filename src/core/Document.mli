
(* This file is free software. See file "license" for more details. *)

(** {1 Simple Formatted Document} *)

type block =
  [ `S of string (* section *)
  | `P of string (* paragraph *)
  | `Pre of string (* formatted paragraph *)
  | `I of string * t (* indented block with header *)
  ]

and t = block list

val section : string -> block
val paragraph : string -> block
val paragraph_f : ('a, Format.formatter, unit, block) format4 -> 'a
val pre : string -> block
val pre_f : ('a, Format.formatter, unit, block) format4 -> 'a
val indent : string -> t -> block

val pp : t CCFormat.printer
(** Regular pretty printer *)

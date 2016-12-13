
(* This file is free software. See file "license" for more details. *)

(** {1 Patterns} *)

open Base_types

type t = Base_types.pattern

(** {2 IO} *)

val pp : t CCFormat.printer
val pp_rule : rewrite_rule CCFormat.printer

(** {2 Compilation} *)

exception Invalid_rule of string
exception Invalid_binding_seq of string

val matches_slice : t -> bool

val compile_rule : expr -> expr -> rewrite_rule

val sp_slice_min_size : slice_pattern -> int

val sup_slice_min_size : slice_unordered_pattern -> int

val compile_binding_seq :
  ret:[< `First | `Last ] ->
  expr array ->
  (binding_seq, string) Result.result


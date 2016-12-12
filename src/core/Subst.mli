
(* This file is free software. See file "license" for more details. *)

(** {1 Substitutions} *)

open Base_types

type t
val empty : t
val add : int -> expr -> t -> t
val mem : int -> t -> bool
val get : int -> t -> expr option
val get_exn : int -> t -> expr
val apply : t -> expr -> expr
val pp : t CCFormat.printer

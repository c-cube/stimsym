
(* This file is free software. See file "license" for more details. *)

(** {1 Builtin Functions} *)

type t = Expr.t
(** A builtin is just a constant expression *)

val hold : t
val plus : t
val full_form : t
val list : t

val all_builtins : t list

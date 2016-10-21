
(* This file is free software. See file "license" for more details. *)

(** {1 Builtin Functions} *)

type t = Expr.t
(** A builtin is just a constant expression *)

val hold : t
val plus : t
val times : t
val full_form : t
val list : t
val true_ : t
val false_ : t
val if_ : t
val blank : t
val blank_seq : t
val blank_null_seq : t
val pattern : t
val same_q : t

val all_builtins : unit -> t list

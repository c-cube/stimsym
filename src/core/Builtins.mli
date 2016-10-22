
(* This file is free software. See file "license" for more details. *)

(** {1 Builtin Functions} *)

type t = Expr.t
(** A builtin is just a constant expression *)

val hold : t
val full_form : t
val list : t
val blank : t
val blank_seq : t
val blank_null_seq : t
val pattern : t
val same_q : t
val set : t
val set_delayed : t
val rule : t
val rule_delayed : t
val replace : t
val replace_repeated : t
val alternatives : t

val true_ : t
val false_ : t
val if_ : t
val and_ : t
val or_ : t
val not_ : t

val plus : t
val times : t
val factorial : t

val equal : t
val less : t
val greater : t
val less_equal : t
val greater_equal : t
val inequality : t

val all_builtins : unit -> t list

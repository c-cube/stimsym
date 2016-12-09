
(* This file is free software. See file "license" for more details. *)

(** {1 Builtin Functions} *)

type t = Expr.t
(** A builtin is just a constant expression *)

exception Eval_does_not_apply
(** Raised internally when an evaluation function does not apply to
    the arguments *)

(** a function definition. Takes [self_cst, eval_fun, t] and evaluates [t]
    into [None] (fail) or [Some t'] *)
type fun_def = Expr.const -> Expr.prim_fun_args -> Expr.t -> Expr.t option

val make :
  ?doc:Document.t ->
  ?printer:int * Expr.const_printer ->
  ?fields:Expr.Properties.field list ->
  ?funs:fun_def list ->
  string -> t
(** [make s] makes a new constant and sets some options/handlers on it *)

val hold : t
val full_form : t
val blank : t
val blank_seq : t
val blank_null_seq : t
val sequence : t
val pattern : t
val same_q : t
val set : t
val set_delayed : t
val rule : t
val rule_delayed : t
val condition : t
val replace_all : t
val replace_repeated : t
val alternatives : t
val compound_expr : t

val slot : t
val function_ : t

val true_ : t
val false_ : t
val if_ : t
val and_ : t
val or_ : t
val not_ : t

val plus : t
val times : t
val factorial : t
val list : t

val equal : t
val less : t
val greater : t
val less_equal : t
val greater_equal : t
val inequality : t

val print : t
val null : t
val doc : t

val nest : t

val all_builtins : unit -> t list

(**/**)
val log_: (string->unit) ref
val log : string -> unit
val logf : ('a, unit, string, unit) format4 -> 'a
(**/**)

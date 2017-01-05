
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
  ?display:Expr.mime_printer ->
  ?fields:Expr.Properties.field list ->
  ?funs:fun_def list ->
  ?rules:(Expr.const -> t * t) list ->
  string -> t
(** [make s] makes a new constant and sets some options/handlers on it *)

val hold : t
val full_form : t
val blank : t
val blank_seq : t
val blank_null_seq : t
val sequence : t
val pattern : t
val pattern_test : t
val same_q : t
val assign : t
val assign_delayed : t
val rule : t
val rule_delayed : t
val condition : t
val replace_all : t
val replace_repeated : t
val alternatives : t
val compound_expr : t
val head : t
val length : t (* number of immediate arguments *)

val slot : t
val function_ : t

val true_ : t
val false_ : t
val if_ : t
val match_ : t
val match_l : t
val matches : t

val and_ : t
val or_ : t
val not_ : t

val plus : t
val times : t
val div : t
val mod_ : t
val factorial : t
val power : t

val list : t
val set : t

val union : t
val inter : t

val random : t
val floor : t
val ceil : t

val match_bind : t
val match_bind1 : t
val comprehension : t
val let_ : t
val fixpoint : t

val set_attributes : t
val remove_attributes : t
val get_attributes : t
val clear_attributes : t

val equal : t
val not_equal : t
val less : t
val greater : t
val less_equal : t
val greater_equal : t
val inequality : t

val integer_q : t
val rational_q : t
val true_q : t

val trace : t
val print : t
val null : t
val doc : t

val nest : t
val range : t

val all_builtins : unit -> t list

val complete_symbol : string -> t list
(** Completion of the given identifier prefix using builtins *)

(**/**)
val log_: (string->unit) ref
val log : string -> unit
val logf : ('a, unit, string, unit) format4 -> 'a
(**/**)

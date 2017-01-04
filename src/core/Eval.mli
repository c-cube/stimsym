
(* This file is free software. See file "license" for more details. *)

(** {1 Evaluation} *)

open Base_types

val def_rule : lhs:expr -> rhs:expr -> (def,string) Result.result
(** [def_rule lhs rhs] makes a proper rewrite rule *)

val def_fun : prim_fun -> def
(** Make a definition from a primitive function *)

exception Eval_fail of string

type eval_side_effect = Base_types.eval_side_effect =
  | Print_doc of Document.t
  | Print_mime of mime_content

val eval : expr -> expr

val eval_full : expr -> expr * eval_side_effect list
(** @returns the normal form, and messages printed in the mean time *)

(**/**)
val set_eval_debug: bool -> unit

val prim_eval : prim_fun_args -> expr -> expr
(** Evaluation function to be called by primitives *)

val prim_with_trace : prim_fun_args -> trace_fun -> (unit -> 'a) -> 'a
(** Set [trace] functions during call to function, then restore old one *)

val prim_fail : prim_fun_args -> string -> 'a
(** To be called by primitives on failure *)

val prim_match_ : prim_fun_args -> Subst.t -> Pattern.t -> expr -> Subst.t Sequence.t

val prim_term_as_rule : prim_fun_args -> expr -> Expr.rewrite_rule

val prim_term_as_rules : prim_fun_args -> expr -> Expr.rewrite_rule list

val prim_failf : prim_fun_args -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val prim_write_doc : prim_fun_args -> Document.t lazy_t -> unit

val prim_print : prim_fun_args -> string -> unit

val prim_printf : prim_fun_args -> ('a, Format.formatter, unit, unit) format4 -> 'a

val prim_write_mime : prim_fun_args -> mime_content lazy_t -> unit

(**/**)


(* This file is free software. See file "license" for more details. *)

(** {1 Expressions}

    The main data structure *)

module Properties : Bit_set.S

val field_protected : Properties.field
(** Cannot modify the evaluation function of this constant *)

val field_hold_all : Properties.field
val field_hold_first : Properties.field
val field_hold_rest : Properties.field
val field_orderless : Properties.field
val field_flatten : Properties.field
val field_one_identity : Properties.field

type def_style =
  | Def_eager
  | Def_lazy

(* (partial) definition of a symbol *)
type def

type rewrite_rule

type const = private {
  cst_name: string;
  cst_id: int;
  mutable cst_properties: Properties.t;
  mutable cst_rules: def list;
  mutable cst_local_rules: rewrite_rule list;
  mutable cst_doc: Document.t;
  mutable cst_printer: (int * const_printer) option;
}

and t = private
  | Const of const
  | App of t * t array
  | Z of Z.t
  | Q of Q.t
  | String of string
  | Reg of int

(* custom printer for a constant *)
and const_printer = const -> (int -> t CCFormat.printer) -> t array CCFormat.printer

exception Print_default
(** Used in {!const_printer} to indicate that default printing should be preferred *)

type prim_fun_args

type pattern

type eval_state
(** Evaluation state *)

type prim_fun = prim_fun_args -> t -> t option
(** takes a context for loading variables, a term [t], return [Some t']
    if it reduces [t] to [t'] *)

(** {2 Basics} *)

val const : const -> t

val const_of_string : string -> t

val const_of_string_with : f:(const -> unit) -> string -> t

val z : Z.t -> t

val q : Q.t -> t

val string : string -> t

val of_int : int -> t

val of_int_ratio : int -> int -> t

val of_float : float -> t

val app : t -> t array -> t

val app_flatten : t -> t array -> t
(** Same as {!app}, but "inlines" arguments of the shape [Flatten[x1...xn]]
    into the list of arguments *)

val app_l : t -> t list -> t

val equal : t -> t -> bool
(** Syntactic deep equality ("SameQ") *)

val hash : t -> int
(** Hash *)

val def_rule : lhs:t -> rhs:t -> (def,string) Result.result
(** [def_rule lhs rhs] makes a proper rewrite rule *)

val def_fun : prim_fun -> def
(** Make a definition from a primitive function *)

module Tbl : CCHashtbl.S with type key = t

(** {2 Constants} *)

exception No_head

val head : t -> const
(** Head constant.
    @raise No_head if the head is a primitive (int,rat,string) *)

module Cst : sig
  type t = const

  val equal : t -> t -> bool

  val set_field : Properties.field -> bool -> t -> unit

  val get_field : Properties.field -> t -> bool

  val add_def : def -> t -> unit

  val set_printer : int -> const_printer -> t -> unit

  val set_doc : Document.t -> t -> unit
end

(** {2 IO} *)

val pp_full_form : t CCFormat.printer
(** Printer without any fancy display, just serialize the raw structure *)

val to_string_compact : t -> string
(** Compact, easy to parser display using FullForm *)

val pp : t CCFormat.printer
(** Nice printer *)

val to_string : t -> string
(** Nice multi-line printer using {!pp} *)

(** {2 Evaluation} *)

exception Eval_fail of string

val eval : t -> t

val eval_full : t -> t * Document.t list
(** @returns the normal form, and messages printed in the mean time *)

(**/**)
val set_eval_trace: bool -> unit

val prim_eval : prim_fun_args -> t -> t
(** Evaluation function to be called by primitives *)

val prim_fail : prim_fun_args -> string -> 'a
(** To be called by primitives on failure *)

val prim_failf : prim_fun_args -> ('a, Format.formatter, unit, 'b) format4 -> 'a

val prim_write_doc : prim_fun_args -> Document.t lazy_t -> unit

val prim_print : prim_fun_args -> string -> unit

val prim_printf : prim_fun_args -> ('a, Format.formatter, unit, unit) format4 -> 'a

(**/**)

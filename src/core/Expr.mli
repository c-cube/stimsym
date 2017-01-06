
(* This file is free software. See file "license" for more details. *)

(** {1 Expressions}

    The main data structure *)

module Properties = Base_types.Properties

val field_protected : Properties.field
(** Cannot modify the evaluation function of this constant *)

val field_hold_all : Properties.field
val field_hold_first : Properties.field
val field_hold_rest : Properties.field
val field_orderless : Properties.field
val field_no_flatten : Properties.field (* no flatten of arguments, even sequence *)
val field_flatten : Properties.field
val field_one_identity : Properties.field
val field_listable : Properties.field (* thread through lists *)
val field_no_duplicates : Properties.field (* no duplicate arguments *)

(* (partial) definition of a symbol *)
type def = Base_types.def

type rewrite_rule = Base_types.rewrite_rule

type const = Base_types.const = {
  cst_name: string;
  cst_id: int;
  mutable cst_properties: Properties.t;
  mutable cst_rules: def list;
  mutable cst_doc: Document.t;
  mutable cst_printer: (int * const_printer) option;
  mutable cst_display : mime_printer option;
}

and t = Base_types.expr =
  | Const of const
  | App of t * t array
  | Z of Z.t
  | Q of Q.t
  | String of string
  | Reg of int

(* custom printer for a constant *)
and const_printer =
  Base_types.const -> (int -> t CCFormat.printer) -> t array CCFormat.printer

and mime_content = Base_types.mime_content = {
  mime_ty: string;
  mime_data: string;
  mime_base64: bool;
}

(* custom display for expressions *)
and mime_printer = t -> mime_content list

exception Print_default
(** Used in {!const_printer} to indicate that default printing should be preferred *)

type prim_fun_args = Base_types.prim_fun_args

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

val compare : t -> t -> int
(** Syntactic ordering, compatible with {!equal} *)

val hash : t -> int
(** Hash *)

module Tbl : CCHashtbl.S with type key = t
module Map : CCMap.S with type key = t
module Set : CCSet.S with type elt = t

(** {2 Constants} *)

exception No_head

val head : t -> const
(** Head constant.
    @raise No_head if the head is a primitive (int,rat,string) *)

module Cst : sig
  type t = const

  val equal : t -> t -> bool

  val hash : t -> int

  val set_field : Properties.field -> bool -> t -> unit

  val get_field : Properties.field -> t -> bool

  val add_def : def -> t -> unit

  val set_printer : int -> const_printer -> t -> unit

  val set_display : mime_printer -> t -> unit

  val set_doc : Document.t -> t -> unit
end

val null : t

(** {2 IO} *)

val pp_full_form : t CCFormat.printer
(** Printer without any fancy display, just serialize the raw structure *)

val to_string_compact : t -> string
(** Compact, easy to parser display using FullForm *)

val pp : t CCFormat.printer
(** Nice printer *)

val to_string : t -> string
(** Nice multi-line printer using {!pp} *)

exception Parse_error of string

val of_string_full_form : string -> (t, string) Result.result
(** parse the given string into a term, in the fullform syntax,
    or return [Error msg] *)

val of_string_full_form_exn : string -> t
(** Unsafe version of {!of_string_full_form}.
    @raise Parse_error if it cannot parse *)

(**/**)
val reg : int -> t
(**/**)

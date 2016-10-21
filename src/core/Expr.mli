
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

type const = private {
  name: string;
  id: int; (* unique ID *)
  mutable properties: Properties.t;
  mutable defs: def list;
}

and t = private
  | Const of const
  | App of t * t array
  | Z of Z.t
  | Q of Q.t
  | String of string

(* (partial) definition of a symbol *)
and def =
  | Rewrite of pattern * t
  | Fun of ((t -> t) -> t -> t option)
  (* takes an evaluation function, a term [t], return [Some t']
     if it reduces [t] to [t'] *)

(* TODO: matching *)
and pattern = t

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

val app_l : t -> t list -> t

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
end

val pp_full_form : t CCFormat.printer
(** Printer without any fancy display, just serialize the raw structure *)


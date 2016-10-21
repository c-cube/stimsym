
(* This file is free software. See file "license" for more details. *)

(** {1 Expressions}

    The main data structure *)

type property =
  | HoldAll
  | HoldFirst
  | HoldRest
  | Orderless
  | Flatten
  | SequenceHold
  | OneIdentity

type const = private {
  name: string;
  mutable properties: property list;
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
  | Fun of (t -> t option)

(* TODO: matching *)
and pattern = t

val const : string -> t

val const_with : properties:property list -> defs:def list -> string -> t

val z : Z.t -> t

val q : Q.t -> t

val string : string -> t

val of_int : int -> t

val of_int_ratio : int -> int -> t

val of_float : float -> t

val app : t -> t array -> t

val app_l : t -> t list -> t

val set_properties : t -> property list -> unit
(** [set_properties t p] sets the properties of [t] to [p].
    @raise Invalid_argument if [t] is not a constant *)

val pp_full_form : t CCFormat.printer
(** Printer without any fancy display, just serialize the raw structure *)


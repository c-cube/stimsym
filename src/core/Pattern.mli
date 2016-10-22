
(* This file is free software. See file "license" for more details. *)

(** {1 Pattern}

    A pattern is the left-hand side of a rewrite rule/definition. It is
    optimized for matching. *)

module IntMap : CCMap.S with type key = int

module Register : sig
  type t = private int

end

type t = private
  | Bind of Register.t  (* bind register *)
  | Check_same of Register.t (* check that sub-term is same as this reg *)

type registers = Expr.t IntMap.t

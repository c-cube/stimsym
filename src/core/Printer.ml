
(* This file is free software. See file "license" for more details. *)

(** {1 Main Printer} *)

(* TODO: traverse expr, flattening terms, with special syntax, etc. *)
let pp = Expr.pp_full_form

(* TODO: if we traverse [FullForm[...]] we switch to full form printing *)


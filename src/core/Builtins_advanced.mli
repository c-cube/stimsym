
(* This file is free software. See file "license" for more details. *)

(** {1 More Advanced Builtins} *)

type t = Builtins.t

val sat_solve : t
(** Call a sat solver on the given arguments *)

val graph : t
(** Graph structure and display *)

val tree_form : t
(** Display the structure of a term *)

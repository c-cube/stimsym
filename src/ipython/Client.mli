
(* This file is free software. See file "license" for more details. *)

(** {1 Main Kernel Loop} *)

module Kernel : sig
  type status_cell =
    | Html of string
    | Mime of string * string (* type, data *)

  val html : string -> status_cell
  val mime : ty:string -> string -> status_cell

  type exec_status = (status_cell list, string) Result.result
  (** error, or list of things to display *)

  type t = {
    exec: count:int -> string -> exec_status Lwt.t;
  }
end

type t

val make : ?key:string -> Sockets.t -> Kernel.t -> t

val run : t -> unit Lwt.t

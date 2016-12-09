
(* This file is free software. See file "license" for more details. *)

(** {1 Main Kernel Loop} *)

module Kernel : sig
  type exec_action =
    | Doc of Document.t
    | Mime of string * string * bool (* type, data, base64? *)

  type exec_status_ok = {
    msg: string option;
    (* main message *)
    actions: exec_action list;
    (* other actions *)
  }

  type exec_status = (exec_status_ok, string) Result.result
  (** error, or list of things to display *)

  val doc : Document.t -> exec_action
  val mime : ?base64:bool -> ty:string -> string -> exec_action

  val ok : ?actions:exec_action list -> string option -> exec_status_ok

  type t = {
    exec: count:int -> string -> exec_status Lwt.t;
  }
end

type t

val make : ?key:string -> Sockets.t -> Kernel.t -> t

val run : t -> unit Lwt.t

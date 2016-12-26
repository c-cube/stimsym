
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

  type completion_status = {
    completion_matches: string list;
    completion_start: int;
    completion_end: int;
  }

  type is_complete_reply =
    | Is_complete
    | Is_not_complete of string (* indent *)

  val doc : Document.t -> exec_action
  val mime : ?base64:bool -> ty:string -> string -> exec_action

  val ok : ?actions:exec_action list -> string option -> exec_status_ok

  type t = {
    exec: count:int -> string -> exec_status Lwt.t;
    is_complete: string -> is_complete_reply;
    complete: pos:int -> string -> completion_status Lwt.t;
  }
end

type t

val make : ?key:string -> Sockets.t -> Kernel.t -> t

type run_result =
  | Run_stop
  | Run_restart

val run : t -> run_result Lwt.t

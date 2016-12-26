(*
 * iocaml - an OCaml kernel for IPython
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: handle zmq sockets
 *
 *)

(** {1 Sockets to Jupyter}

    See https://jupyter-client.readthedocs.io/en/latest/messaging.html *)

type t = {
  shell : [`Router] Lwt_zmq.Socket.t;
  control : [`Router] Lwt_zmq.Socket.t;
  stdin : [`Router] Lwt_zmq.Socket.t;
  iopub : [`Pub] Lwt_zmq.Socket.t;
  heartbeat: [`Rep ] Lwt_zmq.Socket.t;
}

val open_sockets : Ipython_json_t.connection_info -> t

val heartbeat : t -> unit Lwt.t

val dump : string -> [`Router] Lwt_zmq.Socket.t -> unit Lwt.t


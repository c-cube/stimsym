(*
 * iocaml - an OCaml kernel for IPython
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: handle zmq sockets
 *
 *)

type t = {
  shell : [`Router] Lwt_zmq.Socket.t;
  control : [`Router] Lwt_zmq.Socket.t;
  stdin : [`Router] Lwt_zmq.Socket.t;
  iopub : [`Pub] Lwt_zmq.Socket.t;
}

val heartbeat : Ipython_json_t.connection_info -> unit Lwt.t

val open_sockets : Ipython_json_t.connection_info -> t

val dump : string -> [`Router] Lwt_zmq.Socket.t -> unit Lwt.t


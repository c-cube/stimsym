(*
 * iocaml - an OCaml kernel for IPython
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: handle zmq sockets
 *
 *)

let context = ZMQ.Context.create ()
let () = at_exit
    (fun () -> ZMQ.Context.terminate context)

let version =
  let a, b, _ = ZMQ.version () in
  [a;b]

let addr conn port =
  Ipython_json_j.(conn.transport ^ "://" ^ conn.ip ^ ":" ^ string_of_int port)

let open_socket typ conn port =
  let socket = ZMQ.Socket.create context typ in
  let addr = addr conn port in
  let () = ZMQ.Socket.bind socket addr in
  Log.log ("open and bind socket " ^ addr ^ "\n");
  Lwt_zmq.Socket.of_socket socket

let heartbeat conn =
  let socket = open_socket ZMQ.Socket.rep conn conn.Ipython_json_j.hb_port in
  let%lwt () = 
    while%lwt true do
      let%lwt data = Lwt_zmq.Socket.recv socket in
      Log.log("Heartbeat\n");
      Lwt_zmq.Socket.send socket data
    done
  in
  (* XXX close down properly...we never get here *)
  ZMQ.Socket.close (Lwt_zmq.Socket.to_socket socket);
  Lwt.return ()

type sockets = {
  shell : [`Router] Lwt_zmq.Socket.t;
  control : [`Router] Lwt_zmq.Socket.t;
  stdin : [`Router] Lwt_zmq.Socket.t;
  iopub : [`Pub] Lwt_zmq.Socket.t;
}

let open_sockets conn =
  { shell = open_socket ZMQ.Socket.router conn conn.Ipython_json_j.shell_port;
    control = open_socket ZMQ.Socket.router conn conn.Ipython_json_j.control_port;
    stdin = open_socket ZMQ.Socket.router conn conn.Ipython_json_j.stdin_port;
    iopub = open_socket ZMQ.Socket.pub conn conn.Ipython_json_j.iopub_port;
  }

let dump _name socket =
  while%lwt true do
    let%lwt msg = Message.recv socket in
    let () = Message.log msg in
    Lwt.return ()
  done

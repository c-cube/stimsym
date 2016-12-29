(*
 * iocaml - an OCaml kernel for IPython
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: IPython messages
 *
 *)

open Ipython_json_t

type content =
  (* messages received from front end *)
  | Connect_request
  | Kernel_info_request
  | Shutdown_request of shutdown
  | Execute_request of execute_request
  | Object_info_request of object_info_request
  | Complete_request of complete_request
  | Is_complete_request of is_complete_request
  | History_request of history_request
  (* messages sent to front end *)
  | Connect_reply of connect_reply
  | Kernel_info_reply of kernel_info_reply
  | Shutdown_reply of shutdown
  | Execute_reply of execute_reply
  | Object_info_reply of object_info_reply
  | Complete_reply of complete_reply
  | Is_complete_reply of is_complete_reply
  | History_reply of history_reply
  (* other *)
  | Status of status
  | Execute_input of pyin
  | Execute_result of pyout
  | Execute_error of execute_error
  | Stream of stream
  | Clear of clear_output
  | Display_data of display_data
  (* custom messages *)
  | Comm_open

val content_of_json : header_info -> string -> content
val json_of_content : content -> string
val msg_type_of_content : content -> string

type t = private {
  ids : string array;
  hmac : string;
  header : header_info;
  parent : header_info;
  meta : string; (* XXX dict => assoc list I think *)
  content : content;
  raw : string array;
}

val log : string -> t -> unit

val recv : [`Router] Lwt_zmq.Socket.t -> t Lwt.t

val make : parent:t -> msg_type:string -> content -> t
(** make a message with the given type and content, copying header
    from [parent] *)

val make_first : msg_type:string -> content -> t
(** make a message with the given type and content, with a fresh header *)

val send : ?key:string -> [<`Router|`Pub] Lwt_zmq.Socket.t -> t -> unit Lwt.t
(** [send sock msg] sends the message.
    @param key the shared key for signing messages *)


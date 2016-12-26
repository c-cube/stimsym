(*
 * iocaml - an OCaml kernel for IPython
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: IPython messages
 *
 *)

open Ipython_json_j

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
  | Stream of stream
  | Clear of clear_output
  | Display_data of display_data
  (* custom messages *)
  | Comm_open

let content_of_json hdr c =
  match hdr.msg_type with
    | "connect_request" -> Connect_request
    | "kernel_info_request" -> Kernel_info_request
    | "shutdown_request" -> Shutdown_request(shutdown_of_string c)
    | "execute_request" -> Execute_request(execute_request_of_string c)
    | "object_info_request" -> Object_info_request(object_info_request_of_string c)
    | "complete_request" -> Complete_request(complete_request_of_string c)
    | "history_request" -> History_request(history_request_of_string c)

    | "connect_reply" -> Connect_reply(connect_reply_of_string c)
    | "kernel_info_reply" -> Kernel_info_reply(kernel_info_reply_of_string c)
    | "shutdown_reply" -> Shutdown_reply(shutdown_of_string c)
    | "execute_reply" -> Execute_reply(execute_reply_of_string c)
    | "object_info_reply" -> Object_info_reply(object_info_reply_of_string c)
    | "complete_reply" -> Complete_reply(complete_reply_of_string c)
    | "history_reply" -> History_reply(history_reply_of_string c)

    | "status" -> Status(status_of_string c)
    | "execute_input" -> Execute_input (pyin_of_string c)
    | "stream" -> Stream(stream_of_string c)
    | "display_data" -> Display_data(display_data_of_string c)
    | "clear_output" -> Clear(clear_output_of_string c)

    | "comm_open" -> Comm_open

    | _ -> failwith ("content_of_json: " ^ hdr.msg_type)

let json_of_content = function
  | Connect_request -> "{}"
  | Kernel_info_request -> "{}"
  | Shutdown_request(x) -> string_of_shutdown x
  | Execute_request(x) -> string_of_execute_request x
  | Object_info_request(x) -> string_of_object_info_request x
  | Complete_request(x) -> string_of_complete_request x
  | Is_complete_request x -> string_of_is_complete_request x
  | History_request(x) -> string_of_history_request x

  | Connect_reply(x) -> string_of_connect_reply x
  | Kernel_info_reply(x) -> string_of_kernel_info_reply x
  | Shutdown_reply(x) -> string_of_shutdown x
  | Execute_reply(x) -> string_of_execute_reply x
  | Object_info_reply(x) -> string_of_object_info_reply x
  | Complete_reply(x) -> string_of_complete_reply x
  | Is_complete_reply(x) -> string_of_is_complete_reply x
  | History_reply(x) -> string_of_history_reply x

  | Status(x) -> string_of_status x
  | Execute_input(x) -> string_of_pyin x
  | Execute_result (x) -> string_of_pyout x
  | Stream(x) -> string_of_stream x
  | Clear(x) -> string_of_clear_output x
  | Display_data(x) -> string_of_display_data x

  | Comm_open -> "{}"

let msg_type_of_content = function
  | Connect_request -> "connect_request"
  | Kernel_info_request -> "kernel_info_request"
  | Shutdown_request(_) -> "shutdown_request"
  | Execute_request(_) -> "execute_request"
  | Object_info_request(_) -> "object_info_request"
  | Complete_request(_) -> "complete_request"
  | Is_complete_request _ -> "is_complete_request"
  | History_request(_) -> "history_request"

  | Connect_reply(_) -> "connect_reply"
  | Kernel_info_reply(_) -> "kernel_info_reply"
  | Shutdown_reply(_) -> "shutdown_reply"
  | Execute_reply(_) -> "execute_reply"
  | Object_info_reply(_) -> "object_info_reply"
  | Complete_reply(_) -> "complete_reply"
  | Is_complete_reply _ -> "is_complete_reply"
  | History_reply(_) -> "history_reply"

  | Status(_) -> "status"
  | Execute_input (_) -> "execute_input"
  | Execute_result(_) -> "execute_result"
  | Stream(_) -> "stream"
  | Clear(_) -> "clear_output"
  | Display_data(_) -> "display_data"

  | Comm_open -> "comm_open"

(* information used to generate a message *)
type context = {
  ctx_key: string;
  ctx_session: string; (* unique session *)
  ctx_user: string;
}

type t = {
  ids : string array;
  hmac : string;
  header : header_info;
  parent : header_info;
  meta : string; (* XXX dict => assoc list I think *)
  content : content;
  raw : string array;
}

let log prefix msg =
  let open Printf in
  Log.logf "message %s:\n" prefix;
  Array.iter (fun id -> Log.log(id ^ "\n")) msg.ids;
  Log.log ("<IDS|MSG>\n");
  Log.log (sprintf "  HMAC: %s\n" msg.hmac);
  Log.log (sprintf "  header: %s\n" (string_of_header_info msg.header));
  Log.log (sprintf "  parent: %s\n" (string_of_header_info msg.parent));
  Log.log (sprintf "  content: %s\n" (json_of_content msg.content))

(*
let enc_utf8 = Netconversion.convert ~in_enc:`Enc_iso88591 ~out_enc:`Enc_utf8
let dec_utf8 = Netconversion.convert ~in_enc:`Enc_utf8 ~out_enc:`Enc_iso88591
*)
let enc_utf8 x = x
let dec_utf8 x = x

let recv socket : t Lwt.t =
  let%lwt msg = Lwt_zmq.Socket.recv_all socket in
  (*let () =
      Log.log (Printf.sprintf "recv: %i frame(s)\n" (List.length msg));
      List.iter (fun s -> Log.log (s ^ "\n")) msg
    in*)
  let msg = List.map dec_utf8 msg in
  let rec split ids = function
    | [] -> failwith "couldn't find <IDS|MSG> marker"
    | "<IDS|MSG>" :: t -> Array.of_list (List.rev ids), Array.of_list t
    | h :: t -> split (h::ids) t
  in
  let ids, data = split [] msg in
  let len = Array.length data in
  let header = header_info_of_string data.(1) in
  assert (len >= 5);
  (*let () = Log.log ("RECV:\n" ^ data.(4) ^ "\n") in*)
  let msg = {
    ids = ids;
    hmac = data.(0);
    header = header;
    parent = header_info_of_string data.(2);
    meta = data.(3);
    content = content_of_json header data.(4);
    raw = Array.init (len-5) (fun i -> data.(i+5))
  } in
  log "RECV" msg;
  Lwt.return msg

let send ?key socket msg : unit Lwt.t =
  let content = enc_utf8 (json_of_content msg.content) in
  let header  = enc_utf8 (string_of_header_info msg.header) in
  let parent = enc_utf8 (string_of_header_info msg.parent) in
  let meta = enc_utf8 (msg.meta) in
  let hmac = match key with
    | None -> msg.hmac
    | Some k ->
      let c =
        Cstruct.concat
          (List.map (fun s->Cstruct.of_string s)
             [header; parent; meta; content])
      in
      let res = Nocrypto.Hash.mac ~key:(Cstruct.of_string k) `SHA256 c in
      let `Hex s = Hex.of_cstruct res in
      s
  in
  log "SEND" {msg with hmac};
  Lwt_zmq.Socket.send_all socket (List.concat [
      Array.to_list (Array.map enc_utf8 msg.ids);
      [enc_utf8 "<IDS|MSG>"];
      [enc_utf8 hmac];
      [header];
      [parent];
      [meta];
      [content];
      Array.to_list (Array.map enc_utf8 msg.raw);
    ])

let mk_id () = Uuidm.(to_string (create `V4))

let make ~parent ~msg_type content = {
  parent with
    content;
    ids=[| msg_type |];
    header={
      parent.header with
            version = "5.0";
            msg_type;
            msg_id = mk_id();
    };
    parent = parent.header;
}

let empty_header : header_info = {
  version = ""; date=""; username=""; msg_type=""; msg_id = ""; session="";
}

let mk_header msg_type : header_info = {
  empty_header with
  version = "5.0";
  msg_type;
  msg_id = mk_id();
}

let make_first ~msg_type content = {
  parent=empty_header;
  header=mk_header msg_type;
  content;
  ids=[| msg_type |];
  hmac="";
  meta="{}";
  raw=[||];
}



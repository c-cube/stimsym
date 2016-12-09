
(* This file is free software. See file "license" for more details. *)

(** {1 Main Kernel Loop} *)

open Lwt.Infix
open Ipython_json_j

module M = Message

module Kernel = struct
  type status_cell =
    | Html of string
    | Mime of string * string (* type, data *)

  let html x = Html x
  let mime ~ty x = Mime (ty,x)

  type exec_status = (status_cell list, string) Result.result
  (** error, or list of things to display *)

  type t = {
    exec: count:int -> string -> exec_status Lwt.t;
  }
end

type t = {
  sockets: Sockets.t;
  key: string option;
  kernel: Kernel.t;
  mutable e_count: int;
}

let make ?key sockets kernel : t =
  { key; sockets; kernel;
    e_count=0; }

type iopub_message =
  | Iopub_set_current of Message.t
  | Iopub_send_message of Message.content
  | Iopub_send_raw_message of Message.t
  | Iopub_flush
  | Iopub_send_mime of
      Message.t option
      * string (* type *)
      * string (* content *)
      * bool (* base64? *)
  | Iopub_get_current
  | Iopub_stop

type iopub_resp =
  | Iopub_ok
  | Iopub_context of Message.t option

let string_of_iopub_message = function
  | Iopub_set_current m ->
    Printf.sprintf "set_current %s" (M.json_of_content m.M.content)
  | Iopub_send_message m ->
    Printf.sprintf "send_message %s" (M.json_of_content m)
  | Iopub_send_raw_message m ->
    Printf.sprintf "send_raw_message %s" (M.json_of_content m.M.content)
  | Iopub_flush -> "flush"
  | Iopub_stop -> "stop"
  | Iopub_get_current -> "get_current"
  | Iopub_send_mime (_,ty,cont,b64) ->
    Printf.sprintf "send_mime %s %s (base64: %B)" ty cont b64

(* encode mime data, wrap it into a message *)
let mime_message_content mime_type base64 data : M.content =
  let data =
    if not base64 then data
    else Base64.encode data
  in
  (Message.Display_data (Ipython_json_j.({
       dd_source = "stimsym";
       dd_data = `Assoc [mime_type,`String data];
       dd_metadata = `Assoc [];
     })))

let msg : M.t option ref = ref None

(* send a message on the Iopub socket *)
let send_iopub (t:t) (m:iopub_message): iopub_resp Lwt.t =
  Log.logf "send_iopub `%s`\n" (string_of_iopub_message m);
  let socket = t.sockets.Sockets.iopub in
  let send_message content = match !msg with
    | Some msg -> M.send_h ?key:t.key socket msg content >|= fun () -> Iopub_ok
    | None -> Lwt.return Iopub_ok
  and send_raw_message msg =
    M.send ?key:t.key socket msg >|= fun () -> Iopub_ok
  in
  let send_mime context mime_type data base64 =
    (* send mime message *)
    let content = mime_message_content mime_type base64 data in
    match context with
      | Some context -> M.send_h socket context content >|= fun () -> Iopub_ok
      | None -> send_message content
  in
  begin match m with
    | Iopub_set_current m -> msg := Some m; Lwt.return Iopub_ok
    | Iopub_send_message content -> send_message content
    | Iopub_send_raw_message(msg) -> send_raw_message msg
    | Iopub_send_mime (context,mime_type,data,base64) ->
      send_mime context mime_type data base64
    | Iopub_get_current -> Lwt.return (Iopub_context !msg)
    | Iopub_flush
    | Iopub_stop -> Lwt.return Iopub_ok
  end

(* execute code *)
let execute_request (t:t) msg e : unit Lwt.t =
  (* if we are not silent increment execution count *)
  if not e.silent then (
    t.e_count <- t.e_count + 1;
  );

  let execution_count = t.e_count in

  (* set state to busy *)
  let%lwt _ = send_iopub t (Iopub_set_current msg) in
  let%lwt _ =
    send_iopub t (Iopub_send_message (M.Status { execution_state = "busy" }))
  in
  let%lwt _ = send_iopub t (Iopub_send_message
        (M.Pyin {
            pi_code = e.code;
            pi_execution_count = execution_count;
          }))
  in

  (* eval code *)
  let%lwt status = t.kernel.Kernel.exec ~count:execution_count e.code in
  Pervasives.flush stdout;
  Pervasives.flush stderr;
  let%lwt _ = send_iopub t Iopub_flush in

  (* in case of success, how to print *)
  let pyout (s:Kernel.status_cell) =
    send_iopub t (Iopub_send_message
        (M.Pyout {
            po_execution_count = execution_count;
            po_data = begin match s with
              | Kernel.Html s -> `Assoc [ "text/html", `String s]
              | Kernel.Mime (ty,s) -> `Assoc [ ty, `String s ]
            end;
            po_metadata = `Assoc []; }))
  in
  let%lwt () = match status with
    | Result.Ok l ->
      let%lwt () =
        M.send_h ?key:t.key t.sockets.Sockets.shell msg
          (M.Execute_reply {
              status = "ok";
              execution_count;
              ename = None; evalue = None; traceback = None; payload = None;
              er_user_expressions = None;
            })
      in
      (* TODO use display mime for all but the first *)
      Lwt_list.iter_s (fun m -> pyout m >|= fun _ -> ()) l
    | Result.Error err_msg ->
      M.send_h ?key:t.key t.sockets.Sockets.shell msg
        (M.Execute_reply {
            status = "error";
            execution_count;
            ename = Some "error"; evalue = Some err_msg;
            traceback = Some []; payload = None;
            er_user_expressions = None;
          })
  in
  let%lwt _ =
    send_iopub t (Iopub_send_message (M.Status { execution_state = "idle" }))
  in
  Lwt.return_unit

let kernel_info_request (t:t) msg =
  M.send ?key:t.key t.sockets.Sockets.shell
    (M.make_header
       { msg with M.
               content = M.Kernel_info_reply {
                   protocol_version = [ 5; 0 ];
                   language_version = [ 0; 1; 0 ];
                   language = "rewrite";
                 }
       })

let shutdown_request (t:t) msg _ : 'a Lwt.t =
  let%lwt () =
    M.send_h ?key:t.key t.sockets.Sockets.shell msg
      (M.Shutdown_reply { restart = false })
  in
  Lwt.fail Exit

let handle_invalid_message () =
  Lwt.fail (Failure "Invalid message on shell socket")

let complete_request _socket _msg _x =
  () (* TODO: completion *)

let object_info_request _socket _msg _x =
  () (* TODO: completion *)

let connect_request _socket _msg = ()
let history_request _socket _msg _x = ()

let run t : unit Lwt.t =
  let () = Sys.catch_break true in
  Log.log "run on sockets...\n";
  let%lwt _ =
    send_iopub t
      (Iopub_send_message (M.Status { execution_state = "starting" }))
  in
  let handle_message () =
    let open Sockets in
    let%lwt msg = M.recv t.sockets.shell in
    Log.logf "received message `%s`\n" (M.json_of_content msg.M.content);
    match msg.M.content with
      | M.Kernel_info_request -> kernel_info_request t msg
      | M.Execute_request x -> execute_request t msg x
      | M.Connect_request -> connect_request t msg; Lwt.return_unit
      | M.Object_info_request x -> object_info_request t msg x; Lwt.return_unit
      | M.Complete_request x -> complete_request t msg x; Lwt.return_unit
      | M.History_request x -> history_request t msg x; Lwt.return_unit
      | M.Shutdown_request x -> shutdown_request t msg x

      (* messages we should not be getting *)
      | M.Connect_reply(_) | M.Kernel_info_reply(_)
      | M.Shutdown_reply(_) | M.Execute_reply(_)
      | M.Object_info_reply(_) | M.Complete_reply(_)
      | M.History_reply(_) | M.Status(_) | M.Pyin(_)
      | M.Pyout(_) | M.Stream(_) | M.Display_data(_)
      | M.Clear(_) -> handle_invalid_message ()

      | M.Comm_open -> Lwt.return_unit
  in
  let rec run () =
    try%lwt
      handle_message() >>= run
    with
      | Sys.Break ->
        Log.log "Sys.Break\n";
        run ()
      | Exit ->
        Log.log "Exiting, as requested\n";
        Lwt.return_unit
  in
  run ()

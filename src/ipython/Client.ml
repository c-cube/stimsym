
(* This file is free software. See file "license" for more details. *)

(** {1 Main Kernel Loop} *)

open Lwt.Infix
open Ipython_json_j

module M = Message

exception Restart

module Kernel = struct
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

  type completion_status = {
    completion_matches: string list;
    completion_start: int;
    completion_end: int;
  }

  type is_complete_reply =
    | Is_complete
    | Is_not_complete of string (* indent *)

  let doc x = Doc x
  let mime ?(base64=false) ~ty x = Mime (ty,x,base64)

  let ok ?(actions=[]) msg = {msg; actions}

  type t = {
    exec: count:int -> string -> exec_status Lwt.t;
    is_complete: string -> is_complete_reply;
    language: string;
    language_version: int list;
    complete: pos:int -> string -> completion_status Lwt.t;
  }
end

type t = {
  sockets: Sockets.t;
  key: string option;
  kernel: Kernel.t;
  mutable e_count: int;
}

let make ?key sockets kernel : t =
  { key; sockets; kernel; e_count=0; }

type iopub_message =
  | Iopub_send_message of Message.content
  | Iopub_send_mime of
        string (* type *)
      * string (* content *)
      * bool (* base64? *)

let string_of_message content =
  let msg_type = M.msg_type_of_content content in
  Printf.sprintf "{`%s` content `%s`}"
    msg_type (M.json_of_content content)

let string_of_iopub_message = function
  | Iopub_send_message content ->
    Printf.sprintf "send_message %s" (string_of_message content)
  | Iopub_send_mime (ty,cont,b64) ->
    Printf.sprintf "send_mime %s %s (base64: %B)" ty cont b64

(* encode mime data, wrap it into a message *)
let mime_message_content mime_type base64 data : M.content =
  let data =
    if not base64 then data
    else Base64.encode data
  in
  (Message.Display_data (Ipython_json_j.({
       dd_source = "stimsym"; (* TODO *)
       dd_data = `Assoc [mime_type,`String data];
       dd_metadata = `Assoc [];
     })))

let send_shell (t:t) ~parent (content:M.content): unit Lwt.t =
  Log.logf "send_shell `%s`\n" (string_of_message content);
  let socket = t.sockets.Sockets.shell in
  let msg_type = M.msg_type_of_content content in
  let msg' = M.make ~parent ~msg_type content in
  M.send ?key:t.key socket msg'

(* send a message on the Iopub socket *)
let send_iopub (t:t) ?parent (m:iopub_message): unit Lwt.t =
  Log.logf "send_iopub `%s`\n" (string_of_iopub_message m);
  let socket = t.sockets.Sockets.iopub in
  let send_message msg_type content =
    let msg' = match parent with
      | None -> M.make_first ~msg_type content
      | Some parent -> M.make ~parent ~msg_type content
    in
    M.send ?key:t.key socket msg'
  in
  let send_mime mime_type data base64 =
    (* send mime message *)
    let content = mime_message_content mime_type base64 data in
    let msg_type = M.msg_type_of_content content in
    send_message msg_type content
  in
  begin match m with
    | Iopub_send_message content ->
      let msg_type = M.msg_type_of_content content in
      send_message msg_type content
    | Iopub_send_mime (mime_type,data,base64) ->
      send_mime mime_type data base64
  end

module H = Tyxml.Html

(* display a document as HTML *)
let html_of_doc : Document.t -> [<Html_types.div] H.elt =
  let mk_header ~depth l = match depth with
    | 1 -> H.h1 l
    | 2 -> H.h2 l
    | 3 -> H.h3 l
    | 4 -> H.h4 l
    | 5 -> H.h5 l
    | n when n>=6 -> H.h6 l
    | _ -> assert false
  in
  let rec aux ~depth doc =
    H.div (List.map (aux_block ~depth) doc)
  and aux_block ~depth (b:Document.block) =
    let h = match b with
      | `S s -> mk_header ~depth [H.pcdata s]
      | `P s -> H.p [H.pcdata s]
      | `Pre s -> H.pre [H.pcdata s]
      | `L l ->
        H.ul (List.map (fun sub -> H.li [aux ~depth sub]) l)
      | `I (s,sub) ->
        let depth = depth+1 in
        H.div (
          mk_header ~depth [H.pcdata s] :: List.map (aux_block ~depth) sub
        )
    in
    H.div [h]
  in
  aux ~depth:3

(* run [f ()] in a "status:busy" context *)
let within_status (t:t) ~f =
  (* set state to busy *)
  let%lwt _ =
    send_iopub t
      (Iopub_send_message (M.Status { execution_state = "busy" }))
  in
  Lwt.finalize
    f
    (fun () ->
       send_iopub t
         (Iopub_send_message
          (M.Status { execution_state = "idle" })))

(* execute code *)
let execute_request (t:t) ~parent e : unit Lwt.t =
  (* if we are not silent increment execution count *)
  if not e.silent then (
    t.e_count <- t.e_count + 1;
  );

  let execution_count = t.e_count in

  let%lwt _ = send_iopub t ~parent
      (Iopub_send_message
        (M.Execute_input {
            pi_code = e.code;
            pi_execution_count = execution_count;
          }))
  in

  (* eval code *)
  let%lwt status = t.kernel.Kernel.exec ~count:execution_count e.code in

  (* in case of success, how to print *)
  let reply_status_ok (s:string option) = match s with
    | None -> Lwt.return_unit
    | Some msg ->
      send_iopub t ~parent (Iopub_send_message
          (M.Execute_result {
              po_execution_count = execution_count;
              po_data = `Assoc ["text/plain", `String msg];
              po_metadata = `Assoc []; }))
      >|= fun _ -> ()
  and side_action (s:Kernel.exec_action) : unit Lwt.t =
    let ty, payload, b64 = match s with
      | Kernel.Doc d ->
        let html = html_of_doc d in
        let s = CCFormat.sprintf "%a@." (H.pp_elt ()) html in
        "text/html", s, false
      | Kernel.Mime (ty,s,b64) -> ty, s, b64
    in
    send_iopub t ~parent (Iopub_send_mime (ty, payload, b64))
  in
  let%lwt () = match status with
    | Result.Ok ok ->
      let%lwt () =
        send_shell t ~parent
          (M.Execute_reply {
              status = "ok";
              execution_count;
              ename = None; evalue = None; traceback = None; payload = None;
              er_user_expressions = None;
            })
      in
      let%lwt _ = reply_status_ok ok.Kernel.msg in
      (* send mime type in the background *)
      Lwt_list.iter_p side_action ok.Kernel.actions
    | Result.Error err_msg ->
      let content =
        M.Execute_reply {
            status = "error";
            execution_count;
            ename = Some "error"; evalue = Some err_msg;
            traceback = Some ["<eval>"]; payload = None;
            er_user_expressions = None;
        }
      in
      Log.logf "send ERROR `%s`\n" (M.json_of_content content);
      let%lwt () = send_shell t ~parent content in
      send_iopub t ~parent
        (Iopub_send_message (M.Execute_error {
             err_ename="error";
             err_evalue=err_msg;
             err_traceback=[
               "ERROR " ^ err_msg;
               "evaluating " ^ e.code;
             ];
           }))
  in
  Lwt.return_unit

let kernel_info_request (t:t) ~parent =
  let str_of_version = CCFormat.(to_string (list ~sep:"." int |> hbox)) in
  let%lwt _ =
    send_shell t ~parent (M.Kernel_info_reply {
        implementation=t.kernel.Kernel.language;
        implementation_version="0.1.0";  (* TODO *)
        protocol_version = "5.0";
        language_info = {
          li_name = t.kernel.Kernel.language;
          li_version = str_of_version t.kernel.Kernel.language_version;
          li_mimetype="text";
          li_file_extension=".txt"; (* TODO *)
        };
        banner=""; (* TODO *)
        help_links=[];
      })
  in
  Lwt.return_unit

let shutdown_request (t:t) ~parent (r:shutdown) : 'a Lwt.t =
  Log.log "received shutdown request...\n";
  let%lwt () =
    send_shell t ~parent (M.Shutdown_reply r)
  in
  Lwt.fail (if r.restart then Restart else Exit)

let handle_invalid_message () =
  Lwt.fail (Failure "Invalid message on shell socket")

let complete_request t ~parent (r:complete_request): unit Lwt.t =
  let%lwt st = t.kernel.Kernel.complete ~pos:r.cursor_pos r.line in
  let content = {
    matches=st.Kernel.completion_matches;
    cursor_start=st.Kernel.completion_start;
    cursor_end=st.Kernel.completion_end;
    cr_status="ok";
  } in
  send_shell t ~parent (M.Complete_reply content)

let is_complete_request t ~parent (r:is_complete_request): unit Lwt.t =
  let st = t.kernel.Kernel.is_complete r.icr_code in
  let content = match st with
    | Kernel.Is_complete ->
      {icr_status="complete"; icr_indent=""}
    | Kernel.Is_not_complete icr_indent ->
      {icr_status="incomplete"; icr_indent}
  in
  send_shell t ~parent (M.Is_complete_reply content)

let object_info_request _socket ~parent:_msg _x =
  () (* TODO: doc? *)

let connect_request _socket _msg = ()

let history_request t ~parent _x =
  let content = {history=[]} in
  send_shell t ~parent (M.History_reply content)

type run_result =
  | Run_stop
  | Run_restart

let run t : run_result Lwt.t =
  let () = Sys.catch_break true in
  Log.log "run on sockets...\n";
  let heartbeat =
    Sockets.heartbeat t.sockets >|= fun () -> Run_stop
  in
  let%lwt _ =
    send_iopub t
      (Iopub_send_message (M.Status { execution_state = "starting" }))
  in
  let handle_message () =
    let open Sockets in
    let%lwt m = Lwt.pick
        [ M.recv t.sockets.shell;
          M.recv t.sockets.control;
        ] in
    Log.logf "received message `%s`, content `%s`\n"
      (M.msg_type_of_content m.M.content)
      (M.json_of_content m.M.content);
    begin match m.M.content with
      | M.Kernel_info_request ->
        within_status t
          ~f:(fun () -> kernel_info_request t ~parent:m)
      | M.Execute_request x ->
        within_status t
          ~f:(fun () -> execute_request t ~parent:m x)
      | M.Connect_request ->
        Log.log "warning: received deprecated connect_request";
        connect_request t m; Lwt.return_unit
      | M.Object_info_request x -> object_info_request t ~parent:m x; Lwt.return_unit
      | M.Complete_request x ->
        within_status t ~f:(fun () -> complete_request t ~parent:m x)
      | M.Is_complete_request x ->
        within_status t ~f:(fun () -> is_complete_request t ~parent:m x)
      | M.History_request x ->
        within_status t ~f:(fun () -> history_request t ~parent:m x)
      | M.Shutdown_request x -> shutdown_request t ~parent:m x

      (* messages we should not be getting *)
      | M.Connect_reply(_) | M.Kernel_info_reply(_)
      | M.Shutdown_reply(_) | M.Execute_reply(_)
      | M.Object_info_reply(_) | M.Complete_reply(_) | M.Is_complete_reply _
      | M.History_reply(_) | M.Status(_) | M.Execute_input _
      | M.Execute_result _ | M.Stream(_) | M.Display_data(_)
      | M.Execute_error _ | M.Clear(_) -> handle_invalid_message ()

      | M.Comm_open -> Lwt.return_unit
    end
  in
  let rec run () =
    try%lwt
      handle_message() >>= run
    with
      | Sys.Break ->
        Log.log "Sys.Break\n";
        run ()
      | Restart ->
        Log.log "Restart\n";
        Lwt.return Run_restart
      | Exit ->
        Log.log "Exiting, as requested\n";
        Lwt.return Run_stop
  in
  Lwt.pick [run (); heartbeat]

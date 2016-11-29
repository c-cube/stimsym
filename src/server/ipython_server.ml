
(* This file is free software. See file "license" for more details. *)

(** {1 Notebook interface} *)

open Ipython
open Rewrite
open Lwt.Infix

module M = Message

let output_cell_max_height = ref "100px"

(*******************************************************************************)
(* command line *)

let connection_file_name = ref ""
let completion = ref false
let object_info = ref false
let init_file = ref ""

let ci_stdin = ref 50000
let ci_shell = ref 50001
let ci_iopub = ref 50002
let ci_heartbeat = ref 50003
let ci_control = ref 50004
let ci_transport = ref "tcp"
let ci_ip_addr = ref ""

let () =
  Arg.(parse
      (align [
          "--log", String(Log.open_log_file), " <file> open log file";
          "--connection-file", Set_string(connection_file_name),
          " <filename> connection file name";
          "--init", Set_string(init_file), " <file> load <file> instead of default init file";
          "--completion", Set(completion), " enable tab completion";
          "--object-info", Set(object_info), " enable introspection";
          (* pass connection info through command line *)
          "--ci-stdin", Set_int(ci_stdin), " (connection info) stdin zmq port";
          "--ci-iopub", Set_int(ci_iopub), " (connection info) iopub zmq port";
          "--ci-shell", Set_int(ci_shell), " (connection info) shell zmq port";
          "--ci-control", Set_int(ci_control), " (connection info) control zmq port";
          "--ci-heartbeat", Set_int(ci_heartbeat), " (connection info) heartbeat zmq port";
          "--ci-transport", Set_string(ci_transport), " (connection info) transport";
          "--ci-ip", Set_string(ci_ip_addr), " (connection info) ip address"
        ])
      (fun s -> failwith ("invalid anonymous argument: " ^ s))
      "rewrite kernel")

(** {2 Execution of queries} *)

module Exec = struct
  type status = (string,string) Result.result list

  (* blocking function *)
  let run_ count str () =
    let buf = Lexing.from_string str in
    Parse_loc.set_file buf ("cell_" ^ string_of_int count);
    begin match Parser.parse_expr Lexer.token buf with
      | e ->
        Log.log (CCFormat.sprintf "parsed: @[%a@]@." Expr.pp_full_form e);
        begin
          try
            let e' = Expr.eval e in
            [
              Result.Ok
                (CCFormat.sprintf "@[%a@]@." Expr.pp_full_form e')
            ]
          with
            | Stack_overflow ->
              [ Result.Error "stack overflow."]
            | Expr.Eval_fail msg ->
              [
                Result.Error
                  (CCFormat.sprintf "evaluation failed: %s@." msg);
              ]
        end
      | exception e ->
        [
          Result.Error
            (CCFormat.sprintf "error: %s@." (Printexc.to_string e));
        ]
    end

  let run_cell count str : status Lwt.t =
    Lwt_preemptive.detach (run_ count str) ()

  let html_of_status r _ = match r with
    | Result.Ok msg -> "<p>" ^ msg ^ "</p>"
    | Result.Error msg -> "<style=\"color:red\"><p>" ^ msg ^ " </p></style>"
end

(*******************************************************************************)
(* shell messages and iopub thread *)

module Shell = struct
  open Ipython_json_t

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
         dd_source = "rewrite";
         dd_data = `Assoc [mime_type,`String data];
         dd_metadata = `Assoc [];
       })))

  let msg : M.t option ref = ref None

  (* send a message on the Iopub socket *)
  let send_iopub (sockets:Sockets.t) (m:iopub_message): iopub_resp Lwt.t =
    Log.logf "send_iopub `%s`\n" (string_of_iopub_message m);
    let socket = sockets.Sockets.iopub in
    let send_message content = match !msg with
      | Some msg -> M.send_h socket msg content >|= fun () -> Iopub_ok
      | None -> Lwt.return Iopub_ok
    and send_raw_message msg =
      M.send socket msg >|= fun () -> Iopub_ok
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
  let execute_request =
    let execution_count = ref 0 in
    fun sockets msg e : unit Lwt.t ->
      let send_iopub_u m = Lwt.async (fun () -> send_iopub sockets m) in

      (* if we are not silent increment execution count *)
      if not e.silent then incr execution_count;

      (* set state to busy *)
      send_iopub_u (Iopub_set_current msg);
      send_iopub_u (Iopub_send_message (M.Status { execution_state = "busy" }));
      send_iopub_u (Iopub_send_message
          (M.Pyin {
              pi_code = e.code;
              pi_execution_count = !execution_count;
            }));

      (* eval code *)
      let%lwt status = Exec.run_cell !execution_count e.code in
      Pervasives.flush stdout;
      Pervasives.flush stderr;
      send_iopub_u Iopub_flush;

      let pyout message =
        send_iopub_u (Iopub_send_message
            (M.Pyout {
                po_execution_count = !execution_count;
                po_data =
                  `Assoc [ "text/html",
                           `String (Exec.html_of_status message !output_cell_max_height) ];
                po_metadata = `Assoc []; }))
      in

      let%lwt () =
        M.send_h sockets.Sockets.shell msg
          (M.Execute_reply {
              status = "ok";
              execution_count = !execution_count;
              ename = None; evalue = None; traceback = None; payload = None;
              er_user_expressions = None;
            })
      in
      List.iter (fun m -> pyout m) status;
      send_iopub_u (Iopub_send_message (M.Status { execution_state = "idle" }));
      Lwt.return_unit

  let kernel_info_request socket msg =
    M.send socket
      (M.make_header
         { msg with M.
             content = M.Kernel_info_reply {
                 protocol_version = [ 5; 0 ];
                 language_version = [ 0; 1; 0 ];
                 language = "rewrite";
               }
         })

  let shutdown_request socket msg _ : 'a Lwt.t =
    let%lwt () = M.send_h socket msg (M.Shutdown_reply { restart = false }) in
    Lwt.fail (Failure "Exiting")

  let handle_invalid_message () =
    Lwt.fail (Failure "Invalid message on shell socket")

  let complete_request _socket _msg _x =
    () (* TODO: completion *)

  let object_info_request _socket _msg _x =
    () (* TODO: completion *)

  let connect_request _socket _msg = ()
  let history_request _socket _msg _x = ()

  let run sockets =
    let () = Sys.catch_break true in
    Log.log "run on sockets...\n";
    let%lwt _ =
      send_iopub sockets
        (Iopub_send_message (M.Status { execution_state = "starting" }))
    in
    let handle_message () =
      let open Sockets in
      let%lwt msg = M.recv sockets.shell in
      Log.logf "received message `%s`\n" (M.json_of_content msg.M.content);
      match msg.M.content with
        | M.Kernel_info_request -> kernel_info_request sockets.shell msg
        | M.Execute_request x -> execute_request sockets msg x
        | M.Connect_request -> connect_request sockets.shell msg; Lwt.return_unit
        | M.Object_info_request x -> object_info_request sockets.shell msg x; Lwt.return_unit
        | M.Complete_request x -> complete_request sockets.shell msg x; Lwt.return_unit
        | M.History_request x -> history_request sockets.shell msg x; Lwt.return_unit
        | M.Shutdown_request x -> shutdown_request sockets.shell msg x

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
      with Sys.Break ->
        Log.log "Sys.Break\n";
        run ()
    in
    run ()
end

(*******************************************************************************)
(* main *)

let () = Printf.printf "[rewrite] Starting kernel\n%!"
(*let () = Sys.interactive := false*)
let () = Unix.putenv "TERM" "" (* make sure the compiler sees a dumb terminal *)

let connection_info =
  if !ci_ip_addr <> "" then
    (* get configuration parameters from command line *)
    { Ipython_json_t.
      stdin_port = !ci_stdin;
      ip = !ci_ip_addr;
      control_port = !ci_control;
      hb_port = !ci_heartbeat;
      signature_scheme = "hmac-sha256";
      key = "";
      shell_port = !ci_shell;
      transport = !ci_transport;
      iopub_port = !ci_iopub;
    }
  else
    (* read from configuration files *)
    let f_conn_info =
      try open_in !connection_file_name
      with _ ->
        failwith ("Failed to open connection file: '" ^
            !connection_file_name ^ "'")
    in
    let state = Yojson.init_lexer () in
    let lex = Lexing.from_channel f_conn_info in
    let conn = Ipython_json_j.read_connection_info state lex in
    let () = close_in f_conn_info in
    conn

let () =
  Log.log "start main...\n";
  Lwt_main.run
    begin
      try%lwt
        let sockets = Sockets.open_sockets connection_info in
        Lwt.join
          [ Shell.run sockets;
            Sockets.heartbeat connection_info;
            Sockets.dump sockets.Sockets.control;
          ]
        >>= fun () ->
        Log.log "Done.\n";
        Lwt.return_unit
      with e ->
        Log.log (Printf.sprintf "Exception: %s\n" (Printexc.to_string e));
        Log.log "Dying.\n";
        exit 0
    end



(* This file is free software. See file "license" for more details. *)

(** {1 Notebook interface} *)

open Ipython
open Stimsym
open Lwt.Infix

module C = Client

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

(* blocking function *)
let run_ count str : C.Kernel.exec_status =
  let buf = Lexing.from_string str in
  Parse_loc.set_file buf ("cell_" ^ string_of_int count);
  begin match Parser.parse_expr Lexer.token buf with
    | e ->
      Log.log (CCFormat.sprintf "parsed: @[%a@]@." Expr.pp_full_form e);
      begin
        try
          let e', effects = Eval.eval_full e in
          let res =
            if Expr.equal Builtins.null e'
            then None
            else Some (CCFormat.sprintf "@[%a@]@." Expr.pp e')
          and actions =
            List.map
              (function
                | Eval.Print_doc d -> C.Kernel.doc d
                | Eval.Print_mime {Expr.mime_ty;mime_data;mime_base64} ->
                  C.Kernel.mime ~base64:mime_base64 ~ty:mime_ty mime_data)
              effects
          in
          Result.Ok (C.Kernel.ok ~actions res)
        with
          | Stack_overflow ->
            Result.Error "stack overflow."
          | Eval.Eval_fail msg ->
            Result.Error
              (CCFormat.sprintf "evaluation failed: %s@." msg)
      end
    | exception e ->
      Result.Error
        (CCFormat.sprintf "error: %s@." (Printexc.to_string e))
  end

(* auto-completion *)
let complete pos str = 
  let completion_matches =
    if pos > String.length str then []
    else
      let left = String.sub str 0 pos in
      Completion.complete left
      |> List.map (fun c -> c.Completion.text)
  in
  let c = {
    C.Kernel.completion_matches;
    completion_start=0; completion_end=pos
  } in
  c

(* is the block of code complete?
   TODO: a way of asking the parser if it failed because of EOI/unbalanced []*)
let is_complete _ = C.Kernel.Is_complete

let () =
  Builtins.log_ := Log.log

let kernel : C.Kernel.t = {
  C.Kernel.
  exec = (fun ~count msg -> Lwt.return (run_ count msg));
  is_complete;
  complete =
    (fun ~pos msg -> Lwt.return (complete pos msg))
}

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
        let key = connection_info.Ipython_json_t.key in
        let key = if key="" then None else Some key in
        let sh = C.make ?key sockets kernel in
        Lwt.join
          [ C.run sh;
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


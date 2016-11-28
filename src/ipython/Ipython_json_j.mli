(* Auto-generated from "Ipython_json.atd" *)


type stream = Ipython_json_t.stream = { st_name: string; st_data: string }

type status = Ipython_json_t.status = { execution_state: string }

type shutdown = Ipython_json_t.shutdown = { restart: bool }

type dyn = Yojson.Safe.json

type pyout = Ipython_json_t.pyout = {
  po_execution_count: int;
  po_data: dyn;
  po_metadata: dyn
}

type pyin = Ipython_json_t.pyin = {
  pi_code: string;
  pi_execution_count: int
}

type payload = Ipython_json_t.payload = {
  html: string;
  source: string;
  start_line_number: int;
  text: string
}

type object_info_request = Ipython_json_t.object_info_request = {
  oname: string;
  detail_level: int
}

type arg_spec = Ipython_json_t.arg_spec = {
  args: string list;
  varargs: string;
  varkw: string;
  defaults: string list
}

type object_info_reply = Ipython_json_t.object_info_reply = {
  name: string;
  found: bool;
  ismagic: bool;
  isalias: bool;
  namespace: string;
  type_name: string;
  string_form: string;
  base_class: string;
  length: int;
  oi_file: string;
  definition: string;
  argspec: arg_spec;
  init_definition: string;
  docstring: string;
  class_docstring: string;
  call_def: string;
  call_docstring: string;
  oi_source: string
}

type kernel_info_reply = Ipython_json_t.kernel_info_reply = {
  protocol_version: int list;
  language_version: int list;
  language: string
}

type history_request = Ipython_json_t.history_request = {
  output: bool;
  raw: bool;
  hist_access_type: string;
  hr_session: int;
  start: int;
  stop: int;
  n: int;
  pattern: string;
  unique: bool
}

type history_reply = Ipython_json_t.history_reply = { history: string list }

type header_info = Ipython_json_t.header_info = {
  date: string;
  username: string;
  session: string;
  msg_id: string;
  msg_type: string
}

type execute_request = Ipython_json_t.execute_request = {
  code: string;
  silent: bool;
  store_history: bool;
  user_expressions: dyn;
  allow_stdin: bool
}

type execute_reply = Ipython_json_t.execute_reply = {
  status: string;
  execution_count: int;
  ename: string option;
  evalue: string option;
  traceback: string list option;
  payload: payload list option;
  er_user_expressions: dyn option
}

type display_data = Ipython_json_t.display_data = {
  dd_source: string;
  dd_data: dyn;
  dd_metadata: dyn
}

type connection_info = Ipython_json_t.connection_info = {
  stdin_port: int;
  ip: string;
  control_port: int;
  hb_port: int;
  signature_scheme: string;
  key: string;
  shell_port: int;
  transport: string;
  iopub_port: int
}

type connect_reply = Ipython_json_t.connect_reply = {
  cr_shell_port: int;
  cr_iopub_port: int;
  cr_stdin_port: int;
  cr_hb_port: int
}

type complete_request = Ipython_json_t.complete_request = {
  line: string;
  cursor_pos: int
}

type complete_reply = Ipython_json_t.complete_reply = {
  matches: string list;
  matched_text: string;
  cr_status: string
}

type clear_output = Ipython_json_t.clear_output = {
  wait: bool;
  stdout: bool;
  stderr: bool;
  other: bool
}

val write_stream :
  Bi_outbuf.t -> stream -> unit
  (** Output a JSON value of type {!stream}. *)

val string_of_stream :
  ?len:int -> stream -> string
  (** Serialize a value of type {!stream}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_stream :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> stream
  (** Input JSON data of type {!stream}. *)

val stream_of_string :
  string -> stream
  (** Deserialize JSON data of type {!stream}. *)

val write_status :
  Bi_outbuf.t -> status -> unit
  (** Output a JSON value of type {!status}. *)

val string_of_status :
  ?len:int -> status -> string
  (** Serialize a value of type {!status}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_status :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> status
  (** Input JSON data of type {!status}. *)

val status_of_string :
  string -> status
  (** Deserialize JSON data of type {!status}. *)

val write_shutdown :
  Bi_outbuf.t -> shutdown -> unit
  (** Output a JSON value of type {!shutdown}. *)

val string_of_shutdown :
  ?len:int -> shutdown -> string
  (** Serialize a value of type {!shutdown}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_shutdown :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> shutdown
  (** Input JSON data of type {!shutdown}. *)

val shutdown_of_string :
  string -> shutdown
  (** Deserialize JSON data of type {!shutdown}. *)

val write_dyn :
  Bi_outbuf.t -> dyn -> unit
  (** Output a JSON value of type {!dyn}. *)

val string_of_dyn :
  ?len:int -> dyn -> string
  (** Serialize a value of type {!dyn}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_dyn :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> dyn
  (** Input JSON data of type {!dyn}. *)

val dyn_of_string :
  string -> dyn
  (** Deserialize JSON data of type {!dyn}. *)

val write_pyout :
  Bi_outbuf.t -> pyout -> unit
  (** Output a JSON value of type {!pyout}. *)

val string_of_pyout :
  ?len:int -> pyout -> string
  (** Serialize a value of type {!pyout}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pyout :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pyout
  (** Input JSON data of type {!pyout}. *)

val pyout_of_string :
  string -> pyout
  (** Deserialize JSON data of type {!pyout}. *)

val write_pyin :
  Bi_outbuf.t -> pyin -> unit
  (** Output a JSON value of type {!pyin}. *)

val string_of_pyin :
  ?len:int -> pyin -> string
  (** Serialize a value of type {!pyin}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_pyin :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> pyin
  (** Input JSON data of type {!pyin}. *)

val pyin_of_string :
  string -> pyin
  (** Deserialize JSON data of type {!pyin}. *)

val write_payload :
  Bi_outbuf.t -> payload -> unit
  (** Output a JSON value of type {!payload}. *)

val string_of_payload :
  ?len:int -> payload -> string
  (** Serialize a value of type {!payload}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_payload :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> payload
  (** Input JSON data of type {!payload}. *)

val payload_of_string :
  string -> payload
  (** Deserialize JSON data of type {!payload}. *)

val write_object_info_request :
  Bi_outbuf.t -> object_info_request -> unit
  (** Output a JSON value of type {!object_info_request}. *)

val string_of_object_info_request :
  ?len:int -> object_info_request -> string
  (** Serialize a value of type {!object_info_request}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_object_info_request :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> object_info_request
  (** Input JSON data of type {!object_info_request}. *)

val object_info_request_of_string :
  string -> object_info_request
  (** Deserialize JSON data of type {!object_info_request}. *)

val write_arg_spec :
  Bi_outbuf.t -> arg_spec -> unit
  (** Output a JSON value of type {!arg_spec}. *)

val string_of_arg_spec :
  ?len:int -> arg_spec -> string
  (** Serialize a value of type {!arg_spec}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_arg_spec :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> arg_spec
  (** Input JSON data of type {!arg_spec}. *)

val arg_spec_of_string :
  string -> arg_spec
  (** Deserialize JSON data of type {!arg_spec}. *)

val write_object_info_reply :
  Bi_outbuf.t -> object_info_reply -> unit
  (** Output a JSON value of type {!object_info_reply}. *)

val string_of_object_info_reply :
  ?len:int -> object_info_reply -> string
  (** Serialize a value of type {!object_info_reply}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_object_info_reply :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> object_info_reply
  (** Input JSON data of type {!object_info_reply}. *)

val object_info_reply_of_string :
  string -> object_info_reply
  (** Deserialize JSON data of type {!object_info_reply}. *)

val write_kernel_info_reply :
  Bi_outbuf.t -> kernel_info_reply -> unit
  (** Output a JSON value of type {!kernel_info_reply}. *)

val string_of_kernel_info_reply :
  ?len:int -> kernel_info_reply -> string
  (** Serialize a value of type {!kernel_info_reply}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_kernel_info_reply :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> kernel_info_reply
  (** Input JSON data of type {!kernel_info_reply}. *)

val kernel_info_reply_of_string :
  string -> kernel_info_reply
  (** Deserialize JSON data of type {!kernel_info_reply}. *)

val write_history_request :
  Bi_outbuf.t -> history_request -> unit
  (** Output a JSON value of type {!history_request}. *)

val string_of_history_request :
  ?len:int -> history_request -> string
  (** Serialize a value of type {!history_request}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_history_request :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> history_request
  (** Input JSON data of type {!history_request}. *)

val history_request_of_string :
  string -> history_request
  (** Deserialize JSON data of type {!history_request}. *)

val write_history_reply :
  Bi_outbuf.t -> history_reply -> unit
  (** Output a JSON value of type {!history_reply}. *)

val string_of_history_reply :
  ?len:int -> history_reply -> string
  (** Serialize a value of type {!history_reply}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_history_reply :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> history_reply
  (** Input JSON data of type {!history_reply}. *)

val history_reply_of_string :
  string -> history_reply
  (** Deserialize JSON data of type {!history_reply}. *)

val write_header_info :
  Bi_outbuf.t -> header_info -> unit
  (** Output a JSON value of type {!header_info}. *)

val string_of_header_info :
  ?len:int -> header_info -> string
  (** Serialize a value of type {!header_info}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_header_info :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> header_info
  (** Input JSON data of type {!header_info}. *)

val header_info_of_string :
  string -> header_info
  (** Deserialize JSON data of type {!header_info}. *)

val write_execute_request :
  Bi_outbuf.t -> execute_request -> unit
  (** Output a JSON value of type {!execute_request}. *)

val string_of_execute_request :
  ?len:int -> execute_request -> string
  (** Serialize a value of type {!execute_request}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_execute_request :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> execute_request
  (** Input JSON data of type {!execute_request}. *)

val execute_request_of_string :
  string -> execute_request
  (** Deserialize JSON data of type {!execute_request}. *)

val write_execute_reply :
  Bi_outbuf.t -> execute_reply -> unit
  (** Output a JSON value of type {!execute_reply}. *)

val string_of_execute_reply :
  ?len:int -> execute_reply -> string
  (** Serialize a value of type {!execute_reply}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_execute_reply :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> execute_reply
  (** Input JSON data of type {!execute_reply}. *)

val execute_reply_of_string :
  string -> execute_reply
  (** Deserialize JSON data of type {!execute_reply}. *)

val write_display_data :
  Bi_outbuf.t -> display_data -> unit
  (** Output a JSON value of type {!display_data}. *)

val string_of_display_data :
  ?len:int -> display_data -> string
  (** Serialize a value of type {!display_data}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_display_data :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> display_data
  (** Input JSON data of type {!display_data}. *)

val display_data_of_string :
  string -> display_data
  (** Deserialize JSON data of type {!display_data}. *)

val write_connection_info :
  Bi_outbuf.t -> connection_info -> unit
  (** Output a JSON value of type {!connection_info}. *)

val string_of_connection_info :
  ?len:int -> connection_info -> string
  (** Serialize a value of type {!connection_info}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_connection_info :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> connection_info
  (** Input JSON data of type {!connection_info}. *)

val connection_info_of_string :
  string -> connection_info
  (** Deserialize JSON data of type {!connection_info}. *)

val write_connect_reply :
  Bi_outbuf.t -> connect_reply -> unit
  (** Output a JSON value of type {!connect_reply}. *)

val string_of_connect_reply :
  ?len:int -> connect_reply -> string
  (** Serialize a value of type {!connect_reply}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_connect_reply :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> connect_reply
  (** Input JSON data of type {!connect_reply}. *)

val connect_reply_of_string :
  string -> connect_reply
  (** Deserialize JSON data of type {!connect_reply}. *)

val write_complete_request :
  Bi_outbuf.t -> complete_request -> unit
  (** Output a JSON value of type {!complete_request}. *)

val string_of_complete_request :
  ?len:int -> complete_request -> string
  (** Serialize a value of type {!complete_request}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_complete_request :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> complete_request
  (** Input JSON data of type {!complete_request}. *)

val complete_request_of_string :
  string -> complete_request
  (** Deserialize JSON data of type {!complete_request}. *)

val write_complete_reply :
  Bi_outbuf.t -> complete_reply -> unit
  (** Output a JSON value of type {!complete_reply}. *)

val string_of_complete_reply :
  ?len:int -> complete_reply -> string
  (** Serialize a value of type {!complete_reply}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_complete_reply :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> complete_reply
  (** Input JSON data of type {!complete_reply}. *)

val complete_reply_of_string :
  string -> complete_reply
  (** Deserialize JSON data of type {!complete_reply}. *)

val write_clear_output :
  Bi_outbuf.t -> clear_output -> unit
  (** Output a JSON value of type {!clear_output}. *)

val string_of_clear_output :
  ?len:int -> clear_output -> string
  (** Serialize a value of type {!clear_output}
      into a JSON string.
      @param len specifies the initial length
                 of the buffer used internally.
                 Default: 1024. *)

val read_clear_output :
  Yojson.Safe.lexer_state -> Lexing.lexbuf -> clear_output
  (** Input JSON data of type {!clear_output}. *)

val clear_output_of_string :
  string -> clear_output
  (** Deserialize JSON data of type {!clear_output}. *)


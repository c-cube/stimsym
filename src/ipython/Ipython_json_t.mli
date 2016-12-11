(* Auto-generated from "Ipython_json.atd" *)


type stream = { st_name: string; st_data: string }

type status = { execution_state: string }

type shutdown = { restart: bool }

type dyn = Yojson.Safe.json

type pyout = { po_execution_count: int; po_data: dyn; po_metadata: dyn }

type pyin = { pi_code: string; pi_execution_count: int }

type payload = {
  html: string;
  source: string;
  start_line_number: int;
  text: string
}

type object_info_request = { oname: string; detail_level: int }

type arg_spec = {
  args: string list;
  varargs: string;
  varkw: string;
  defaults: string list
}

type object_info_reply = {
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

type kernel_info_reply = {
  protocol_version: int list;
  language_version: int list;
  language: string
}

type history_request = {
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

type history_reply = { history: string list }

type header_info = {
  date: string;
  username: string;
  session: string;
  msg_id: string;
  msg_type: string
}

type execute_request = {
  code: string;
  silent: bool;
  store_history: bool;
  user_expressions: dyn;
  allow_stdin: bool
}

type execute_reply = {
  status: string;
  execution_count: int;
  ename: string option;
  evalue: string option;
  traceback: string list option;
  payload: payload list option;
  er_user_expressions: dyn option
}

type display_data = { dd_source: string; dd_data: dyn; dd_metadata: dyn }

type connection_info = {
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

type connect_reply = {
  cr_shell_port: int;
  cr_iopub_port: int;
  cr_stdin_port: int;
  cr_hb_port: int
}

type complete_request = { line: string; cursor_pos: int }

type complete_reply = {
  matches: string list;
  cursor_start: int;
  cursor_end: int;
  cr_status: string
}

type clear_output = { wait: bool; stdout: bool; stderr: bool; other: bool }

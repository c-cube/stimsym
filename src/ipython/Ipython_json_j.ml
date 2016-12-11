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
  cursor_start: int;
  cursor_end: int;
  cr_status: string
}

type clear_output = Ipython_json_t.clear_output = {
  wait: bool;
  stdout: bool;
  stderr: bool;
  other: bool
}

let write_stream : _ -> stream -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.st_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"data\":";
    (
      Yojson.Safe.write_string
    )
      ob x.st_data;
    Bi_outbuf.add_char ob '}';
)
let string_of_stream ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_stream ob x;
  Bi_outbuf.contents ob
let read_stream = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_st_name = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_st_data = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 4 then (
            match String.unsafe_get s pos with
              | 'd' -> (
                  if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 'n' -> (
                  if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_st_name := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_st_data := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 4 then (
              match String.unsafe_get s pos with
                | 'd' -> (
                    if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                      1
                    )
                    else (
                      -1
                    )
                  )
                | 'n' -> (
                    if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                      0
                    )
                    else (
                      -1
                    )
                  )
                | _ -> (
                    -1
                  )
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_st_name := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_st_data := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields p [| !bits0 |] [| "st_name"; "st_data" |];
        (
          {
            st_name = !field_st_name;
            st_data = !field_st_data;
          }
         : stream)
      )
)
let stream_of_string s =
  read_stream (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_status : _ -> status -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"execution_state\":";
    (
      Yojson.Safe.write_string
    )
      ob x.execution_state;
    Bi_outbuf.add_char ob '}';
)
let string_of_status ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_status ob x;
  Bi_outbuf.contents ob
let read_status = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_execution_state = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 15 && String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'x' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'e' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_execution_state := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 15 && String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'x' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'a' && String.unsafe_get s (pos+13) = 't' && String.unsafe_get s (pos+14) = 'e' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_execution_state := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Ag_oj_run.missing_fields p [| !bits0 |] [| "execution_state" |];
        (
          {
            execution_state = !field_execution_state;
          }
         : status)
      )
)
let status_of_string s =
  read_status (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_shutdown : _ -> shutdown -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"restart\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.restart;
    Bi_outbuf.add_char ob '}';
)
let string_of_shutdown ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_shutdown ob x;
  Bi_outbuf.contents ob
let read_shutdown = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_restart = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 7 && String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 't' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_restart := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 7 && String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 't' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_restart := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Ag_oj_run.missing_fields p [| !bits0 |] [| "restart" |];
        (
          {
            restart = !field_restart;
          }
         : shutdown)
      )
)
let shutdown_of_string s =
  read_shutdown (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_dyn = (
  Yojson.Safe.write_json
)
let string_of_dyn ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_dyn ob x;
  Bi_outbuf.contents ob
let read_dyn = (
  Yojson.Safe.read_json
)
let dyn_of_string s =
  read_dyn (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_pyout : _ -> pyout -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"execution_count\":";
    (
      Yojson.Safe.write_int
    )
      ob x.po_execution_count;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"data\":";
    (
      write_dyn
    )
      ob x.po_data;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"metadata\":";
    (
      write_dyn
    )
      ob x.po_metadata;
    Bi_outbuf.add_char ob '}';
)
let string_of_pyout ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_pyout ob x;
  Bi_outbuf.contents ob
let read_pyout = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_po_execution_count = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_po_data = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_po_metadata = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                  1
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'a' then (
                  2
                )
                else (
                  -1
                )
              )
            | 15 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'x' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 't' then (
                  0
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_po_execution_count := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_po_data := (
              (
                read_dyn
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_po_metadata := (
              (
                read_dyn
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'a' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 15 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'x' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_po_execution_count := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_po_data := (
                (
                  read_dyn
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_po_metadata := (
                (
                  read_dyn
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x7 then Ag_oj_run.missing_fields p [| !bits0 |] [| "po_execution_count"; "po_data"; "po_metadata" |];
        (
          {
            po_execution_count = !field_po_execution_count;
            po_data = !field_po_data;
            po_metadata = !field_po_metadata;
          }
         : pyout)
      )
)
let pyout_of_string s =
  read_pyout (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_pyin : _ -> pyin -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"code\":";
    (
      Yojson.Safe.write_string
    )
      ob x.pi_code;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"execution_count\":";
    (
      Yojson.Safe.write_int
    )
      ob x.pi_execution_count;
    Bi_outbuf.add_char ob '}';
)
let string_of_pyin ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_pyin ob x;
  Bi_outbuf.contents ob
let read_pyin = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_pi_code = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_pi_execution_count = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 15 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'x' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 't' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_pi_code := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_pi_execution_count := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 15 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'x' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 't' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_pi_code := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_pi_execution_count := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields p [| !bits0 |] [| "pi_code"; "pi_execution_count" |];
        (
          {
            pi_code = !field_pi_code;
            pi_execution_count = !field_pi_execution_count;
          }
         : pyin)
      )
)
let pyin_of_string s =
  read_pyin (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_payload : _ -> payload -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"html\":";
    (
      Yojson.Safe.write_string
    )
      ob x.html;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"source\":";
    (
      Yojson.Safe.write_string
    )
      ob x.source;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"start_line_number\":";
    (
      Yojson.Safe.write_int
    )
      ob x.start_line_number;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"text\":";
    (
      Yojson.Safe.write_string
    )
      ob x.text;
    Bi_outbuf.add_char ob '}';
)
let string_of_payload ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_payload ob x;
  Bi_outbuf.contents ob
let read_payload = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_html = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_source = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_start_line_number = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_text = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                match String.unsafe_get s pos with
                  | 'h' -> (
                      if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'l' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = 't' then (
                        3
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 6 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'e' then (
                  1
                )
                else (
                  -1
                )
              )
            | 17 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'm' && String.unsafe_get s (pos+14) = 'b' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 'r' then (
                  2
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_html := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_source := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_start_line_number := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            field_text := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  match String.unsafe_get s pos with
                    | 'h' -> (
                        if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'l' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'x' && String.unsafe_get s (pos+3) = 't' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'e' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 17 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = '_' && String.unsafe_get s (pos+11) = 'n' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'm' && String.unsafe_get s (pos+14) = 'b' && String.unsafe_get s (pos+15) = 'e' && String.unsafe_get s (pos+16) = 'r' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_html := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_source := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_start_line_number := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              field_text := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0xf then Ag_oj_run.missing_fields p [| !bits0 |] [| "html"; "source"; "start_line_number"; "text" |];
        (
          {
            html = !field_html;
            source = !field_source;
            start_line_number = !field_start_line_number;
            text = !field_text;
          }
         : payload)
      )
)
let payload_of_string s =
  read_payload (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_object_info_request : _ -> object_info_request -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"oname\":";
    (
      Yojson.Safe.write_string
    )
      ob x.oname;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"detail_level\":";
    (
      Yojson.Safe.write_int
    )
      ob x.detail_level;
    Bi_outbuf.add_char ob '}';
)
let string_of_object_info_request ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_object_info_request ob x;
  Bi_outbuf.contents ob
let read_object_info_request = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_oname = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_detail_level = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 5 -> (
                if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 12 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'v' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'l' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_oname := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_detail_level := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 5 -> (
                  if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 12 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 'v' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'l' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_oname := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_detail_level := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields p [| !bits0 |] [| "oname"; "detail_level" |];
        (
          {
            oname = !field_oname;
            detail_level = !field_detail_level;
          }
         : object_info_request)
      )
)
let object_info_request_of_string s =
  read_object_info_request (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__2 = (
  Ag_oj_run.write_list (
    Yojson.Safe.write_string
  )
)
let string_of__2 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__2 ob x;
  Bi_outbuf.contents ob
let read__2 = (
  Ag_oj_run.read_list (
    Ag_oj_run.read_string
  )
)
let _2_of_string s =
  read__2 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_arg_spec : _ -> arg_spec -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"args\":";
    (
      write__2
    )
      ob x.args;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"varargs\":";
    (
      Yojson.Safe.write_string
    )
      ob x.varargs;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"varkw\":";
    (
      Yojson.Safe.write_string
    )
      ob x.varkw;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"defaults\":";
    (
      write__2
    )
      ob x.defaults;
    Bi_outbuf.add_char ob '}';
)
let string_of_arg_spec ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_arg_spec ob x;
  Bi_outbuf.contents ob
let read_arg_spec = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_args = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_varargs = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_varkw = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_defaults = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'k' && String.unsafe_get s (pos+4) = 'w' then (
                  2
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'f' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 's' then (
                  3
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_args := (
              (
                read__2
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_varargs := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_varkw := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            field_defaults := (
              (
                read__2
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'k' && String.unsafe_get s (pos+4) = 'w' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'v' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'f' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'l' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 's' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_args := (
                (
                  read__2
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_varargs := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_varkw := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              field_defaults := (
                (
                  read__2
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0xf then Ag_oj_run.missing_fields p [| !bits0 |] [| "args"; "varargs"; "varkw"; "defaults" |];
        (
          {
            args = !field_args;
            varargs = !field_varargs;
            varkw = !field_varkw;
            defaults = !field_defaults;
          }
         : arg_spec)
      )
)
let arg_spec_of_string s =
  read_arg_spec (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_object_info_reply : _ -> object_info_reply -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"found\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.found;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"ismagic\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.ismagic;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"isalias\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.isalias;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"namespace\":";
    (
      Yojson.Safe.write_string
    )
      ob x.namespace;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"type_name\":";
    (
      Yojson.Safe.write_string
    )
      ob x.type_name;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"string_form\":";
    (
      Yojson.Safe.write_string
    )
      ob x.string_form;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"base_class\":";
    (
      Yojson.Safe.write_string
    )
      ob x.base_class;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"length\":";
    (
      Yojson.Safe.write_int
    )
      ob x.length;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"file\":";
    (
      Yojson.Safe.write_string
    )
      ob x.oi_file;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"definition\":";
    (
      Yojson.Safe.write_string
    )
      ob x.definition;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"argspec\":";
    (
      write_arg_spec
    )
      ob x.argspec;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"init_definition\":";
    (
      Yojson.Safe.write_string
    )
      ob x.init_definition;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"docstring\":";
    (
      Yojson.Safe.write_string
    )
      ob x.docstring;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"class_docstring\":";
    (
      Yojson.Safe.write_string
    )
      ob x.class_docstring;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"call_def\":";
    (
      Yojson.Safe.write_string
    )
      ob x.call_def;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"call_docstring\":";
    (
      Yojson.Safe.write_string
    )
      ob x.call_docstring;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"source\":";
    (
      Yojson.Safe.write_string
    )
      ob x.oi_source;
    Bi_outbuf.add_char ob '}';
)
let string_of_object_info_reply ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_object_info_reply ob x;
  Bi_outbuf.contents ob
let read_object_info_reply = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_name = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_found = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_ismagic = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_isalias = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_namespace = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_type_name = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_string_form = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_base_class = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_length = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_oi_file = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_definition = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_argspec = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_init_definition = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_docstring = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_class_docstring = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_call_def = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_call_docstring = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_oi_source = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                match String.unsafe_get s pos with
                  | 'f' -> (
                      if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' then (
                        9
                      )
                      else (
                        -1
                      )
                    )
                  | 'n' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'd' then (
                  1
                )
                else (
                  -1
                )
              )
            | 6 -> (
                match String.unsafe_get s pos with
                  | 'l' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'h' then (
                        8
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'e' then (
                        17
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 7 -> (
                match String.unsafe_get s pos with
                  | 'a' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'p' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'c' then (
                        11
                      )
                      else (
                        -1
                      )
                    )
                  | 'i' -> (
                      if String.unsafe_get s (pos+1) = 's' then (
                        match String.unsafe_get s (pos+2) with
                          | 'a' -> (
                              if String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 's' then (
                                3
                              )
                              else (
                                -1
                              )
                            )
                          | 'm' -> (
                              if String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'c' then (
                                2
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'f' then (
                  15
                )
                else (
                  -1
                )
              )
            | 9 -> (
                match String.unsafe_get s pos with
                  | 'd' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 'g' then (
                        13
                      )
                      else (
                        -1
                      )
                    )
                  | 'n' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'p' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'e' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | 't' -> (
                      if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'e' then (
                        5
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 10 -> (
                match String.unsafe_get s pos with
                  | 'b' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = 's' then (
                        7
                      )
                      else (
                        -1
                      )
                    )
                  | 'd' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'f' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'n' then (
                        10
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 11 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'f' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'm' then (
                  6
                )
                else (
                  -1
                )
              )
            | 14 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'n' && String.unsafe_get s (pos+13) = 'g' then (
                  16
                )
                else (
                  -1
                )
              )
            | 15 -> (
                match String.unsafe_get s pos with
                  | 'c' -> (
                      if String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 'g' then (
                        14
                      )
                      else (
                        -1
                      )
                    )
                  | 'i' -> (
                      if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'f' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'n' then (
                        12
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_name := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_found := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_ismagic := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            field_isalias := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | 4 ->
            field_namespace := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x10;
          | 5 ->
            field_type_name := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x20;
          | 6 ->
            field_string_form := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x40;
          | 7 ->
            field_base_class := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x80;
          | 8 ->
            field_length := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x100;
          | 9 ->
            field_oi_file := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x200;
          | 10 ->
            field_definition := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x400;
          | 11 ->
            field_argspec := (
              (
                read_arg_spec
              ) p lb
            );
            bits0 := !bits0 lor 0x800;
          | 12 ->
            field_init_definition := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1000;
          | 13 ->
            field_docstring := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x2000;
          | 14 ->
            field_class_docstring := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x4000;
          | 15 ->
            field_call_def := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x8000;
          | 16 ->
            field_call_docstring := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x10000;
          | 17 ->
            field_oi_source := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x20000;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  match String.unsafe_get s pos with
                    | 'f' -> (
                        if String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' then (
                          9
                        )
                        else (
                          -1
                        )
                      )
                    | 'n' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'f' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'd' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  match String.unsafe_get s pos with
                    | 'l' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'h' then (
                          8
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'e' then (
                          17
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 7 -> (
                  match String.unsafe_get s pos with
                    | 'a' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'p' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'c' then (
                          11
                        )
                        else (
                          -1
                        )
                      )
                    | 'i' -> (
                        if String.unsafe_get s (pos+1) = 's' then (
                          match String.unsafe_get s (pos+2) with
                            | 'a' -> (
                                if String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 's' then (
                                  3
                                )
                                else (
                                  -1
                                )
                              )
                            | 'm' -> (
                                if String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'g' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 'c' then (
                                  2
                                )
                                else (
                                  -1
                                )
                              )
                            | _ -> (
                                -1
                              )
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'f' then (
                    15
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  match String.unsafe_get s pos with
                    | 'd' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'c' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'n' && String.unsafe_get s (pos+8) = 'g' then (
                          13
                        )
                        else (
                          -1
                        )
                      )
                    | 'n' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'p' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'e' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | 't' -> (
                        if String.unsafe_get s (pos+1) = 'y' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'n' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'm' && String.unsafe_get s (pos+8) = 'e' then (
                          5
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 10 -> (
                  match String.unsafe_get s pos with
                    | 'b' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = 'a' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = 's' then (
                          7
                        )
                        else (
                          -1
                        )
                      )
                    | 'd' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'f' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 'i' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'n' then (
                          10
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 11 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 'g' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'f' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 'r' && String.unsafe_get s (pos+10) = 'm' then (
                    6
                  )
                  else (
                    -1
                  )
                )
              | 14 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 'i' && String.unsafe_get s (pos+12) = 'n' && String.unsafe_get s (pos+13) = 'g' then (
                    16
                  )
                  else (
                    -1
                  )
                )
              | 15 -> (
                  match String.unsafe_get s pos with
                    | 'c' -> (
                        if String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'd' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'c' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 't' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 'g' then (
                          14
                        )
                        else (
                          -1
                        )
                      )
                    | 'i' -> (
                        if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'd' && String.unsafe_get s (pos+6) = 'e' && String.unsafe_get s (pos+7) = 'f' && String.unsafe_get s (pos+8) = 'i' && String.unsafe_get s (pos+9) = 'n' && String.unsafe_get s (pos+10) = 'i' && String.unsafe_get s (pos+11) = 't' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'n' then (
                          12
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_name := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_found := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_ismagic := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              field_isalias := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | 4 ->
              field_namespace := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x10;
            | 5 ->
              field_type_name := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x20;
            | 6 ->
              field_string_form := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x40;
            | 7 ->
              field_base_class := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x80;
            | 8 ->
              field_length := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x100;
            | 9 ->
              field_oi_file := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x200;
            | 10 ->
              field_definition := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x400;
            | 11 ->
              field_argspec := (
                (
                  read_arg_spec
                ) p lb
              );
              bits0 := !bits0 lor 0x800;
            | 12 ->
              field_init_definition := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1000;
            | 13 ->
              field_docstring := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x2000;
            | 14 ->
              field_class_docstring := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x4000;
            | 15 ->
              field_call_def := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x8000;
            | 16 ->
              field_call_docstring := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x10000;
            | 17 ->
              field_oi_source := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x20000;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3ffff then Ag_oj_run.missing_fields p [| !bits0 |] [| "name"; "found"; "ismagic"; "isalias"; "namespace"; "type_name"; "string_form"; "base_class"; "length"; "oi_file"; "definition"; "argspec"; "init_definition"; "docstring"; "class_docstring"; "call_def"; "call_docstring"; "oi_source" |];
        (
          {
            name = !field_name;
            found = !field_found;
            ismagic = !field_ismagic;
            isalias = !field_isalias;
            namespace = !field_namespace;
            type_name = !field_type_name;
            string_form = !field_string_form;
            base_class = !field_base_class;
            length = !field_length;
            oi_file = !field_oi_file;
            definition = !field_definition;
            argspec = !field_argspec;
            init_definition = !field_init_definition;
            docstring = !field_docstring;
            class_docstring = !field_class_docstring;
            call_def = !field_call_def;
            call_docstring = !field_call_docstring;
            oi_source = !field_oi_source;
          }
         : object_info_reply)
      )
)
let object_info_reply_of_string s =
  read_object_info_reply (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__7 = (
  Ag_oj_run.write_list (
    Yojson.Safe.write_int
  )
)
let string_of__7 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__7 ob x;
  Bi_outbuf.contents ob
let read__7 = (
  Ag_oj_run.read_list (
    Ag_oj_run.read_int
  )
)
let _7_of_string s =
  read__7 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_kernel_info_reply : _ -> kernel_info_reply -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"protocol_version\":";
    (
      write__7
    )
      ob x.protocol_version;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"language_version\":";
    (
      write__7
    )
      ob x.language_version;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"language\":";
    (
      Yojson.Safe.write_string
    )
      ob x.language;
    Bi_outbuf.add_char ob '}';
)
let string_of_kernel_info_reply ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_kernel_info_reply ob x;
  Bi_outbuf.contents ob
let read_kernel_info_reply = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_protocol_version = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_language_version = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_language = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 8 -> (
                if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 'e' then (
                  2
                )
                else (
                  -1
                )
              )
            | 16 -> (
                match String.unsafe_get s pos with
                  | 'l' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'v' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'o' && String.unsafe_get s (pos+15) = 'n' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'v' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'o' && String.unsafe_get s (pos+15) = 'n' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_protocol_version := (
              (
                read__7
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_language_version := (
              (
                read__7
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_language := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 8 -> (
                  if String.unsafe_get s pos = 'l' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 'e' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 16 -> (
                  match String.unsafe_get s pos with
                    | 'l' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'g' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'g' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'v' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'o' && String.unsafe_get s (pos+15) = 'n' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'c' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'l' && String.unsafe_get s (pos+8) = '_' && String.unsafe_get s (pos+9) = 'v' && String.unsafe_get s (pos+10) = 'e' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 's' && String.unsafe_get s (pos+13) = 'i' && String.unsafe_get s (pos+14) = 'o' && String.unsafe_get s (pos+15) = 'n' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_protocol_version := (
                (
                  read__7
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_language_version := (
                (
                  read__7
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_language := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x7 then Ag_oj_run.missing_fields p [| !bits0 |] [| "protocol_version"; "language_version"; "language" |];
        (
          {
            protocol_version = !field_protocol_version;
            language_version = !field_language_version;
            language = !field_language;
          }
         : kernel_info_reply)
      )
)
let kernel_info_reply_of_string s =
  read_kernel_info_reply (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_history_request : _ -> history_request -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"output\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.output;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"raw\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.raw;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"hist_access_type\":";
    (
      Yojson.Safe.write_string
    )
      ob x.hist_access_type;
    if x.hr_session <> 0 then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"session\":";
      (
        Yojson.Safe.write_int
      )
        ob x.hr_session;
    );
    if x.start <> 0 then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"start\":";
      (
        Yojson.Safe.write_int
      )
        ob x.start;
    );
    if x.stop <> 0 then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"stop\":";
      (
        Yojson.Safe.write_int
      )
        ob x.stop;
    );
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"n\":";
    (
      Yojson.Safe.write_int
    )
      ob x.n;
    if x.pattern <> "" then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"pattern\":";
      (
        Yojson.Safe.write_string
      )
        ob x.pattern;
    );
    if x.unique <> false then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"unique\":";
      (
        Yojson.Safe.write_bool
      )
        ob x.unique;
    );
    Bi_outbuf.add_char ob '}';
)
let string_of_history_request ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_history_request ob x;
  Bi_outbuf.contents ob
let read_history_request = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_output = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_raw = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_hist_access_type = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_hr_session = ref (0) in
    let field_start = ref (0) in
    let field_stop = ref (0) in
    let field_n = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_pattern = ref ("") in
    let field_unique = ref (false) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 1 -> (
                if String.unsafe_get s pos = 'n' then (
                  6
                )
                else (
                  -1
                )
              )
            | 3 -> (
                if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'w' then (
                  1
                )
                else (
                  -1
                )
              )
            | 4 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'p' then (
                  5
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 't' then (
                  4
                )
                else (
                  -1
                )
              )
            | 6 -> (
                match String.unsafe_get s pos with
                  | 'o' -> (
                      if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | 'u' -> (
                      if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'q' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'e' then (
                        8
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 7 -> (
                match String.unsafe_get s pos with
                  | 'p' -> (
                      if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'n' then (
                        7
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'n' then (
                        3
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 16 -> (
                if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = 'y' && String.unsafe_get s (pos+14) = 'p' && String.unsafe_get s (pos+15) = 'e' then (
                  2
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_output := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_raw := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_hist_access_type := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_hr_session := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
            )
          | 4 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_start := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
            )
          | 5 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_stop := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
            )
          | 6 ->
            field_n := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | 7 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_pattern := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
            )
          | 8 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_unique := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 1 -> (
                  if String.unsafe_get s pos = 'n' then (
                    6
                  )
                  else (
                    -1
                  )
                )
              | 3 -> (
                  if String.unsafe_get s pos = 'r' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'w' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 4 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'p' then (
                    5
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 't' then (
                    4
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  match String.unsafe_get s pos with
                    | 'o' -> (
                        if String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | 'u' -> (
                        if String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 'q' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'e' then (
                          8
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 7 -> (
                  match String.unsafe_get s pos with
                    | 'p' -> (
                        if String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'n' then (
                          7
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'n' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 16 -> (
                  if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'c' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = 's' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = '_' && String.unsafe_get s (pos+12) = 't' && String.unsafe_get s (pos+13) = 'y' && String.unsafe_get s (pos+14) = 'p' && String.unsafe_get s (pos+15) = 'e' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_output := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_raw := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_hist_access_type := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_hr_session := (
                  (
                    Ag_oj_run.read_int
                  ) p lb
                );
              )
            | 4 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_start := (
                  (
                    Ag_oj_run.read_int
                  ) p lb
                );
              )
            | 5 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_stop := (
                  (
                    Ag_oj_run.read_int
                  ) p lb
                );
              )
            | 6 ->
              field_n := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | 7 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_pattern := (
                  (
                    Ag_oj_run.read_string
                  ) p lb
                );
              )
            | 8 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_unique := (
                  (
                    Ag_oj_run.read_bool
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0xf then Ag_oj_run.missing_fields p [| !bits0 |] [| "output"; "raw"; "hist_access_type"; "n" |];
        (
          {
            output = !field_output;
            raw = !field_raw;
            hist_access_type = !field_hist_access_type;
            hr_session = !field_hr_session;
            start = !field_start;
            stop = !field_stop;
            n = !field_n;
            pattern = !field_pattern;
            unique = !field_unique;
          }
         : history_request)
      )
)
let history_request_of_string s =
  read_history_request (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_history_reply : _ -> history_reply -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"history\":";
    (
      write__2
    )
      ob x.history;
    Bi_outbuf.add_char ob '}';
)
let string_of_history_reply ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_history_reply ob x;
  Bi_outbuf.contents ob
let read_history_reply = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_history = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          if len = 7 && String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'y' then (
            0
          )
          else (
            -1
          )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_history := (
              (
                read__2
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            if len = 7 && String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 'y' then (
              0
            )
            else (
              -1
            )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_history := (
                (
                  read__2
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1 then Ag_oj_run.missing_fields p [| !bits0 |] [| "history" |];
        (
          {
            history = !field_history;
          }
         : history_reply)
      )
)
let history_reply_of_string s =
  read_history_reply (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_header_info : _ -> header_info -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if x.date <> "" then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"date\":";
      (
        Yojson.Safe.write_string
      )
        ob x.date;
    );
    if x.username <> "" then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"username\":";
      (
        Yojson.Safe.write_string
      )
        ob x.username;
    );
    if x.session <> "" then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"session\":";
      (
        Yojson.Safe.write_string
      )
        ob x.session;
    );
    if x.msg_id <> "" then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"msg_id\":";
      (
        Yojson.Safe.write_string
      )
        ob x.msg_id;
    );
    if x.msg_type <> "" then (
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"msg_type\":";
      (
        Yojson.Safe.write_string
      )
        ob x.msg_type;
    );
    Bi_outbuf.add_char ob '}';
)
let string_of_header_info ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_header_info ob x;
  Bi_outbuf.contents ob
let read_header_info = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_date = ref ("") in
    let field_username = ref ("") in
    let field_session = ref ("") in
    let field_msg_id = ref ("") in
    let field_msg_type = ref ("") in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'd' then (
                  3
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'n' then (
                  2
                )
                else (
                  -1
                )
              )
            | 8 -> (
                match String.unsafe_get s pos with
                  | 'm' -> (
                      if String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'y' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'e' then (
                        4
                      )
                      else (
                        -1
                      )
                    )
                  | 'u' -> (
                      if String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = 'e' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_date := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
            )
          | 1 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_username := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
            )
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_session := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
            )
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_msg_id := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
            )
          | 4 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_msg_type := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'd' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 's' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'i' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'n' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  match String.unsafe_get s pos with
                    | 'm' -> (
                        if String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = '_' && String.unsafe_get s (pos+4) = 't' && String.unsafe_get s (pos+5) = 'y' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'e' then (
                          4
                        )
                        else (
                          -1
                        )
                      )
                    | 'u' -> (
                        if String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'm' && String.unsafe_get s (pos+7) = 'e' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_date := (
                  (
                    Ag_oj_run.read_string
                  ) p lb
                );
              )
            | 1 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_username := (
                  (
                    Ag_oj_run.read_string
                  ) p lb
                );
              )
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_session := (
                  (
                    Ag_oj_run.read_string
                  ) p lb
                );
              )
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_msg_id := (
                  (
                    Ag_oj_run.read_string
                  ) p lb
                );
              )
            | 4 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_msg_type := (
                  (
                    Ag_oj_run.read_string
                  ) p lb
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        (
          {
            date = !field_date;
            username = !field_username;
            session = !field_session;
            msg_id = !field_msg_id;
            msg_type = !field_msg_type;
          }
         : header_info)
      )
)
let header_info_of_string s =
  read_header_info (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_execute_request : _ -> execute_request -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"code\":";
    (
      Yojson.Safe.write_string
    )
      ob x.code;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"silent\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.silent;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"store_history\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.store_history;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"user_expressions\":";
    (
      write_dyn
    )
      ob x.user_expressions;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"allow_stdin\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.allow_stdin;
    Bi_outbuf.add_char ob '}';
)
let string_of_execute_request ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_execute_request ob x;
  Bi_outbuf.contents ob
let read_execute_request = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_code = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_silent = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_store_history = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_user_expressions = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_allow_stdin = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 't' then (
                  1
                )
                else (
                  -1
                )
              )
            | 11 -> (
                if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'w' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'd' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'n' then (
                  4
                )
                else (
                  -1
                )
              )
            | 13 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'h' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 'y' then (
                  2
                )
                else (
                  -1
                )
              )
            | 16 -> (
                if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'x' && String.unsafe_get s (pos+7) = 'p' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 's' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 's' then (
                  3
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_code := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_silent := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_store_history := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            field_user_expressions := (
              (
                read_dyn
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | 4 ->
            field_allow_stdin := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x10;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = 't' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 11 -> (
                  if String.unsafe_get s pos = 'a' && String.unsafe_get s (pos+1) = 'l' && String.unsafe_get s (pos+2) = 'l' && String.unsafe_get s (pos+3) = 'o' && String.unsafe_get s (pos+4) = 'w' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 's' && String.unsafe_get s (pos+7) = 't' && String.unsafe_get s (pos+8) = 'd' && String.unsafe_get s (pos+9) = 'i' && String.unsafe_get s (pos+10) = 'n' then (
                    4
                  )
                  else (
                    -1
                  )
                )
              | 13 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'o' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'h' && String.unsafe_get s (pos+7) = 'i' && String.unsafe_get s (pos+8) = 's' && String.unsafe_get s (pos+9) = 't' && String.unsafe_get s (pos+10) = 'o' && String.unsafe_get s (pos+11) = 'r' && String.unsafe_get s (pos+12) = 'y' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 16 -> (
                  if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'x' && String.unsafe_get s (pos+7) = 'p' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 's' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 's' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_code := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_silent := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_store_history := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              field_user_expressions := (
                (
                  read_dyn
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | 4 ->
              field_allow_stdin := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x10;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1f then Ag_oj_run.missing_fields p [| !bits0 |] [| "code"; "silent"; "store_history"; "user_expressions"; "allow_stdin" |];
        (
          {
            code = !field_code;
            silent = !field_silent;
            store_history = !field_store_history;
            user_expressions = !field_user_expressions;
            allow_stdin = !field_allow_stdin;
          }
         : execute_request)
      )
)
let execute_request_of_string s =
  read_execute_request (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__6 = (
  Ag_oj_run.write_std_option (
    write_dyn
  )
)
let string_of__6 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__6 ob x;
  Bi_outbuf.contents ob
let read__6 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 then (
                  match String.unsafe_get s pos with
                    | 'N' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | 'S' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          1
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | _ -> (
                        raise (Exit)
                      )
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | 1 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read_dyn
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'N' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              (None : _ option)
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read_dyn
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
)
let _6_of_string s =
  read__6 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__4 = (
  Ag_oj_run.write_list (
    write_payload
  )
)
let string_of__4 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__4 ob x;
  Bi_outbuf.contents ob
let read__4 = (
  Ag_oj_run.read_list (
    read_payload
  )
)
let _4_of_string s =
  read__4 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__5 = (
  Ag_oj_run.write_std_option (
    write__4
  )
)
let string_of__5 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__5 ob x;
  Bi_outbuf.contents ob
let read__5 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 then (
                  match String.unsafe_get s pos with
                    | 'N' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | 'S' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          1
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | _ -> (
                        raise (Exit)
                      )
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | 1 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read__4
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'N' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              (None : _ option)
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__4
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
)
let _5_of_string s =
  read__5 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__3 = (
  Ag_oj_run.write_std_option (
    write__2
  )
)
let string_of__3 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__3 ob x;
  Bi_outbuf.contents ob
let read__3 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 then (
                  match String.unsafe_get s pos with
                    | 'N' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | 'S' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          1
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | _ -> (
                        raise (Exit)
                      )
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | 1 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  read__2
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'N' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              (None : _ option)
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  read__2
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
)
let _3_of_string s =
  read__3 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write__1 = (
  Ag_oj_run.write_std_option (
    Yojson.Safe.write_string
  )
)
let string_of__1 ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write__1 ob x;
  Bi_outbuf.contents ob
let read__1 = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    
    match Yojson.Safe.start_any_variant p lb with
      | `Edgy_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 then (
                  match String.unsafe_get s pos with
                    | 'N' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                          0
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | 'S' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                          1
                        )
                        else (
                          raise (Exit)
                        )
                      )
                    | _ -> (
                        raise (Exit)
                      )
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (None : _ option)
            | 1 ->
              Ag_oj_run.read_until_field_value p lb;
              let x = (
                  Ag_oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_gt p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
      | `Double_quote -> (
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'N' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_string p f lb in
          match i with
            | 0 ->
              (None : _ option)
            | _ -> (
                assert false
              )
        )
      | `Square_bracket -> (
          Yojson.Safe.read_space p lb;
          let f =
            fun s pos len ->
              if pos < 0 || len < 0 || pos + len > String.length s then
                invalid_arg "out-of-bounds substring position or length";
              try
                if len = 4 && String.unsafe_get s pos = 'S' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'm' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  raise (Exit)
                )
              with Exit -> (
                  Ag_oj_run.invalid_variant_tag p (String.sub s pos len)
                )
          in
          let i = Yojson.Safe.map_ident p f lb in
          match i with
            | 0 ->
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_comma p lb;
              Yojson.Safe.read_space p lb;
              let x = (
                  Ag_oj_run.read_string
                ) p lb
              in
              Yojson.Safe.read_space p lb;
              Yojson.Safe.read_rbr p lb;
              (Some x : _ option)
            | _ -> (
                assert false
              )
        )
)
let _1_of_string s =
  read__1 (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_execute_reply : _ -> execute_reply -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"status\":";
    (
      Yojson.Safe.write_string
    )
      ob x.status;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"execution_count\":";
    (
      Yojson.Safe.write_int
    )
      ob x.execution_count;
    (match x.ename with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"ename\":";
      (
        Yojson.Safe.write_string
      )
        ob x;
    );
    (match x.evalue with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"evalue\":";
      (
        Yojson.Safe.write_string
      )
        ob x;
    );
    (match x.traceback with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"traceback\":";
      (
        write__2
      )
        ob x;
    );
    (match x.payload with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"payload\":";
      (
        write__4
      )
        ob x;
    );
    (match x.er_user_expressions with None -> () | Some x ->
      if !is_first then
        is_first := false
      else
        Bi_outbuf.add_char ob ',';
      Bi_outbuf.add_string ob "\"user_expressions\":";
      (
        write_dyn
      )
        ob x;
    );
    Bi_outbuf.add_char ob '}';
)
let string_of_execute_reply ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_execute_reply ob x;
  Bi_outbuf.contents ob
let read_execute_reply = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_status = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_execution_count = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_ename = ref (None) in
    let field_evalue = ref (None) in
    let field_traceback = ref (None) in
    let field_payload = ref (None) in
    let field_er_user_expressions = ref (None) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 5 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'e' then (
                  2
                )
                else (
                  -1
                )
              )
            | 6 -> (
                match String.unsafe_get s pos with
                  | 'e' -> (
                      if String.unsafe_get s (pos+1) = 'v' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'e' then (
                        3
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 's' then (
                        0
                      )
                      else (
                        -1
                      )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'y' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'd' then (
                  5
                )
                else (
                  -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'b' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'k' then (
                  4
                )
                else (
                  -1
                )
              )
            | 15 -> (
                if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'x' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 't' then (
                  1
                )
                else (
                  -1
                )
              )
            | 16 -> (
                if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'x' && String.unsafe_get s (pos+7) = 'p' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 's' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 's' then (
                  6
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_status := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_execution_count := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_ename := (
                Some (
                  (
                    Ag_oj_run.read_string
                  ) p lb
                )
              );
            )
          | 3 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_evalue := (
                Some (
                  (
                    Ag_oj_run.read_string
                  ) p lb
                )
              );
            )
          | 4 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_traceback := (
                Some (
                  (
                    read__2
                  ) p lb
                )
              );
            )
          | 5 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_payload := (
                Some (
                  (
                    read__4
                  ) p lb
                )
              );
            )
          | 6 ->
            if not (Yojson.Safe.read_null_if_possible p lb) then (
              field_er_user_expressions := (
                Some (
                  (
                    read_dyn
                  ) p lb
                )
              );
            )
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 5 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'n' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'm' && String.unsafe_get s (pos+4) = 'e' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  match String.unsafe_get s pos with
                    | 'e' -> (
                        if String.unsafe_get s (pos+1) = 'v' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 'e' then (
                          3
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        if String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 's' then (
                          0
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'p' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'y' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 'd' then (
                    5
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'e' && String.unsafe_get s (pos+5) = 'b' && String.unsafe_get s (pos+6) = 'a' && String.unsafe_get s (pos+7) = 'c' && String.unsafe_get s (pos+8) = 'k' then (
                    4
                  )
                  else (
                    -1
                  )
                )
              | 15 -> (
                  if String.unsafe_get s pos = 'e' && String.unsafe_get s (pos+1) = 'x' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'i' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 'c' && String.unsafe_get s (pos+11) = 'o' && String.unsafe_get s (pos+12) = 'u' && String.unsafe_get s (pos+13) = 'n' && String.unsafe_get s (pos+14) = 't' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 16 -> (
                  if String.unsafe_get s pos = 'u' && String.unsafe_get s (pos+1) = 's' && String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = '_' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 'x' && String.unsafe_get s (pos+7) = 'p' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 'e' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 's' && String.unsafe_get s (pos+12) = 'i' && String.unsafe_get s (pos+13) = 'o' && String.unsafe_get s (pos+14) = 'n' && String.unsafe_get s (pos+15) = 's' then (
                    6
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_status := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_execution_count := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_ename := (
                  Some (
                    (
                      Ag_oj_run.read_string
                    ) p lb
                  )
                );
              )
            | 3 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_evalue := (
                  Some (
                    (
                      Ag_oj_run.read_string
                    ) p lb
                  )
                );
              )
            | 4 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_traceback := (
                  Some (
                    (
                      read__2
                    ) p lb
                  )
                );
              )
            | 5 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_payload := (
                  Some (
                    (
                      read__4
                    ) p lb
                  )
                );
              )
            | 6 ->
              if not (Yojson.Safe.read_null_if_possible p lb) then (
                field_er_user_expressions := (
                  Some (
                    (
                      read_dyn
                    ) p lb
                  )
                );
              )
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields p [| !bits0 |] [| "status"; "execution_count" |];
        (
          {
            status = !field_status;
            execution_count = !field_execution_count;
            ename = !field_ename;
            evalue = !field_evalue;
            traceback = !field_traceback;
            payload = !field_payload;
            er_user_expressions = !field_er_user_expressions;
          }
         : execute_reply)
      )
)
let execute_reply_of_string s =
  read_execute_reply (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_display_data : _ -> display_data -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"source\":";
    (
      Yojson.Safe.write_string
    )
      ob x.dd_source;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"data\":";
    (
      write_dyn
    )
      ob x.dd_data;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"metadata\":";
    (
      write_dyn
    )
      ob x.dd_metadata;
    Bi_outbuf.add_char ob '}';
)
let string_of_display_data ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_display_data ob x;
  Bi_outbuf.contents ob
let read_display_data = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_dd_source = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_dd_data = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_dd_metadata = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                  1
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 8 -> (
                if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'a' then (
                  2
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_dd_source := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_dd_data := (
              (
                read_dyn
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_dd_metadata := (
              (
                read_dyn
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'd' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'u' && String.unsafe_get s (pos+3) = 'r' && String.unsafe_get s (pos+4) = 'c' && String.unsafe_get s (pos+5) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 8 -> (
                  if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'a' && String.unsafe_get s (pos+4) = 'd' && String.unsafe_get s (pos+5) = 'a' && String.unsafe_get s (pos+6) = 't' && String.unsafe_get s (pos+7) = 'a' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_dd_source := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_dd_data := (
                (
                  read_dyn
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_dd_metadata := (
                (
                  read_dyn
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x7 then Ag_oj_run.missing_fields p [| !bits0 |] [| "dd_source"; "dd_data"; "dd_metadata" |];
        (
          {
            dd_source = !field_dd_source;
            dd_data = !field_dd_data;
            dd_metadata = !field_dd_metadata;
          }
         : display_data)
      )
)
let display_data_of_string s =
  read_display_data (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_connection_info : _ -> connection_info -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"stdin_port\":";
    (
      Yojson.Safe.write_int
    )
      ob x.stdin_port;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"ip\":";
    (
      Yojson.Safe.write_string
    )
      ob x.ip;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"control_port\":";
    (
      Yojson.Safe.write_int
    )
      ob x.control_port;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"hb_port\":";
    (
      Yojson.Safe.write_int
    )
      ob x.hb_port;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"signature_scheme\":";
    (
      Yojson.Safe.write_string
    )
      ob x.signature_scheme;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"key\":";
    (
      Yojson.Safe.write_string
    )
      ob x.key;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"shell_port\":";
    (
      Yojson.Safe.write_int
    )
      ob x.shell_port;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"transport\":";
    (
      Yojson.Safe.write_string
    )
      ob x.transport;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"iopub_port\":";
    (
      Yojson.Safe.write_int
    )
      ob x.iopub_port;
    Bi_outbuf.add_char ob '}';
)
let string_of_connection_info ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_connection_info ob x;
  Bi_outbuf.contents ob
let read_connection_info = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_stdin_port = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_ip = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_control_port = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_hb_port = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_signature_scheme = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_key = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_shell_port = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_transport = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_iopub_port = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 2 -> (
                if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'p' then (
                  1
                )
                else (
                  -1
                )
              )
            | 3 -> (
                if String.unsafe_get s pos = 'k' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'y' then (
                  5
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 't' then (
                  3
                )
                else (
                  -1
                )
              )
            | 9 -> (
                if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'p' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 't' then (
                  7
                )
                else (
                  -1
                )
              )
            | 10 -> (
                match String.unsafe_get s pos with
                  | 'i' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'b' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 't' then (
                        8
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      match String.unsafe_get s (pos+1) with
                        | 'h' -> (
                            if String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 't' then (
                              6
                            )
                            else (
                              -1
                            )
                          )
                        | 't' -> (
                            if String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 't' then (
                              0
                            )
                            else (
                              -1
                            )
                          )
                        | _ -> (
                            -1
                          )
                    )
                  | _ -> (
                      -1
                    )
              )
            | 12 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'p' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 't' then (
                  2
                )
                else (
                  -1
                )
              )
            | 16 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 'c' && String.unsafe_get s (pos+12) = 'h' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 'm' && String.unsafe_get s (pos+15) = 'e' then (
                  4
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_stdin_port := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_ip := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_control_port := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            field_hb_port := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | 4 ->
            field_signature_scheme := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x10;
          | 5 ->
            field_key := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x20;
          | 6 ->
            field_shell_port := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x40;
          | 7 ->
            field_transport := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x80;
          | 8 ->
            field_iopub_port := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x100;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 2 -> (
                  if String.unsafe_get s pos = 'i' && String.unsafe_get s (pos+1) = 'p' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | 3 -> (
                  if String.unsafe_get s pos = 'k' && String.unsafe_get s (pos+1) = 'e' && String.unsafe_get s (pos+2) = 'y' then (
                    5
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 't' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 9 -> (
                  if String.unsafe_get s pos = 't' && String.unsafe_get s (pos+1) = 'r' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 's' && String.unsafe_get s (pos+5) = 'p' && String.unsafe_get s (pos+6) = 'o' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 't' then (
                    7
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  match String.unsafe_get s pos with
                    | 'i' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'b' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 't' then (
                          8
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        match String.unsafe_get s (pos+1) with
                          | 'h' -> (
                              if String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 't' then (
                                6
                              )
                              else (
                                -1
                              )
                            )
                          | 't' -> (
                              if String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 't' then (
                                0
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                    | _ -> (
                        -1
                      )
                )
              | 12 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'n' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'o' && String.unsafe_get s (pos+6) = 'l' && String.unsafe_get s (pos+7) = '_' && String.unsafe_get s (pos+8) = 'p' && String.unsafe_get s (pos+9) = 'o' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 't' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 16 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 'i' && String.unsafe_get s (pos+2) = 'g' && String.unsafe_get s (pos+3) = 'n' && String.unsafe_get s (pos+4) = 'a' && String.unsafe_get s (pos+5) = 't' && String.unsafe_get s (pos+6) = 'u' && String.unsafe_get s (pos+7) = 'r' && String.unsafe_get s (pos+8) = 'e' && String.unsafe_get s (pos+9) = '_' && String.unsafe_get s (pos+10) = 's' && String.unsafe_get s (pos+11) = 'c' && String.unsafe_get s (pos+12) = 'h' && String.unsafe_get s (pos+13) = 'e' && String.unsafe_get s (pos+14) = 'm' && String.unsafe_get s (pos+15) = 'e' then (
                    4
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_stdin_port := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_ip := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_control_port := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              field_hb_port := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | 4 ->
              field_signature_scheme := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x10;
            | 5 ->
              field_key := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x20;
            | 6 ->
              field_shell_port := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x40;
            | 7 ->
              field_transport := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x80;
            | 8 ->
              field_iopub_port := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x100;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x1ff then Ag_oj_run.missing_fields p [| !bits0 |] [| "stdin_port"; "ip"; "control_port"; "hb_port"; "signature_scheme"; "key"; "shell_port"; "transport"; "iopub_port" |];
        (
          {
            stdin_port = !field_stdin_port;
            ip = !field_ip;
            control_port = !field_control_port;
            hb_port = !field_hb_port;
            signature_scheme = !field_signature_scheme;
            key = !field_key;
            shell_port = !field_shell_port;
            transport = !field_transport;
            iopub_port = !field_iopub_port;
          }
         : connection_info)
      )
)
let connection_info_of_string s =
  read_connection_info (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_connect_reply : _ -> connect_reply -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"shell_port\":";
    (
      Yojson.Safe.write_int
    )
      ob x.cr_shell_port;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"iopub_port\":";
    (
      Yojson.Safe.write_int
    )
      ob x.cr_iopub_port;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"stdin_port\":";
    (
      Yojson.Safe.write_int
    )
      ob x.cr_stdin_port;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"hb_port\":";
    (
      Yojson.Safe.write_int
    )
      ob x.cr_hb_port;
    Bi_outbuf.add_char ob '}';
)
let string_of_connect_reply ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_connect_reply ob x;
  Bi_outbuf.contents ob
let read_connect_reply = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_cr_shell_port = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_cr_iopub_port = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_cr_stdin_port = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_cr_hb_port = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 7 -> (
                if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 't' then (
                  3
                )
                else (
                  -1
                )
              )
            | 10 -> (
                match String.unsafe_get s pos with
                  | 'i' -> (
                      if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'b' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 't' then (
                        1
                      )
                      else (
                        -1
                      )
                    )
                  | 's' -> (
                      match String.unsafe_get s (pos+1) with
                        | 'h' -> (
                            if String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 't' then (
                              0
                            )
                            else (
                              -1
                            )
                          )
                        | 't' -> (
                            if String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 't' then (
                              2
                            )
                            else (
                              -1
                            )
                          )
                        | _ -> (
                            -1
                          )
                    )
                  | _ -> (
                      -1
                    )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_cr_shell_port := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_cr_iopub_port := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_cr_stdin_port := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            field_cr_hb_port := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 7 -> (
                  if String.unsafe_get s pos = 'h' && String.unsafe_get s (pos+1) = 'b' && String.unsafe_get s (pos+2) = '_' && String.unsafe_get s (pos+3) = 'p' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = 't' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  match String.unsafe_get s pos with
                    | 'i' -> (
                        if String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'p' && String.unsafe_get s (pos+3) = 'u' && String.unsafe_get s (pos+4) = 'b' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 't' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | 's' -> (
                        match String.unsafe_get s (pos+1) with
                          | 'h' -> (
                              if String.unsafe_get s (pos+2) = 'e' && String.unsafe_get s (pos+3) = 'l' && String.unsafe_get s (pos+4) = 'l' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 't' then (
                                0
                              )
                              else (
                                -1
                              )
                            )
                          | 't' -> (
                              if String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'i' && String.unsafe_get s (pos+4) = 'n' && String.unsafe_get s (pos+5) = '_' && String.unsafe_get s (pos+6) = 'p' && String.unsafe_get s (pos+7) = 'o' && String.unsafe_get s (pos+8) = 'r' && String.unsafe_get s (pos+9) = 't' then (
                                2
                              )
                              else (
                                -1
                              )
                            )
                          | _ -> (
                              -1
                            )
                      )
                    | _ -> (
                        -1
                      )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_cr_shell_port := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_cr_iopub_port := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_cr_stdin_port := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              field_cr_hb_port := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0xf then Ag_oj_run.missing_fields p [| !bits0 |] [| "cr_shell_port"; "cr_iopub_port"; "cr_stdin_port"; "cr_hb_port" |];
        (
          {
            cr_shell_port = !field_cr_shell_port;
            cr_iopub_port = !field_cr_iopub_port;
            cr_stdin_port = !field_cr_stdin_port;
            cr_hb_port = !field_cr_hb_port;
          }
         : connect_reply)
      )
)
let connect_reply_of_string s =
  read_connect_reply (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_complete_request : _ -> complete_request -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"code\":";
    (
      Yojson.Safe.write_string
    )
      ob x.line;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"cursor_pos\":";
    (
      Yojson.Safe.write_int
    )
      ob x.cursor_pos;
    Bi_outbuf.add_char ob '}';
)
let string_of_complete_request ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_complete_request ob x;
  Bi_outbuf.contents ob
let read_complete_request = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_line = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_cursor_pos = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' then (
                  0
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'p' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 's' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_line := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_cursor_pos := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'o' && String.unsafe_get s (pos+2) = 'd' && String.unsafe_get s (pos+3) = 'e' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'p' && String.unsafe_get s (pos+8) = 'o' && String.unsafe_get s (pos+9) = 's' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_line := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_cursor_pos := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0x3 then Ag_oj_run.missing_fields p [| !bits0 |] [| "line"; "cursor_pos" |];
        (
          {
            line = !field_line;
            cursor_pos = !field_cursor_pos;
          }
         : complete_request)
      )
)
let complete_request_of_string s =
  read_complete_request (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_complete_reply : _ -> complete_reply -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"matches\":";
    (
      write__2
    )
      ob x.matches;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"cursor_start\":";
    (
      Yojson.Safe.write_int
    )
      ob x.cursor_start;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"cursor_end\":";
    (
      Yojson.Safe.write_int
    )
      ob x.cursor_end;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"status\":";
    (
      Yojson.Safe.write_string
    )
      ob x.cr_status;
    Bi_outbuf.add_char ob '}';
)
let string_of_complete_reply ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_complete_reply ob x;
  Bi_outbuf.contents ob
let read_complete_reply = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_matches = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_cursor_start = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_cursor_end = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_cr_status = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 6 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 's' then (
                  3
                )
                else (
                  -1
                )
              )
            | 7 -> (
                if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 's' then (
                  0
                )
                else (
                  -1
                )
              )
            | 10 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'd' then (
                  2
                )
                else (
                  -1
                )
              )
            | 12 -> (
                if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 't' then (
                  1
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_matches := (
              (
                read__2
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_cursor_start := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_cursor_end := (
              (
                Ag_oj_run.read_int
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            field_cr_status := (
              (
                Ag_oj_run.read_string
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 6 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'a' && String.unsafe_get s (pos+3) = 't' && String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 's' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 7 -> (
                  if String.unsafe_get s pos = 'm' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 't' && String.unsafe_get s (pos+3) = 'c' && String.unsafe_get s (pos+4) = 'h' && String.unsafe_get s (pos+5) = 'e' && String.unsafe_get s (pos+6) = 's' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 10 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 'e' && String.unsafe_get s (pos+8) = 'n' && String.unsafe_get s (pos+9) = 'd' then (
                    2
                  )
                  else (
                    -1
                  )
                )
              | 12 -> (
                  if String.unsafe_get s pos = 'c' && String.unsafe_get s (pos+1) = 'u' && String.unsafe_get s (pos+2) = 'r' && String.unsafe_get s (pos+3) = 's' && String.unsafe_get s (pos+4) = 'o' && String.unsafe_get s (pos+5) = 'r' && String.unsafe_get s (pos+6) = '_' && String.unsafe_get s (pos+7) = 's' && String.unsafe_get s (pos+8) = 't' && String.unsafe_get s (pos+9) = 'a' && String.unsafe_get s (pos+10) = 'r' && String.unsafe_get s (pos+11) = 't' then (
                    1
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_matches := (
                (
                  read__2
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_cursor_start := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_cursor_end := (
                (
                  Ag_oj_run.read_int
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              field_cr_status := (
                (
                  Ag_oj_run.read_string
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0xf then Ag_oj_run.missing_fields p [| !bits0 |] [| "matches"; "cursor_start"; "cursor_end"; "cr_status" |];
        (
          {
            matches = !field_matches;
            cursor_start = !field_cursor_start;
            cursor_end = !field_cursor_end;
            cr_status = !field_cr_status;
          }
         : complete_reply)
      )
)
let complete_reply_of_string s =
  read_complete_reply (Yojson.Safe.init_lexer ()) (Lexing.from_string s)
let write_clear_output : _ -> clear_output -> _ = (
  fun ob x ->
    Bi_outbuf.add_char ob '{';
    let is_first = ref true in
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"wait\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.wait;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"stdout\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.stdout;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"stderr\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.stderr;
    if !is_first then
      is_first := false
    else
      Bi_outbuf.add_char ob ',';
    Bi_outbuf.add_string ob "\"other\":";
    (
      Yojson.Safe.write_bool
    )
      ob x.other;
    Bi_outbuf.add_char ob '}';
)
let string_of_clear_output ?(len = 1024) x =
  let ob = Bi_outbuf.create len in
  write_clear_output ob x;
  Bi_outbuf.contents ob
let read_clear_output = (
  fun p lb ->
    Yojson.Safe.read_space p lb;
    Yojson.Safe.read_lcurl p lb;
    let field_wait = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_stdout = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_stderr = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let field_other = ref (Obj.magic (Sys.opaque_identity 0.0)) in
    let bits0 = ref 0 in
    try
      Yojson.Safe.read_space p lb;
      Yojson.Safe.read_object_end lb;
      Yojson.Safe.read_space p lb;
      let f =
        fun s pos len ->
          if pos < 0 || len < 0 || pos + len > String.length s then
            invalid_arg "out-of-bounds substring position or length";
          match len with
            | 4 -> (
                if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' then (
                  0
                )
                else (
                  -1
                )
              )
            | 5 -> (
                if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'h' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'r' then (
                  3
                )
                else (
                  -1
                )
              )
            | 6 -> (
                if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'd' then (
                  match String.unsafe_get s (pos+3) with
                    | 'e' -> (
                        if String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'r' then (
                          2
                        )
                        else (
                          -1
                        )
                      )
                    | 'o' -> (
                        if String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' then (
                          1
                        )
                        else (
                          -1
                        )
                      )
                    | _ -> (
                        -1
                      )
                )
                else (
                  -1
                )
              )
            | _ -> (
                -1
              )
      in
      let i = Yojson.Safe.map_ident p f lb in
      Ag_oj_run.read_until_field_value p lb;
      (
        match i with
          | 0 ->
            field_wait := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x1;
          | 1 ->
            field_stdout := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x2;
          | 2 ->
            field_stderr := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x4;
          | 3 ->
            field_other := (
              (
                Ag_oj_run.read_bool
              ) p lb
            );
            bits0 := !bits0 lor 0x8;
          | _ -> (
              Yojson.Safe.skip_json p lb
            )
      );
      while true do
        Yojson.Safe.read_space p lb;
        Yojson.Safe.read_object_sep p lb;
        Yojson.Safe.read_space p lb;
        let f =
          fun s pos len ->
            if pos < 0 || len < 0 || pos + len > String.length s then
              invalid_arg "out-of-bounds substring position or length";
            match len with
              | 4 -> (
                  if String.unsafe_get s pos = 'w' && String.unsafe_get s (pos+1) = 'a' && String.unsafe_get s (pos+2) = 'i' && String.unsafe_get s (pos+3) = 't' then (
                    0
                  )
                  else (
                    -1
                  )
                )
              | 5 -> (
                  if String.unsafe_get s pos = 'o' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'h' && String.unsafe_get s (pos+3) = 'e' && String.unsafe_get s (pos+4) = 'r' then (
                    3
                  )
                  else (
                    -1
                  )
                )
              | 6 -> (
                  if String.unsafe_get s pos = 's' && String.unsafe_get s (pos+1) = 't' && String.unsafe_get s (pos+2) = 'd' then (
                    match String.unsafe_get s (pos+3) with
                      | 'e' -> (
                          if String.unsafe_get s (pos+4) = 'r' && String.unsafe_get s (pos+5) = 'r' then (
                            2
                          )
                          else (
                            -1
                          )
                        )
                      | 'o' -> (
                          if String.unsafe_get s (pos+4) = 'u' && String.unsafe_get s (pos+5) = 't' then (
                            1
                          )
                          else (
                            -1
                          )
                        )
                      | _ -> (
                          -1
                        )
                  )
                  else (
                    -1
                  )
                )
              | _ -> (
                  -1
                )
        in
        let i = Yojson.Safe.map_ident p f lb in
        Ag_oj_run.read_until_field_value p lb;
        (
          match i with
            | 0 ->
              field_wait := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x1;
            | 1 ->
              field_stdout := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x2;
            | 2 ->
              field_stderr := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x4;
            | 3 ->
              field_other := (
                (
                  Ag_oj_run.read_bool
                ) p lb
              );
              bits0 := !bits0 lor 0x8;
            | _ -> (
                Yojson.Safe.skip_json p lb
              )
        );
      done;
      assert false;
    with Yojson.End_of_object -> (
        if !bits0 <> 0xf then Ag_oj_run.missing_fields p [| !bits0 |] [| "wait"; "stdout"; "stderr"; "other" |];
        (
          {
            wait = !field_wait;
            stdout = !field_stdout;
            stderr = !field_stderr;
            other = !field_other;
          }
         : clear_output)
      )
)
let clear_output_of_string s =
  read_clear_output (Yojson.Safe.init_lexer ()) (Lexing.from_string s)

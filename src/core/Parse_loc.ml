
(* This file is free software. See file "license" for more details. *)

(** {1 Location in a file} *)

type t = {
  file : string;
  start_line : int;
  start_column : int;
  stop_line : int;
  stop_column : int;
}

let mk file start_line start_column stop_line stop_column =
  { file; start_line; start_column; stop_line; stop_column; }

let mk_pair file (a,b)(c,d) = mk file a b c d

let mk_pos start stop =
  let open Lexing in
  mk
    start.pos_fname
    start.pos_lnum (start.pos_cnum - start.pos_bol)
    stop.pos_lnum (stop.pos_cnum - stop.pos_bol)

let equal = (=)
let hash = Hashtbl.hash
let hash_fun x h = CCHash.int (hash x) h

let _min_pos (l1,c1) (l2,c2) =
  if l1 = l2
    then l1, min c1 c2
  else if l1 < l2
    then l1, c1
  else l2, c2

let _max_pos (l1,c1) (l2,c2) =
  if l1 = l2
    then l1, max c1 c2
  else if l1 < l2
    then l2, c2
  else l1, c1

let combine p1 p2 =
  let start_line, start_column =
    _min_pos (p1.start_line, p1.start_column) (p2.start_line, p2.start_column)
  in
  let stop_line, stop_column =
    _max_pos (p1.stop_line, p1.stop_column) (p2.stop_line, p2.stop_column)
  in
  { file=p1.file; start_line; start_column; stop_line; stop_column; }

let rec combine_list l = match l with
  | [] -> raise (Invalid_argument "Location.combine_list")
  | [p] -> p
  | p1::((_ ::_) as l') ->
    let p' = combine_list l' in
    combine p1 p'

let smaller p ~than =
  (p.start_line > than.start_line
   ||  (p.start_line = than.start_line && p.start_column >= than.start_column))
  &&
  (p.stop_line < than.stop_line
   ||  (p.stop_line = than.stop_line && p.stop_column <= than.stop_column))

let pp out pos =
  if pos.start_line = pos.stop_line
  then
    Format.fprintf out "file '%s': line %d, col %d to %d"
      pos.file pos.start_line pos.start_column pos.stop_column
  else
    Format.fprintf out "file '%s': line %d, col %d to line %d, col %d"
      pos.file
      pos.start_line pos.start_column
      pos.stop_line pos.stop_column

let to_string = CCFormat.to_string pp

let pp_opt out = function
  | None -> Format.fprintf out "<no location>"
  | Some pos -> pp out pos

let to_string_opt = CCFormat.to_string pp_opt

(** {2 Lexbuf} *)

let set_file buf filename =
  let open Lexing in
  buf.lex_curr_p <- {buf.lex_curr_p with pos_fname=filename;};
  ()

let get_file buf =
  let open Lexing in
  buf.lex_curr_p.pos_fname

let of_lexbuf lexbuf =
  let start = Lexing.lexeme_start_p lexbuf in
  let end_ = Lexing.lexeme_end_p lexbuf in
  let s_l = start.Lexing.pos_lnum in
  let s_c = start.Lexing.pos_cnum - start.Lexing.pos_bol in
  let e_l = end_.Lexing.pos_lnum in
  let e_c = end_.Lexing.pos_cnum - end_.Lexing.pos_bol in
  let file = start.Lexing.pos_fname in
  mk file s_l s_c e_l e_c

(** {2 Error} *)

exception Parse_error of t * string

let () = Printexc.register_printer
    (function
      | Parse_error (loc, msg) ->
        Some (CCFormat.sprintf "parse error at %a:@ %s" pp loc msg)
      | _ -> None)

let parse_error loc msg = raise (Parse_error (loc, msg))

let parse_errorf loc msg =
  CCFormat.ksprintf ~f:(parse_error loc) msg

let parse_error_buf lexbuf msg =
  let loc = of_lexbuf lexbuf in
  parse_error loc msg

let parse_errorf_buf lexbuf msg =
  CCFormat.ksprintf ~f:(parse_error_buf lexbuf) msg

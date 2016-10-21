
(* This file is free software. See file "license" for more details. *)

(** {1 CLI entry point} *)

open Rewrite

let rec main_loop () =
  match LNoise.linenoise "> " with
    | None -> ()
    | Some res ->
      let buf = Lexing.from_string res in
      begin match Parser.parse_expr Lexer.token buf with
        | e ->
          ignore (LNoise.history_add res);
          Format.printf "parsed: @[%a@]@." Expr.pp_full_form e;
        | exception e ->
          Format.printf "error: %s@." (Printexc.to_string e);
      end;
      main_loop()

let history_file =
  try Some (Filename.concat (Sys.getenv "HOME") ".rewrite-history")
  with _ -> None

let () =
  ignore (LNoise.history_set ~max_length:1000);
  CCOpt.iter (fun f -> ignore (LNoise.history_load ~filename:f)) history_file;
  main_loop();
  CCOpt.iter (fun f -> ignore (LNoise.history_save ~filename:f)) history_file;
  ()


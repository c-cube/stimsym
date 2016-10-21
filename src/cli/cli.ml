
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
          Format.printf "parsed: @[%a@]@." Expr.pp_full_form e;
        | exception e ->
          Format.printf "error: %s@." (Printexc.to_string e);
      end;
      main_loop()

let () =
  main_loop()

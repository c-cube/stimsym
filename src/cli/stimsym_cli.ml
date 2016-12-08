
(* This file is free software. See file "license" for more details. *)

(** {1 CLI entry point} *)

open Stimsym

type config = {
  verbose: bool;
}

let rec main_loop ~config () =
  match LNoise.linenoise "> " with
    | None -> ()
    | Some res when String.trim res = "" -> main_loop ~config ()
    | Some res ->
      let buf = Lexing.from_string res in
      ignore (LNoise.history_add res);
      Parse_loc.set_file buf "<stdin>";
      begin match Parser.parse_expr Lexer.token buf with
        | e ->
          if config.verbose then (
            Format.printf "parsed: @[%a@]@." Expr.pp_full_form e;
          );
          begin
            try
              let e' = Expr.eval e in
              Format.printf "@[%a@]@." Expr.pp e';
            with
              | Stack_overflow ->
                Format.printf "stack overflow.@.";
              | Expr.Eval_fail msg ->
                Format.printf "evaluation failed:@ %s@." msg;
          end
        | exception e ->
          Format.printf "error: %s@." (Printexc.to_string e);
      end;
      main_loop ~config ()

let history_file =
  try Some (Filename.concat (Sys.getenv "HOME") ".rewrite-history")
  with _ -> None

let () =
  let verbose = ref false in
  Arg.parse
    [ "-v", Arg.Set verbose, " enable verbose output";
      "--verbose", Arg.Set verbose, " enable verbose output";
    ]
    (fun _ -> ())
    "./cli [options]

    A simple command-line REPL for rewriting expressions.
    ";
  let config = {
    verbose= !verbose;
  } in
  ignore (LNoise.history_set ~max_length:1000);
  CCOpt.iter (fun f -> ignore (LNoise.history_load ~filename:f)) history_file;
  if config.verbose then (
    Builtins.log_ := (fun s -> print_endline ("log: " ^ s));
  );
  main_loop ~config ();
  CCOpt.iter (fun f -> ignore (LNoise.history_save ~filename:f)) history_file;
  ()


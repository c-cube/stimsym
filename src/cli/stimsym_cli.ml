
(* This file is free software. See file "license" for more details. *)

(** {1 CLI entry point} *)

open Stimsym

type config = {
  verbose: bool;
}

type mime_view =
  | Mime_xdg_open of string (* extension *)
  | Mime_unknown

let mime_classify (m:Expr.mime_content): mime_view = match m.Expr.mime_ty with
  | "image/png" -> Mime_xdg_open ".png"
  | "image/svg" -> Mime_xdg_open ".svg"
  | _ -> Mime_unknown

(* try to display the given mime type *)
let display_mime (m:Expr.mime_content): unit = match mime_classify m with
  | Mime_xdg_open ext ->
    CCIO.File.with_temp ~prefix:"stimsym_cli" ~suffix:ext
      (fun file ->
         CCIO.with_out file
           (fun oc -> output_string oc m.Expr.mime_data; flush oc);
         let p = CCUnix.call_full "xdg-open '%s'" file in
         ignore p#errcode);
    ()
  | Mime_unknown ->
    Format.printf "<mime data type='%s', length=%d>"
      m.Expr.mime_ty (String.length m.Expr.mime_data)

(* completion callback *)
let completion str (lnoise:LNoise.completions): unit =
  Completion.complete str ~cursor_pos:(String.length str)
  |> List.iter (fun c -> LNoise.add_completion lnoise c.Completion.text)

let pp_rule out n =
  Format.fprintf out "%s@," (String.make n '=')

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
              let e', docs = Eval.eval_full e in
              if not (Expr.equal e' Builtins.null) then (
                Format.printf "@[%a@]@." Expr.pp e';
              );
              List.iter
                (function
                  | Eval.Print_doc doc ->
                    Format.printf "%a@.%a@.%a@." pp_rule 50 Document.pp doc pp_rule 50
                  | Eval.Print_mime m -> display_mime m)
                docs
            with
              | Stack_overflow ->
                Format.printf "stack overflow.@.";
              | Eval.Eval_fail msg ->
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
      "--trace", Arg.Unit (fun () -> Eval.set_eval_debug true), " enable tracing of evaluation";
    ]
    (fun _ -> ())
    "./cli [options]

    A simple command-line REPL for rewriting expressions.
    ";
  let config = {
    verbose= !verbose;
  } in
  ignore (LNoise.history_set ~max_length:1000);
  LNoise.set_completion_callback completion;
  CCOpt.iter (fun f -> ignore (LNoise.history_load ~filename:f)) history_file;
  if config.verbose then (
    Builtins.log_ := (fun s -> print_endline ("log: " ^ s));
  );
  main_loop ~config ();
  CCOpt.iter (fun f -> ignore (LNoise.history_save ~filename:f)) history_file;
  ()


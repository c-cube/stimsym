(* This file is free software. See file "license" for more details. *)

(** {1 Notebook interface} *)

open Stimsym

module H = Tyxml.Html
module C = Jupyter_kernel.Client
module Main = Jupyter_kernel.Client_main
module Log = Jupyter_kernel.Log

(** {2 Execution of queries} *)

(* display a document as HTML *)
let html_of_doc : Document.t -> [<Html_types.div] H.elt =
  let mk_header ~depth l = match depth with
    | 1 -> H.h1 l
    | 2 -> H.h2 l
    | 3 -> H.h3 l
    | 4 -> H.h4 l
    | 5 -> H.h5 l
    | n when n>=6 -> H.h6 l
    | _ -> assert false
  in
  let rec aux ~depth doc =
    H.div (List.map (aux_block ~depth) doc)
  and aux_block ~depth (b:Document.block) =
    let h = match b with
      | `S s -> mk_header ~depth [H.txt s]
      | `P s -> H.p [H.txt s]
      | `Pre s -> H.pre [H.txt s]
      | `L l ->
        H.ul (List.map (fun sub -> H.li [aux ~depth sub]) l)
      | `I (s,sub) ->
        let depth = depth+1 in
        H.div (
          mk_header ~depth [H.txt s] :: List.map (aux_block ~depth) sub
        )
    in
    H.div [h]
  in
  aux ~depth:3

let mime_of_html (h:_ H.elt) : C.mime_data = 
  let s = CCFormat.sprintf "%a@." (H.pp_elt ()) h in
  {C.mime_type="text/html"; mime_content=s; mime_b64=false}

let mime_of_txt (s:string) : C.mime_data =
  {C.mime_type="text/plain"; mime_content=s; mime_b64=false}

(* blocking function *)
let run_ count str : C.Kernel.exec_status_ok C.or_error =
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
                | Eval.Print_doc d ->
                  C.Kernel.Mime [d |> html_of_doc |> mime_of_html]
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
  let start, stop, l =
    if pos > String.length str then 0,0, []
    else (
      let {Completion.start;stop;l} = Completion.complete ~cursor_pos:pos str in
      start, stop, List.map (fun c -> c.Completion.text) l
    )
  in
  let c = {
    C.Kernel.completion_matches=l;
    completion_start=start; completion_end=stop;
  } in
  c

(* inspection *)
let inspect (r:C.Kernel.inspect_request) : (C.Kernel.inspect_reply_ok, string) result =
  let {C.Kernel.ir_code=c; ir_cursor_pos=pos; ir_detail_level=lvl} = r in
  Log.logf "inspection request %s :pos %d :lvl %d" c pos lvl;
  let cl = Completion.find_constants ~exact:true c ~cursor_pos:pos in
  let r = match cl.Completion.l with
    | [e] ->
      let txt = mime_of_txt @@ Document.to_string @@ Expr.Cst.get_doc e in
      let html = Expr.Cst.get_doc e |> html_of_doc |> mime_of_html in
      {C.Kernel.iro_status="ok"; iro_found=true; iro_data=[txt;html]}
    | _ ->
      (* not found *)
      {C.Kernel.iro_status="ok"; iro_found=false; iro_data=[]}
  in
  Result.Ok r

(* is the block of code complete?
   TODO: a way of asking the parser if it failed because of EOI/unbalanced []*)
let is_complete _ = Lwt.return C.Kernel.Is_complete

let () =
  Builtins.log_ := Log.log

let kernel : C.Kernel.t =
  C.Kernel.make
    ~banner:"Stimsym"
    ~exec:(fun ~count msg -> Lwt.return (run_ count msg))
    ~is_complete
    ~history:(fun _ -> Lwt.return [])
    ~inspect:(fun r -> Lwt.return (inspect r))
    ~language:"stimsym"
    ~language_version:[0;1;0]
    ~codemirror_mode:"mathematica"
    ~complete: (fun ~pos msg -> Lwt.return (complete pos msg))
    ()

(* main *)

let () =
  Stimsym.init();
  let config = Main.mk_config ~usage:"stimsym" () in
  Lwt_main.run (Main.main ~config ~kernel)

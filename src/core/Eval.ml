
(* This file is free software. See file "license" for more details. *)

(** {1 Evaluation} *)

module E = Expr

let rec eval_rec e = match e with
  | E.Z _
  | E.Q _
  | E.String _
  | E.Const _
    -> e
  | E.App (hd, args) ->
    let hd = eval_rec hd in
    (* evaluate arguments, but only if [hd] allows it *)
    let args = eval_args_of hd args in
    begin match hd with
      | E.Const c ->
        (* try every definition of [c] *)
        try_rules (E.app hd args) c.Expr.defs
      | _ ->
        (* just return the new term *)
        E.app hd args
    end

(* eval arguments [args], depending on whether the attributes
   of [hd] allow it *)
and eval_args_of hd args = match hd with
  | E.Const c when E.Cst.get_field E.field_hold_all c ->
    (* hold: stop evaluation *)
    args
  | E.Const c when E.Cst.get_field E.field_hold_first c ->
    Array.mapi
      (fun i a -> if i=0 then a else eval_rec a)
      args
  | E.Const c when E.Cst.get_field E.field_hold_rest c ->
    Array.mapi
      (fun i a -> if i>0 then a else eval_rec a)
      args
  | _ ->
    Array.map eval_rec args

(* try rules [defs] one by one, until one matches *)
and try_rules t defs = match defs with
  | [] -> t
  | E.Rewrite _ :: _ -> assert false (* TODO *)
  | E.Fun f :: tail ->
    begin match f eval_rec t with
      | None -> try_rules t tail
      | Some t' -> t' (* TODO: evaluate again? *)
    end

let eval e = eval_rec e


(* This file is free software. See file "license" for more details. *)

(** {1 Evaluation} *)

open Base_types

module Fmt = CCFormat
module E = Expr

type eval_side_effect = Base_types.eval_side_effect =
  | Print_doc of Document.t
  | Print_mime of mime_content

let def_fun f = Fun f

let def_rule ~lhs ~rhs =
  try
    let r = Pattern.compile_rule lhs rhs in
    Result.Ok (Rewrite r)
  with Pattern.Invalid_rule str ->
    Result.Error str

exception Eval_fail of string

let () = Printexc.register_printer
    (function
      | Eval_fail s ->
        Some ("evaluation failed:\n" ^ s)
      | _ -> None)

let eval_fail msg = raise (Eval_fail msg)
let eval_failf msg = Fmt.ksprintf msg ~f:eval_fail


(* set of definitions and rules we can use for rewriting *)
type rewrite_set =
  | RS_empty
  | RS_add_rules of rewrite_rule list * rewrite_set
  | RS_add_defs of def list * rewrite_set

let rs_of_st st: rewrite_set =
  RS_add_rules (st.st_local_rules, RS_add_rules (st.st_rules, RS_empty))

let rs_of_cst st c: rewrite_set =
  RS_add_rules (c.cst_local_rules,
    RS_add_defs (c.cst_rules, rs_of_st st))

let pp_slice out s =
  Format.fprintf out "[@[%a@]]"
    (Slice.print E.pp_full_form) s

let trace_on_ : bool ref = ref false

(* tracing evaluation *)
let trace_eval_ k =
  if !trace_on_
  then (
    k (fun msg ->
      Format.kfprintf (fun out -> Format.fprintf out "@.") Format.std_formatter msg)
  )

let set_eval_trace b = trace_on_ := b

(* @raise No_head if there is no head *)
let rec pattern_head (p:pattern): const = match p with
  | P_const c -> c
  | P_z _ | P_q _ | P_string _
  | P_blank | P_blank_sequence | P_blank_sequence_null | P_fail
    -> raise E.No_head
  | P_app_assoc (f,_) | P_app (f,_)
    -> pattern_head f
  | P_bind (_,p')
  | P_conditional (p',_)
  | P_check_same (_,p') -> pattern_head p'
  | P_alt [] -> raise E.No_head
  | P_alt (x::tail) ->
    begin match pattern_head x with
      | c -> c
      | exception E.No_head -> pattern_head (P_alt tail)
    end

let true_ = E.const_of_string "True"
let false_ = E.const_of_string "False"
let sequence = E.const_of_string "Sequence"

let sequence_of_array (a:E.t array) = E.app sequence a
let sequence_of_slice (a:E.t Slice.t) = sequence_of_array (Slice.copy a)

(** {2 Undo Stack}

    Used for local operations *)
module Undo : sig
  type t = undo_state
  type level
  val create : unit -> t
  val push : t -> (unit -> unit) -> unit
  val save : t -> level
  val restore : t -> level -> unit
end = struct
  type t = undo_state
  type level = int

  let create() : t = CCVector.create()

  let push = CCVector.push

  let save v = CCVector.length v

  let restore v lev =
    assert (lev < CCVector.length v);
    while lev < CCVector.length v do
      let op = CCVector.pop_exn v in
      op ();
    done
end

let equal_with (subst:Subst.t) a b: bool =
  let rec eq_aux a b = match a, b with
    | Z n1, Z n2 -> Z.equal n1 n2
    | Z z, Q q
    | Q q, Z z -> Q.equal q (Q.of_bigint z)
    | Q n1, Q n2 -> Q.equal n1 n2
    | String s1, String s2 -> s1=s2
    | Const c1, Const c2 -> c1.cst_id = c2.cst_id
    | App (f1,a1), App (f2,a2) ->
      Array.length a1=Array.length a2 &&
      eq_aux f1 f2 &&
      CCArray.equal eq_aux a1 a2
    | Reg i, Reg j when i=j -> true
    | Reg i, _ ->
      (* lookup *)
      begin match Subst.get i subst with
        | None -> false
        | Some a' -> eq_aux a' b
      end
    | _, Reg j ->
      (* lookup *)
      begin match Subst.get j subst with
        | None -> false
        | Some b' -> eq_aux a b'
      end
    | Z _, _ | Q _, _ | String _, _ | Const _, _ | App _, _
      -> false
  in
  eq_aux a b

(* return all the matches of [pat] against [e], modifying [st]
   every time in a backtracking way *)
let rec match_ (st:eval_state) (subst:Subst.t) (pat:pattern) (e:E.t): Subst.t Sequence.t =
  (* TODO: AC matching… *)
  let rec match_pair (subst:Subst.t) (pat:pattern) (e:E.t) yield: unit =
    trace_eval_ (fun k->k "@[<2>match @[%a@]@ with: @[%a@]@ subst: @[%a@]@]"
      Pattern.pp pat E.pp_full_form e Subst.pp subst);
    begin match pat, e with
      | P_z a, Z b -> if Z.equal a b then yield subst
      | P_q a, Q b -> if Q.equal a b then yield subst
      | P_q a, Z b -> if Q.equal a (Q.of_bigint b) then yield subst
      | P_z a, Q b -> if Q.equal (Q.of_bigint a) b then yield subst
      | P_string a, String b -> if a=b then yield subst
      | P_const c, Const d -> if E.Cst.equal c d then yield subst
      | P_blank, _ -> yield subst
      | P_bind (i, P_blank), _ ->
        (* easy case: bind [i] *)
        assert (not (Subst.mem i subst));
        let subst = Subst.add i e subst in
        yield subst
      | P_bind (i, sub_pat), _ ->
        match_pair subst sub_pat e
          (fun subst ->
             assert (not (Subst.mem i subst));
             (* bind [i] *)
             let subst = Subst.add i e subst in
             yield subst)
      | P_check_same (i, sub_pat), _ ->
        match_pair subst sub_pat e
          (fun subst ->
             (* get current binding for [i] *)
             let other = Subst.get_exn i subst in
             trace_eval_
               (fun k->k "(@[<2>check_same@ %a@ %a@])" E.pp_full_form e E.pp_full_form other);
             if E.equal e other then yield subst)
      | P_app (hd, args), App (hd', args')
        when Array.length args = Array.length args' ->
        match_pair subst hd hd'
          (fun subst -> match_arrays subst args args' 0 yield)
      | P_app_assoc (hd, tree), App (hd', args) ->
        (* associative matching *)
        match_pair subst hd hd'
          (fun subst -> match_assoc subst tree (Slice.full args) yield)
      | P_conditional (p', cond), _ ->
        match_pair subst p' e
          (fun subst ->
             if check_cond subst cond then yield subst)
      | P_fail, _ -> ()
      | P_alt l, _ -> match_alt subst l e yield
      | P_z _, _
      | P_q _, _
      | P_const _, _
      | P_string _, _
      | P_app_assoc _, _
      | P_app _, _
      | P_blank_sequence, _
      | P_blank_sequence_null, _
        -> () (* fail *)
    end
  (* match arrays pairwise *)
  and match_arrays subst a b (i:int) yield: unit =
    if i = Array.length a then (
      assert (i = Array.length b);
      yield subst
    ) else
      match_pair subst a.(i) b.(i)
        (fun subst ->
           match_arrays subst a b (i+1) yield
    )
  (* try alternatives *)
  and match_alt subst (l:pattern list) e yield: unit =
    List.iter (fun pat -> match_pair subst pat e yield) l
  (* check if [cond --> true] *)
  and check_cond (subst:Subst.t)(cond:E.t): bool =
    let cond' = Subst.apply subst cond in
    begin match eval_rec st cond' with
      | Const {cst_name="True";_} -> true
      | Const {cst_name="False";_} -> false
      | cond' ->
        eval_failf
          "@[<2>expected True/False,@ but condition `@[%a@]`@ \
           reduces to `@[%a@]`@ in subst %a@]"
          E.pp_full_form cond E.pp_full_form cond' Subst.pp subst
    end
  (* match tree [ap] to slice [slice] *)
  and match_assoc subst (ap:assoc_pattern) (slice:E.t Slice.t) yield: unit =
    match ap with
      | AP_vantage apv -> match_ap_vantage subst apv slice yield
      | AP_pure (l,_) -> match_ap_pure subst l slice yield
  and match_ap_vantage subst (apv:assoc_pattern_vantage) slice yield =
    trace_eval_ (fun k->k "@[<2>match_ap_vantage @[%a@]@ slice @[%a@]@]"
      Pattern.pp apv.ap_vantage pp_slice slice);
    (* check that there are enough elements *)
    let n = Slice.length slice in
    if apv.ap_min_size > n then ()
    else (
      (* the range in which we can match [ap.ap_vantage] safely *)
      let min, max =
        Pattern.ap_assoc_min_size apv.ap_left,
        n - Pattern.ap_assoc_min_size apv.ap_right
      in
      for vantage_idx = min to max-1 do
        (* try with this index *)
        trace_eval_ (fun k->k
            "@[match_ap_vantage@ at idx %d,@ pat @[%a@]@ \
             (min %d, max %d, slice @[%a@])@]"
          vantage_idx Pattern.pp apv.ap_vantage min max pp_slice slice);
        match_pair subst apv.ap_vantage (Slice.get slice vantage_idx)
          (fun subst ->
             let slice_left = Slice.sub slice 0 vantage_idx in
             match_assoc subst apv.ap_left slice_left
               (fun subst ->
                  let slice_right =
                    Slice.sub slice (vantage_idx+1) (n-vantage_idx-1)
                  in
                  match_assoc subst apv.ap_right slice_right yield))
      done
    )
  and match_ap_pure subst (l:pattern list) slice yield =
    let n = Slice.length slice in
    begin match l, n with
      | [], 0 -> yield subst
      | [], _ -> () (* fail to match some elements *)
      | [p], _ ->
        (* match [p] with the whole slice *)
        match_pat_slice subst p slice yield
      | p1 :: tail, _ ->
        (* cut [slice] into two parts, one to be matched with [p1],
           the rest with [tail]
           TODO a bit too naive, use info about min length *)
        for i=0 to n do
          let slice1 = Slice.sub slice 0 i in
          match_pat_slice subst p1 slice1
            (fun subst ->
               let slice2 = Slice.sub slice i (n-i) in
               match_ap_pure subst tail slice2 yield)
        done
    end

  (* match [p] with the whole slice, if possible *)
  and match_pat_slice subst (p:pattern) slice yield =
    let n = Slice.length slice in
    begin match p with
      | P_blank_sequence ->
        if n>0 then yield subst (* yield if non empty slice *)
      | P_blank_sequence_null -> yield subst (* always matches *)
      | P_alt [] -> ()
      | P_alt (p1::tail) ->
        (* try alternatives *)
        match_pat_slice subst p1 slice yield;
        match_pat_slice subst (P_alt tail) slice yield
      | P_check_same (i, p') ->
        (* check that [i] corresponds to [Sequence[slice]] *)
        let e_slice = lazy (sequence_of_slice slice) in
        match_pat_slice subst p' slice
          (fun subst ->
             (* get current binding for [i] *)
             let other = Subst.get_exn i subst in
             trace_eval_
               (fun k->k "(@[<2>check_same@ %a@ %a@])"
                   E.pp_full_form (Lazy.force e_slice) E.pp_full_form other);
             if E.equal (Lazy.force e_slice) other then yield subst)
      | P_bind (i, p') ->
        (* bind [i] to [Sequence[slice]] *)
        match_pat_slice subst p' slice
          (fun subst ->
             let subst = Subst.add i (sequence_of_slice slice) subst in
             yield subst)
      | P_conditional (p', cond) ->
        match_pat_slice subst p' slice
          (fun subst ->
             if check_cond subst cond then yield subst)
      | P_fail -> ()
      | P_blank | P_q _ | P_z _ | P_string _ | P_app _
      | P_const _ | P_app_assoc _
        ->
        if n=1 then (
          (* non-sequence pattern, match against the only element *)
          match_pair subst p (Slice.get slice 0) yield
        )
    end
  in
  match_pair subst pat e

and eval_rec (st:eval_state) e =
  (* trace_eval_ (fun k->k "@[<2>eval_rec @[%a@]@]" E.pp_full_form e); *)
  match e with
  | App (Const {cst_name="CompoundExpression";_}, ([| |] | [| _ |])) -> assert false
  | App (Const {cst_name="CompoundExpression";_}, args) ->
    (* sequence of `a;b;c…`. Return same as last expression *)
    let rec aux i =
      if i+1 = Array.length args
      then eval_rec st args.(i)
      else (
        let _ = eval_rec st args.(i) in
        aux (i+1)
      )
    in
    aux 0
  | App (Const {cst_name="ReplaceAll";_}, [| _; _ |]) ->
    (* FIXME: how to efficiently rewrite only once?
        maybe parametrize [eval_rec] with max num of rewrite steps? *)
    eval_failf "not implemented: ReplaceAll"
  | App (Const {cst_name="ReplaceRepeated";_}, [| a; b |]) ->
    (* first, eval [b] *)
    let b = eval_rec st b in
    (* rewrite [a] with rules in [b], until fixpoint *)
    let rules = term_as_rules st b in
    trace_eval_
      (fun k->k "(@[replace_repeated@ %a@ rules: (@[%a@])@])"
          E.pp_full_form a (Fmt.list ~start:"" ~stop:"" Pattern.pp_rule) rules);
    (* add rules to definitions of symbols, etc. and on [st.st_undo]
       so the changes are reversible *)
    let lev = Undo.save st.st_undo in
    List.iter
      (fun r -> match pattern_head r.rr_pat with
         | c ->
           E.Cst.add_local_rule st.st_undo r c
         | exception E.No_head ->
           let old_rules = st.st_rules in
           Undo.push st.st_undo (fun () -> st.st_rules <- old_rules);
           st.st_local_rules <- r :: st.st_local_rules)
      rules;
    CCFun.finally2
      ~h:(fun () -> Undo.restore st.st_undo lev) (* restore old state *)
      eval_rec st a
  | App (Const {cst_name="Comprehension";_}, args) when Array.length args>0 ->
    (* sequence binding_seq. First evaluate all terms but the first
       one, then compile into a binding_seq *)
    let args =
      Array.mapi (fun i arg -> if i>0 then eval_rec st arg else arg) args
    in
    begin match Pattern.compile_binding_seq ~ret:`First args with
      | Result.Ok c -> eval_comprehension st c
      | Result.Error msg ->
        eval_failf "@[<2>could not evaluate@ `%a`@ reason: %s@]" E.pp e msg
    end
  | App (Const {cst_name="Let";_}, args) when Array.length args>0 ->
    (* sequence of bindings. First evaluate all terms but the first
       one, then compile into a binding_seq *)
    let args =
      Array.mapi (fun i arg -> if i+1<Array.length args then eval_rec st arg else arg) args
    in
    begin match Pattern.compile_binding_seq ~ret:`Last args with
      | Result.Ok c -> eval_let st c
      | Result.Error msg ->
        eval_failf "@[<2>could not evaluate@ `%a`@ reason: %s@]" E.pp e msg
    end
  | App (Const {cst_name="SetDelayed";_}, [| a; b |]) ->
    (* lazy set: add rewrite rule [a :> b] to the definitions of [head a] *)
    begin match E.head a with
      | c ->
        let rule = Pattern.compile_rule a b in
        E.Cst.add_def (Rewrite rule) c
      | exception E.No_head ->
        eval_failf "cannot assign to %a" E.pp_full_form a
    end;
    E.null
  | App (Const {cst_name="Set";_}, [| a; b |]) ->
    (* eager set: eval [b], then add [a :> b] to the defs of [head a] *)
    let b = eval_rec st b in
    begin match E.head a with
      | c ->
        let rule = Pattern.compile_rule a b in
        E.Cst.add_def (Rewrite rule) c;
      | exception E.No_head ->
        eval_failf "cannot assign to %a" E.pp_full_form a
    end;
    b
  | App (App (Const {cst_name="Function";_}, [| body |]) as hd, args) ->
    (* evaluate args, then apply function *)
    let args = eval_args_of st hd args in
    eval_beta_reduce st body args
  | App (hd, args) ->
    let hd = eval_rec st hd in
    (* evaluate arguments, but only if [hd] allows it *)
    let args = eval_args_of st hd args in
    let t' = E.app_flatten hd args in
    begin match t' with
      | App (Const c as hd,
          [| App (Const {cst_name="List";_} as list_, args) |])
        when E.Cst.get_field E.field_listable c ->
        (* distribute [hd] on the list and evaluate it *)
        let args = Array.map (fun a -> eval_rec st (E.app hd [| a |])) args in
        (* return the list of results *)
        E.app_flatten list_ args
      | App (Const c, _) ->
        (* try every definition of [c] *)
        try_defs st t' (rs_of_cst st c)
      | _ ->
        (* just try the global rewrite rules *)
        try_defs st t' (rs_of_st st)
    end
  | Reg _ -> e (* cannot evaluate *)
  | Z _
  | Q _
  | String _ ->
    (* try global rules *)
    try_defs st e (rs_of_st st)
  | Const c ->
    (* [c] might have a definition *)
    try_defs st e (rs_of_cst st c)

and term_as_rule st e : rewrite_rule = match e with
  | App (Const {cst_name="Rule";_}, [| lhs; rhs |]) ->
    let rhs = eval_rec st rhs in
    Pattern.compile_rule lhs rhs
  | App (Const {cst_name="RuleDelayed";_}, [| lhs; rhs |]) ->
    Pattern.compile_rule lhs rhs
  | _ -> eval_failf "cannot interpret `@[%a@]` as a rule" E.pp_full_form e

and term_as_rules st e: rewrite_rule list = match e with
  | App (Const {cst_name="List";_}, args) ->
    CCList.init (Array.length args) (fun i -> term_as_rule st args.(i))
  | _ -> [term_as_rule st e]

(* eval arguments [args], depending on whether the attributes
   of [hd] allow it *)
and eval_args_of (st:eval_state) hd args = match hd with
  | Const c when E.Cst.get_field E.field_hold_all c ->
    (* hold: stop evaluation *)
    args
  | Const c when E.Cst.get_field E.field_hold_first c ->
    Array.mapi
      (fun i a -> if i=0 then a else eval_rec st a)
      args
  | Const c when E.Cst.get_field E.field_hold_rest c ->
    Array.mapi
      (fun i a -> if i>0 then a else eval_rec st a)
      args
  | _ ->
    Array.map (eval_rec st) args

(* try rules [rules] and definitions [defs] one by one, until one matches *)
and try_defs (st:eval_state) t (rs:rewrite_set) = match rs with
  | RS_empty -> t
  | RS_add_defs ([], rs')
  | RS_add_rules ([], rs') -> try_defs st t rs'
  | RS_add_rules (r :: rules_trail, rs') ->
    try_rule st t r (RS_add_rules (rules_trail, rs'))
  | RS_add_defs (Rewrite r :: trail, rs') ->
    try_rule st t r (RS_add_defs (trail, rs'))
  | RS_add_defs (Fun f :: trail, rs') ->
    begin match f st t with
      | None -> try_defs st t (RS_add_defs (trail, rs'))
      | Some t' ->
        st.st_iter_count <- st.st_iter_count + 1;
        eval_rec st t'
    end

and try_rule st t rule (rs:rewrite_set) =
  let subst_opt =
    match_ st Subst.empty rule.rr_pat t |> Sequence.head
  in
  begin match subst_opt with
    | None -> try_defs st t rs
    | Some subst ->
      let t' = Subst.apply subst rule.rr_rhs in
      st.st_iter_count <- st.st_iter_count + 1;
      eval_rec st t'
  end

(* beta-reduction of given function expression *)
and eval_beta_reduce st fun_body args =
  let rec replace (t:expr): expr = match t with
    | Reg _ -> assert false
    | App (Const {cst_name="Function"; _}, _) ->
      t (* do not enter functions *)
    | App (Const {cst_name="Slot";_}, [| Z n |]) ->
      (* slot substitution *)
      let i = Z.to_int n in
      if i < 0 then eval_failf "invalid slot `%d`: must be >= 0" i;
      if i > Array.length args then (
        eval_failf "invalid slot `%d`: not enough arguments" i;
      );
      (* dereference argument *)
      if i=0
      then sequence_of_array args
      else args.(i-1)
    | Const _ | Z _ | Q _ | String _ -> t
    | App (hd, args) ->
      E.app_flatten (replace hd) (Array.map replace args)
  in
  let t = replace fun_body in
  eval_rec st t

and eval_bindings st subst (l:binding_seq_body_item list): Subst.t Sequence.t =
  let open Sequence.Infix in
  match l with
    | [] -> Sequence.return subst
    | op :: tail ->
      eval_binding st subst op >>= fun subst -> eval_bindings st subst tail

and eval_binding st subst (op:binding_seq_body_item): Subst.t Sequence.t =
  let open Sequence.Infix in
  let eval_subst subst t = Subst.apply subst t |> eval_rec st in
  match op with
    | Comp_test t ->
      let t' = Subst.apply subst t |> eval_rec st in
      begin match t' with
        | Const {cst_name="True";_} -> Sequence.return subst
        | _ -> Sequence.empty
      end
    | Comp_match (pat, rhs) ->
      match_ st subst pat (eval_subst subst rhs)
    | Comp_match1 (pat, rhs) ->
      let rhs' = eval_subst subst rhs in
      (* match each subterm of [rhs] with [pat] *)
      begin match rhs' with
        | App (_, args) ->
          Sequence.of_array args
          >>= fun sub_rhs ->
          match_ st subst pat sub_rhs
        | _ -> Sequence.empty
      end

(* evaluate a comprehension *)
and eval_comprehension st (c:binding_seq) =
  let eval_subst subst t = Subst.apply subst t |> eval_rec st in
  (* recurse through the body *)
  begin
    eval_bindings st Subst.empty c.comp_body
    |> Sequence.map (fun subst -> eval_subst subst c.comp_yield)
    |> Sequence.to_list
    |> Array.of_list
    |> E.app_flatten sequence
  end

(* let is like a binding_seq, but we only return the first result *)
and eval_let st (c:binding_seq) =
  let eval_subst subst t = Subst.apply subst t |> eval_rec st in
  (* recurse through the body *)
  begin
    eval_bindings st Subst.empty c.comp_body
    |> Sequence.map (fun subst -> eval_subst subst c.comp_yield)
    |> Sequence.head
    |> (function
      | Some t -> t
      | None -> eval_failf "no match for `Let`")
  end

let create_eval_state ~buf () : eval_state = {
  st_iter_count=0;
  st_rules=[];
  st_local_rules=[];
  st_undo=Undo.create();
  st_effects=buf;
}

let eval e =
  let st = create_eval_state ~buf:None () in
  eval_rec st e

let eval_full e : E.t * eval_side_effect list =
  let q = Stack.create() in
  let st = create_eval_state ~buf:(Some q) () in
  let e' = eval_rec st e in
  let effects = Stack.fold (fun acc d -> d::acc) [] q in
  (* also check if there is a custom display *)
  let e_display = match e' with
    | Const {cst_display=Some f;_}
    | App (Const {cst_display=Some f;_}, _) ->
      f e' |> List.map (fun d -> Print_mime d)
    | _ -> []
  in
  e', e_display @ effects

(* primitive API *)

let prim_eval = eval_rec
let prim_fail _ = eval_fail
let prim_failf _ msg = eval_failf msg

let prim_write_doc st = match st.st_effects with
  | None -> (fun _ -> ())
  | Some q -> fun msg -> Stack.push (Print_doc (Lazy.force msg)) q

let prim_print st m = prim_write_doc st (Lazy.from_val [Document.paragraph m])

let prim_printf st = match st.st_effects with
  | None -> (fun msg -> Format.ikfprintf (fun _ -> ()) Format.str_formatter msg)
  | Some _ ->
    fun msg -> Fmt.ksprintf msg ~f:(fun msg -> prim_print st msg)

let prim_write_mime st = match st.st_effects with
  | None -> (fun _ -> ())
  | Some q -> fun (lazy m) -> Stack.push (Print_mime m) q

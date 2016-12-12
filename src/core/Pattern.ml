
(* This file is free software. See file "license" for more details. *)

(** {1 Patterns} *)

include Base_types

type t = Base_types.pattern

module E = Expr
module Fmt = CCFormat

(** {2 IO} *)

let rec pp out (p:pattern) = match p with
  | P_const {cst_name; _} -> Format.pp_print_string out cst_name
  | P_app (head, args) ->
    Format.fprintf out "@[<2>%a[@[<hv>%a@]]@]"
      pp head (Fmt.array ~start:"" ~stop:"" ~sep:"," pp) args
  | P_app_assoc (head, arg) ->
    Format.fprintf out "@[<2>%a[@[%a@]]@]"
      pp head pp_assoc_pattern arg
  | P_z n -> Z.pp_print out n
  | P_q n -> Q.pp_print out n
  | P_string s -> Format.fprintf out "%S" s
  | P_blank -> Fmt.string out "Blank[]"
  | P_blank_sequence -> Fmt.string out "BlankSequence[]"
  | P_blank_sequence_null -> Fmt.string out "BlankNullSequence[]"
  | P_fail -> Fmt.string out "Fail[]"
  | P_alt ([] | [_]) -> assert false
  | P_alt l ->
    Format.fprintf out "(@[<hv>%a@])"
      (Fmt.list ~start:"" ~stop:"" ~sep:"|" pp) l
  | P_bind (i,p) -> Format.fprintf out "Pattern[%d,@[%a@]]" i pp p
  | P_check_same (i,p) -> Format.fprintf out "CheckSame[%d,@[%a@]]" i pp p
  | P_conditional (p,cond) ->
    Format.fprintf out "Condition[%a,%a]" pp p E.pp_full_form cond

and pp_assoc_pattern out = function
  | AP_vantage apv ->
    Format.fprintf out "[@[<2>%a,@,vantage(@[%a@]),@,%a@]]"
      pp_assoc_pattern apv.ap_left
      pp apv.ap_vantage pp_assoc_pattern apv.ap_right
  | AP_pure (l,_) ->
    Format.fprintf out "[@[<hv>%a@]]"
      (Fmt.list ~start:"" ~stop:"" pp) l

let pp_binding_seq out (c:binding_seq) =
  let pp_body out = function
    | Comp_match (pat,e) ->
      Format.fprintf out "@[%a@,<-%a@]"
        pp pat E.pp_full_form e
    | Comp_match1 (pat,e) ->
      Format.fprintf out "@[%a@,<<-%a@]"
        pp pat E.pp_full_form e
    | Comp_test e -> E.pp_full_form out e
  in
  Format.fprintf out "Comprehension[@[%a,@,@[<hv>%a@]@]]"
    E.pp_full_form c.comp_yield
    (Fmt.list ~start:"" ~stop:"" ~sep:"," pp_body) c.comp_body

let pp_rule out (r:rewrite_rule): unit =
  Format.fprintf out "@[%a @<1>→@ %a@]" pp r.rr_pat E.pp_full_form r.rr_rhs

let pp_def out = function
  | Rewrite r -> pp_rule out r
  | Fun _ -> Fmt.string out "<primi>"

(** {2 Compilation} *)

exception Invalid_rule of string

let invalid_rule msg = raise (Invalid_rule msg)
let invalid_rulef msg = Fmt.ksprintf msg ~f:invalid_rule

let rec matches_slice (p:pattern): bool = match p with
  | P_blank_sequence | P_blank_sequence_null -> true
  | P_alt l -> List.exists matches_slice l
  | P_bind (_, sub_p)
  | P_conditional (sub_p, _)
  | P_check_same (_, sub_p) -> matches_slice sub_p
  | P_blank -> false
  | P_q _ | P_z _ | P_string _ | P_app _ | P_const _ | P_fail | P_app_assoc _
    -> false

(* 0 or 1, depending on whether the pattern can be Null *)
let rec pat_assoc_min_size (p:pattern): int = match p with
  | P_blank_sequence -> 1
  | P_blank_sequence_null -> 0
  | P_alt [] -> assert false
  | P_alt (x::l) ->
    List.fold_left (fun n p -> min n (pat_assoc_min_size p)) (pat_assoc_min_size x) l
  | P_bind (_, sub_p)
  | P_conditional (sub_p, _)
  | P_check_same (_, sub_p) -> pat_assoc_min_size sub_p
  | P_blank | P_q _ | P_z _ | P_string _ | P_app _ | P_const _
  | P_fail | P_app_assoc _
    -> 1

let ap_assoc_min_size (ap:assoc_pattern): int = match ap with
  | AP_vantage apv -> apv.ap_min_size
  | AP_pure (_,i) -> i

module Pat_compile = struct
  type state = {
    tbl: (string, int) Hashtbl.t;
    (* var name -> register *)
    surrounding: string Stack.t;
    (* variables bound in superterms, to avoid cyclical substitutions *)
  }

  let create() : state = {
    tbl = Hashtbl.create 12;
    surrounding=Stack.create();
  }

  (* convert LHS into a proper pattern *)
  let rec tr_pattern st t = match t with
    | Const c -> P_const c
    | String s -> P_string s
    | Z n -> P_z n
    | Q n -> P_q n
    | App (Const {cst_name="Blank";_},[||]) -> P_blank
    | App (Const {cst_name="BlankSequence";_},[||]) -> P_blank_sequence
    | App (Const {cst_name="BlankNullSequence";_},[||]) -> P_blank_sequence_null
    | App (Const {cst_name="Pattern";_},
        [| Const {cst_name=x;_}; sub |]) ->
      (* [x] on the stack -> failure, would lead to cyclical subst *)
      if Sequence.of_stack st.surrounding |> Sequence.mem x then (
        invalid_rulef "variable `%s` cannot appear in its own pattern" x
      );
      (* compute pattern itself *)
      let sub_p = match sub with
        | App (Const {cst_name="Blank";_}, [||]) -> P_blank (* trivial case *)
        | _ ->
          Stack.push x st.surrounding;
          CCFun.finally2 ~h:(fun () -> ignore (Stack.pop st.surrounding))
            tr_pattern st sub
      in
      begin match CCHashtbl.get st.tbl x with
        | None ->
          (* bind content of sub to [i] *)
          let i = Hashtbl.length st.tbl in
          Hashtbl.add st.tbl x i;
          P_bind (i, sub_p)
        | Some i ->
          (* already bound, check SameQ *)
          P_check_same (i, sub_p)
      end
    | App (Const {cst_name="Alternatives";_}, [| |]) -> P_fail
    | App (Const {cst_name="Alternatives";_}, [| p |]) -> tr_pattern st p
    | App (Const {cst_name="Alternatives";_}, a) ->
      let l = CCList.init (Array.length a) (fun i -> tr_pattern st a.(i)) in
      P_alt l
    | App (Const {cst_name="Condition";_}, [| p; cond |]) ->
      let p = tr_pattern st p in
      let cond = tr_term st cond in (* replace variables, etc. in condition *)
      (* TODO: check vars(cond) ⊆ vars(p) *)
      P_conditional (p, cond)
    | App (hd, args) ->
      let hd = tr_pattern st hd in
      let args = Array.map (tr_pattern st) args in
      if CCArray.exists matches_slice args
      then (
        (* associative match *)
        let ap = ap_of_pats (Slice.full args) in
        P_app_assoc (hd, ap)
      ) else (
        (* otherwise, match structurally *)
        P_app (hd, args)
      )
    | Reg _ -> assert false
  (* convert variables in RHS *)
  and tr_term st t = match t with
    | Z _ | Q _ | String _ -> t
    | App (hd, args) ->
      let hd = tr_term st hd in
      let args = Array.map (tr_term st) args in
      E.app hd args
    | Reg _  -> assert false
    | Const {cst_name;_} ->
      begin match CCHashtbl.get st.tbl cst_name with
        | None -> t
        | Some i -> E.reg i (* lookup *)
      end
  (* build an associative pattern tree out of this list of patterns *)
  and ap_of_pats (a:pattern Slice.t): assoc_pattern =
    let n = Slice.length a in
    if n=0 then AP_pure ([],0)
    else (
      (* TODO: refine this, e.g. with a "specificity" score that
         is higher when the pattern is more specific (low for Blank, high
         for constant applications, literals, etc.) and pick the most
         specific non-slice pattern as vantage point *)
      let matches_single p = not (matches_slice p) in
      (* try to find a vantage point *)
      begin match Slice.find_idx matches_single a with
        | Some (i, vantage) ->
          (* recurse in left and right parts of the pattern *)
          let left = ap_of_pats (Slice.sub a 0 i) in
          let right = ap_of_pats (Slice.sub a (i+1) (n-i-1)) in
          let ap_min_size =
            pat_assoc_min_size vantage +
              ap_assoc_min_size left +
              ap_assoc_min_size right
          in
          AP_vantage {
            ap_vantage=vantage;
            ap_left=left;
            ap_right=right;
            ap_min_size;
          }
        | None ->
          (* pure pattern: only slice-matching patterns *)
          let l = Slice.copy a |> Array.to_list in
          let min_size =
            List.fold_left
              (fun acc p -> acc+pat_assoc_min_size p) 0 l
          in
          AP_pure (l, min_size)
      end
    )
end

(* raise Invalid_rule if cannot compile *)
let compile_rule (lhs:E.t) (rhs:E.t): rewrite_rule =
  let st = Pat_compile.create() in
  let pat = Pat_compile.tr_pattern st lhs in
  let rhs = Pat_compile.tr_term st rhs in
  {rr_pat=pat; rr_pat_as_expr=lhs; rr_rhs=rhs }

exception Invalid_binding_seq of string

let invalid_binding_seq msg = raise (Invalid_binding_seq msg)

let compile_binding_seq_body (s:E.t Slice.t) (ret:E.t): binding_seq =
  let st = Pat_compile.create() in
  (* evaluation order matters *)
  let body =
    Slice.fold_left
      (fun acc sub ->
         begin match sub with
           | App (Const {cst_name="MatchBind";_}, [| pat; rhs |]) ->
             let pat = Pat_compile.tr_pattern st pat in
             let rhs = Pat_compile.tr_term st rhs in
             Comp_match (pat,rhs) :: acc
           | App (Const {cst_name="MatchBind1";_}, [| pat; rhs |]) ->
             let pat = Pat_compile.tr_pattern st pat in
             let rhs = Pat_compile.tr_term st rhs in
             Comp_match1 (pat,rhs) :: acc
           | _ ->
             let t = Pat_compile.tr_term st sub in
             Comp_test t :: acc
         end)
      []
      s
  in
  let ret = Pat_compile.tr_term st ret in
  { comp_yield=ret; comp_body=List.rev body }

let compile_binding_seq ~ret (args:E.t array): (binding_seq,string) Result.result =
  try
    let n = Array.length args in
    match args, ret with
      | [||], _ -> Result.Error "need at least 2 arguments"
      | _, `First ->
        let ret = args.(0) in
        let body = Slice.make args 1 ~len:(n-1) in
        let c = compile_binding_seq_body body ret in
        Result.Ok c
      | _, `Last ->
        let ret = args.(n-1) in
        let body = Slice.make args 0 ~len:(n-1) in
        let c = compile_binding_seq_body body ret in
        Result.Ok c
  with
    | Invalid_binding_seq msg -> Result.Error msg

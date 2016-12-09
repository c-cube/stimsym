
(* This file is free software. See file "license" for more details. *)

(** {1 Expressions} *)

module Properties = Bit_set.Make(struct end)

let field_protected = Properties.mk_field()
let field_hold_all = Properties.mk_field()
let field_hold_first = Properties.mk_field()
let field_hold_rest = Properties.mk_field()
let field_orderless = Properties.mk_field()
let field_flatten = Properties.mk_field()
let field_one_identity = Properties.mk_field()
let () = Properties.freeze()

(* TODO: remove? *)
type def_style =
  | Def_eager
  | Def_lazy

(** {2 Basics} *)

exception Print_default

type const = {
  cst_name: string;
  cst_id: int;
  mutable cst_properties: Properties.t;
  mutable cst_rules: def list;
  mutable cst_local_rules: rewrite_rule list;
  mutable cst_doc: string;
  mutable cst_printer: (int * const_printer) option;
}

and t =
  | Const of const
  | App of t * t array
  | Z of Z.t
  | Q of Q.t
  | String of string
  | Reg of int (* only in rules RHS *)

and const_printer = const -> (int -> t CCFormat.printer) -> t array CCFormat.printer

(* (partial) definition of a symbol *)
and def =
  | Rewrite of rewrite_rule
  | Fun of prim_fun

and rewrite_rule = {
  rr_pat: pattern;
  rr_pat_as_expr: t;
  rr_rhs: t;
}

and pattern =
  | P_const of const
  | P_z of Z.t
  | P_q of Q.t
  | P_string of string
  | P_app of pattern * pattern array
  | P_blank
  | P_blank_sequence (* >= 1 elements *)
  | P_blank_sequence_null (* >= 0 elements *)
  | P_fail
  | P_bind of int * pattern
    (* match, then bind register *)
  | P_check_same of int * pattern
    (* match, then check syntactic equality with content of register *)
  | P_alt of pattern list
  | P_app_assoc of pattern * assoc_pattern (* for slices *)
  | P_conditional of pattern * t (* pattern if condition *)

and assoc_pattern =
  | AP_vantage of assoc_pattern_vantage
  | AP_pure of pattern list * int (* only sequence/sequencenull; min size *)

(* a subtree used for associative pattern matching *)
and assoc_pattern_vantage = {
  ap_min_size: int; (* minimum length of matched slice *)
  ap_left: assoc_pattern; (* matches left slice *)
  ap_vantage: pattern; (* match this unary pattern first *)
  ap_right: assoc_pattern; (* matches right slice *)
}

(* TODO? *)
and prim_fun_args = eval_state

and prim_fun = prim_fun_args -> t -> t option

(* state for evaluation *)
and eval_state = {
  mutable st_iter_count: int;
  (* number of iterations *)
  mutable st_rules: rewrite_rule list;
  (* permanent list of rules *)
  mutable st_local_rules: rewrite_rule list;
  (* backtrackable list of rules *)
  st_undo: undo_state;
  (* undo stack, for local operations *)
  st_print: Buffer.t option;
  (* temporary messages *)
}

and undo_state = (unit -> unit) CCVector.vector

type expr = t

module Str_tbl = CCHashtbl.Make(struct
    type t = string
    let equal (a:string) b = a=b
    let hash (a:string) = Hashtbl.hash a
  end)

type bank = {
  by_name: t Str_tbl.t; (* name -> const *)
  mutable const_id: int; (* unique name *)
}

let bank : bank = {
  by_name = Str_tbl.create 1_024;
  const_id = 0;
}

let const c = Const c

let const_of_string name =
  try Str_tbl.find bank.by_name name
  with Not_found ->
    let c = Const {
        cst_name=name;
        cst_properties=Properties.empty;
        cst_id= bank.const_id;
        cst_rules=[];
        cst_local_rules=[];
        cst_doc="";
        cst_printer=None;
      } in
    bank.const_id <- bank.const_id + 1;
    Str_tbl.add bank.by_name name c;
    c

let true_ = const_of_string "True"
let false_ = const_of_string "False"
let null = const_of_string "Null"
let sequence = const_of_string "Sequence"

let app hd args = App (hd, args)

let sequence_of_array (a:t array) = app sequence a
let sequence_of_slice (a:t Slice.t) = sequence_of_array (Slice.copy a)

let app_flatten hd args =
  (* splicing *)
  let must_splice, res_len =
    Array.fold_left
      (fun (must_split,len) arg -> match arg with
         | App (Const {cst_name="Sequence";_}, sub) -> true, len+Array.length sub
         | _ -> must_split, len+1)
      (false,0) args
  in
  if must_splice
  then (
    let args_flat = Array.make res_len null in
    (* make a flattened array *)
    let len' =
      Array.fold_left
        (fun offset arg -> match arg with
           | App (Const {cst_name="Sequence";_}, sub) ->
             Array.blit sub 0 args_flat offset (Array.length sub);
             offset + Array.length sub
           | _ ->
             args_flat.(offset) <- arg;
             offset + 1)
        0 args
    in
    assert (len' = res_len);
    App (hd, args_flat)
  ) else
    App (hd, args)

let app_l head args = app head (Array.of_list args)

let z n = Z n

let q n = Q n

let string s = String s

let reg i = Reg i

let of_int i = Z (Z.of_int i)

let of_int_ratio a b = Q (Q.of_ints a b)

let of_float x = Q (Q.of_float x)

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

(** {2 Constants} *)

exception No_head

let rec head = function
  | Const c -> c
  | App (hd, _) -> head hd
  | Z _ | Q _ | String _ | Reg _ -> raise No_head

module Cst = struct
  type t = const

  let equal a b = a.cst_id = b.cst_id
  let hash a = Hash.int a.cst_id

  let get_field f c = Properties.get f c.cst_properties

  let set_field f b c = c.cst_properties <- Properties.set f b c.cst_properties

  let add_def d c = match d with
    | Rewrite {rr_pat=P_const c'; _} when equal c c' ->
      c.cst_rules <- [d] (* shadowing *)
    | _ -> c.cst_rules <- d :: c.cst_rules

  let add_local_rule (u:Undo.t) rule c = match rule with
    | {rr_pat=P_const c'; _} when equal c c' ->
      let old_local_rules = c.cst_local_rules in
      c.cst_local_rules <- [rule];
      Undo.push u (fun () -> c.cst_local_rules <- old_local_rules)
    | _ ->
      let old_local_rules = c.cst_local_rules in
      c.cst_local_rules <- rule :: c.cst_local_rules;
      Undo.push u (fun () -> c.cst_local_rules <- old_local_rules)

  let set_doc d c = c.cst_doc <- d

  let set_printer i f c = c.cst_printer <- Some (i,f)
end

let const_of_string_with ~f name =
  let c = const_of_string name in
  begin match c with
    | Const r -> f r;
    | App _ | Z _ | Q _ | String _ | Reg _ -> assert false
  end;
  c

(** {2 IO} *)

let rec pp_full_form out (t:t) = match t with
  | Const {cst_name; _} -> Format.pp_print_string out cst_name
  | App (head, args) ->
    Format.fprintf out "@[<2>%a[@[<hv>%a@]]@]"
      pp_full_form head (CCFormat.array ~start:"" ~stop:"" ~sep:"," pp_full_form) args
  | Z n -> Z.pp_print out n
  | Q n -> Q.pp_print out n
  | String s -> Format.fprintf out "%S" s
  | Reg i -> Format.fprintf out "Slot[%d]" i

let rec pp_pattern out (p:pattern) = match p with
  | P_const {cst_name; _} -> Format.pp_print_string out cst_name
  | P_app (head, args) ->
    Format.fprintf out "@[<2>%a[@[<hv>%a@]]@]"
      pp_pattern head (CCFormat.array ~start:"" ~stop:"" ~sep:"," pp_pattern) args
  | P_app_assoc (head, arg) ->
    Format.fprintf out "@[<2>%a[@[%a@]]@]"
      pp_pattern head pp_assoc_pattern arg
  | P_z n -> Z.pp_print out n
  | P_q n -> Q.pp_print out n
  | P_string s -> Format.fprintf out "%S" s
  | P_blank -> CCFormat.string out "Blank[]"
  | P_blank_sequence -> CCFormat.string out "BlankSequence[]"
  | P_blank_sequence_null -> CCFormat.string out "BlankNullSequence[]"
  | P_fail -> CCFormat.string out "Fail[]"
  | P_alt ([] | [_]) -> assert false
  | P_alt l ->
    Format.fprintf out "(@[<hv>%a@])"
      (CCFormat.list ~start:"" ~stop:"" ~sep:"|" pp_pattern) l
  | P_bind (i,p) -> Format.fprintf out "Pattern[%d,@[%a@]]" i pp_pattern p
  | P_check_same (i,p) -> Format.fprintf out "CheckSame[%d,@[%a@]]" i pp_pattern p
  | P_conditional (p,cond) ->
    Format.fprintf out "Condition[%a,%a]" pp_pattern p pp_full_form cond

and pp_assoc_pattern out = function
  | AP_vantage apv ->
    Format.fprintf out "[@[<2>%a,@,vantage(@[%a@]),@,%a@]]"
      pp_assoc_pattern apv.ap_left
      pp_pattern apv.ap_vantage pp_assoc_pattern apv.ap_right
  | AP_pure (l,_) ->
    Format.fprintf out "[@[<hv>%a@]]"
      (CCFormat.list ~start:"" ~stop:"" pp_pattern) l

let pp_rule out (r:rewrite_rule): unit =
  Format.fprintf out "@[%a @<1>→@ %a@]" pp_pattern r.rr_pat pp_full_form r.rr_rhs

let pp_def out = function
  | Rewrite r -> pp_rule out r
  | Fun _ -> CCFormat.string out "<primi>"

let to_string_compact t =
  let buf = Buffer.create 32 in
  let rec aux t = match t with
    | Const {cst_name; _} -> Buffer.add_string buf cst_name
    | App (head, args) ->
      aux head;
      Buffer.add_char buf '[';
      Array.iteri
        (fun i t' ->
           if i>0 then Buffer.add_char buf ',';
           aux t')
        args;
      Buffer.add_char buf ']';
    | Z n -> Z.bprint buf n
    | Q n -> Q.bprint buf n
    | String s ->
      Buffer.add_char buf '"';
      Buffer.add_string buf (String.escaped s);
      Buffer.add_char buf '"';
    | Reg _ -> assert false
  in
  aux t;
  Buffer.contents buf

let pp out (t:t) =
  let rec pp prec out t = match t with
    | Const {cst_name; _} -> Format.pp_print_string out cst_name
    | App (Const {cst_name="FullForm";_}, [|t|]) -> pp_full_form out t
    | App (Const ({cst_printer=Some (_, pp_special); _} as c), [||]) ->
      pp_special c pp out [||]
    | App (Const ({cst_printer=Some (prec', pp_special); _} as c), args) ->
      if prec' > prec
      then (
        try pp_special c pp out args
        with Print_default ->  pp_default out (const c, args)
      )
      else CCFormat.within "(" ")" (pp_special c pp) out args
    | App (head, args) -> pp_default out (head, args)
    | Z n -> Z.pp_print out n
    | Q n -> Q.pp_print out n
    | String s -> Format.fprintf out "%S" s
    | Reg i -> Format.fprintf out "#%d" i
  and pp_default out (head, args) =
    Format.fprintf out "@[<2>%a[@[<hv>%a@]]@]"
      (pp 0) head (CCFormat.array ~start:"" ~stop:"" ~sep:"," (pp 0)) args
  in
  begin match t with
    | Const {cst_name="Null";_} -> () (* do not print toplevel "null" *)
    | _ -> pp 0 out t
  end

let to_string t = CCFormat.to_string pp t

(** {2 Evaluation} *)

exception Invalid_rule of string

let invalid_rule msg = raise (Invalid_rule msg)
let invalid_rulef msg = CCFormat.ksprintf msg ~f:invalid_rule

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

(* raise Invalid_rule if cannot compile *)
let compile_rule lhs rhs: rewrite_rule =
  (* variable name -> register *)
  let tbl : (string, int) Hashtbl.t = Hashtbl.create 16 in
  (* variables bound in super-terms. Necessary to avoid cyclical substs *)
  let surrounding : string Stack.t = Stack.create () in
  (* convert LHS into a proper pattern *)
  let rec aux_pat t = match t with
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
      if Sequence.of_stack surrounding |> Sequence.mem x then (
        invalid_rulef "variable `%s` cannot appear in its own pattern" x
      );
      (* compute pattern itself *)
      let sub_p = match sub with
        | App (Const {cst_name="Blank";_}, [||]) -> P_blank (* trivial case *)
        | _ ->
          Stack.push x surrounding;
          CCFun.finally1 ~h:(fun () -> ignore (Stack.pop surrounding))
            aux_pat sub
      in
      begin match CCHashtbl.get tbl x with
        | None ->
          (* bind content of sub to [i] *)
          let i = Hashtbl.length tbl in
          Hashtbl.add tbl x i;
          P_bind (i, sub_p)
        | Some i ->
          (* already bound, check SameQ *)
          P_check_same (i, sub_p)
      end
    | App (Const {cst_name="Alternatives";_}, [| |]) -> P_fail
    | App (Const {cst_name="Alternatives";_}, [| p |]) -> aux_pat p
    | App (Const {cst_name="Alternatives";_}, a) ->
      let l = CCList.init (Array.length a) (fun i -> aux_pat a.(i)) in
      P_alt l
    | App (Const {cst_name="Condition";_}, [| p; cond |]) ->
      let p = aux_pat p in
      let cond = aux_rhs cond in (* replace variables, etc. in condition *)
      (* TODO: check vars(cond) ⊆ vars(p) *)
      P_conditional (p, cond)
    | App (hd, args) ->
      let hd = aux_pat hd in
      let args = Array.map aux_pat args in
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
  and aux_rhs t = match t with
    | Z _ | Q _ | String _ -> t
    | App (hd, args) ->
      let hd = aux_rhs hd in
      let args = Array.map aux_rhs args in
      app hd args
    | Reg _ -> assert false
    | Const {cst_name;_} ->
      begin match CCHashtbl.get tbl cst_name with
        | None -> t
        | Some i -> reg i (* lookup *)
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
  in
  let pat = aux_pat lhs in
  let rhs = aux_rhs rhs in
  {rr_pat=pat; rr_pat_as_expr=lhs; rr_rhs=rhs }

let def_fun f = Fun f

let def_rule ~lhs ~rhs =
  try
    let r = compile_rule lhs rhs in
    Result.Ok (Rewrite r)
  with Invalid_rule str ->
    Result.Error str

exception Eval_fail of string

let () = Printexc.register_printer
    (function
      | Eval_fail s ->
        Some ("evaluation failed:\n" ^ s)
      | _ -> None)

let eval_fail msg = raise (Eval_fail msg)
let eval_failf msg = CCFormat.ksprintf msg ~f:eval_fail

module Subst : sig
  type t
  val empty : t
  val add : int -> expr -> t -> t
  val mem : int -> t -> bool
  val get : int -> t -> expr option
  val get_exn : int -> t -> expr
  val apply : t -> expr -> expr
  val pp : t CCFormat.printer
end = struct
  module IntMap = CCMap.Make(CCInt)
  type t = expr IntMap.t
  let empty = IntMap.empty
  let add = IntMap.add
  let mem = IntMap.mem
  let get = IntMap.get

  let pp out (s:t) =
    Format.fprintf out "{@[<hv>%a@]}"
      (IntMap.print ~start:"" ~stop:"" CCFormat.int pp_full_form) s

  let get_exn i s =
    try IntMap.find i s
    with Not_found ->
      invalid_arg (CCFormat.sprintf "could not find %d in %a" i pp s)

  let rec apply (s:t) (t:expr): expr = match t with
    | Reg i ->
      begin match IntMap.get i s with
        | None -> assert false
        | Some u -> u
      end
    | Const _ | Z _ | Q _ | String _ -> t
    | App (hd, args) ->
      app_flatten (apply s hd) (Array.map (apply s) args)
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

let equal = equal_with Subst.empty

(* hash up to a given depth *)
let rec hash_limit n t =
  if n=0 then 0x42
  else match t with
    | Reg i -> Hash.int i
    | Z n -> Z.hash n
    | Q n -> Q.to_string n |> Hash.string
    | String s -> Hash.string s
    | Const c -> Cst.hash c
    | App (f, a) ->
      Hash.combine3 0x11
        (hash_limit (n-1) f)
        (Hash.array (hash_limit (n-1)) a)

let hash t = hash_limit 5 t

module Tbl = CCHashtbl.Make(struct
    type t = expr
    let equal = equal
    let hash = hash
  end)

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
    (Slice.print pp_full_form) s

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
    -> raise No_head
  | P_app_assoc (f,_) | P_app (f,_)
    -> pattern_head f
  | P_bind (_,p')
  | P_conditional (p',_)
  | P_check_same (_,p') -> pattern_head p'
  | P_alt [] -> raise No_head
  | P_alt (x::tail) ->
    begin match pattern_head x with
      | c -> c
      | exception No_head -> pattern_head (P_alt tail)
    end

(* return all the matches of [pat] against [e], modifying [st]
   every time in a backtracking way *)
let rec match_ (st:eval_state) (subst:Subst.t) (pat:pattern) (e:t): Subst.t Sequence.t =
  (* TODO: AC matching… *)
  let rec match_pair (subst:Subst.t) (pat:pattern) (e:t) yield: unit =
    trace_eval_ (fun k->k "@[<2>match @[%a@]@ with: @[%a@]@ subst: @[%a@]@]"
      pp_pattern pat pp_full_form e Subst.pp subst);
    begin match pat, e with
      | P_z a, Z b -> if Z.equal a b then yield subst
      | P_q a, Q b -> if Q.equal a b then yield subst
      | P_q a, Z b -> if Q.equal a (Q.of_bigint b) then yield subst
      | P_z a, Q b -> if Q.equal (Q.of_bigint a) b then yield subst
      | P_string a, String b -> if a=b then yield subst
      | P_const c, Const d -> if Cst.equal c d then yield subst
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
               (fun k->k "(@[<2>check_same@ %a@ %a@])" pp_full_form e pp_full_form other);
             if equal e other then yield subst)
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
  and check_cond (subst:Subst.t)(cond:t): bool =
    let cond' = Subst.apply subst cond in
    begin match eval_rec st cond' with
      | Const {cst_name="True";_} -> true
      | Const {cst_name="False";_} -> false
      | cond' ->
        eval_failf
          "@[<2>expected True/False,@ but condition `@[%a@]`@ \
           reduces to `@[%a@]`@ in subst %a@]"
          pp_full_form cond pp_full_form cond' Subst.pp subst
    end
  (* match tree [ap] to slice [slice] *)
  and match_assoc subst (ap:assoc_pattern) (slice:t Slice.t) yield: unit =
    match ap with
      | AP_vantage apv -> match_ap_vantage subst apv slice yield
      | AP_pure (l,_) -> match_ap_pure subst l slice yield
  and match_ap_vantage subst (apv:assoc_pattern_vantage) slice yield =
    trace_eval_ (fun k->k "@[<2>match_ap_vantage @[%a@]@ slice @[%a@]@]"
      pp_pattern apv.ap_vantage pp_slice slice);
    (* check that there are enough elements *)
    let n = Slice.length slice in
    if apv.ap_min_size > n then ()
    else (
      (* the range in which we can match [ap.ap_vantage] safely *)
      let min, max =
        ap_assoc_min_size apv.ap_left,
        n - ap_assoc_min_size apv.ap_right
      in
      for vantage_idx = min to max-1 do
        (* try with this index *)
        trace_eval_ (fun k->k
            "@[match_ap_vantage@ at idx %d,@ pat @[%a@]@ \
             (min %d, max %d, slice @[%a@])@]"
          vantage_idx pp_pattern apv.ap_vantage min max pp_slice slice);
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
        for i=0 to n-1 do
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
                   pp_full_form (Lazy.force e_slice) pp_full_form other);
             if equal (Lazy.force e_slice) other then yield subst)
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
  (* trace_eval_ (fun k->k "@[<2>eval_rec @[%a@]@]" pp_full_form e); *)
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
    (* rewrite [a] with rules in [b], until fixpoint *)
    let rules = term_as_rules st b in
    trace_eval_
      (fun k->k "(@[replace_repeated@ %a@ rules: (@[%a@])@])"
          pp_full_form a (CCFormat.list ~start:"" ~stop:"" pp_rule) rules);
    (* add rules to definitions of symbols, etc. and on [st.st_undo]
       so the changes are reversible *)
    let lev = Undo.save st.st_undo in
    List.iter
      (fun r -> match pattern_head r.rr_pat with
         | c ->
           Cst.add_local_rule st.st_undo r c
         | exception No_head ->
           let old_rules = st.st_rules in
           Undo.push st.st_undo (fun () -> st.st_rules <- old_rules);
           st.st_local_rules <- r :: st.st_local_rules)
      rules;
    CCFun.finally2
      ~h:(fun () -> Undo.restore st.st_undo lev) (* restore old state *)
      eval_rec st a
  | App (Const {cst_name="SetDelayed";_}, [| a; b |]) ->
    (* lazy set: add rewrite rule [a :> b] to the definitions of [head a] *)
    begin match head a with
      | c ->
        let rule = compile_rule a b in
        Cst.add_def (Rewrite rule) c
      | exception No_head ->
        eval_failf "cannot assign to %a" pp_full_form a
    end;
    null
  | App (Const {cst_name="Set";_}, [| a; b |]) ->
    (* eager set: eval [b], then add [a :> b] to the defs of [head a] *)
    let b = eval_rec st b in
    begin match head a with
      | c ->
        let rule = compile_rule a b in
        Cst.add_def (Rewrite rule) c;
      | exception No_head ->
        eval_failf "cannot assign to %a" pp_full_form a
    end;
    b
  | App (hd, args) ->
    let hd = eval_rec st hd in
    (* evaluate arguments, but only if [hd] allows it *)
    let args = eval_args_of st hd args in
    let t' = app_flatten hd args in
    begin match head hd with
      | c ->
        (* try every definition of [c] *)
        try_defs st t' (rs_of_cst st c)
      | exception No_head ->
        (* just return the new term *)
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
    compile_rule lhs rhs
  | App (Const {cst_name="RuleDelayed";_}, [| lhs; rhs |]) ->
    compile_rule lhs rhs
  | _ -> eval_failf "cannot interpret `@[%a@]` as a rule" pp_full_form e

and term_as_rules st e: rewrite_rule list = match e with
  | App (Const {cst_name="List";_}, args) ->
    CCList.init (Array.length args) (fun i -> term_as_rule st args.(i))
  | _ -> [term_as_rule st e]

(* eval arguments [args], depending on whether the attributes
   of [hd] allow it *)
and eval_args_of (st:eval_state) hd args = match hd with
  | Const c when Cst.get_field field_hold_all c ->
    (* hold: stop evaluation *)
    args
  | Const c when Cst.get_field field_hold_first c ->
    Array.mapi
      (fun i a -> if i=0 then a else eval_rec st a)
      args
  | Const c when Cst.get_field field_hold_rest c ->
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

let create_eval_state ~buf() : eval_state = {
  st_iter_count=0;
  st_rules=[];
  st_local_rules=[];
  st_undo=Undo.create();
  st_print=if buf then Some(Buffer.create 16) else None;
}

let eval e =
  let st = create_eval_state ~buf:false () in
  eval_rec st e

let eval_full e =
  let st = create_eval_state ~buf:true () in
  let e' = eval_rec st e in
  let str = match st.st_print with
    | None -> assert false
    | Some buf -> Buffer.contents buf
  in
  e', str

(* primitive API *)

let prim_eval = eval_rec
let prim_fail _ = eval_fail
let prim_failf _ msg = eval_failf msg
let prim_printf st = match st.st_print with
  | None -> (fun msg -> Format.ikfprintf (fun _ -> ()) Format.str_formatter msg)
  | Some buf ->
    fun msg ->
    CCFormat.ksprintf msg
      ~f:(fun msg -> Buffer.add_string buf msg)

let prim_print st = match st.st_print with
  | None -> (fun _ -> ())
  | Some buf -> Buffer.add_string buf

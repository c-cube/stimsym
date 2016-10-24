
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

type const = {
  name: string;
  id: int;
  mutable properties: Properties.t;
  mutable defs: def list;
  mutable doc: string;
}

and t =
  | Const of const
  | App of t * t array
  | Z of Z.t
  | Q of Q.t
  | String of string
  | Reg of int (* only in rules RHS *)

(* (partial) definition of a symbol *)
and def =
  | Rewrite of rewrite_rule
  | Fun of prim_fun

and rewrite_rule = {
  rr_pat: pattern;
  rr_rhs: t;
}

and pattern =
  | P_const of const
  | P_z of Z.t
  | P_q of Q.t
  | P_string of string
  | P_app of pattern * pattern array (* TODO: actually a list/tree for A/AC match *)
  | P_blank
  | P_fail
  | P_bind of int * pattern
    (* match, then bind register *)
  | P_check_same of int * pattern
    (* match, then check syntactic equality with content of register *)
  | P_alt of pattern list

(* state for evaluation *)
and eval_state = {
  mutable st_iter_count: int;
  (* number of iterations *)
  st_prim: prim_fun_args;
  (* to give to primitive functions *)
}

(* TODO? *)
and prim_fun_args = unit

and prim_fun = prim_fun_args -> t -> t option

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
        name;
        properties=Properties.empty;
        id= bank.const_id;
        defs=[];
        doc="";
      } in
    bank.const_id <- bank.const_id + 1;
    Str_tbl.add bank.by_name name c;
    c

let true_ = const_of_string "True"
let false_ = const_of_string "False"
let null = const_of_string "Null"

let app hd args = App (hd, args)

let app_flatten hd args =
  (* splicing *)
  let must_splice, res_len =
    Array.fold_left
      (fun (must_split,len) arg -> match arg with
         | App (Const {name="Sequence";_}, sub) -> true, len+Array.length sub
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
           | App (Const {name="Sequence";_}, sub) ->
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

(** {2 Constants} *)

exception No_head

let rec head = function
  | Const c -> c
  | App (hd, _) -> head hd
  | Z _ | Q _ | String _ | Reg _ -> raise No_head

module Cst = struct
  type t = const

  let equal a b = a.id = b.id

  let get_field f c = Properties.get f c.properties

  let set_field f b c = c.properties <- Properties.set f b c.properties

  let add_def d c = c.defs <- d :: c.defs

  let set_doc d c = c.doc <- d
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
  | Const {name; _} -> Format.pp_print_string out name
  | App (head, args) ->
    Format.fprintf out "@[<2>%a[@[<hv>%a@]]@]"
      pp_full_form head (CCFormat.array ~start:"" ~stop:"" ~sep:"," pp_full_form) args
  | Z n -> Z.pp_print out n
  | Q n -> Q.pp_print out n
  | String s -> Format.fprintf out "%S" s
  | Reg i -> Format.fprintf out "Slot[%d]" i

let rec pp_pattern out (p:pattern) = match p with
  | P_const {name; _} -> Format.pp_print_string out name
  | P_app (head, args) ->
    Format.fprintf out "@[<2>%a[@[<hv>%a@]]@]"
      pp_pattern head (CCFormat.array ~start:"" ~stop:"" ~sep:"," pp_pattern) args
  | P_z n -> Z.pp_print out n
  | P_q n -> Q.pp_print out n
  | P_string s -> Format.fprintf out "%S" s
  | P_blank -> CCFormat.string out "Blank[]"
  | P_fail -> CCFormat.string out "Fail[]"
  | P_alt ([] | [_]) -> assert false
  | P_alt l ->
    Format.fprintf out "(@[<hv>%a@])"
      (CCFormat.list ~start:"" ~stop:"" ~sep:"|" pp_pattern) l
  | P_bind (i,p) -> Format.fprintf out "Pattern[%d,@[%a@]]" i pp_pattern p
  | P_check_same (i,p) -> Format.fprintf out "CheckSame[%d,@[%a@]]" i pp_pattern p

let to_string_compact t =
  let buf = Buffer.create 32 in
  let rec aux t = match t with
    | Const {name; _} -> Buffer.add_string buf name
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

(** {2 Evaluation} *)

exception Invalid_rule of string

let invalid_rule msg = raise (Invalid_rule msg)
let invalid_rulef msg = CCFormat.ksprintf msg ~f:invalid_rule

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
    | App (Const {name="Blank";_},[||]) -> P_blank
    | App (Const {name="Pattern";_},
        [| Const {name=x;_}; sub |]) ->
      (* [x] on the stack -> failure, would lead to cyclical subst *)
      if Sequence.of_stack surrounding |> Sequence.mem x then (
        invalid_rulef "variable `%s` cannot appear in its own pattern" x
      );
      (* compute pattern itself *)
      let sub_p = match sub with
        | App (Const {name="Blank";_}, [||]) -> P_blank (* trivial case *)
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
    | App (Const {name="Alternatives";_}, [| |]) -> P_fail
    | App (Const {name="Alternatives";_}, [| p |]) -> aux_pat p
    | App (Const {name="Alternatives";_}, a) ->
      let l = CCList.init (Array.length a) (fun i -> aux_pat a.(i)) in
      P_alt l
    | App (hd, args) ->
      (* otherwise, match structurally *)
      P_app (aux_pat hd, Array.map aux_pat args)
    | Reg _ -> assert false
  (* convert variables in RHS *)
  and aux_rhs t = match t with
    | Z _ | Q _ | String _ -> t
    | App (hd, args) ->
      let hd = aux_rhs hd in
      let args = Array.map aux_rhs args in
      app hd args
    | Reg _ -> assert false
    | Const {name;_} ->
      begin match CCHashtbl.get tbl name with
        | None -> t
        | Some i -> reg i (* lookup *)
      end
  in
  let pat = aux_pat lhs in
  let rhs = aux_rhs rhs in
  {rr_pat=pat; rr_rhs=rhs }

let def_fun f = Fun f

let def_rule ~lhs ~rhs =
  try
    let r = compile_rule lhs rhs in
    Result.Ok (Rewrite r)
  with Invalid_rule str ->
    Result.Error str

exception Eval_fail of string

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
    | Const c1, Const c2 -> c1.id = c2.id
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

(* return all the matches of [pat] against [e], modifying [st]
   every time in a backtracking way *)
let rec match_ (subst:Subst.t) (pat:pattern) (e:t): Subst.t Sequence.t =
  (* TODO: Condition *)
  (* TODO: BlankSequence *)
  (* TODO: BlankSequenceNull *)
  (* TODO: AC matching… *)
  let rec match_pair (subst:Subst.t) (pat:pattern) (e:t) yield: unit =
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
             if equal e other then yield subst)
      | P_app (hd, args), App (hd', args')
        when Array.length args = Array.length args' ->
        match_pair subst hd hd'
          (fun subst -> match_arrays subst args args' 0 yield)
      | P_fail, _ -> ()
      | P_alt l, _ -> match_alt subst l e yield
      | P_z _, _
      | P_q _, _
      | P_const _, _
      | P_string _, _
      | P_app _, _
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
  in
  match_pair subst pat e

and eval_rec (st:eval_state) e = match e with
  | Z _
  | Q _
  | String _ -> e
  | Const c ->
    (* [c] might have a definition *)
    try_rules st e c.defs
  | App (Const {name="CompoundExpression";_}, ([| |] | [| _ |])) -> assert false
  | App (Const {name="CompoundExpression";_}, args) ->
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
  | App (Const {name="SetDelayed";_}, [| a; b |]) ->
    (* lazy set: add rewrite rule [a :> b] to the definitions of [head a] *)
    begin match head a with
      | c ->
        let rule = compile_rule a b in
        c.defs <- Rewrite rule :: c.defs;
      | exception No_head ->
        eval_failf "cannot assign to %a" pp_full_form a
    end;
    null
  | App (Const {name="Set";_}, [| a; b |]) ->
    (* eager set: eval [b], then add [a :> b] to the defs of [head a] *)
    let b = eval_rec st b in
    begin match head a with
      | c ->
        let rule = compile_rule a b in
        c.defs <- Rewrite rule :: c.defs;
      | exception No_head ->
        eval_failf "cannot assign to %a" pp_full_form a
    end;
    b
  | App (hd, args) ->
    let hd = eval_rec st hd in
    (* evaluate arguments, but only if [hd] allows it *)
    let args = eval_args_of st hd args in
    begin match head hd with
      | c ->
        (* try every definition of [c] *)
        try_rules st (app_flatten hd args) c.defs
      | exception No_head ->
        (* just return the new term *)
        app_flatten hd args
    end
  | Reg _ -> e (* cannot evaluate *)

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

(* try rules [defs] one by one, until one matches *)
and try_rules (st:eval_state) t defs = match defs with
  | [] -> t
  | Rewrite rule :: tail ->
    let subst_opt =
      match_ Subst.empty rule.rr_pat t |> Sequence.head
    in
    begin match subst_opt with
      | None -> try_rules st t tail
      | Some subst ->
        let t' = Subst.apply subst rule.rr_rhs in
        st.st_iter_count <- st.st_iter_count + 1;
        eval_rec st t'
    end
  | Fun f :: tail ->
    begin match f st.st_prim t with
      | None -> try_rules st t tail
      | Some t' ->
        st.st_iter_count <- st.st_iter_count + 1;
        eval_rec st t'
    end

let create_eval_state() : eval_state = {
  st_iter_count=0;
  st_prim=();
}

let eval e =
  let st = create_eval_state () in
  eval_rec st e

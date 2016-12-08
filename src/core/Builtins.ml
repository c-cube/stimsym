
(* This file is free software. See file "license" for more details. *)

(** {1 Builtin Functions} *)

module E = Expr
module Fmt = CCFormat

type t = Expr.t

exception Eval_does_not_apply

let all_ = ref []

(* a function definition. Takes [self_cst, eval_fun, t] and evaluates [t]
   into [None] (fail) or [Some t'] *)
type fun_def = E.const -> (E.t -> E.t) -> E.t -> E.t option

let mk_ ?(doc="") ?printer ?(fields=[]) ?(funs=[]) name =
  let c =
    E.const_of_string_with name
      ~f:(fun c ->
        List.iter
          (fun field -> E.Cst.set_field field true c)
          fields;
        List.iter
          (fun d ->
             let d_protected eval t =
               try d c eval t with Eval_does_not_apply -> None
             in
             E.Cst.add_def (E.def_fun d_protected) c)
          funs;
        CCOpt.iter (fun (i,p) -> E.Cst.set_printer i p c) printer;
        E.Cst.set_doc doc c)
  in
  all_ := c :: !all_;
  c

let prec_semicolon = 2
let prec_set = 3
let prec_rule = 5
let prec_condition = 6
let prec_replace = 7
let prec_alternatives = 8
let prec_same = 10

let prec_or = 21
let prec_and = 22
let prec_not = 23

let prec_plus = 40
let prec_times = 41
let prec_factorial = 45

let prec_pattern = 90
let prec_blank = 95
let prec_list = 100

let hold = mk_ "Hold" ~fields:[E.field_hold_all]

let full_form = mk_ "FullForm" ~fields:[E.field_hold_all]

let sequence = mk_ "Sequence" ~fields:[E.field_flatten]

type arith_res =
  | Arith_q of Q.t * E.t list
  | Arith_z of Z.t * E.t list

let true_ = mk_ "True"
let false_ = mk_ "False"

let print_infix_ prec sep neutral _ pp_sub out args = match args with
  | [||] -> Fmt.string out neutral
  | [|x|] -> pp_sub prec out x
  | _ ->
    Fmt.fprintf out "@[<hv>%a@]"
      (Fmt.array ~start:"" ~stop:"" ~sep (pp_sub prec)) args

let print_infix_bin_ prec op _ pp_sub out args = match args with
  | [|x;y|] ->
    Fmt.fprintf out "@[<hv>%a%s@,%a@]" (pp_sub prec) x op (pp_sub prec) y
  | _ -> raise E.Print_default

let plus =
  let eval self _ (e:E.t): E.t option =
    let compute args =
      Array.fold_left
        (fun acc e -> match acc, e with
           | Arith_z (n1,l), E.Z n2 -> Arith_z (Z.(n1 + n2),l)
           | Arith_z (n1,l), E.Q n2 -> Arith_q (Q.(of_bigint n1 + n2),l)
           | Arith_q (n1,l), E.Z n2 -> Arith_q (Q.(n1 + of_bigint n2),l)
           | Arith_q (n1,l), E.Q n2 -> Arith_q (Q.(n1 + n2), l)
           | Arith_z (n,l), _ -> Arith_z (n,e::l)
           | Arith_q (n,l), _ -> Arith_q (n,e::l))
        (Arith_z (Z.zero, [])) args
    in
    match e with
      | E.App (_, [| |]) -> None
      | E.App (E.Const c as hd, args) ->
        assert (E.Cst.equal c self);
        let args' = match compute args with
          | Arith_z (n,[]) -> [E.z n]
          | Arith_z (n,l) when Z.sign n=0 -> List.rev l
          | Arith_z (n,l) -> E.z n :: List.rev l
          | Arith_q (n,[]) -> [E.q n]
          | Arith_q (n,l) when Q.sign n=0 -> List.rev l
          | Arith_q (n,l) -> E.q n :: List.rev l
        in
        begin match args' with
          | [] -> assert false
          | [e] -> Some e
          | _ ->
            let args' = Array.of_list args' in
            if Array.length args = Array.length args'
            then None
            else Some (E.app hd args')
        end
      | _ -> None
  in
  mk_ "Plus" ~funs:[eval]
    ~fields:[ E.field_one_identity; E.field_flatten; E.field_orderless]
    ~printer:(prec_plus, print_infix_ prec_plus "+" "0")
    ~doc:"Addition operator (infix form: `a + b + c + d`)"

let times =
  let eval self _ (e:E.t): E.t option =
    let z_is_one z = Z.equal z Z.one in
    let q_is_one q = Q.equal q Q.one in
    let compute args =
      Array.fold_left
        (fun acc e -> match acc, e with
           | Arith_z (n1,l), E.Z n2 -> Arith_z (Z.(n1 * n2),l)
           | Arith_z (n1,l), E.Q n2 -> Arith_q (Q.(of_bigint n1 * n2),l)
           | Arith_q (n1,l), E.Z n2 -> Arith_q (Q.(n1 * of_bigint n2),l)
           | Arith_q (n1,l), E.Q n2 -> Arith_q (Q.(n1 * n2), l)
           | Arith_z (n,l), _ -> Arith_z (n,e::l)
           | Arith_q (n,l), _ -> Arith_q (n,e::l))
        (Arith_z (Z.one, [])) args
    in
    match e with
      | E.App (_, [| |]) -> None
      | E.App (E.Const c as hd, args) ->
        assert (E.Cst.equal c self);
        let args' = match compute args with
          | Arith_z (n,[]) -> [E.z n]
          | Arith_z (n,l) when z_is_one n -> List.rev l
          | Arith_z (n,l) -> E.z n :: List.rev l
          | Arith_q (n,[]) -> [E.q n]
          | Arith_q (n,l) when q_is_one n -> List.rev l
          | Arith_q (n,l) -> E.q n :: List.rev l
        in
        begin match args' with
          | [] -> assert false
          | [e] -> Some e
          | _ ->
            let args' = Array.of_list args' in
            if Array.length args = Array.length args'
            then None
            else Some (E.app hd args')
        end
      | _ -> None
  in
  mk_ "Times" ~funs:[eval]
    ~fields:[ E.field_one_identity; E.field_flatten; E.field_orderless]
    ~printer:(prec_times, print_infix_ prec_times " " "1")
    ~doc:"Product operator (infix: `a b c d`)"

let factorial =
  let eval self _eval t = match t with
    | E.App (E.Const c, [| E.Z n |]) when Z.sign n >=0 ->
      assert (E.Cst.equal self c);
      let n = try Z.to_int n with _ -> raise Eval_does_not_apply in
      let rec aux n acc : Z.t =
        if n=0 then acc
        else aux (n-1) Z.(of_int n * acc)
      in
      Some (E.z (aux n Z.one))
    | _ -> None
  in
  let pp _ pp_sub out = function
    | [| a |] -> Fmt.fprintf out "%a!" (pp_sub prec_factorial) a
    | _ -> raise E.Print_default
  in
  mk_ "Factorial" ~printer:(prec_factorial,pp)
    ~doc:"Factorial operator (postfix: `a!`)" ~funs:[eval]

let list =
  let pp_list _ pp_sub out a =
    Fmt.fprintf out "{@[<hv>%a@]}"
      (Fmt.array ~start:"" ~stop:"" ~sep:"," (pp_sub 0)) a
  in
  mk_ ~printer:(prec_list,pp_list) "List"

let blank =
  let pp _ _ out args = match args with
    | [||] -> Fmt.string out "_"
    | _ -> raise E.Print_default
  in
  mk_ "Blank"
    ~printer:(prec_blank,pp) ~fields:[E.field_hold_all; E.field_protected] ~doc:"`_`"

let blank_seq =
  let pp _ _ out args = match args with
    | [||] -> Fmt.string out "__"
    | _ -> raise E.Print_default
  in
  mk_ "BlankSequence"
    ~printer:(prec_blank,pp)~fields:[E.field_hold_all; E.field_protected] ~doc:"`__`"

let blank_null_seq =
  let pp _ _ out args = match args with
    | [||] -> Fmt.string out "___"
    | _ -> raise E.Print_default
  in
  mk_ "BlankNullSequence"
    ~printer:(prec_blank,pp) ~fields:[E.field_hold_all; E.field_protected] ~doc:"`___`"

let pattern =
  let pp _ _pp_sub out = function
    | [| E.Const {E.cst_name=x;_}; sub |] ->
      begin match sub with
        | E.App (E.Const {E.cst_name="Blank";_}, [||]) ->
          Fmt.fprintf out "%s_" x
        | E.App (E.Const {E.cst_name="BlankSequence";_}, [||]) ->
          Fmt.fprintf out "%s__" x
        | E.App (E.Const {E.cst_name="BlankNullSequence";_}, [||]) ->
          Fmt.fprintf out "%s___" x
        | _ -> raise E.Print_default
      end
    | _ -> raise E.Print_default
  in
  mk_ "Pattern" ~printer:(prec_pattern,pp) ~fields:[E.field_hold_all; E.field_protected]

let same_q =
  let eval _ _ e = match e with
    | E.App (_, args) when Array.length args >= 2 ->
      let rec aux i =
        if i+1 = Array.length args then Some true_
        else (
          if E.equal args.(i) args.(i+1)
          then aux (i+1)
          else Some false_
        )
      in
      aux 0
    | _ -> None
  in
  mk_ "SameQ" ~funs:[eval]
    ~fields:[E.field_protected]
    ~printer:(prec_same, print_infix_bin_ prec_same "===")
    ~doc:"symbolic identity (infix: `a === b`)"

let set =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a = %a" (pp_sub prec_set) lhs (pp_sub prec_set) rhs
    | _ -> raise E.Print_default
  in
  mk_ "Set" ~printer:(prec_set, pp)
    ~fields:[E.field_hold_first; E.field_protected]
    ~doc:"Eager assignment. Infix: `a = b`"

let set_delayed =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a := %a" (pp_sub prec_set) lhs (pp_sub prec_set) rhs
    | _ -> raise E.Print_default
  in
  mk_ "SetDelayed" ~printer:(prec_set, pp)
    ~fields:[E.field_hold_all; E.field_protected]
    ~doc:"Lazy assignment. Infix: `a := b`"

let rule =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a -> %a" (pp_sub prec_rule) lhs (pp_sub prec_rule) rhs
    | _ -> raise E.Print_default
  in
  mk_ "Rule" ~printer:(prec_rule,pp)
    ~fields:[E.field_hold_first; E.field_protected]
    ~doc:"Eager rewrite rule. Infix: `a -> b`"

let rule_delayed =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a :> %a" (pp_sub prec_rule) lhs (pp_sub prec_rule) rhs
    | _ -> raise E.Print_default
  in
  mk_ "RuleDelayed"  ~printer:(prec_rule,pp)
    ~fields:[E.field_hold_all; E.field_protected]
    ~doc:"Lazy rewrite rule. Infix: `a :> b`"

let condition =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a /; %a" (pp_sub prec_condition) lhs (pp_sub prec_condition) rhs
    | _ -> raise E.Print_default
  in
  mk_ "Condition"
    ~printer:(prec_condition,pp)
    ~fields:[E.field_protected; E.field_hold_all]
    ~doc:"Conditional pattern. Infix: `a /; b`"

let replace_all =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a /. %a" (pp_sub prec_replace) lhs (pp_sub prec_replace) rhs
    | _ -> raise E.Print_default
  in
  mk_ "ReplaceAll" ~printer:(prec_replace,pp)
    ~fields:[E.field_hold_first; E.field_protected]
    ~doc:"Replacement by rewrite rules. Infix: `a /. rules`"

let replace_repeated =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a //. %a" (pp_sub prec_replace) lhs (pp_sub prec_replace) rhs
    | _ -> raise E.Print_default
  in
  mk_ "ReplaceRepeated" ~printer:(prec_replace,pp)
    ~fields:[E.field_hold_all; E.field_protected]
    ~doc:"Replacement by rewrite rules until fixpoint. Infix: `a //. rules`"

let alternatives =
  mk_
    ~printer:(prec_alternatives,print_infix_ prec_alternatives "|" "")
    "Alternatives"

let compound_expr =
  mk_ "CompoundExpression"
    ~printer:(prec_semicolon,print_infix_ prec_semicolon ";" "")
    ~doc:"Sequence of operations. Infix: `a; b`"

let if_ =
  let eval _ eval_st t = match t with
    | E.App (E.Const _, [| a; b; c |]) ->
      begin match Expr.prim_eval eval_st a with
        | Expr.Const {Expr.cst_name="True";_} -> Some b
        | Expr.Const {Expr.cst_name="False";_} -> Some c
        | _ -> None
      end
    | _ -> None
  in
  mk_ "If" ~funs:[eval]
    ~fields:[E.field_hold_rest] ~doc:"Test operator. `If[A,B,C]`."

type changed = bool

type and_res =
  | And_lst of Expr.t list * changed
  | And_false

let and_ =
  let eval self eval_st t = match t with
    | E.App (E.Const _, arr) ->
      let res =
        Array.fold_left
          (fun acc e -> match acc with
             | And_false -> acc
             | And_lst (l_acc, changed) ->
               begin match Expr.prim_eval eval_st e with
                 | Expr.Const {Expr.cst_name="True";_} -> And_lst (l_acc,true)
                 | Expr.Const {Expr.cst_name="False";_} -> And_false
                 | e' ->
                   let changed = changed || not (Expr.equal e e') in
                   And_lst (e' :: l_acc, changed)
               end)
          (And_lst ([], false)) arr
      in
      begin match res with
        | And_false -> Some false_
        | And_lst ([], _) -> Some true_
        | And_lst (_, false) -> None
        | And_lst (l, true) ->
          Some (E.app_l (E.const self) (List.rev l))
      end
    | _ -> None
  in
  mk_ "And" ~fields:[E.field_flatten; E.field_orderless] ~funs:[eval]
    ~printer:(prec_and,print_infix_ prec_and "&&" "True")
    ~doc:"Logical conjunction. Infix: `a && b`"

type or_res =
  | Or_lst of Expr.t list * changed
  | Or_true

let or_ =
  let eval self eval_st t = match t with
    | E.App (E.Const _, arr) ->
      let res =
        Array.fold_left
          (fun acc e -> match acc with
             | Or_true -> acc
             | Or_lst (l_acc, changed) ->
               begin match E.prim_eval eval_st e with
                 | E.Const {E.cst_name="True";_} -> Or_true
                 | E.Const {E.cst_name="False";_} -> Or_lst (l_acc, true)
                 | e' ->
                   let changed = changed || not (Expr.equal e e') in
                   Or_lst (e' :: l_acc, changed)
               end)
          (Or_lst ([], false)) arr
      in
      begin match res with
        | Or_true -> Some true_
        | Or_lst ([], _) -> Some false_
        | Or_lst (_, false) -> None
        | Or_lst (l, true) ->
          Some (E.app_l (E.const self) (List.rev l))
      end
    | _ -> None
  in
  mk_ "Or"
    ~fields:[E.field_flatten; E.field_orderless] ~funs:[eval]
    ~printer:(prec_or,print_infix_ prec_or "||" "False")
    ~doc:"Logical disjunction. Infix: `a || b`"

let not_ =
  let eval self eval_st t = match t with
    | E.App (E.Const self', [| a |]) ->
      assert (Expr.Cst.equal self self');
      begin match Expr.prim_eval eval_st a with
        | Expr.Const {Expr.cst_name="True";_} -> Some false_
        | Expr.Const {Expr.cst_name="False";_} -> Some true_
        | a' ->
          if Expr.equal a a' then None
          else Some (E.app (E.const self) [| a' |])
      end
    | _ -> None
  and pp _ pp_sub out = function
    | [| a |] -> Fmt.fprintf out "!%a" (pp_sub prec_not) a
    | _ -> raise E.Print_default
  in
  mk_ "Not"
    ~fields:[E.field_flatten; E.field_orderless] ~funs:[eval]
    ~printer:(prec_not,pp)
    ~doc:"Logical negation. Prefix: `!a`"

let nest =
  let eval _self _ e = match e with
    | E.App (_, [| f; e; E.Z n |]) ->
      if Z.sign n < 0 then None
      else (
        try
          let n = Z.to_int n in
          let rec aux n k =
            if n=0 then k e
            else aux (n-1) (fun sub -> k (E.app f [| sub |]))
          in
          Some (aux n (fun e->e))
        with _ ->
          None
      )
    | _ -> None
  in
  mk_ "Nest"
    ~doc:"`Nest[f,e,n]` returns `f[f[…[f[e]]]]` nested `n` times"
    ~funs:[eval]

(* TODO: define them all (on constants) *)
let equal = mk_ "Equal" ~doc:"value identity (infix: `a == b`)"
let less = mk_ "Less" ~doc:"value comparison (infix: `a < b`)"
let less_equal = mk_ "LessEqual" ~doc:"value comparison (infix: `a <= b`)"
let greater = mk_ "Greater" ~doc:"value comparison (infix: `a > b`)"
let greater_equal = mk_ "GreaterEqual" ~doc:"value comparison (infix: `a >= b`)"
let inequality =
  mk_ "Inequality"
    ~fields:[E.field_flatten; E.field_protected]
    ~doc:"conjunction of comparisons"

let null = mk_ "Null"
let print = mk_ "Print" ~doc:"Print the expression and return Null"

let all_builtins () = !all_

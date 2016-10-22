
(* This file is free software. See file "license" for more details. *)

(** {1 Builtin Functions} *)

module E = Expr

type t = Expr.t

exception Eval_does_not_apply

let all_ = ref []

(* a function definition. Takes [self_cst, eval_fun, t] and evaluates [t]
   into [None] (fail) or [Some t'] *)
type fun_def = E.const -> (E.t -> E.t) -> E.t -> E.t option

let mk_ ?(doc="") ?(fields=[]) ?(funs=[]) name =
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
        E.Cst.set_doc doc c)
  in
  all_ := c :: !all_;
  c

let hold = mk_ "Hold" ~fields:[E.field_hold_all]

let full_form = mk_ "FullForm" ~fields:[E.field_hold_all]

(* TODO: auto-flatten inside anything else: `F[Sequence[a,b],c] --> F[a,b,c]` *)
let sequence = mk_ "Sequence" ~fields:[E.field_flatten]

type arith_res =
  | Arith_q of Q.t * E.t list
  | Arith_z of Z.t * E.t list

let true_ = mk_ "True"
let false_ = mk_ "False"

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
  mk_ "Factorial"
    ~doc:"Factorial operator (postfix: `a!`)" ~funs:[eval]

let list = mk_ "List"

let blank =
  mk_ "Blank" ~fields:[E.field_hold_all; E.field_protected] ~doc:"`_`"
let blank_seq =
  mk_ "BlankSequence" ~fields:[E.field_hold_all; E.field_protected] ~doc:"`__`"
let blank_null_seq =
  mk_ "BlankNullSequence" ~fields:[E.field_hold_all; E.field_protected] ~doc:"`___`"
let pattern = mk_ "Pattern" ~fields:[E.field_hold_all; E.field_protected]

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
    ~doc:"symbolic identity (infix: `a === b`)"

let set =
  mk_ "Set"
    ~fields:[E.field_hold_first; E.field_protected]
    ~doc:"Eager assignment. Infix: `a = b`"

let set_delayed =
  mk_ "SetDelayed"
    ~fields:[E.field_hold_all; E.field_protected]
    ~doc:"Lazy assignment. Infix: `a := b`"

let rule =
  mk_ "Rule"
    ~fields:[E.field_hold_first; E.field_protected]
    ~doc:"Eager rewrite rule. Infix: `a -> b`"

let rule_delayed =
  mk_ "RuleDelayed"
    ~fields:[E.field_hold_all; E.field_protected]
    ~doc:"Lazy rewrite rule. Infix: `a :> b`"

let condition =
  mk_ "Condition" ~fields:[E.field_protected; E.field_hold_all]
    ~doc:"Conditional pattern. Infix: `a /; b`"

(* TODO: define *)
let replace_all =
  mk_ "ReplaceAll"
    ~fields:[E.field_hold_first; E.field_protected]
    ~doc:"Replacement by rewrite rules. Infix: `a /. rules`"

(* TODO: define *)
let replace_repeated =
  mk_ "ReplaceRepeated"
    ~fields:[E.field_hold_all; E.field_protected]
    ~doc:"Replacement by rewrite rules until fixpoint. Infix: `a //. rules`"

let alternatives = mk_ "Alternatives"

(* TODO: define *)
let if_ =
  mk_ "If" ~fields:[E.field_hold_rest] ~doc:"Test operator. `If[A,B,C]`."

let and_ =
  (* TODO eval rule *)
  mk_ "And" ~fields:[E.field_flatten; E.field_orderless]
    ~doc:"Logical conjunction. Infix: `a && b`"

let or_ =
  (* TODO eval rule *)
  mk_ "Or"
    ~fields:[E.field_flatten; E.field_orderless]
    ~doc:"Logical disjunction. Infix: `a || b`"

let not_ =
  (* TODO eval rule *)
  mk_ "Not"
    ~fields:[E.field_flatten; E.field_orderless]
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

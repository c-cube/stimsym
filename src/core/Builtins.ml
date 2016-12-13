
(* This file is free software. See file "license" for more details. *)

(** {1 Builtin Functions} *)

module E = Expr
module Fmt = CCFormat

type t = Expr.t

exception Eval_does_not_apply

let all_ : E.t list ref = ref []
let log_ : (string -> unit) ref = ref (fun _ -> ())
let log s = !log_ s
let logf s = Printf.ksprintf log s

(* a function definition. Takes [self_cst, eval_fun, t] and evaluates [t]
   into [None] (fail) or [Some t'] *)
type fun_def = E.const -> E.prim_fun_args -> E.t -> E.t option

let make ?(doc:Document.t=[])
    ?printer ?display ?(fields=[])
    ?(funs:fun_def list = [])
    ?(rules:(E.const -> t * t) list = [])
    name =
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
             E.Cst.add_def (Eval.def_fun d_protected) c)
          funs;
        List.iter
          (fun mk_rule ->
             let lhs, rhs = mk_rule c in
             match Eval.def_rule ~lhs ~rhs with
               | Result.Ok r -> E.Cst.add_def r c
               | Result.Error msg ->
                 failwith (Fmt.sprintf
                     "rule `@[%a := %a@]`@ is not valid: %s"
                     E.pp_full_form lhs E.pp_full_form rhs msg))
          rules;
        CCOpt.iter (fun (i,p) -> E.Cst.set_printer i p c) printer;
        CCOpt.iter (fun f -> E.Cst.set_display f c) display;
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

let prec_function = 15

let prec_comprehension = 18
let prec_comprehension_args = 19

let prec_or = 21
let prec_and = 22
let prec_not = 23

let prec_ineq = 30

let prec_plus = 40
let prec_times = 41
let prec_power = 42
let prec_factorial = 45

let prec_pattern_test = 70

let prec_pattern = 90
let prec_blank = 95
let prec_slot = 96
let prec_list = 100

let prec_const = 200

let null = make "Null"
let hold =
  make "Hold" ~fields:[E.field_hold_all]
    ~doc:[
      `S "Hold";
      `P "`Hold[e]` holds `e` as is, blocking evaluation. \
          It does not do anything else."
    ]

let full_form =
  make "FullForm" ~fields:[E.field_hold_all]
    ~doc:[
      `S "FullForm";
      `P "`FullForm[e]` prints `e` in the unsugared syntax, \
          as a S-expression. It is useful to see how exactly \
          an expression is parsed.";
      `P "`FullForm[e]` blocks the evaluation of `e`.";
      `I ("example", [`P "`FullForm[1+1]` prints `Plus[1,1]`"]);
    ]

let sequence = make "Sequence" ~fields:[E.field_flatten]

type arith_res =
  | Arith_q of Q.t * E.t list
  | Arith_z of Z.t * E.t list

let true_ = make "True"
let false_ = make "False"

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

(* print an unapplied constant *)
let print_const str _ _ out = function
  | [||] -> Fmt.string out str
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
  make "Plus" ~funs:[eval]
    ~fields:[ E.field_one_identity; E.field_flatten; E.field_orderless]
    ~printer:(prec_plus, print_infix_ prec_plus "+" "0")
    ~doc:[
      `S "Plus";
      `P "Addition operator. Associative, Commutative, with \
         regular evaluation on numbers.";
      `I ("neutral element", [`P "0"]);
      `I ("infix form", [`P "`a + b + c + d`"]);
    ]

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
  make "Times" ~funs:[eval]
    ~fields:[ E.field_one_identity; E.field_flatten; E.field_orderless]
    ~printer:(prec_times, print_infix_ prec_times " " "1")
    ~doc:[
      `S "Times";
      `P "Product operator, associative commutative.\
          Interpreted on numbers.";
      `I ("neutral element", [`P "1"]);
      `I ("infix", [`P "`a b c d`"]);
    ]

let power =
  (* fast exponentiation on Q *)
  let rec q_pow x n : Q.t =
    if n=0 then Q.one
    else if n=1 then x
    else if n mod 2 = 0 then (
      let x2 = q_pow x (n/2) in
      Q.mul x2 x2
    ) else Q.mul x (q_pow x (n-1))
  in
  let eval_aux e n = match e with
    | E.Z x -> E.z (Z.pow x n)
    | E.Q x -> E.q (q_pow x n)
    | _ -> assert false
  in
  let eval self _ (e:E.t): E.t option = match e with
    | E.App (E.Const _, [| (E.Q _ | E.Z _) as x; E.Z n |]) when Z.sign n >= 0 ->
      let n = try Z.to_int n with _ -> raise Eval_does_not_apply in
      Some (eval_aux x n)
    | _ -> raise Eval_does_not_apply
  (* rule: (f_^n_?IntegerQ)[x_] := Nest[f,x,n] *)
  and rule_power_fun _ =
    E.of_string_full_form_exn
      "Power[Pattern[f,Blank[]],\
       PatternTest[Pattern[n,Blank[]],IntegerQ]][Pattern[x,Blank[]]]",
    E.of_string_full_form_exn "Nest[f,x,n]"
  in
  make "Power"
    ~funs:[eval]
    ~rules:[rule_power_fun]
    ~fields:[ E.field_one_identity; E.field_flatten; E.field_orderless]
    ~printer:(prec_times, print_infix_ prec_power "^" "1")
    ~doc:[
      `S "Power";
      `P "Power operator, interpreted on numbers if the second argument \
          is an integer.";
      `P "`(f^n)[x]` is interpreted as `Nest[f,x,n]`";
      `I ("infix", [`P "`a^b`"]);
      `I ("example", [`P "`2^10` yield 1024"]);
    ]

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
  make "Factorial" ~printer:(prec_factorial,pp)
    ~funs:[eval] ~fields:[E.field_listable]
    ~doc:[
      `S "Factorial";
      `P "The factorial function";
      `I ("example", [`P "`5! == 120"]);
      `I ("postfix", [`P "`a!`"]);
    ]

let q_is_int_ q = Q.equal (Q.to_bigint q |> Q.of_bigint) q

let floor =
  let eval _ _ = function
    | E.App (_, [| E.Z _ as x |]) -> Some x
    | E.App (_, [| E.Q q |]) ->
      let x =
        if Q.sign q>=0 || q_is_int_ q
        then Q.to_bigint q
        else Q.to_bigint (Q.add q Q.minus_one)
      in
      Some (E.z x)
    | _ -> raise Eval_does_not_apply
  in
  make "Floor" ~funs:[eval] ~fields:[E.field_listable]

let ceil =
  let eval _ _ = function
    | E.App (_, [| E.Z _ as x |]) -> Some x
    | E.App (_, [| E.Q q |]) ->
      let x =
        if Q.sign q<0 || q_is_int_ q
        then Q.to_bigint q
        else Q.to_bigint (Q.add Q.one q)
      in
      Some (E.z x)
    | _ -> raise Eval_does_not_apply
  in
  make "Ceil" ~funs:[eval] ~fields:[E.field_listable]

let random =
  let st = lazy (Random.State.make_self_init ()) in
  let rand i j : Q.t =
    if i>=j then raise Eval_does_not_apply;
    let lazy st = st in
    let fraction_precision = 1 lsl 20 in
    let fraction = Random.State.int st (fraction_precision+1) in
    let offset = Random.State.int st (j-i) in
    Q.(of_int i + of_int offset + (of_int fraction / of_int fraction_precision))
  in
  let eval _ _ = function
    | E.App (_, [| |]) -> Some (rand 0 1 |> E.q)
    | E.App (_, [| E.Z i |]) when Z.sign i>0 ->
      let i = try Z.to_int i with _ -> raise Eval_does_not_apply in
      Some (rand 0 i |> E.q)
    | E.App (_, [| E.Z i; E.Z j |]) when Z.compare i j<0 ->
      let i = try Z.to_int i with _ -> raise Eval_does_not_apply in
      let j = try Z.to_int j with _ -> raise Eval_does_not_apply in
      Some (rand i j |> E.q)
    | _ -> raise Eval_does_not_apply
  in
  make "Random" ~funs:[eval]
    ~doc:[
      `S "Random";
      `P "generates random numbers.";
      `L [
        [`P "`Random[]` returns a random rational in [0,1]"];
        [`P "`Random[i]` returns a random rational in [0,i] (if i>0)"];
        [`P "`Random[i,j]` returns a random rational in [i,j] (if i>j)"];
      ];
    ]

let list =
  let pp_list _ pp_sub out a =
    Fmt.fprintf out "{@[<hv>%a@]}"
      (Fmt.array ~start:"" ~stop:"" ~sep:"," (pp_sub 0)) a
  in
  make ~printer:(prec_list,pp_list) "List"

let match_bind =
  let pp _ pp_sub out = function
    | [| pat; rhs |] ->
      Fmt.fprintf out "@[<2>%a@ <- %a@]"
        (pp_sub prec_comprehension_args) pat (pp_sub prec_comprehension_args) rhs
    | _ -> raise E.Print_default
  in
  make "MatchBind" ~printer:(prec_comprehension_args,pp)
    ~fields:[E.field_hold_first]
    ~doc:[
      `S "MatchBind";
      `P "`MatchBind[pat,e]` matches `e` against pattern `pat`. \
          It will yield every matching substitution.";
      `I ("infix", [`P "`pat <- e`"]);
    ]

let match_bind1 =
  let pp _ pp_sub out = function
    | [| pat; rhs |] ->
      Fmt.fprintf out "@[<2>%a@ <<- %a@]"
        (pp_sub prec_comprehension_args) pat (pp_sub prec_comprehension_args) rhs
    | _ -> raise E.Print_default
  in
  make "MatchBind1" ~printer:(prec_comprehension_args,pp)
    ~fields:[E.field_hold_first]
    ~doc:[
      `S "MatchBind1";
      `P "`MatchBind1[pat,e]` matches every argument of `e` \
          against pattern `pat`. \
          For instance, `MatchBind1[f[x_], {a,f[b],c,f[d]}]` will \
          try the pattern `f[x_]` against each element of the list.";
      `I ("infix", [`P "`pat <<- e`"]);
    ]

(* sequence comprehension *)
let comprehension =
  let pp _ pp_sub out args = match args with
    | [||] -> raise E.Print_default
    | _ ->
      let res = args.(0) in
      Fmt.fprintf out "(@[<2>%a :: %a@])" (pp_sub prec_comprehension) res
        (Slice.print ~sep:"," (pp_sub prec_comprehension))
        (Slice.make args 1 ~len:(Array.length args-1))
  in
  make "Comprehension"
    ~fields:[E.field_hold_all; E.field_protected]
    ~printer:(prec_comprehension,pp)
    ~doc:[
      `S "Comprehension";
      `P "Sequence comprehension. It returns the sequence of all possibles \
          solutions to the definition.";
      (* TODO: have finer grained documents, with lists, etc. *)
      `P "General form: `Comprehension[t,body1,…,bodyn]` where `t` \
          is the returned term, and each `body` element is one of:";
      `L [
        [`P "`pat <- expr` (see `MatchBind`), \
             matching `pat` against expression `expr`. \
             Each resulting substitution is used to evaluate the following \
             body elements;"];
        [`P "`pat <<- expr` (see `MatchBind`), \
             matching `pat` against immediate arguments of expression `expr`;"];
        [`P "`expr`, meaning evaluation proceeds iff `expr` evaluates \
             to `True."];
      ];
      `I ("infix", [
          `P "The following is short for `Comprehension[t,body1,body2,…,bodyn]`:";
          `Pre "`t :: body1,body2,…,bodyn`";
        ]);
      `I ("example", [
          `Pre "{f[x y] :: x_<<-{1,2,3,4,5}, y_<-3, x+y<7}";
          `P "yields `{f[3],f[6],f[9]}`";
        ]);
      `I ("example", [
          `Pre "`f[x,y] :: g[x_]<<-{a,g[b],c,g[d]},y_<<-{1,2+x,3}`";
          `P "yields `Sequence[f[b,1],f[b,2+b],f[b,3],f[d,1],f[d,2+d],f[d,3]]`";
        ]);
      `I ("example", [
          `P "computing all prefixes of a list:";
          `Pre "`Inits[l_] := {{x} :: {x___,___} <- l}`";
          `Pre "`Inits[{1,2,3,4}]`";
          `P "the result is `{{},{1},{1,2},{1,2,3},{1,2,3,4}}`";
        ]);
      `I ("example", [
          `P "computing the permutations of a list:";
          `Pre "`Perms[{}] := {{}}`";
          `Pre "`Perms[{x_,r___}] := { {l1,x,l2} :: {l1___,l2___} <<- Perms[{r}]}`";
          `Pre "`Perms[{1,2,3}]`";
          `P "returns `{{1,2,3},{2,1,3},{2,3,1},{1,3,2},{3,1,2},{3,2,1}}`";
        ]);
    ]

let let_ =
  make "Let"
    ~fields:[E.field_hold_all; E.field_protected]
    ~doc:[
      `S "Let";
      `P "Let binding. It evaluates bindings in order, \
          then returns the last expression. \
          Only one result is returned, all backtracking is ignored.";
    ]

let blank =
  let pp _ _ out args = match args with
    | [||] -> Fmt.string out "_"
    | _ -> raise E.Print_default
  in
  make "Blank"
    ~printer:(prec_blank,pp) ~fields:[E.field_hold_all; E.field_protected]
    ~doc:[
      `S "Blank";
      `P "Blank pattern, `_`. Matches anything.\
          Can also be used with a name: `a_` binds `a` to anything.";
      `P "`Blank[f]` matches anything with head `f`. \
          It will match `f[a,b,c]` but not `f` or `g` or `g[a]`.";
      `I ("infix", [
          `P "`_` is `Blank[]`, matches anything";
          `P "`x_` is `Pattern[x,Blank[]]`, matches anything and binds it to `x`";
          `P "`_f` is `Blank[f]`, matches any application of symbol `f`";
          `P "`x_f` is `Pattern[x,Blank[f]]`, \
              matches any application of symbol `f` and binds to `x`";
        ]);
    ]

let blank_seq =
  let pp _ _ out args = match args with
    | [||] -> Fmt.string out "__"
    | _ -> raise E.Print_default
  in
  make "BlankSequence"
    ~printer:(prec_blank,pp)~fields:[E.field_hold_all; E.field_protected]
    ~doc:[
      `S "BlankSequence";
      `P "Pattern that matches a non-empty sequence.\
          For instance, `a__` will match Sequence[1,2] but not Sequence[].";
      `I ("infix",
        [ `P "`__` matches any non-empty sequence";
          `P "`__f` matches any non-empty sequence of applications of `f`";
          `P "`x__` matches any non-empty sequence and binds to `x`";
        ]);
      ]

let blank_null_seq =
  let pp _ _ out args = match args with
    | [||] -> Fmt.string out "___"
    | _ -> raise E.Print_default
  in
  make "BlankNullSequence"
    ~printer:(prec_blank,pp) ~fields:[E.field_hold_all; E.field_protected]
    ~doc:[
      `S "BlankNullSequence";
      `P "Pattern that matches any sequence.";
      `I ("infix",
        [`P "`___`, `___f`, `x___`"]);
      `I ("see", [
          `P "Blank";
          `P "BlankSequence";
        ]);
      ]

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
  make "Pattern" ~printer:(prec_pattern,pp) ~fields:[E.field_hold_all; E.field_protected]

let pattern_test =
  let pp = print_infix_bin_ prec_pattern_test "?" in
  make "PatternTest"
    ~fields:[E.field_hold_rest] ~printer:(prec_pattern_test,pp)
    ~doc:[
      `S "PatternTest";
      `P "`PatternTest[p,t]` matches an expression `e` \
          iff `p` matches `e` and `t[e]` evaluates to True.";
      `I ("infix", [ `Pre "`pat?test`" ]);
    ]

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
  make "SameQ" ~funs:[eval]
    ~fields:[E.field_protected]
    ~printer:(prec_same, print_infix_ prec_same "===" "")
    ~doc:[
      `S "SameQ";
      `P "symbolic identity, returns True on syntactically identical\
          terms, False otherwise.";
      `I ("infix", [`P "`a===b`"]);
    ]

let set =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a=%a" (pp_sub prec_set) lhs (pp_sub prec_set) rhs
    | _ -> raise E.Print_default
  in
  make "Set" ~printer:(prec_set, pp)
    ~fields:[E.field_hold_first; E.field_protected]
    ~doc:[
      `S "Set";
      `P "Eager assignment: `a = b` sets `a` to the value of `b`.";
      `I ("infix", [`P "`a = b`"]);
    ]

let set_delayed =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a:=%a" (pp_sub prec_set) lhs (pp_sub prec_set) rhs
    | _ -> raise E.Print_default
  in
  make "SetDelayed" ~printer:(prec_set, pp)
    ~fields:[E.field_hold_all; E.field_protected]
    ~doc:[
      `S "SetDelayed";
      `P "Eager assignment: `a = b` sets `a` to `b`, as an expression,\
          without evaluating `b` (it will be evaluated when `a` is
          used).";
      `I ("infix", [`P "`a := b`"]);
      `I ("example", [`P "`[Log[x_ y_] := Log[x] + Log[y]`"]);
    ]

let rule =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a->%a" (pp_sub prec_rule) lhs (pp_sub prec_rule) rhs
    | _ -> raise E.Print_default
  in
  make "Rule" ~printer:(prec_rule,pp)
    ~fields:[E.field_hold_first; E.field_protected]
    ~doc:[
      `S "Rule";
      `P "Eager rewrite rule. `a -> b` evaluates `b`, then rewrites\
          anything matching `a` to the value of `b`.";
      `I ("infix", [`P "`a -> b`"]);
    ]

let rule_delayed =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a:>%a" (pp_sub prec_rule) lhs (pp_sub prec_rule) rhs
    | _ -> raise E.Print_default
  in
  make "RuleDelayed"  ~printer:(prec_rule,pp)
    ~fields:[E.field_hold_all; E.field_protected]
    ~doc:[
      `S "RuleDelayed";
      `P "Lazy rewrite rule. `a :> b` is the rule that rewrites
          anything matching `a` to `b`, which is only evaluated
          after the rewrite step is done.";
      `I ("infix", [`P "`a :> b`"]);
    ]

let condition =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a/;%a" (pp_sub prec_condition) lhs (pp_sub prec_condition) rhs
    | _ -> raise E.Print_default
  in
  make "Condition"
    ~printer:(prec_condition,pp)
    ~fields:[E.field_protected; E.field_hold_all]
    ~doc:[
      `S "Condition";
      `P "Conditional pattern. `Condition[p,e]` matches the same \
          way `p` does, but then check if `e` reduces to `True`.";
      `I ("infix", [`P "`a /; b`"]);
    ]

let replace_all =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a /. %a" (pp_sub prec_replace) lhs (pp_sub prec_replace) rhs
    | _ -> raise E.Print_default
  in
  make "ReplaceAll" ~printer:(prec_replace,pp)
    ~fields:[E.field_hold_first; E.field_protected]
    ~doc:[
      `S "ReplaceAll";
      `P "Replacement by rewrite rules. `a /. b` expects `b` to be \
         a pattern or a list of patterns.";
      `I ("infix", [`P "`a /. rules`"]);
    ]

let replace_repeated =
  let pp _ pp_sub out = function
    | [| lhs; rhs |] ->
      Fmt.fprintf out "%a //. %a" (pp_sub prec_replace) lhs (pp_sub prec_replace) rhs
    | _ -> raise E.Print_default
  in
  make "ReplaceRepeated" ~printer:(prec_replace,pp)
    ~fields:[E.field_hold_first; E.field_protected]
    ~doc:[
      `S "ReplaceRepeated";
      `P "Replacement by rewrite rules until fixpoint. \
          `a //. b` expects `b` to be \
          a pattern or a list of patterns, and then evaluates `a`
          and rewrites it until no rule applies.";
      `I ("infix", [`P "`a //. rules`"]);
      `I ("example", [
          `S "The naive bubble sort";
          `Pre "`Sort[l_] := l //. {x___,y_,z_,k___}/;(y>z) :> {x,z,y,k}`";
          `P "This simple sort function rewrites `l` using one \
              conditional rule that finds `y`,`z` and swaps them.";
        ]);
    ]

let alternatives =
  make
    ~printer:(prec_alternatives,print_infix_ prec_alternatives "|" "")
    "Alternatives"

let compound_expr =
  make "CompoundExpression"
    ~printer:(prec_semicolon,print_infix_ prec_semicolon ";" "")
    ~doc:[
      `S "CompoundExpression";
      `P "Sequence of operations.";
      `P "`a;b;c` evaluates `a`, then `b`, then `c`, and \
          returns the value of `c`.";
      `I ("infix", [`P "`a; b`"]);
    ]

let head =
  let eval _ _ = function
    | E.App (_, [| E.App (hd, _) |]) -> Some hd
    | _ -> raise Eval_does_not_apply
  in
  make "Head" ~funs:[eval]

let length =
  let eval _ _ = function
    | E.App (_, [| E.App (_, args) |]) -> Some (E.of_int (Array.length args))
    | _ -> raise Eval_does_not_apply
  in
  make "Length" ~funs:[eval]

let slot =
  let pp _ _ out = function
    | [| E.Z i |] ->
      Fmt.fprintf out "#%a" Z.pp_print i
    | _ -> raise E.Print_default
  in
  make "Slot" ~printer:(prec_slot,pp)
    ~doc:[
      `S "Slot";
      `P "Nameless argument of a function. \
          `#0` is the whole sequence of arguments, \
          then `#1`, `#2`, etc. are  positional arguments."
    ]

let function_ =
  let pp _ pp_sub out args = match args with
    | [| body |] ->
      Fmt.fprintf out "%a&" (pp_sub prec_function) body
    | _ -> raise Eval_does_not_apply
  in
  make "Function" ~printer:(prec_function,pp)
    ~doc:[
      `S "Function";
      `P "Anonymous function (also called \"lambda\").";
      `I ("suffix", [
          `P "Anonymous function `fun x->a+x`:";
          `Pre "(a+ #1)&";
        ]);
      `I ("example", [
          `Pre "({#0}&)[a,b,c,d]";
          `P "Thanks to the use of `#0` slot, this \
              reduces to `{a,b,c,d}`";
        ]);
      `I ("example", [
          `Pre "Nest[f[#1,#1]&,a,2]";
          `P "This reduces to `f[f[a,a],f[a,a]]`";
        ]);
      `I ("example", [
          `P "The following \"omega combinator\" does not terminate. \
              Do not evaluate!";
          `Pre "(#1[#1]&)[#1[#1]&]";
        ]);
    ]

let if_ =
  let eval _ eval_st t = match t with
    | E.App (E.Const _, [| a; b; c |]) ->
      begin match Eval.prim_eval eval_st a with
        | Expr.Const {Expr.cst_name="True";_} -> Some b
        | Expr.Const {Expr.cst_name="False";_} -> Some c
        | _ -> None
      end
    | _ -> None
  in
  make "If" ~funs:[eval]
    ~fields:[E.field_hold_rest]
    ~doc:[
      `S "If";
      `P "Test operator. `If[A,B,C]` evaluates `A`, \
          then reduces to `B` or `C` depending on whether \
          `A` is `True` or `False`.";
    ]

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
               begin match Eval.prim_eval eval_st e with
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
  make "And" ~fields:[E.field_flatten; E.field_orderless] ~funs:[eval]
    ~printer:(prec_and,print_infix_ prec_and "&&" "True")
    ~doc:[
      `S "And";
      `P "Logical conjunction.";
      `I ("infix", [`P "`a && b`"]);
    ]

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
               begin match Eval.prim_eval eval_st e with
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
  make "Or"
    ~fields:[E.field_flatten; E.field_orderless] ~funs:[eval]
    ~printer:(prec_or,print_infix_ prec_or "||" "False")
    ~doc:[
      `S "Or";
      `P "Logical disjunction.";
      `I ("infix", [`P "`a || b`"]);
    ]

let not_ =
  let eval self eval_st t = match t with
    | E.App (E.Const self', [| a |]) ->
      assert (Expr.Cst.equal self self');
      begin match Eval.prim_eval eval_st a with
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
  make "Not"
    ~fields:[E.field_listable] ~funs:[eval]
    ~printer:(prec_not,pp)
    ~doc:[
      `S "Not";
      `P "Logical negation.";
      `I ("prefix", [`P "`!a`"]);
    ]

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
  make "Nest"
    ~funs:[eval]
    ~doc:[
      `S "Nest";
      `P "Nesting functions.";
      `P "`Nest[f,e,n]` returns `f[f[…[f[e]]]]` nested `n` times";
      `I ("example", [
          `P "`Nest[F,a,5]` returns `F[F[F[F[F[a]]]]]`";
        ])
    ]

let range =
  let mk_ arg i j ~step =
    let rec aux_incr acc i j step =
      if Z.gt i j then List.rev acc
      else aux_incr (E.z i :: acc) Z.(i + step) j step
    and aux_decr acc i j step =
      if Z.lt i j then List.rev acc
      else aux_decr (E.z i :: acc) Z.(i + step) j step
    in
    let l = match Z.sign step with
      | 0 -> Eval.prim_failf arg "invalid `step` argument in Range: 0"
      | n when n>0 -> aux_incr [] i j step
      | _ -> aux_decr [] i j step
    in
    Some (E.app_l list l)
  in
  let eval _ arg e = match e with
    | E.App (_, [| E.Z i |]) -> mk_ arg Z.zero i ~step:Z.one
    | E.App (_, [| E.Z i; E.Z j |]) -> mk_ arg i j ~step:Z.one
    | E.App (_, [| E.Z i; E.Z j; E.Z step |]) -> mk_ arg i j ~step
    | _ -> raise Eval_does_not_apply
  in
  make "Range"
    ~funs:[eval] ~fields:[E.field_listable]
    ~doc:[
      `S "Range";
      `P "Integer Range, inclusive of both bounds.";
      `P "- `Range[i]` returns `{0,1,2,…,i}`";
      `P "- `Range[i,j]` returns `{i,i+1,…,j}`";
      `P "- `Range[i,j,step]` returns `{i,i+step,…}` until `j` is reached";
      `P "Note: the `Range[]` operator distributes on lists.";
      `I ("example", [
          `P "`Range[3]` returns `{0,1,2,3}`";
        ]);
      `I ("example", [
          `P "`Range[10,3,-2]` returns `{10,8,6,4}`";
        ]);
      `I ("example", [
          `P "Range[Range[5]]";
          `P "returns `{{0},{0,1},{0,1,2},{0,1,2,3},{0,1,2,3,4},{0,1,2,3,4,5}}`";
        ]);
    ]

let mk_ineq_ name desc infix : t =
  let infix_str = Printf.sprintf "`a %s b`" infix in
  let printer = prec_const, print_const infix in
  make name ~printer ~doc:[`S name; `P desc; `I ("infix", [`P infix_str])]

module Attrs = struct
  type attr = {
    view: attr_view;
    field: E.Properties.field;
  }
  and attr_view =
    | Hold_all
    | Hold_first
    | Hold_rest
    | Orderless
    | Flatten
    | One_identity
    | Listable
    | No_duplicates

  exception Not_a_const of E.t
  exception Not_an_attr of E.t

  let mk_pair view field = {view; field}

  let make_const name =
    make name ~doc:[`S name; `P "Attribute. See `GetAttributes`"]

  let tbl =
    [ mk_pair Hold_all E.field_hold_all, make_const "HoldAll";
      mk_pair Hold_first E.field_hold_first, make_const "HoldFirst";
      mk_pair Hold_rest E.field_hold_rest, make_const "HoldRest";
      mk_pair Orderless E.field_orderless, make_const "Orderless";
      mk_pair Flatten E.field_flatten, make_const "Flatten";
      mk_pair One_identity E.field_one_identity, make_const "OneIdentity";
      mk_pair Listable E.field_listable, make_const "Listable";
      mk_pair No_duplicates E.field_no_duplicates, make_const "NoDuplicates";
    ]

  let to_e e =
    try List.assoc e tbl
    with Not_found -> assert false

  let of_e (e:t): attr = match e with
    | E.Const c ->
      let r =
        CCList.find_map
          (function
            | (attr,E.Const c') when E.Cst.equal c c' -> Some attr
            | _ -> None)
          tbl
      in
      begin match r with
        | None -> raise (Not_an_attr e)
        | Some a -> a
      end
    | _ -> raise (Not_an_attr e)

  let get c : attr array =
    tbl
    |> List.map fst
    |> List.filter (fun {field;_} -> E.Cst.get_field field c)
    |> Array.of_list

  let set c (b:bool) (l:attr array) =
    Array.iter (fun {field;_} -> E.Cst.set_field field b c) l

  let terms_as_cst_attrs (args:t array): (E.const * attr array) option =
    if Array.length args=0 then None
    else (
      let c = match args.(0) with
        | E.Const c -> c
        | _ -> raise (Not_a_const args.(0))
      in
      let args =
        Array.init (Array.length args-1)
          (fun i -> of_e args.(i+1))
      in
      Some (c,args)
    )

  let term_as_cst_attrs arg (e:E.t): (E.const * attr array) option =
    match e with
      | E.App (_, args) ->
        begin try terms_as_cst_attrs args
          with
            | Not_a_const e ->
              Eval.prim_failf arg "expected first parameter `%a` to be a constant"
                E.pp_full_form e
            | Not_an_attr e ->
              Eval.prim_failf arg "expected `%a` to be an attribute" E.pp_full_form e
        end
      | _ -> None

  let doc : Document.t = [
    `S "Attributes";
    `I ("demo", [
        `Pre "> GetAttributes[Plus]";
         `Pre  "{Orderless,Flatten,One_identity}";
         `Pre "> GetAttributes[Times]";
         `Pre "{Orderless,Flatten,One_identity}";
         `Pre "> SetAttributes[f,Listable]";
         `Pre "> GetAttributes[f]";
         `Pre "{Listable}";
         `Pre "> f[{1,2,3}]";
         `Pre "{f[1],f[2],f[3]}";
      ]);
  ]
end

let set_attributes =
  let eval _ arg e = match Attrs.term_as_cst_attrs arg e with
    | Some (c, attrs) -> Attrs.set c true attrs; Some null
    | None -> raise Eval_does_not_apply
  in
  make "SetAttributes" ~funs:[eval] ~doc:Attrs.doc

let remove_attributes =
  let eval _ arg e = match Attrs.term_as_cst_attrs arg e with
    | Some (c, attrs) -> Attrs.set c false attrs; Some null
    | None -> raise Eval_does_not_apply
  in
  make "RemoveAttributes" ~funs:[eval]  ~doc:Attrs.doc

let get_attributes =
  let eval _ _ = function
    | E.App (_, [| E.Const c |]) ->
      Attrs.get c
      |> Array.map (fun a -> Attrs.to_e a)
      |> E.app list |> CCOpt.return
    | _ -> raise Eval_does_not_apply
  in
  make "GetAttributes" ~funs:[eval] ~doc:Attrs.doc

let clear_attributes =
  let eval _ _ = function
    | E.App (_, [| E.Const c |]) ->
      if E.Cst.get_field E.field_protected c then None
      else (
        Attrs.get c
        |> Attrs.set c false;
        Some null
      )
    | _ -> raise Eval_does_not_apply
  in
  make "ClearAttributes" ~funs:[eval] ~doc:Attrs.doc

let equal = mk_ineq_ "Equal" "value identity" "=="
let not_equal = mk_ineq_ "NotEqual" "negation of `==`" "!="
let less = mk_ineq_ "Less" "value comparison" "<"
let less_equal = mk_ineq_ "LessEqual" "value comparison" "<="
let greater = mk_ineq_ "Greater" "value comparison" ">"
let greater_equal = mk_ineq_ "GreaterEqual" "value comparison" ">="

module Ineq = struct
  type num = Q.t

  type op = Eq | Neq | Less | Lesseq | Gt | Geq

  type chain = num * (op * num) list

  let as_chain (a:E.t array) : chain option =
    try
      let as_num e : num = match e with
        | E.Z z -> Q.of_bigint z
        | E.Q n -> n
        | _ -> raise Exit
      and as_op e : op = match e with
        | E.Const {E.cst_name="Equal";_} -> Eq
        | E.Const {E.cst_name="NotEqual";_} -> Neq
        | E.Const {E.cst_name="Less";_} -> Less
        | E.Const {E.cst_name="LessEqual";_} -> Lesseq
        | E.Const {E.cst_name="Greater";_} -> Gt
        | E.Const {E.cst_name="GreaterEqual";_} -> Geq
        | _ -> raise Exit
      in
      let st =
        Array.fold_left
          (fun acc e -> match acc with
             | `Begin -> `At (as_num e, [])
             | `At (n, l) -> `At_op (n, l, as_op e)
             | `At_op (n, l, op) ->
               let n2 = as_num e in
               `At (n, (op,n2) :: l)
          )
          `Begin a
      in
      begin match st with
        | `At (n,l) -> Some (n, List.rev l)
        | `Begin | `At_op _ -> None
      end
    with Exit -> None

  let eval_tup a op b = match op with
    | Eq -> Q.equal a b
    | Neq -> not (Q.equal a b)
    | Less -> Q.lt a b
    | Lesseq -> Q.leq a b
    | Gt -> Q.gt a b
    | Geq -> Q.geq a b

  let eval_chain ((n,l):chain): bool =
    let rec aux left = function
      | [] -> true
      | (op,right) :: tail -> eval_tup left op right && aux right tail
    in
    aux n l
end

let inequality =
  let eval _ _ e = match e with
    | E.App (_, arr) ->
      begin match Ineq.as_chain arr with
        | Some c ->
          Some (if Ineq.eval_chain c then true_ else false_)
        | None -> raise Eval_does_not_apply
      end

    | _ -> raise Eval_does_not_apply
  and pp _ pp_sub out args =
    Fmt.fprintf out "@[%a@]"
      (Fmt.array ~start:"" ~stop:"" ~sep:"" (pp_sub prec_ineq)) args
  in
  make "Inequality"
    ~fields:[E.field_flatten; E.field_protected] ~funs:[eval]
    ~printer:(prec_ineq,pp)
    ~doc:[
      `S "Inequality";
      `P "conjunction of comparisons";
      `P "`a>b==c<d<=e` is parsed as \
          `Inequality[a,Greater,b,Equal,c,Less,d,LessEqual,e]` \
          and is `True` iff all the tests hold."
    ]

let integer_q =
  let eval _ _ = function
    | E.App (_, [| E.Z _ |]) -> Some true_
    | E.App (_, [| _ |]) -> Some false_
    | _ -> raise Eval_does_not_apply
  in
  make "IntegerQ" ~funs:[eval]

let rational_q =
  let eval _ _ = function
    | E.App (_, [| E.Z _ | E.Q _ |]) -> Some true_
    | E.App (_, [| _ |]) -> Some false_
    | _ -> raise Eval_does_not_apply
  in
  make "RationalQ" ~funs:[eval]

let true_q =
  let eval _ _ = function
    | E.App (_, [| E.Const {E.cst_name="True";_} |]) -> Some true_
    | E.App (_, [| _ |]) -> Some false_
    | _ -> raise Eval_does_not_apply
  in
  make "TrueQ" ~funs:[eval]

let print =
  let eval _ arg e = match e with
    | E.App (_, [| t |]) ->
      Eval.prim_printf arg "%a@." E.pp t;
      Some null
    | _ -> raise Eval_does_not_apply
  in
  make "Print" ~funs:[eval]
    ~doc:[
      `S "Print";
      `P "`Print[e]` evaluates `e`, prints it and returns `Null`";
    ]

let all_builtins () = !all_

let doc =
  let eval _ arg e = match e with
    | E.App (_, [| E.Const {E.cst_doc=doc;_} |]) ->
      Eval.prim_write_doc arg (Lazy.from_val doc);
      Some null
    | E.App (_, [||]) ->
      let all_docs = lazy (
        all_builtins ()
        |> List.map (fun e -> [`Pre (Fmt.sprintf "%a" E.pp_full_form e)])
        |> Document.list |> CCList.return
        |> Document.indent "symbols" |> CCList.return
      ) in
      Eval.prim_write_doc arg all_docs;
      Some null
    | _ -> raise Eval_does_not_apply
  in
  make "Doc" ~funs:[eval]
    ~doc:[
      `S "Doc";
      `P "Print the documentation for a symbol.";
      `I ("example", [
          `P "The following prints the documentation of `Nest`:";
          `Pre "`Doc[Nest]`";
        ]);
    ]

let complete_symbol s : t list =
  if String.length s = 0 then []
  else (
    CCList.filter_map
      (fun c -> match c with
        | E.Const {E.cst_name=name;_} when CCString.prefix ~pre:s name -> Some c
        | _ -> None)
      !all_
  )

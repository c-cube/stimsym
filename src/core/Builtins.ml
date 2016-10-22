
(* This file is free software. See file "license" for more details. *)

(** {1 Builtin Functions} *)

module E = Expr

type t = Expr.t

let all_ = ref []

(* a function definition. Takes [self_cst, eval_fun, t] and evaluates [t]
   into [None] (fail) or [Some t'] *)
type fun_def = E.const -> (E.t -> E.t) -> E.t -> E.t option

let mk_ ?(fields=[]) ?(funs=[]) name =
  let c =
    E.const_of_string_with name
      ~f:(fun c ->
        List.iter
          (fun field -> E.Cst.set_field field true c)
          fields;
        List.iter (fun d -> E.Cst.add_def (E.Fun (d c)) c) funs)
  in
  all_ := c :: !all_;
  c

let hold = mk_ "Hold" ~fields:[E.field_hold_all]

let full_form = mk_ "FullForm" ~fields:[E.field_hold_all]

type arith_res =
  | Arith_q of Q.t * E.t list
  | Arith_z of Z.t * E.t list

let plus =
  let eval self (_eval:E.t -> E.t) (e:E.t): E.t option =
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
            if CCArray.equal (==) args args'
            then None
            else Some (E.app hd args')
        end
      | _ -> None
  in
  mk_ "Plus" ~funs:[eval]
    ~fields:[ E.field_one_identity; E.field_flatten; E.field_orderless]

let times =
  let eval self (_eval:E.t -> E.t) (e:E.t): E.t option =
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
            if CCArray.equal (==) args args'
            then None
            else Some (E.app hd args')
        end
      | _ -> None
  in
  mk_ "Times" ~funs:[eval]
    ~fields:[ E.field_one_identity; E.field_flatten; E.field_orderless]

let list = mk_ "List"
let true_ = mk_ "True"
let false_ = mk_ "False"
let if_ = mk_ "If" ~fields:[E.field_hold_rest]

let blank = mk_ "Blank" ~fields:[E.field_hold_all; E.field_protected]
let blank_seq =
  mk_ "BlankSequence" ~fields:[E.field_hold_all; E.field_protected]
let blank_null_seq =
  mk_ "BlankNullSequence" ~fields:[E.field_hold_all; E.field_protected]
let pattern = mk_ "Pattern" ~fields:[E.field_hold_all; E.field_protected]

let same_q = mk_ "SameQ" ~fields:[E.field_hold_all; E.field_protected]

(* TODO: defs *)

let set =
  let eval self _eval e : E.t option = match e with
    | E.App (E.Const c, [|a; b|]) ->
      assert (E.Cst.equal self c);
      (* look for the constant to modify *)
      assert false
    (* TODO:
       - try to turn [a] into a pattern
       - obtain its head symbol
       - evaluate [b]
       - register [a -> b] into [a.head] *)
    | _ -> None
  in
  mk_ "Set" ~funs:[eval]
    ~fields:[E.field_hold_first; E.field_protected] 

let set_delayed =
  let eval self _eval e : E.t option = match e with
    | E.App (E.Const c, [|a; b|]) ->
      assert (E.Cst.equal self c);
      (* look for the constant to modify *)
      assert false
    (* TODO:
       - try to turn [a] into a pattern
       - obtain its head symbol
       - register [a -> b] into [a.head] *)
    | _ -> None
  in
  mk_ "SetDelayed" ~funs:[eval]
    ~fields:[E.field_hold_all; E.field_protected] 

let rule =
  mk_ "Rule"
    ~fields:[E.field_hold_first; E.field_protected] 

let rule_delayed =
  mk_ "RuleDelayed"
    ~fields:[E.field_hold_all; E.field_protected] 

let all_builtins () = !all_

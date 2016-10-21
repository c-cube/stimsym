
(* This file is free software. See file "license" for more details. *)

(** {1 Builtin Functions} *)

module E = Expr

type t = Expr.t

let hold =
  E.const_of_string_with "Hold"
    ~f:(fun c ->
      E.Cst.set_field E.field_hold_all true c)

let full_form =
  E.const_of_string_with "FullForm"
    ~f:(fun c ->
      E.Cst.set_field E.field_hold_all true c)

type plus_res =
  | Plus_q of Q.t * E.t list
  | Plus_z of Z.t * E.t list

let plus_eval plus_cst (_eval:E.t -> E.t) (e:E.t): E.t option =
  let compute args =
    Array.fold_left
      (fun acc e -> match acc, e with
         | Plus_z (n1,l), E.Z n2 -> Plus_z (Z.(n1 + n2),l)
         | Plus_z (n1,l), E.Q n2 -> Plus_q (Q.(of_bigint n1 + n2),l)
         | Plus_q (n1,l), E.Z n2 -> Plus_q (Q.(n1 + of_bigint n2),l)
         | Plus_q (n1,l), E.Q n2 -> Plus_q (Q.(n1 + n2), l)
         | Plus_z (n,l), _ -> Plus_z (n,e::l)
         | Plus_q (n,l), _ -> Plus_q (n,e::l))
      (Plus_z (Z.zero, [])) args
  in
  match e with
    | E.App (_, [| |]) -> None
    | E.App (E.Const c as hd, args) ->
      assert (E.Cst.equal c plus_cst);
      let args' = match compute args with
        | Plus_z (n,[]) -> [E.z n]
        | Plus_z (n,l) when Z.sign n=0 -> List.rev l
        | Plus_z (n,l) -> E.z n :: List.rev l
        | Plus_q (n,[]) -> [E.q n]
        | Plus_q (n,l) when Q.sign n=0 -> List.rev l
        | Plus_q (n,l) -> E.q n :: List.rev l
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

let plus =
  E.const_of_string_with "Plus"
    ~f:(fun c ->
      E.Cst.set_field E.field_one_identity true c;
      E.Cst.set_field E.field_flatten true c;
      E.Cst.set_field E.field_orderless true c;
      E.Cst.add_def (E.Fun (plus_eval c)) c;
    )

let list = E.const_of_string "List"

let all_builtins =
  [ hold;
    full_form;
    plus;
  ]


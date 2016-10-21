
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

type plus_classify =
  | Plus_all_Z
  | Plus_all_Q_or_Z
  | Plus_other

let plus_eval plus_cst (eval:E.t -> E.t) (e:E.t): E.t option =
  (* categorize arguments *)
  let categorize args =
    Array.fold_left
      (fun acc e -> match acc, e with
         | Plus_all_Z, E.Z _ -> Plus_all_Z
         | (Plus_all_Z | Plus_all_Q_or_Z), (E.Z _ | E.Q _) -> Plus_all_Q_or_Z
         | _ -> Plus_other)
      Plus_all_Z args
  and compute_z args =
    Array.fold_left
      (fun acc e -> match e with
         | E.Z n -> Z.(acc + n) | _ -> assert false)
      Z.zero args
  and compute_q args =
    Array.fold_left
      (fun acc e -> match e with
         | E.Z n -> Q.(acc + of_bigint n)
         | E.Q n -> Q.(acc + n)
         | _ -> assert false)
      Q.zero args
  in
  match e with
    | E.App (_, [| |]) -> None
    | E.App (E.Const c as hd, args) ->
      assert (E.Cst.equal c plus_cst);
      let args' = Array.map eval args in
      begin match categorize args' with
        | Plus_all_Z -> Some (E.z (compute_z args'))
        | Plus_all_Q_or_Z -> Some (E.q (compute_q args'))
        | Plus_other ->
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



(* This file is free software. See file "license" for more details. *)

(** {1 Basic Types} *)

module Properties = Bit_set.Make(struct end)

type mime_content = {
  mime_ty: string;
  mime_data: string;
  mime_base64: bool;
}

type eval_side_effect =
  | Print_doc of Document.t
  | Print_mime of mime_content

type const = {
  cst_name: string;
  cst_id: int;
  mutable cst_properties: Properties.t;
  mutable cst_rules: def list;
  mutable cst_local_rules: rewrite_rule list;
  mutable cst_doc: Document.t;
  mutable cst_printer: (int * const_printer) option;
  mutable cst_display : mime_printer option;
}

and expr =
  | Const of const
  | App of expr * expr array
  | Z of Z.t
  | Q of Q.t
  | String of string
  | Reg of int (* only in rules RHS *)

and const_printer = const -> (int -> expr CCFormat.printer) -> expr array CCFormat.printer

(* (partial) definition of a symbol *)
and def =
  | Rewrite of rewrite_rule
  | Fun of prim_fun

and rewrite_rule = {
  rr_pat: pattern;
  rr_pat_as_expr: expr;
  rr_rhs: expr;
}

and pattern =
  | P_const of const
  | P_z of Z.t
  | P_q of Q.t
  | P_string of string
  | P_app of pattern * pattern array
  | P_blank of const option (* anything, or anything with the given head *)
  | P_blank_sequence of const option (* >= 1 elements *)
  | P_blank_sequence_null of const option (* >= 0 elements *)
  | P_fail
  | P_bind of int * pattern
    (* match, then bind register *)
  | P_check_same of int * pattern
    (* match, then check syntactic equality with content of register *)
  | P_alt of pattern list
  | P_app_assoc of pattern * assoc_pattern (* for slices *)
  | P_conditional of pattern * expr (* pattern if condition *)
  | P_test of pattern * expr (* `p?t` pattern + test on value *)

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

and binding_seq_body_item =
  | Comp_match of pattern * expr
  | Comp_match1 of pattern * expr
  | Comp_test of expr

and binding_seq = {
  comp_body: binding_seq_body_item list;
  comp_yield: expr;
}

(* TODO? *)
and prim_fun_args = eval_state

and prim_fun = prim_fun_args -> expr -> expr option

(* state for evaluation *)
and eval_state = {
  mutable st_iter_count: int;
  (* number of iterations *)
  mutable st_rules: rewrite_rule list;
  (* permanent list of rules *)
  mutable st_local_rules: rewrite_rule list;
  (* backtrackable list of rules *)
  st_effects: (eval_side_effect Stack.t) option;
  (* temporary messages *)
}

(* custom display for expressions *)
and mime_printer = expr -> mime_content list

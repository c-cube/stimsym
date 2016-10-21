
(* This file is free software. See file "license" for more details. *)

(** {1 Builtin Functions} *)

type t = Expr.t

let hold =
  Expr.const_with "Hold" ~properties:[Expr.HoldAll] ~defs:[]

let full_form =
  Expr.const_with "FullForm" ~properties:[Expr.HoldAll] ~defs:[]

(* TODO: the definitions (on lists of Q/Z) *)
let plus =
  Expr.const_with "Plus"
    ~properties:[Expr.Flatten; Expr.OneIdentity; Expr.Orderless] ~defs:[]

let all_builtins =
  [ hold;
    full_form;
    plus;
  ]


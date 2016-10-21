
(* This file is free software. See file "license" for more details. *)

type property =
  | HoldAll
  | HoldFirst
  | HoldRest
  | Orderless
  | Flatten
  | SequenceHold
  | OneIdentity

type const = {
  name: string;
  mutable properties: property list;
  mutable defs: def list;
}

and t =
  | Const of const
  | App of t * t array
  | Z of Z.t
  | Q of Q.t
  | String of string

(* (partial) definition of a symbol *)
and def =
  | Rewrite of pattern * t
  | Fun of (t -> t option)

and pattern = t

module Str_tbl = CCHashtbl.Make(struct
    type t = string
    let equal (a:string) b = a=b
    let hash (a:string) = Hashtbl.hash a
  end)

type bank = {
  by_name: t Str_tbl.t; (* name -> const *)
}

let bank : bank = {
  by_name = Str_tbl.create 1_024;
}

let const name =
  try Str_tbl.find bank.by_name name
  with Not_found ->
    let c = Const {name; properties=[]; defs=[]} in
    Str_tbl.add bank.by_name name c;
    c

let app head args = App (head, args)

let app_l head args = app head (Array.of_list args)

let z n = Z n

let q n = Q n

let string s = String s

let of_int i = Z (Z.of_int i)

let of_int_ratio a b = Q (Q.of_ints a b)

let of_float x = Q (Q.of_float x)

let set_properties t l = match t with
  | Const r -> r.properties <- l
  | Z _ | Q _ | App _ | String _ -> invalid_arg "Expr.set_properties"

let const_with ~properties ~defs name =
  let c = const name in
  begin match c with
    | Const r ->
      r.properties <- properties;
      r.defs <- defs;
    | App _ | Z _ | Q _ | String _ -> assert false
  end;
  c

let rec pp_full_form out t = match t with
  | Const {name; _} -> Format.pp_print_string out name
  | App (head, args) ->
    Format.fprintf out "@[<2>%a[@[<hv>%a@]]@]"
      pp_full_form head (CCFormat.array ~start:"" ~stop:"" ~sep:"," pp_full_form) args
  | Z n -> Z.pp_print out n
  | Q n -> Q.pp_print out n
  | String s -> Format.fprintf out "%S" s



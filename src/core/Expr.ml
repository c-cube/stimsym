
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

type const = {
  name: string;
  id: int;
  mutable properties: Properties.t;
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
  | Fun of ((t -> t) -> t -> t option)

and pattern = t

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
      } in
    bank.const_id <- bank.const_id + 1;
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

exception No_head

let rec head = function
  | Const c -> c
  | App (hd, _) -> head hd
  | Z _ | Q _ | String _ -> raise No_head

module Cst = struct
  type t = const

  let equal a b = a.id = b.id

  let get_field f c = Properties.get f c.properties

  let set_field f b c = c.properties <- Properties.set f b c.properties

  let add_def d c = c.defs <- d :: c.defs
end

let const_of_string_with ~f name =
  let c = const_of_string name in
  begin match c with
    | Const r -> f r;
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



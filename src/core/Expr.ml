
(* This file is free software. See file "license" for more details. *)

(** {1 Expressions} *)

module Fmt = CCFormat
include Base_types

let field_protected = Properties.mk_field()
let field_hold_all = Properties.mk_field()
let field_hold_first = Properties.mk_field()
let field_hold_rest = Properties.mk_field()
let field_orderless = Properties.mk_field()
let field_flatten = Properties.mk_field()
let field_one_identity = Properties.mk_field()
let field_listable = Properties.mk_field ()
let field_no_duplicates = Properties.mk_field()
let () = Properties.freeze()

(** {2 Basics} *)

type t = Base_types.expr =
  | Const of const
  | App of t * t array
  | Z of Z.t
  | Q of Q.t
  | String of string
  | Reg of int

exception Print_default

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
        cst_name=name;
        cst_properties=Properties.empty;
        cst_id= bank.const_id;
        cst_rules=[];
        cst_doc=[];
        cst_printer=None;
        cst_display=None;
      } in
    bank.const_id <- bank.const_id + 1;
    Str_tbl.add bank.by_name name c;
    c

let app hd args = App (hd, args)

let null = const_of_string "Null"

let app_l head args = app head (Array.of_list args)

let z n = Z n

let q n = Q n

let string s = String s

let reg i = Reg i

let of_int i = Z (Z.of_int i)

let of_int_ratio a b = Q (Q.of_ints a b)

let of_float x = Q (Q.of_float x)

(** {2 Constants} *)

exception No_head

let rec head = function
  | Const c -> c
  | App (hd, _) -> head hd
  | Z _ | Q _ | String _ | Reg _ -> raise No_head

module Cst = struct
  type t = const

  let equal a b = a.cst_id = b.cst_id
  let hash a = Hash.int a.cst_id

  let get_field f c = Properties.get f c.cst_properties

  let set_field f b c = c.cst_properties <- Properties.set f b c.cst_properties

  let add_def d c = match d with
    | Rewrite {rr_pat=P_const c'; _} when equal c c' ->
      c.cst_rules <- [d] (* shadowing *)
    | _ -> c.cst_rules <- d :: c.cst_rules

  let set_doc d c = c.cst_doc <- d

  let set_printer i f c = c.cst_printer <- Some (i,f)
  let set_display f c = c.cst_display <- Some f
end

let const_of_string_with ~f name =
  let c = const_of_string name in
  begin match c with
    | Const r -> f r;
    | App _ | Z _ | Q _ | String _ | Reg _ -> assert false
  end;
  c

let app_flatten hd args =
  let as_sub = match hd with
    | Const c when Cst.get_field field_flatten c ->
      (function
        | App (Const {cst_name="Sequence";_}, sub) -> Some sub
        | App (Const c', sub) when Cst.equal c c' -> Some sub
        | _ -> None)
    | _ ->
      (function
        | App (Const {cst_name="Sequence";_}, sub) -> Some sub
        | _ -> None)
  in
  (* splicing *)
  let must_splice, res_len =
    Array.fold_left
      (fun (must_split,len) arg -> match as_sub arg with
         | Some sub ->
           true, len+Array.length sub
         | _ -> must_split, len+1)
      (false,0) args
  in
  if must_splice
  then (
    let args_flat = Array.make res_len null in
    (* make a flattened array *)
    let len' =
      Array.fold_left
        (fun offset arg -> match as_sub arg with
           | Some sub ->
             Array.blit sub 0 args_flat offset (Array.length sub);
             offset + Array.length sub
           | _ ->
             args_flat.(offset) <- arg;
             offset + 1)
        0 args
    in
    assert (len' = res_len);
    App (hd, args_flat)
  ) else
    App (hd, args)

(** {2 Comparisons} *)

let rec equal a b = match a, b with
  | Z n1, Z n2 -> Z.equal n1 n2
  | Z z, Q q
  | Q q, Z z -> Q.equal q (Q.of_bigint z)
  | Q n1, Q n2 -> Q.equal n1 n2
  | String s1, String s2 -> s1=s2
  | Const c1, Const c2 -> c1.cst_id = c2.cst_id
  | App (f1,a1), App (f2,a2) ->
    Array.length a1=Array.length a2 &&
    equal f1 f2 &&
    CCArray.equal equal a1 a2
  | Reg i, Reg j when i=j -> true
  | Z _, _ | Q _, _ | String _, _ | Const _, _ | App _, _ | Reg _, _
    -> false

(* hash up to a given depth *)
let rec hash_limit n t =
  if n=0 then 0x42
  else match t with
    | Reg i -> Hash.int i
    | Z n -> Z.hash n
    | Q n -> Q.to_string n |> Hash.string
    | String s -> Hash.string s
    | Const c -> Cst.hash c
    | App (f, a) ->
      Hash.combine3 0x11
        (hash_limit (n-1) f)
        (Hash.array (hash_limit (n-1)) a)

let hash t = hash_limit 5 t

module Tbl = CCHashtbl.Make(struct
    type t = expr
    let equal = equal
    let hash = hash
  end)

(** {2 IO} *)

let rec pp_full_form out (t:t) = match t with
  | Const {cst_name; _} -> Format.pp_print_string out cst_name
  | App (head, args) ->
    Format.fprintf out "@[<2>%a[@[<hv>%a@]]@]"
      pp_full_form head (Fmt.array ~start:"" ~stop:"" ~sep:"," pp_full_form) args
  | Z n -> Z.pp_print out n
  | Q n -> Q.pp_print out n
  | String s -> Format.fprintf out "%S" s
  | Reg i -> Format.fprintf out "Reg[%d]" i

let to_string_compact t =
  let buf = Buffer.create 32 in
  let rec aux t = match t with
    | Const {cst_name; _} -> Buffer.add_string buf cst_name
    | App (head, args) ->
      aux head;
      Buffer.add_char buf '[';
      Array.iteri
        (fun i t' ->
           if i>0 then Buffer.add_char buf ',';
           aux t')
        args;
      Buffer.add_char buf ']';
    | Z n -> Z.bprint buf n
    | Q n -> Q.bprint buf n
    | String s ->
      Buffer.add_char buf '"';
      Buffer.add_string buf (String.escaped s);
      Buffer.add_char buf '"';
    | Reg _ -> assert false
  in
  aux t;
  Buffer.contents buf

let pp out (t:t) =
  let rec pp prec out t = match t with
    | App (Const {cst_name="FullForm";_}, [|t|]) -> pp_full_form out t
    | Const ({cst_printer=Some (_, pp_special); _} as c)
    | App (Const ({cst_printer=Some (_, pp_special); _} as c), [||]) ->
      pp_const_custom pp_special c [||] out ()
    | App (Const ({cst_printer=Some (prec', pp_special); _} as c), args) ->
      if prec' > prec
      then pp_const_custom pp_special c args out ()
      else Fmt.within "(" ")" (pp_special c pp) out args
    | Const {cst_name; _} -> Format.pp_print_string out cst_name
    | App (head, args) -> pp_default out (head, args)
    | Z n -> Z.pp_print out n
    | Q n -> Q.pp_print out n
    | String s -> Format.fprintf out "%S" s
    | Reg i -> Format.fprintf out "Reg[%d]" i
  and pp_default out (head, args) =
    Format.fprintf out "@[<2>%a[@[<hv>%a@]]@]"
      (pp 0) head (Fmt.array ~start:"" ~stop:"" ~sep:"," (pp 0)) args
  and pp_const_custom pp_special c args out () =
    try pp_special c pp out args
    with Print_default ->  pp_default out (const c, args)
  in
  begin match t with
    | Const {cst_name="Null";_} -> () (* do not print toplevel "null" *)
    | _ -> pp 0 out t
  end

let to_string t = Fmt.to_string pp t



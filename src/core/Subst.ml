
(* This file is free software. See file "license" for more details. *)

(** {1 Substitutions} *)

open Base_types

module E = Expr
module IntMap = CCMap.Make(CCInt)
module Fmt = CCFormat

type t = expr IntMap.t
let empty = IntMap.empty
let add = IntMap.add
let mem = IntMap.mem
let get = IntMap.get

let pp out (s:t) =
  Format.fprintf out "{@[<hv>%a@]}"
    (IntMap.print ~start:"" ~stop:"" Fmt.int E.pp_full_form) s

let get_exn i s =
  try IntMap.find i s
  with Not_found ->
    invalid_arg (Fmt.sprintf "could not find %d in %a" i pp s)

let rec apply (s:t) (t:expr): expr = match t with
  | Reg i ->
    begin match IntMap.get i s with
      | None -> assert false
      | Some u -> u
    end
  | Const _ | Z _ | Q _ | String _ -> t
  | App (hd, args) ->
    E.app_flatten (apply s hd) (Array.map (apply s) args)

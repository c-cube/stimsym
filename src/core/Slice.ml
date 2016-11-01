
(* This file is free software. See file "license" for more details. *)

(** {1 Array Slice} *)

type 'a t = {
  arr : 'a array;
  i : int; (** Start index (included) *)
  j : int; (** Stop index (excluded) *)
}

let empty = {
  arr = [||];
  i = 0;
  j = 0;
}

let make arr i ~len =
  if i<0||i+len > Array.length arr then invalid_arg "Slice.make";
  { arr; i; j=i+len; }

let of_slice (arr,i,len) = make arr i ~len

let to_slice a = a.arr, a.i, a.j-a.i

let full arr = { arr; i=0; j=Array.length arr; }

let get a i =
  let j = a.i + i in
  if i<0 || j>=a.j then invalid_arg "Slice.get";
  a.arr.(j)

let underlying a = a.arr

let length a = a.j - a.i

let copy a = Array.sub a.arr a.i (length a)

let sub a i len = make a.arr (a.i + i) ~len

let rec _find f a i j =
  if i = j then None
  else match f i a.(i) with
    | Some _ as res -> res
    | None -> _find f a (i+1) j

let find_idx p a =
  _find (fun i x -> if p x then Some (i-a.i,x) else None) a.arr a.i a.j

let print ?(sep=", ") pp_item fmt a =
  let _print ~sep pp_item fmt a i j =
    for k = i to j - 1 do
      if k > i then (Format.pp_print_string fmt sep; Format.pp_print_cut fmt ());
      pp_item fmt a.(k)
    done
  in
  _print ~sep pp_item fmt a.arr a.i a.j

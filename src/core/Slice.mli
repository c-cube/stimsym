
(* This file is free software. See file "license" for more details. *)

(** {1 Array Slice} *)

type 'a t
(** A slice is an array, an offset, and a length *)

val length : _ t -> int

val make : 'a array -> int -> len:int -> 'a t
(** Create a slice.
      @raise Invalid_argument if the slice isn't valid *)

val of_slice : ('a array * int * int) -> 'a t
(** Make a sub-array from a triple [(arr, i, len)] where [arr] is the array,
    [i] the offset in [arr], and [len] the number of elements of the slice.
    @raise Invalid_argument if the slice isn't valid (See {!make}) *)

val to_slice : 'a t -> ('a array * int * int)
(** Convert into a triple [(arr, i, len)] where [len] is the length of
    the subarray of [arr] starting at offset [i] *)

val full : 'a array -> 'a t
(** Slice that covers the full array *)

val underlying : 'a t -> 'a array
(** Underlying array (shared). Modifying this array will modify the slice *)

val get : 'a t -> int -> 'a

val copy : 'a t -> 'a array
(** Copy into a new array *)

val sub : 'a t -> int -> int -> 'a t
(** Sub-slice *)

val find_idx : ('a -> bool) -> 'a t -> (int * 'a) option
(** [find_idx p x] returns [Some (i,x)] where [x] is the [i]-th element of [l],
    and [p x] holds. Otherwise returns [None] *)

val print : ?sep:string -> (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a t -> unit
(** Print an array of items with printing function *)

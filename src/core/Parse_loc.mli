
(* This file is free software. See file "license" for more details. *)

(** {1 Location in a file} *)

(** {2 Location} *)

type t = {
  file : string;
  start_line : int;
  start_column : int;
  stop_line : int;
  stop_column : int;
}

val mk : string -> int -> int -> int -> int -> t

val mk_pair : string -> (int * int) -> (int * int) -> t

val mk_pos : Lexing.position -> Lexing.position -> t
(** Use the two positions of lexbufs. The file of the first lexbuf is used *)

val combine : t -> t -> t
(** Position that spans the two given positions. The file is assumed to be
    the same in both case, and is chosen from one of the two positions. *)

val combine_list : t list -> t
(** N-ary version of {!combine}.
    @raise Invalid_argument if the list is empty *)

val smaller : t -> than:t -> bool
(** [smaller p ~than] is true if [p] is included in [than], ie
    [p] is a sub-location of [than] (interval inclusion) *)

val pp : t CCFormat.printer

val to_string : t -> string

val pp_opt : t option CCFormat.printer

val to_string_opt : t option -> string

(** {2 Lexbuf}

  Utils to set/get the file in a lexbuf *)

val set_file : Lexing.lexbuf -> string -> unit
(** Change the file name used for positions in this lexbuf *)

val get_file : Lexing.lexbuf -> string
(** Obtain the filename *)

val of_lexbuf : Lexing.lexbuf -> t
(** Recover a position from a lexbuf *)

(** {2 Error} *)

exception Parse_error of t * string

val parse_error : Lexing.lexbuf -> string -> _

val parse_errorf : Lexing.lexbuf -> ('a, Format.formatter, unit, 'b) format4 -> 'a


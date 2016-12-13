
(* This file is free software. See file "license" for more details. *)

  type token =
    | T_EOI
    | T_OPEN
    | T_CLOSE
    | T_COMMA
    | T_INT of string
    | T_RAT of string
    | T_STRING of string
    | T_ATOM of string

val token : Lexing.lexbuf -> token

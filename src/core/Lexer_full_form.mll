

(* This file is free software. See file "license" for more details. *)

{
  type token =
    | T_EOI
    | T_OPEN
    | T_CLOSE
    | T_COMMA
    | T_INT of string
    | T_RAT of string
    | T_STRING of string
    | T_ATOM of string
}

let quoted_string_char = [^ '"' '\\' ] |  "\\\\" | "\\\""
let quoted_string = '"' quoted_string_char* '"'

let zero_numeric = '0'
let non_zero_numeric = ['1' - '9']
let numeric = ['0' - '9']

let dot_decimal = '.' numeric +
let positive_decimal = non_zero_numeric numeric*
let decimal = zero_numeric | positive_decimal
let unsigned_integer = decimal
let signed_integer = '-' unsigned_integer
let integer = signed_integer | unsigned_integer

let decimal_fraction = decimal dot_decimal
let unsigned_rational = decimal '/' positive_decimal
let signed_rational = '-' unsigned_rational
let rational = signed_rational | unsigned_rational

let lower_alpha = ['a' - 'z']
let upper_alpha = ['A' - 'Z']
let alpha_numeric = lower_alpha | upper_alpha | numeric | '_'
let symbol = (lower_alpha | upper_alpha) (lower_alpha | upper_alpha | numeric)*

let space = ['\n' '\t' ' ']

rule token = parse
  | space         { token lexbuf }
  | eof           { T_EOI }
  | rational      { T_RAT (Lexing.lexeme lexbuf) }
  | integer       { T_INT (Lexing.lexeme lexbuf) }
  | quoted_string { T_STRING (Lexing.lexeme lexbuf) }
  | ','           { T_COMMA }
  | '['           { T_OPEN }
  | ']'           { T_CLOSE }
  | symbol        { T_ATOM (Lexing.lexeme lexbuf) }
  | _ as c
    { Parse_loc.parse_errorf_buf lexbuf "lexer failed on char '%c'" c }

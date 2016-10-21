
(* This file is free software. See file "license" for more details. *)

{
  open Parser (* tokens *)
}

let printable_char = [^ '\n']
let comment = ';' printable_char*

let quoted_string_char = [^ '"' '\\' ] |  "\\\\" | "\\\""
let quoted_string = '"' quoted_string_char* '"'

let zero_numeric = '0'
let non_zero_numeric = ['1' - '9']
let numeric = ['0' - '9']
let sign = ['+' '-']

let dot_decimal = '.' numeric +
let positive_decimal = non_zero_numeric numeric*
let decimal = zero_numeric | positive_decimal
let unsigned_integer = decimal
let signed_integer = sign unsigned_integer
let integer = signed_integer | unsigned_integer

let decimal_fraction = decimal dot_decimal
let unsigned_rational = decimal '/' positive_decimal
let signed_rational = sign unsigned_rational
let rational = signed_rational | unsigned_rational

let lower_alpha = ['a' - 'z']
let upper_alpha = ['A' - 'Z']
let alpha_numeric = lower_alpha | upper_alpha | numeric | '_'
let symbol = (lower_alpha | upper_alpha) (lower_alpha | upper_alpha | numeric)*

rule token = parse
  | eof { EOI }
  | comment { token lexbuf }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | decimal { INT_LIT (Lexing.lexeme lexbuf) }
  | rational { RAT_LIT (Lexing.lexeme lexbuf) }
  | quoted_string { STRING_LIT (Lexing.lexeme lexbuf) }
  | ',' { COMMA }
  | '+' { PLUS }
  | '[' { LEFT_BRACKET }
  | ']' { RIGHT_BRACKET }
  | '(' { LEFT_PAREN }
  | ')' { RIGHT_PAREN }
  | symbol { SYMBOL (Lexing.lexeme lexbuf) }
  | _ as c
    { Parse_loc.parse_errorf lexbuf "lexer failed on char '%c'" c }

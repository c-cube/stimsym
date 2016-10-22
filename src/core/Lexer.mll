
(* This file is free software. See file "license" for more details. *)

{
  open Parser (* tokens *)

  let cntspace b =
    let s = Lexing.lexeme b in
    CCString.iter (fun c -> if c='\n' then Lexing.new_line b) s
}

let printable_char = [^ '\n']
let comment = ';' printable_char*

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
  | space* eof    { cntspace lexbuf; EOI }
  | comment       { token lexbuf }
  | rational      { RAT_LIT (Lexing.lexeme lexbuf) }
  | decimal       { INT_LIT (Lexing.lexeme lexbuf) }
  | quoted_string { STRING_LIT (Lexing.lexeme lexbuf) }
  | space+        { cntspace lexbuf; SPACE }
  | space* ','    { cntspace lexbuf; COMMA }
  | space* '['    { cntspace lexbuf; LEFT_BRACKET }
  | space* ']'    { cntspace lexbuf; RIGHT_BRACKET }
  | space* '('    { cntspace lexbuf; LEFT_PAREN }
  | space* ')'    { cntspace lexbuf; RIGHT_PAREN }
  | space* '{'    { cntspace lexbuf; LEFT_BRACE }
  | space* '}'    { cntspace lexbuf; RIGHT_BRACE }
  | space* '+'    { cntspace lexbuf; O_PLUS }
  | space* ":="   { cntspace lexbuf; O_SET_DELAYED }
  | space* "="    { cntspace lexbuf; O_SET }
  | space* ":="   { cntspace lexbuf; O_SET_DELAYED }
  | "___"         { O_BLANK_NULL_SEQ }
  | "__"          { O_BLANK_SEQ }
  | "_"           { O_BLANK }
  | symbol        { SYMBOL (Lexing.lexeme lexbuf) }
  | _ as c
    { Parse_loc.parse_errorf_buf lexbuf "lexer failed on char '%c'" c }



%{
  (* This file is free software. See file "license" for more details. *)

  (** Main parser *)

  module L = Parse_loc
  module E = Expr
  module B = Builtins

  (* remove quote from some symbols *)
  let remove_quotes s =
    assert (s.[0] = '\'' && s.[String.length s - 1] = '\'');
    String.sub s 1 (String.length s - 2)
%}

%token EOI

%token LEFT_BRACKET
%token RIGHT_BRACKET
%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token COMMA

%token <string> SYMBOL
%token <string> STRING_LIT
%token <string> INT_LIT
%token <string> RAT_LIT

%token PLUS

%start <Expr.t> parse_expr

%%

parse_expr: e=expr EOI { e }

expr:
  | e=app_expr { e }
  | a=app_expr PLUS l=separated_nonempty_list(PLUS,app_expr)
    { E.app_l B.plus (a::l) }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      Parse_loc.parse_error loc "expected expression" }

app_expr:
  | e=atomic_expr { e }
  | hd=app_expr LEFT_BRACKET args=separated_list(COMMA,expr) RIGHT_BRACKET
    { E.app_l hd args }

atomic_expr:
  | LEFT_PAREN e=expr RIGHT_PAREN { e }
  | LEFT_BRACE l=separated_list(COMMA,expr) RIGHT_BRACE
    { E.app_l B.list l }
  | t=SYMBOL { E.const_of_string t }
  | n=INT_LIT { E.z (Z.of_string n) }
  | n=RAT_LIT { E.q (Q.of_string n) }
  | s=STRING_LIT { E.string (remove_quotes s) }


%%

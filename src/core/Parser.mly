

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
%token SPACE

%token <string> SYMBOL
%token <string> STRING_LIT
%token <string> INT_LIT
%token <string> RAT_LIT

%token O_PLUS
%token O_SET
%token O_SET_DELAYED
%token O_RULE
%token O_RULE_DELAYED
%token O_BLANK
%token O_BLANK_SEQ
%token O_BLANK_NULL_SEQ

%start <Expr.t> parse_expr

%%

skip_space:
  | {}
  | SPACE {}

parse_expr: e=expr EOI { e }

expr: skip_space e=expr_nospace skip_space { e }

expr_nospace:
  | e=sum_expr { e }
  | e=rule_expr { e }
  | e=set_expr { e }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      Parse_loc.parse_error loc "expected expression" }

set_expr:
  | a=app_expr_nospace O_SET b=expr_nospace
    { E.app_l B.set [a;b] }
  | a=app_expr_nospace O_SET_DELAYED b=expr_nospace
    { E.app_l B.set_delayed [a;b] }

rule_expr:
  | a=app_expr_nospace O_RULE b=expr_nospace
    { E.app_l B.rule [a;b] }
  | a=app_expr_nospace O_RULE_DELAYED b=expr_nospace
    { E.app_l B.rule_delayed [a;b] }

sum_expr:
  | e=prod_expr { e }
  | a=prod_expr
      O_PLUS
      l=separated_nonempty_list(O_PLUS,prod_expr)
    { E.app_l B.plus (a::l) }

prod_expr:
  | l=prod_expr_l
    { match l with
      | [] -> assert false
      | [e] -> e
      | _ -> E.app_l B.times (List.rev l) }

prod_expr_l:
  | e=app_expr_nospace { [e] }
  | a=prod_expr_l SPACE b=app_expr_nospace { b::a }

app_expr_nospace:
  | e=atomic_expr_nospace { e }
  | hd=app_expr_nospace
      LEFT_BRACKET
        args=separated_list(COMMA,expr)
      RIGHT_BRACKET
    { E.app_l hd args }

%inline blank:
  | O_BLANK t=SYMBOL { E.app_l B.blank [E.const_of_string t] }
  | O_BLANK_SEQ t=SYMBOL { E.app_l B.blank_seq [E.const_of_string t] }
  | O_BLANK_NULL_SEQ t=SYMBOL { E.app_l B.blank_null_seq [E.const_of_string t] }
  | O_BLANK { E.app_l B.blank [] }
  | O_BLANK_SEQ { E.app_l B.blank_seq [] }
  | O_BLANK_NULL_SEQ { E.app_l B.blank_null_seq [] }

atomic_expr_nospace:
  | LEFT_PAREN e=expr_nospace RIGHT_PAREN { e }
  | LEFT_BRACE l=separated_list(COMMA,expr) RIGHT_BRACE
    { E.app_l B.list l }
  | a=SYMBOL b=blank
    { E.app_l B.pattern [E.const_of_string a; b] }
  | b=blank { b }
  | t=SYMBOL { E.const_of_string t }
  | n=INT_LIT { E.z (Z.of_string n) }
  | n=RAT_LIT { E.q (Q.of_string n) }
  | s=STRING_LIT { E.string (remove_quotes s) }


%%

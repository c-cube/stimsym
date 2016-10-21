

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
%token O_BLANK
%token O_BLANK_SEQ
%token O_BLANK_NULL_SEQ

%start <Expr.t> parse_expr

%%

skip_space:
  | {}
  | SPACE {}

%inline pre(X): x=X skip_space { x }
%inline post(X): x=X skip_space { x }
%inline prepost(X): skip_space x=X skip_space { x }

parse_expr: skip_space e=expr EOI { e }

expr:
  | e=sum_expr { e }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      Parse_loc.parse_error loc "expected expression" }

sum_expr:
  | e=prod_expr { e }
  | a=prod_expr
      post(O_PLUS)
      l=separated_nonempty_list(post(O_PLUS),prod_expr)
    { E.app_l B.plus (a::l) }

prod_expr:
  | e=prod_expr_nospace skip_space { e }

prod_expr_nospace:
  | e=app_expr_nospace { e }
  | a=app_expr_nospace
      SPACE
      l=separated_nonempty_list(SPACE,app_expr_nospace)
    { E.app_l B.times (a::l) }

app_expr_nospace:
  | e=atomic_expr_nospace { e }
  | hd=app_expr_nospace
    prepost(LEFT_BRACKET)
      args=separated_list(post(COMMA),expr)
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
  | LEFT_PAREN skip_space e=expr RIGHT_PAREN { e }
  | LEFT_BRACE
      skip_space
      l=separated_list(post(COMMA),expr)
    RIGHT_BRACE
    { E.app_l B.list l }
  | a=SYMBOL b=blank
    { E.app_l B.pattern [E.const_of_string a; b] }
  | b=blank { b }
  | t=SYMBOL { E.const_of_string t }
  | n=INT_LIT { E.z (Z.of_string n) }
  | n=RAT_LIT { E.q (Q.of_string n) }
  | s=STRING_LIT { E.string (remove_quotes s) }


%%

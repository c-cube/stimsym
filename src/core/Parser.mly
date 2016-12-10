

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

%token <int> SLOT
%token ANCHOR

%token <string> SYMBOL
%token <string> STRING_LIT
%token <string> INT_LIT
%token <string> RAT_LIT

%token O_ALTERNATIVE

%token O_SET
%token O_SET_DELAYED
%token O_RULE
%token O_RULE_DELAYED
%token O_REPLACE_ALL
%token O_REPLACE_REPEATED
%token O_PATTERN
%token O_CONDITION
%token O_BLANK
%token O_BLANK_SEQ
%token O_BLANK_NULL_SEQ
%token O_SAME_Q
%token O_SEMI_COLON

%token O_MATCH_BIND
%token O_MATCH_BIND1
%token O_COMPREHENSION

%token O_PLUS
%token O_EQUAL
%token O_LESS
%token O_LESS_EQUAL
%token O_GREATER
%token O_GREATER_EQUAL

%token O_AND
%token O_OR
%token O_BANG

%start <Expr.t> parse_expr

%%

skip_space:
  | {}
  | SPACE {}

parse_expr: e=expr EOI { e }

expr: skip_space e=expr_nospace skip_space { e }

expr_nospace:
  | e=comprehension_expr { e }
  | error
    {
      let loc = L.mk_pos $startpos $endpos in
      Parse_loc.parse_error loc "expected expression" }

comprehension_expr:
  | e=compound_expr { e }
  | e=compound_expr
    O_COMPREHENSION
    l=separated_nonempty_list(COMMA,set_expr)
    { E.app_l B.comprehension (e::l) }

compound_expr:
  | e=set_expr { e }
  | e=set_expr O_SEMI_COLON l=separated_nonempty_list(O_SEMI_COLON, compound_expr)
    { E.app_l B.compound_expr (e::l) }

set_expr:
  | e = set_expr_rhs { e }
  | a=pattern_expr O_SET b=set_expr_rhs
    { E.app_l B.set [a;b] }
  | a=pattern_expr O_SET_DELAYED b=set_expr_rhs
    { E.app_l B.set_delayed [a;b] }
  | a=pattern_expr O_MATCH_BIND b=set_expr_rhs
    { E.app_l B.match_bind [a;b] }
  | a=pattern_expr O_MATCH_BIND1 b=set_expr_rhs
    { E.app_l B.match_bind1 [a;b] }

set_expr_rhs:
  | e=replace_expr { e }
  | e=rule_expr { e }
  | e=pattern_expr { e }

replace_expr:
  | a=pattern_expr O_REPLACE_ALL b=set_expr_rhs
    { E.app_l B.replace_all [a;b] }
  | a=pattern_expr O_REPLACE_REPEATED b=set_expr_rhs
    { E.app_l B.replace_repeated [a;b] }

rule_expr:
  | a=pattern_expr O_RULE b=set_expr_rhs
    { E.app_l B.rule [a;b] }
  | a=pattern_expr O_RULE_DELAYED b=set_expr_rhs
    { E.app_l B.rule_delayed [a;b] }

pattern_expr:
  | e=alternative_expr { e }
  | x=SYMBOL O_PATTERN e=alternative_expr
    { E.app_l B.pattern [E.const_of_string x; e] }
  | a=pattern_expr O_CONDITION b=same_expr
    { E.app_l B.condition [a;b] }

alternative_expr:
  | a=fun_expr { a }
  | a=fun_expr O_ALTERNATIVE
    b=separated_nonempty_list(O_ALTERNATIVE,fun_expr)
    { E.app_l B.alternatives (a::b) }

fun_expr:
  | e=same_expr { e }
  | e=same_expr ANCHOR { E.app B.function_ [| e |] }

same_expr:
  | e=or_expr { e }
  | a=or_expr O_SAME_Q b=separated_nonempty_list(O_SAME_Q, or_expr)
    { E.app_l B.same_q (a::b) }

or_expr:
  | e=and_expr { e }
  | a=and_expr O_OR b=separated_nonempty_list(O_OR, and_expr)
    { E.app_l B.or_ (a::b) }

and_expr:
  | e=not_expr { e }
  | a=not_expr O_AND b=separated_nonempty_list(O_AND, not_expr)
    { E.app_l B.and_ (a::b) }

not_expr:
  | e=ineq_expr { e }
  | O_BANG skip_space e=not_expr { E.app_l B.not_ [e] }

%inline ineq_op:
  | O_EQUAL { B.equal }
  | O_LESS { B.less }
  | O_LESS_EQUAL { B.less_equal }
  | O_GREATER { B.greater }
  | O_GREATER_EQUAL { B.greater_equal }

ineq_expr:
  | l=ineq_expr_l
  { match l with
  | [] -> assert false
  | [e] -> e
  | l -> E.app_l B.inequality (List.rev l) }

ineq_expr_l:
  | e=sum_expr { [e] }
  | a=ineq_expr_l op=ineq_op b=sum_expr { b :: op :: a }

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
      | _ -> E.app_l B.times l }

prod_expr_l:
  | e=factorial_expr { [e] }
  | a=factorial_expr SPACE b=prod_expr_l { a::b }

factorial_expr:
  | e=app_expr_nospace { e }
  | e=factorial_expr O_BANG { E.app_l B.factorial [e] }

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
  | n=SLOT { E.app B.slot [| E.z (Z.of_int n) |] }
  | b=blank { b }
  | t=SYMBOL { E.const_of_string t }
  | n=INT_LIT { E.z (Z.of_string n) }
  | n=RAT_LIT { E.q (Q.of_string n) }
  | s=STRING_LIT { E.string (remove_quotes s) }


%%

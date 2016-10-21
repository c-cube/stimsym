
(* This file is free software. See file "license" for more details. *)

(** {1 Test Suite} *)

open Rewrite
open OUnit

let mk_name prefix line = Printf.sprintf "%s_line_%d" prefix line

(** {2 Parser} *)

let test_parser line a b : test =
  mk_name "ok" line >:: (fun _ ->
    let buf = Lexing.from_string a in
    let e = Parser.parse_expr Lexer.token buf in
    OUnit.assert_equal ~cmp:CCString.equal ~printer:CCFun.id
      b (CCFormat.to_string Expr.pp_full_form e)
  )

let test_parser_fail line a : test =
  mk_name "fail" line >:: (fun _ ->
    let buf = Lexing.from_string a in
    try
      let _ = Parser.parse_expr Lexer.token buf in
      OUnit.assert_failure (Printf.sprintf "should have failed to parse %S" a)
    with e ->
      OUnit.assert_bool
        (Printf.sprintf "properly failed to parse %S with exn %s" a (Printexc.to_string e))
        true
  )

let suite_parser =
  "parser" >::: [
    test_parser __LINE__ "f" "f";
    test_parser __LINE__ "f[]" "f[]";
    test_parser __LINE__ "f[a,b,c]" "f[a,b,c]";
    test_parser __LINE__ "a+b" "Plus[a,b]";
    test_parser __LINE__ "a+b+c+d" "Plus[a,b,c,d]";
    test_parser __LINE__ "f[a+b+c,d]" "f[Plus[a,b,c],d]";
    test_parser __LINE__ "3/2" "3/2";
    test_parser __LINE__ "6/4" "3/2";
    test_parser_fail __LINE__ "f[a+b+,d]";
    test_parser_fail __LINE__ "+ +";
    test_parser __LINE__ 
      "f[g[h[i[j[k,l]+m],n,o+p+(q)]]]+r"
      "Plus[f[g[h[i[Plus[j[k,l],m]],n,Plus[o,p,q]]]],r]";
    test_parser __LINE__ "{1,2,3}" "List[1,2,3]";
    test_parser __LINE__ "{1,{2},{3,a+0}}" "List[1,List[2],List[3,Plus[a,0]]]";
    test_parser __LINE__ "{}" "List[]";
    test_parser __LINE__ "{a, b,c,d+e+1 + 3}" "List[a,b,c,Plus[d,e,1,3]]";
    test_parser __LINE__ "  {  a, b,c,d +e +1  + 3 } " "List[a,b,c,Plus[d,e,1,3]]";
    test_parser __LINE__ "_" "Blank[]";
    test_parser __LINE__ "_a" "Blank[a]";
    test_parser __LINE__ "a_b" "Pattern[a,Blank[b]]";
    test_parser __LINE__ "a__b" "Pattern[a,BlankSequence[b]]";
    test_parser __LINE__ "a___b" "Pattern[a,BlankNullSequence[b]]";
    test_parser __LINE__ "a___" "Pattern[a,BlankNullSequence[]]";
    test_parser __LINE__ "___" "BlankNullSequence[]";
    test_parser __LINE__ "a b c" "Times[a,b,c]";
    test_parser __LINE__ "f[a b, c+d e + 1]" "f[Times[a,b],Plus[c,Times[d,e],1]]";
    test_parser __LINE__ "a ___b" "Times[a,BlankNullSequence[b]]";
    test_parser __LINE__ "a ___ b" "Times[a,BlankNullSequence[],b]";
  ]

(** {2 Eval} *)

let mk_eval line a b : test =
  mk_name "ok" line >:: (fun _ ->
    let buf = Lexing.from_string a in
    let e = Parser.parse_expr Lexer.token buf in
    let e = Eval.eval e in
    OUnit.assert_equal ~cmp:CCString.equal ~printer:CCFun.id
      b (CCFormat.to_string Expr.pp_full_form e)
  )

let suite_eval =
  "eval" >::: [
    mk_eval __LINE__ "1+2" "3";
    mk_eval __LINE__ "f[1+2,a,b]" "f[3,a,b]";
    mk_eval __LINE__ "f[g[1+2],a,b]" "f[g[3],a,b]";
    mk_eval __LINE__ "f[a+b+1+c+2+d+3]" "f[Plus[6,a,b,c,d]]";
    mk_eval __LINE__ "f[Hold[1+2],3]" "f[Hold[Plus[1,2]],3]";
    mk_eval __LINE__ "f[2/3+1/3]" "f[1]";
    mk_eval __LINE__ "{1,{a,1+0+b},{3,a+0}}" "List[1,List[a,Plus[1,b]],List[3,a]]";
    mk_eval __LINE__ "f[a+1+b+2/3+c,1/34+d]" "f[Plus[5/3,a,b,c],Plus[1/34,d]]";
    mk_eval __LINE__ "f[a+b+2+c,d]" "f[Plus[2,a,b,c],d]";
  ]

(** {2 Main} *)

let suite =
  "rewrite" >::: [
    suite_parser;
    suite_eval;
  ]

let () =
  let _ = OUnit.run_test_tt_main suite in
  ()

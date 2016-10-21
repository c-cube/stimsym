
(* This file is free software. See file "license" for more details. *)

(** {1 Test Suite} *)

open Rewrite
open OUnit

let mk_name prefix line = Printf.sprintf "%s (line %d)" prefix line

let test_parser line a b : test =
  mk_name "parser" line >:: (fun _ ->
    let buf = Lexing.from_string a in
    let e = Parser.parse_expr Lexer.token buf in
    OUnit.assert_equal ~cmp:CCString.equal ~printer:CCFun.id
      b (CCFormat.to_string Expr.pp_full_form e)
  )

let test_parser_fail line a : test =
  mk_name "parser_fail" line >:: (fun _ ->
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
    test_parser __LINE__ "f[a+b+2+c,d]" "f[Plus[a,b,2,c],d]";
    test_parser __LINE__ "f[a+b+2/3+c,1/34+d]" "f[Plus[a,b,2/3,c],Plus[1/34,d]]";
    test_parser_fail __LINE__ "f[a+b+,d]";
    test_parser_fail __LINE__ "+ +";
    test_parser __LINE__ 
      "f[g[h[i[j[k,l]+m],n,o+p+(q)]]]+r"
      "Plus[f[g[h[i[Plus[j[k,l],m]],n,Plus[o,p,q]]]],r]";
  ]

let suite =
  "rewrite" >::: [
    suite_parser;
  ]

(** {2 Main} *)

let () =
  let _ = OUnit.run_test_tt_main suite in
  ()

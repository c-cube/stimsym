
(* This file is free software. See file "license" for more details. *)

(** {1 Test Suite} *)

open Rewrite
open OUnit

(* fresh name for a test *)
let mk_name =
  let n = ref 0 in
  fun prefix ->
    let s = Printf.sprintf "%s%d" prefix  !n in
    incr n;
    s

let test_parser a b : test =
  mk_name "parser" >:: (fun _ ->
    let buf = Lexing.from_string a in
    let e = Parser.parse_expr Lexer.token buf in
    OUnit.assert_equal ~cmp:CCString.equal ~printer:CCFun.id
      b (CCFormat.to_string Expr.pp_full_form e)
  )


let suite_parser =
  "parser" >::: [
    test_parser "f" "f";
    test_parser "f[]" "f[]";
    test_parser "f[a,b,c]" "f[a,b,c]";
    test_parser "a+b" "Plus[a,b]";
    test_parser "a+b+c+d" "Plus[a,b,c,d]";
    test_parser "f[a+b+c,d]" "f[Plus[a,b,c],d]";
    test_parser "f[a+b+c,d]" "f[Plus[a,b,c],d]";
  ]

let suite =
  "rewrite" >::: [
    suite_parser;
  ]

(** {2 Main} *)

let () =
  let _ = OUnit.run_test_tt_main suite in
  ()

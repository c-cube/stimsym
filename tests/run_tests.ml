
(* This file is free software. See file "license" for more details. *)

(** {1 Test Suite} *)

open Stimsym
open OUnit

let mk_name prefix line = Printf.sprintf "%s_line_%d" prefix line

(** {2 Parser} *)

exception Test_fail of string

let () = Printexc.register_printer
    (function
      | Test_fail s -> Some s
      | _ -> None)

let test_parser line a b : test =
  mk_name "ok" line >:: (fun _ ->
    let buf = Lexing.from_string a in
    try
      let e = Parser.parse_expr Lexer.token buf in
      OUnit.assert_equal ~cmp:CCString.equal ~printer:CCFun.id
        b (Expr.to_string_compact e)
    with Parse_loc.Parse_error (_,s) ->
      let msg =
        CCFormat.sprintf "failed to parse `%s`:@ %s@ (expected: `%s`)" a s b
      in
      raise (Test_fail msg)
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
    test_parser __LINE__ "  {  a, b,c,d +e+1 +3 }" "List[a,b,c,Plus[d,e,1,3]]";
    test_parser __LINE__ "  {  a, b,c,d +e +1  + 3 } " "List[a,b,c,Plus[d,e,1,3]]";
    test_parser __LINE__ " a + (* coucou lol *) b" "Plus[a,b]";
    test_parser __LINE__ " (*foo *) 1" "1";
    test_parser __LINE__ "2 (*why hello*) b" "Times[2,b]";
    test_parser __LINE__ "_" "Blank[]";
    test_parser __LINE__ "_a" "Blank[a]";
    test_parser __LINE__ "a_b" "Pattern[a,Blank[b]]";
    test_parser __LINE__ "a__b" "Pattern[a,BlankSequence[b]]";
    test_parser __LINE__ "a___b" "Pattern[a,BlankNullSequence[b]]";
    test_parser __LINE__ "a___" "Pattern[a,BlankNullSequence[]]";
    test_parser __LINE__ "___" "BlankNullSequence[]";
    test_parser __LINE__ "a b c" "Times[a,b,c]";
    test_parser __LINE__ "a b (c+d) " "Times[a,b,Plus[c,d]]";
    test_parser __LINE__ "f[a b, c+d e + 1]" "f[Times[a,b],Plus[c,Times[d,e],1]]";
    test_parser __LINE__
      "a_|f[b__]|c "
      "Alternatives[Pattern[a,Blank[]],f[Pattern[b,BlankSequence[]]],c]";
    test_parser __LINE__ "a ___b" "Times[a,BlankNullSequence[b]]";
    test_parser __LINE__ "a ___ b" "Times[a,BlankNullSequence[],b]";
    test_parser __LINE__ "f[a_] = b " "Set[f[Pattern[a,Blank[]]],b]";
    test_parser __LINE__
      "f[a_|foo] := b[c] d+f "
      "SetDelayed[f[Alternatives[Pattern[a,Blank[]],foo]],Plus[Times[b[c],d],f]]";
    test_parser __LINE__ "f[a_] = b " "Set[f[Pattern[a,Blank[]]],b]";
    test_parser __LINE__ "f[a_] :> g[a,a]" "RuleDelayed[f[Pattern[a,Blank[]]],g[a,a]]";
    test_parser __LINE__ "f[x] -> g[x,a]" "Rule[f[x],g[x,a]]";
    test_parser __LINE__ "f[x]:> g[x,a]" "RuleDelayed[f[x],g[x,a]]";
    test_parser __LINE__ "f[x]->g[x,a]" "Rule[f[x],g[x,a]]";
    test_parser __LINE__ "f[x]:> g[x]+ h[x] 3" "RuleDelayed[f[x],Plus[g[x],Times[h[x],3]]]";
    test_parser __LINE__ "f[a==b==c,1]" "f[Inequality[a,Equal,b,Equal,c],1]";
    test_parser __LINE__
      "1==a>=b<d<=e b+c|d<e"
      "Alternatives[Inequality[1,Equal,a,GreaterEqual,b,Less,d,LessEqual,Plus[Times[e,b],c]],\
       Inequality[d,Less,e]]";
    test_parser __LINE__ "!a!" "Not[Factorial[a]]";
    test_parser __LINE__ "a&&b||!c" "Or[And[a,b],Not[c]]";
    test_parser __LINE__ "a||b&&!c" "Or[a,And[b,Not[c]]]";
    test_parser __LINE__ "a===b" "SameQ[a,b]";
    test_parser __LINE__ "f-> a===b===c" "Rule[f,SameQ[a,b,c]]";
    test_parser __LINE__ "a!" "Factorial[a]";
    test_parser __LINE__ "a!+b" "Plus[Factorial[a],b]";
    test_parser __LINE__ "a! b" "Times[Factorial[a],b]";
    test_parser __LINE__ "! a + b ! c" "Not[Plus[a,Times[Factorial[b],c]]]";
    test_parser __LINE__ "a/; b" "Condition[a,b]";
    test_parser __LINE__ "f[a_]/; b" "Condition[f[Pattern[a,Blank[]]],b]";
    test_parser __LINE__ "a_/; b :> a+1" "RuleDelayed[Condition[Pattern[a,Blank[]],b],Plus[a,1]]";
    test_parser __LINE__
      "a_[_|__]/; b===1 :> a+1"
      "RuleDelayed[Condition[Pattern[a,Blank[]][Alternatives[Blank[],BlankSequence[]]]\
       ,SameQ[b,1]],Plus[a,1]]";
    test_parser __LINE__
      "t:f[_,_]/;t==1 :> g[t]"
      "RuleDelayed[Condition[Pattern[t,f[Blank[],Blank[]]],Inequality[t,Equal,1]],g[t]]";
    test_parser __LINE__
      "t:f[x___] :> g[t,x]"
      "RuleDelayed[Pattern[t,f[Pattern[x,BlankNullSequence[]]]],g[t,x]]";
    test_parser __LINE__ "f[a] /. {x,y,z}" "ReplaceAll[f[a],List[x,y,z]]";
    test_parser __LINE__ "f[a] //. {x,y,z} d" "ReplaceRepeated[f[a],Times[List[x,y,z],d]]";
    test_parser __LINE__
      "f[a,b+c d!] //. {f[x,y,z___] :> f[x y,z]}"
        "ReplaceRepeated[f[a,Plus[b,Times[c,Factorial[d]]]],\
         List[RuleDelayed[f[x,y,Pattern[z,BlankNullSequence[]]],f[Times[x,y],z]]]]";
    test_parser __LINE__ "a := b; c" "CompoundExpression[SetDelayed[a,b],c]";
    test_parser __LINE__ "a/; test := b ; c" "CompoundExpression[SetDelayed[Condition[a,test],b],c]";
    test_parser __LINE__ "a := b :> c; d//.e"
      "CompoundExpression[SetDelayed[a,RuleDelayed[b,c]],ReplaceRepeated[d,e]]";
    test_parser __LINE__ "#1" "Slot[1]";
    test_parser __LINE__ "#1 #2" "Times[Slot[1],Slot[2]]";
    test_parser __LINE__ "(#1 + #2[#0])&" "Function[Plus[Slot[1],Slot[2][Slot[0]]]]";
    test_parser __LINE__ "f = a; 1+#2&"
      "CompoundExpression[Set[f,a],Function[Plus[1,Slot[2]]]]";
    test_parser __LINE__ "({#0}&)+#1&"
      "Function[Plus[Function[List[Slot[0]]],Slot[1]]]";
    test_parser __LINE__ "a<-b" "MatchBind[a,b]";
    test_parser __LINE__ "a_<-b+c" "MatchBind[Pattern[a,Blank[]],Plus[b,c]]";
    test_parser __LINE__ "a<<-b" "MatchBind1[a,b]";
    test_parser __LINE__ "a_<<-b+c" "MatchBind1[Pattern[a,Blank[]],Plus[b,c]]";
    test_parser __LINE__ "{a<-b,c<<-d }" "List[MatchBind[a,b],MatchBind1[c,d]]";
    test_parser __LINE__
      "{f[x] :: x_<<-{1}, y_<-b, t}"
      "List[Comprehension[f[x],MatchBind1[Pattern[x,Blank[]],List[1]],MatchBind[Pattern[y,Blank[]],b],t]]";
    test_parser __LINE__ "_?IntegerQ" "PatternTest[Blank[],IntegerQ]";
    test_parser __LINE__
      "f[_]?(#===f[a]&)"
      "PatternTest[f[Blank[]],Function[SameQ[Slot[1],f[a]]]]";
  ]

(** {2 Printer} *)

let test_printer ?(strip_space=false) line a b : test =
  mk_name "same" line >:: (fun _ ->
    let buf = Lexing.from_string a in
    try
      let e = Parser.parse_expr Lexer.token buf in
      let res = Expr.to_string e in
      let res =
        if strip_space
        then CCString.filter (function ' '|'\n' -> false | _ -> true) res
        else res
      in
      OUnit.assert_equal ~cmp:CCString.equal ~printer:CCFun.id b res
    with Parse_loc.Parse_error (_,s) ->
      let msg =
        CCFormat.sprintf "failed to parse `%s`:@ %s@ (expected: `%s`)" a s b
      in
      raise (Test_fail msg)
  )

let test_printer_same line a = test_printer line a a

let suite_printer =
  "printer" >::: [
    test_printer_same __LINE__ "1+2";
    test_printer __LINE__ "1+(2 3)" "1+2 3";
    test_printer_same __LINE__ "1+f[x,y]";
    test_printer_same __LINE__ "{{1},{2+3,f[{4}]}}";
    test_printer_same __LINE__ "f";
    test_printer_same __LINE__ "f[]";
    test_printer_same __LINE__ "f[a,b,c]";
    test_printer_same __LINE__ "a+b";
    test_printer_same __LINE__ "a+b+c+d";
    test_printer_same __LINE__ "f[a+b+c,d]";
    test_printer_same __LINE__ "3/2";
    test_printer __LINE__ 
      "f[g[h[i[j[k,l]+m],n,o+p+(q)]]]+r"
      "f[g[h[i[j[k,l]+m],n,o+p+q]]]+r";
    test_printer_same __LINE__ "{1,2,3}";
    test_printer_same __LINE__ "{1,{2},{3,a+0}}";
    test_printer_same __LINE__ "{}";
    test_printer __LINE__ "{a, b,c,d+e+1 + 3}" "{a,b,c,d+e+1+3}";
    test_printer __LINE__ "  {  a, b,c,d +e+1 +3 }" "{a,b,c,d+e+1+3}";
    test_printer __LINE__ "  {  a, b,c,d +e +1  + 3 } " "{a,b,c,d+e+1+3}";
    test_printer __LINE__ " a + (* coucou lol *) b" "a+b";
    test_printer __LINE__ " (*foo *) 1" "1";
    test_printer __LINE__ "2 (*why hello*) b" "2 b";
    test_printer_same __LINE__ "_";
    (* TODO: what to do there?
    test_printer_same __LINE__ "_a";
    test_printer_same __LINE__ "a_b";
    test_printer_same __LINE__ "a__b";
    test_printer_same __LINE__ "a___b";
    test_printer_same __LINE__ "a ___b";
    *)
    test_printer_same __LINE__ "a___";
    test_printer_same __LINE__ "___";
    test_printer_same __LINE__ "a b c";
    test_printer __LINE__ "a b (c+d) " "a b (c+d)";
    test_printer __LINE__ "f[a b, c+d e + 1]" "f[a b,c+d e+1]";
    test_printer_same __LINE__ "a_|f[b__]|c";
    test_printer_same __LINE__ "a ___ b";
    test_printer __LINE__ "f[a_]=b " "f[a_]=b";
    test_printer __LINE__
      "f[a_|foo]:=b[c] d+f "
      "f[a_|foo]:=b[c] d+f";
    test_printer __LINE__ ~strip_space:true
      "FullForm[1==a>=b<d<=e b+c|d<e]"
      "Alternatives[Inequality[1,Equal,a,GreaterEqual,b,Less,d,LessEqual,Plus[Times[e,b],c]],\
       Inequality[d,Less,e]]";
    test_printer_same __LINE__ "!a!";
    test_printer_same __LINE__ "a&&b||!c";
    test_printer_same __LINE__ "a||b&&!c";
    test_printer_same __LINE__ "a===b";
    test_printer_same __LINE__ "a!";
    test_printer_same __LINE__ "a!+b";
    test_printer_same __LINE__ "a! b";
    test_printer_same __LINE__ "!a+b! c";
    test_printer_same __LINE__ "a/;b";
    test_printer_same __LINE__ "f[a_]/;b";
    test_printer_same __LINE__ "a_/;b:>a+1";
    test_printer_same __LINE__ "f[a] /. {x,y,z}";
    test_printer_same __LINE__ "f[a] //. {x,y,z} d";
    test_printer_same __LINE__ "f[a,b+c d!] //. {f[x,y,z___]:>f[x y,z]}";
    test_printer_same __LINE__ "f[a_]=b";
    test_printer_same __LINE__ "f[a_]:>g[a,a]";
    test_printer_same __LINE__ "f[x]->g[x,a]";
    test_printer_same __LINE__ "f[x]:>g[x,a]";
    test_printer_same __LINE__ "f[x]->g[x,a]";
    test_printer_same __LINE__ "f[x]:>g[x]+h[x] 3";
    test_printer_same __LINE__ "f->a===b===c";
    test_printer_same __LINE__ "f[a==b==c,1]";
    test_printer __LINE__
      "Comprehension[f[x,y],MatchBind[g[x_],g[a]],MatchBind1[y_,{1,2,3,4}]]"
      "(f[x,y] :: g[x_] <- g[a],y_ <<- {1,2,3,4})";
    (* TODO
    test_printer_same __LINE__
      "1==a>=b<d<=e b+c|d<e"
      "Alternatives[Inequality[1,Equal,a,GreaterEqual,b,Less,d,LessEqual,Plus[Times[e,b],c]],\
       Inequality[d,Less,e]]";
    test_printer_same __LINE__
      "a_[_|__]/; b===1 :> a+1"
      "RuleDelayed[Condition[Pattern[a,Blank[]][Alternatives[Blank[],BlankSequence[]]]\
       ,SameQ[b,1]],Plus[a,1]]";
    test_printer_same __LINE__
      "t:f[_,_]/;t==1 :> g[t]"
      "RuleDelayed[Condition[Pattern[t,f[Blank[],Blank[]]],Inequality[t,Equal,1]],g[t]]";
    test_printer_same __LINE__
      "t:f[x___] :> g[t,x]"
      "RuleDelayed[Pattern[t,f[Pattern[x,BlankNullSequence[]]]],g[t,x]]";
       *)
  ]

(** {2 Eval} *)

let mk_eval line a b : test =
  mk_name "ok" line >:: (fun _ ->
    try
      let buf = Lexing.from_string a in
      let e = Parser.parse_expr Lexer.token buf in
      let e = Eval.eval e in
      OUnit.assert_equal ~cmp:CCString.equal ~printer:CCFun.id
        b (CCFormat.to_string Expr.pp_full_form e)
    with Parse_loc.Parse_error (_,s) ->
      let msg =
        CCFormat.sprintf "failed to parse `%s`:@ %s" a s
      in
      raise (Test_fail msg)
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
    mk_eval __LINE__ "f[a+b+2+c,d]" "f[Plus[a,b,2,c],d]";
    mk_eval __LINE__ "f[10 2+3,a b c]" "f[23,Times[a,b,c]]";
    mk_eval __LINE__
      "f[10 a+3 b+c+0,a (b+ c)] "
      "f[Plus[Times[10,a],Times[3,b],c],Times[a,Plus[b,c]]]";
    mk_eval __LINE__ "0!" "1";
    mk_eval __LINE__ "5!" "120";
    mk_eval __LINE__ "10!" "3628800";
    mk_eval __LINE__ "(foo[x_] := g[x]); foo[a]" "g[a]";
    mk_eval __LINE__ "(foo[x_] := Sequence[x,x]); g[foo[a]]" "g[a,a]";
    mk_eval __LINE__
      "(Plus[S[x_],y_] := S[Plus[x,y]]); (Plus[0,y_] := y); Plus[S[S[0]],a]"
      "S[S[a]]";
    mk_eval __LINE__ "Plus[Nest[S,0,10],Nest[S,0,10]] === Nest[S,0,20]" "True";
    mk_eval __LINE__
      "(Mult[S[x_],y_] := Plus[Mult[x,y],y]); (Mult[0,y_] := 0); Mult[S[0],a]"
      "a";
    mk_eval __LINE__ "Mult[Nest[S,0,10],Nest[S,0,10]] === Nest[S,0,100]" "True";
    mk_eval __LINE__ "(test1 = 1); f[test1]" "f[1]";
    mk_eval __LINE__ "f[a] //. {a->b}" "f[b]";
    mk_eval __LINE__ "f[a] //. {a->b,b->c}" "f[c]";
    mk_eval __LINE__ "{a,b,c} //. {b->2}" "List[a,2,c]";
    mk_eval __LINE__ "{1,2,3} //. {2->a}" "List[1,a,3]";
    mk_eval __LINE__ "f[1,2,g[3],4,5] //. f[l__,g[x_],r__] :> h[r,x,l]" "h[4,5,3,1,2]";
    mk_eval __LINE__ "f[1,2,g[3],4,5] //. f[l1__,l2__,g[x_],r__] :> h[r,x,l2,l1]" "h[4,5,3,2,1]";
    mk_eval __LINE__
      "f[1,2,2,g[2],2,2,g[3],4,5] //. f[l1__,l2__,g[x_],r__] :> h[r,x,l2,last[l1]]"
      "h[2,2,g[3],4,5,2,2,2,last[1]]";
    mk_eval __LINE__
      "f[1,2,2,g[2],2,2,g[3],4,5] //. f[l1___,l2__,g[x_],r__] :> h[r,x,l2,last[l1]]"
      "h[2,2,g[3],4,5,2,1,2,2,last[]]";
    mk_eval __LINE__
      "f[g[a1,b,c1],h[a2,b,c2]] //. f[g[___,x_,___],h[___,x_,___]] :> {x}"
      "List[b]";
    mk_eval __LINE__
      "f[g[a1,b,c1],h[a2,b,c2]] //. f[g[___,x__,___],h[___,x__,___]] :> {x}"
      "List[b]";
    mk_eval __LINE__
      "{f[a],f[b],f[c],f[d]} //. f[x_] /; ((x===a)||(x===c)) :> g[x]"
      "List[g[a],f[b],g[c],f[d]]";
    mk_eval __LINE__
      "f[g[a1,b,c1],h[a2,b,c2]] //. f[g[___,x_,___],h[___,y_,___]] /; x===y :> {x}"
      "List[b]";
    mk_eval __LINE__
      "sortRule := {x___,y_,z_,k___}/;(y>z) :> {x,z,y,k} ; \
       {64, 44, 71, 48, 96, 47, 59, 71, 73, 51, 67, 50, 26, 49, 49}//.sortRule"
      "List[26,44,47,48,49,49,50,51,59,64,67,71,71,73,96]";
    mk_eval __LINE__ "1<2<3<4" "True";
    mk_eval __LINE__ "1<2<3>4" "False";
    mk_eval __LINE__ "4>2==1+1<3" "True";
    mk_eval __LINE__ "(1+#1&)[41]" "42";
    mk_eval __LINE__ "(1+#&)[41,0]" "42";
    mk_eval __LINE__ "({#0}&)[a,b,c,d]" "List[a,b,c,d]";
    mk_eval __LINE__ "Nest[f[#1,#1]&,a,2]" "f[f[a,a],f[a,a]]";
    mk_eval __LINE__ "Nest[f[#1,#1]&,a,3]" "f[f[f[a,a],f[a,a]],f[f[a,a],f[a,a]]]";
    mk_eval __LINE__
      "Comprehension[f[x],x_<<-{1,2,3,4}]"
      "Sequence[f[1],f[2],f[3],f[4]]";
    mk_eval __LINE__
      "{Comprehension[f[x],x_<<-{1,2,3,4}]}"
      "List[f[1],f[2],f[3],f[4]]";
    mk_eval __LINE__
      "Comprehension[f[x,y],g[x_]<-g[a],y_<<-{1,2,3,4}]"
      "Sequence[f[a,1],f[a,2],f[a,3],f[a,4]]";
    mk_eval __LINE__
      "Comprehension[f[x,y],g[x_]<<-{a,g[b],c,g[d]},y_<<-{1,2+x,3}]"
      "Sequence[f[b,1],f[b,Plus[2,b]],f[b,3],f[d,1],f[d,Plus[2,d]],f[d,3]]";
    mk_eval __LINE__ "{f[x y] :: x_<<-{1,2,3,4,5}, y_<-3, x+y<7}" "List[f[3],f[6],f[9]]";
    mk_eval __LINE__ "Range[5]" "List[0,1,2,3,4,5]";
    mk_eval __LINE__ "Range[2,5]" "List[2,3,4,5]";
    mk_eval __LINE__ "Range[1]" "List[0,1]";
    mk_eval __LINE__ "Range[-1]" "List[]";
    mk_eval __LINE__ "Range[2,9,3]" "List[2,5,8]";
    mk_eval __LINE__ "Range[10,5,-1]" "List[10,9,8,7,6,5]";
    mk_eval __LINE__ "Range[Range[3]]" "List[List[0],List[0,1],List[0,1,2],List[0,1,2,3]]";
    mk_eval __LINE__ "a+(b+(c+d))" "Plus[a,b,c,d]";
    mk_eval __LINE__ "Nest[#+1&,a,3]" "Plus[3,a]";
    mk_eval __LINE__ "Let[x_<-1,f[x]]" "f[1]";
    mk_eval __LINE__ "Let[x_<-1,y_<<-{1,2,3},x+y==3,f[y]]" "f[2]";
    mk_eval __LINE__ "Let[{___,___}<-{},1]" "1";
    mk_eval __LINE__
      "Inits[l_] := {x :: {x___,___}<-l}; Inits[{1,2,3,4}]"
      "List[1,1,2,1,2,3,1,2,3,4]";
    mk_eval __LINE__
      "Perms[{}] := {{}}; \
       Perms[{x_,r___}] := { {l1,x,l2} :: {l1___,l2___} <<- Perms[{r}]}; \
       Perms[{1,2,3}] === {{1,2,3},{2,1,3},{2,3,1},{1,3,2},{3,1,2},{3,2,1}}"
      "True";
    mk_eval __LINE__ "IntegerQ[1]" "True";
    mk_eval __LINE__ "IntegerQ[1/2]" "False";
    mk_eval __LINE__ "IntegerQ[a]" "False";
    mk_eval __LINE__ "RationalQ[1]" "True";
    mk_eval __LINE__ "RationalQ[1/2]" "True";
    mk_eval __LINE__ "RationalQ[a]" "False";
    mk_eval __LINE__ "TrueQ[True]" "True";
    mk_eval __LINE__ "TrueQ[1]" "False";
    mk_eval __LINE__ "TrueQ[1/2]" "False";
    mk_eval __LINE__ "TrueQ[a]" "False";
    mk_eval __LINE__ "f[Plus[a,Plus[b,c]]]" "f[Plus[a,b,c]]";
    mk_eval __LINE__ "f[Sequence[a,Sequence[b,c]]]" "f[a,b,c]";
    mk_eval __LINE__ "Head[f[a,b,c]]" "f";
    mk_eval __LINE__ "Head[f]" "Head[f]";
    mk_eval __LINE__ "Length[f[a,b,c]]" "3";
    mk_eval __LINE__ "Length[f]" "Length[f]";
    mk_eval __LINE__ "Ceil[1/2]" "1";
    mk_eval __LINE__ "Ceil[-1/2]" "0";
    mk_eval __LINE__ "Floor[1/2]" "0";
    mk_eval __LINE__ "Floor[-1/2]" "-1";
    mk_eval __LINE__ "Ceil[0]==Floor[0]==0" "True";
    mk_eval __LINE__ "{f[],g[]} //. {_f -> a}" "List[a,g[]]";
    mk_eval __LINE__ "f[PatternTest[x_,IntegerQ]] := a; {f[b],f[1],f[1/2]}" "List[f[b],a,f[1/2]]";
    mk_eval __LINE__ "f[__g] := check; {f[g[1],g[],h],f[g[],g[]],f[],f[a,g[]]}"
      "List[f[g[1],g[],h],check,f[],f[a,g[]]]";
    mk_eval __LINE__ "f[___g] := check; {f[g[1],g[],h],f[g[],g[]],f[],f[a,g[]]}"
      "List[f[g[1],g[],h],check,check,f[a,g[]]]";
    mk_eval __LINE__ "f[a] /. f[x_] :> f[f[x]]" "f[f[a]]";
  ]

(** {2 Main} *)

let suite =
  "stimsym" >::: [
    suite_parser;
    suite_eval;
    suite_printer;
  ]

let () =
  let _ = OUnit.run_test_tt_main suite in
  ()

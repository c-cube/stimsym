
(* This file is free software. See file "license" for more details. *)

(** {1 More Advanced Builtins} *)

module E = Expr
module B = Builtins

module E_tbl = E.Tbl

type t = B.t

module Sat_solve = struct
  type form =
    | And of form array
    | Or of form array
    | True
    | False
    | Not of form
    | Atom of E.t

  let not_ = function
    | True -> False
    | False -> True
    | Not a -> a
    | f -> Not f

  type atom = {
    a_expr: E.t;
    a_int: int;  (* the corresponding atom for the SAT solver *)
  }

  type lit = atom * bool
  type clause = lit list

  type res =
    | Unsat
    | Sat of lit list

  let rec as_form (e:E.t): form = match e with
    | E.Const {E.cst_name="True";_} -> True
    | E.Const {E.cst_name="False";_} -> False
    | E.App (E.Const {E.cst_name="And";_}, a)->
      And (Array.map as_form a)
    | E.App (E.Const {E.cst_name="Or";_}, a)->
      Or (Array.map as_form a)
    | E.App (E.Const {E.cst_name="Rule";_}, [| a; b |])->
      (* implication *)
      Or [| not_ (as_form a); as_form b |]
    | E.App (E.Const {E.cst_name="Not";_}, [| f |])->
      not_ (as_form f)
    | E.App (E.Const {E.cst_name="Not";_}, _)->
      raise B.Eval_does_not_apply
    | _ -> Atom e

  type cnf_state = {
    cnf_arg: E.prim_fun_args;
    cnf_tbl: atom E_tbl.t;
    mutable cnf_count: int; (* for allocating new symbols *)
    cnf_rev: (int, atom) Hashtbl.t; (* retrieve atom by index *)
  }

  (* expr -> atom *)
  let get_atom_ st (e:E.t): atom =
    try E_tbl.find st.cnf_tbl e
    with Not_found ->
      let n = st.cnf_count in
      st.cnf_count <- n+1;
      let a = {a_expr=e; a_int=n} in
      Hashtbl.add st.cnf_rev n a;
      E_tbl.add st.cnf_tbl e a;
      a

  (* all the ways to pick one element in each list *)
  let array_prod (a:'a list array) : 'a list Sequence.t =
    let rec aux i k =
      if i=Array.length a then k []
      else (
        List.iter (fun x -> aux (i+1) (fun tail -> k (x::tail))) a.(i)
      )
    in
    aux 0

  let cnf (st:cnf_state) (f:form): clause list =
    let rec nnf (f:form): form = match f with
      | Atom _ | Not (Atom _) -> f
      | True | False -> f
      | Not True -> False
      | Not False -> True
      | Not (Not f) -> nnf f
      | Not (And a) -> Or (Array.map nnf_neg a)
      | Not (Or a) -> And (Array.map nnf_neg a)
      | And a -> And (Array.map nnf a)
      | Or a -> Or (Array.map nnf a)
    and nnf_neg f = match f with
      | Not a -> nnf a
      | _ -> nnf (not_ f)
    in
    (* precond: in NNF *)
    let rec aux_cnf f = match f with
      | Atom a -> [[get_atom_ st a, true]]
      | True -> []
      | False -> [[]]
      | Not (Atom a) -> [[get_atom_ st a, false]]
      | Not _ -> assert  false
      | Or a ->
        (* TODO: tseitin transformation *)
        let a = Array.map aux_cnf a in
        array_prod a
        |> Sequence.map List.flatten
        |> Sequence.to_rev_list
      | And a ->
        Array.map aux_cnf a |> Array.to_list |> List.flatten
    in
    f |> nnf |> aux_cnf

  let print_problem st (out:out_channel) (pb:clause list) =
    Printf.fprintf out "p cnf %d %d\n" st.cnf_count (List.length pb);
    List.iter
      (fun c ->
         List.iter
           (fun ({a_int=i;_},b) -> Printf.fprintf out "%s%d " (if b then "" else "-") i)
           c;
         Printf.fprintf out "0\n")
      pb;
    flush out

  let parse_res st (out:string list): res = match out with
    | ["UNSAT"] -> Unsat
    | ["SAT"; model] ->
      let model =
        model
        |> CCString.Split.list_cpy ~by:" "
        |> CCList.filter_map
          (fun i ->
             let i = int_of_string i in
             if i<>0 then
               try
                 let a = Hashtbl.find st.cnf_rev (abs i) in
                 Some (a, i>0)
               with Not_found -> None
             else None)
      in
      Sat model
    | _ ->
      Eval.prim_failf st.cnf_arg "could not parse minisat's output:\n`%s`\n"
        (String.concat "\n" out)

  let call_ (st:cnf_state) (pb:clause list): res =
    CCIO.File.with_temp ~prefix:"stimsym" ~suffix:".cnf"
      (fun file_in ->
         CCIO.File.with_temp ~prefix:"stimsym_out" ~suffix:".res"
           (fun file_out ->
              CCIO.with_out file_in (fun oc -> print_problem st oc pb);
              let limit = 30 in
              let cmd =
                Printf.sprintf "minisat -cpu-lim=%d %s %s" limit file_in file_out
              in
              B.logf "call minisat with `%s`\n" cmd;
              let p = CCUnix.call "%s" cmd in
              let err = p#errcode in
              begin match err with
                | 10 -> B.log "should return sat\n"
                | 20 -> B.log "should return unsat\n"
                | n -> B.logf "unknown return: %d\n" n
              end;
              let out = CCIO.with_in file_out CCIO.read_lines_l in
              parse_res st out
           ))

  let call (st:cnf_state) (pb:clause list): res =
    if List.exists (function [] -> true | _ -> false) pb
    then Unsat
    else
      try call_ st pb
      with e ->
        Eval.prim_failf st.cnf_arg
          "error while calling minisat:@ %s" (Printexc.to_string e)

  let eval _ arg e = match e with
    | E.App (_, [| form |]) ->
      let st = {
        cnf_arg=arg;
        cnf_count=1;
        cnf_tbl=E_tbl.create 64;
        cnf_rev=Hashtbl.create 64;
      } in
      let clauses = cnf st (as_form form) in
      B.logf "call solver with %d clauses\n" (List.length clauses);
      let res = match call st clauses with
        | Unsat -> E.app (E.const_of_string "Unsat") [| |]
        | Sat m ->
          let m =
            Array.of_list m
            |> Array.map
              (fun (a,sign) ->
                 E.app B.rule [| a.a_expr; if sign then B.true_ else B.false_ |])
            |> E.app B.list
          in
          E.app (E.const_of_string "Sat") [| m |]
      in
      Some res
    | _ -> raise B.Eval_does_not_apply
end

(* TODO: optional timeout *)

let sat_solve =
  B.make "SatSolve"
    ~funs:[Sat_solve.eval]
    ~doc:[
      `S "SatSolve";
      `P "`SatSolve[form]` calls a SAT solver on the formula given \
          as parameter. The formula is reduced to CNF automatically \
          before calling Minisat.";
      `P "If Minisat is not installed, this does not reduce.";
      `P "Returns either `Sat[{m___}]` where `m` is the model, as a \
          list of bindings `Atom -> True` or `Atom -> False`, \
          or Unsat[].";
      (* TODO: update once we have typing? *)
      `I ("example", [
          `P "The following call will return `Unsat[]`.";
          `Pre "`SatSolve[(A || B)&& (!A || !B) && !B]`";
        ]);
      `I ("example", [
          `P "The following call will return `Sat[A -> False,B->True]`, \
              containing a model for each atom appearing in the formulas.";
          `Pre "`SatSolve[And[A || B,!A]]`";
        ]);
      `I ("example", [`L [
          [`P "Find a model of `a&&b` and extract the value of `a` in the \
               model using `Let`:";
           `Pre "`Let[{___,a->r_,___}<<-SatSolve[a&&b],r]`"];
          [`P "also check that the model reduces the formula to `True`:";
           `Pre "`Let[Sat[m_]<-SatSolve[a&&b], a&&b//. m]`"];
          [`P "convert the model into (possibly negated) atoms:";
           `Pre "Let[Sat[m_]<-SatSolve[a&&b,!c], m//.{(x_->False):>!x, (x_->True):>x}]";
           `P "(yield `{a,b,!c}`)"]
        ]]);
      `I ("requires", [`P "`minisat` must be on the $PATH"]);
    ]

module Graph = struct
  (* convenient representation *)
  type t = {
    vertices: E.t array;
    edges: E.t list E.Tbl.t;
  }

  let make vertices (edges:E.t array): t =
    let tbl = E.Tbl.create 32 in
    Array.iter
      (function
        | E.App (E.Const {E.cst_name="Rule";_}, [| lhs; rhs |]) ->
          let l = E.Tbl.get_or ~or_:[] tbl lhs in
          E.Tbl.replace tbl lhs (rhs :: l)
        | _ -> raise B.Eval_does_not_apply)
      edges;
    { vertices; edges=tbl }

  let as_graph g =
    CCGraph.make_tuple (fun v -> E.Tbl.get_or ~or_:[] g.edges v |> Sequence.of_list)

  let pp_dot out (g:t): unit =
    let fmt = Format.formatter_of_out_channel out in
    let attrs_v v = [`Label (E.to_string v); `Shape "box"] in
    let attrs_e _ = [] in
    Format.fprintf fmt "%a@."
      (CCGraph.Dot.pp_seq
         ~eq:E.equal ~name:"some_graph"
         ~attrs_v ~attrs_e
         ~tbl:(CCGraph.mk_table ~eq:E.equal ~hash:E.hash 32)
         ~graph:(as_graph g))
       (Sequence.of_array g.vertices)

  let get_png (g:t): E.mime_content =
    B.log "get png for graph...\n";
    CCIO.File.with_temp ~prefix:"stimsym_graph" ~suffix:".dot"
      (fun dot_file ->
         (* write file, then invoke `dot` *)
         CCIO.with_out dot_file (fun oc -> pp_dot oc g);
         let p = CCUnix.call "dot '%s' -Tpng " dot_file in
         let _ = p#errcode in
         let data = p#stdout in
         B.logf "got png (%d bytes)\n" (String.length data);
         {E.mime_data=data; mime_ty="image/png"; mime_base64=true})

  let display e = match e with
    | E.App (_,
        [| E.App (E.Const {E.cst_name="List";_}, vertices);
           E.App (E.Const {E.cst_name="List";_}, edges)
        |]) ->
      [make vertices edges |> get_png]
    | _ -> []

  let eval _ _ e = match e with
    | E.App (hd, [| E.App (E.Const {E.cst_name="List";_}, edges) |]) ->
      (* convert to the canonical form *)
      let vertices =
        Sequence.of_array edges
        |> Sequence.flat_map_l
          (function
            | E.App (E.Const {E.cst_name="Rule";_}, [| lhs; rhs |]) ->
              [lhs;rhs]
            | _ -> raise B.Eval_does_not_apply)
        |> E.Tbl.of_seq_count
        |> E.Tbl.keys_list
        |> Array.of_list
      in
      Some (E.app hd [| E.app B.list vertices; E.app B.list edges |])
    | _ -> raise B.Eval_does_not_apply
end

let graph =
  B.make "Graph" ~funs:[Graph.eval] ~display:Graph.display
    ~doc:[
      `S "Graph";
      `P "A directed graph structure.";
      `P "`Graph[{a->b,…}]` builds a graph from a list of edges.";
      `P "`Graph[{a,b,c},{a->b,…}]` builds a graph from a list of vertices \
          and a list of edges.";
      `P "The graph has a graphical display in the notebook.";
      `I ("example", [
          `P "a simple 2-layers graph";
          `Pre "Graph[{i->j :: i_<<- {1,2,3,4}, j_<<-{a,b,c,d}}]";
        ]);
    ]


(* Permet l'impression du résultat du compilateur  à différents moments de la
 * compilation *)

(* Ce fichier sert uniquement au débogage et est inutile au bon fonctionnement
 * du compilateur *)

open Format
open Lexing
open Parser
open Ast
open UncurriedAst
open TypedAst
open FreeVarsAst
open ClosureAst
open AllocatedAst

(* tion des options proposées pour l'exécution *)

let opt_print_tokens = ref false
let opt_print_ast = ref false
let opt_print_uncurried_ast = ref false
let opt_print_typed_ast = ref false
let opt_print_free_vars_ast = ref false
let opt_print_closure_ast = ref false
let opt_print_allocated_ast = ref false

let spec = [
  "--print-tokens", Arg.Set opt_print_tokens,
      " print result of lexing then exit" ;
  "--print-ast", Arg.Set opt_print_ast, " print result of parsing" ;
  "--print-uncurried-ast", Arg.Set opt_print_uncurried_ast,
      " print result of uncurrification" ;
  "--print-typed-ast", Arg.Set opt_print_typed_ast, " print result of typing" ;
  "--print-free-vars-ast", Arg.Set opt_print_free_vars_ast,
      " print free variables of expressions" ;
  "--print-closure-ast", Arg.Set opt_print_closure_ast,
      " print ast with thunks and closures" ;
  "--print-allocated-ast", Arg.Set opt_print_allocated_ast,
      " print result of allocation"]

(* Fonctions d'impression *)

let toks = Hashtbl.create 59
let () = List.iter (fun (t,s) -> Hashtbl.add toks t s )
  [ELSE, "else" ; IF, "if" ; IN, "in" ; LET, "let" ; CASE, "case" ; OF, "of" ;
   THEN, "then" ; RETURN, "return" ; DO, "do" ; LB, "(" ; RB, ")" ; LSB, "[" ;
   RSB, "]" ; LCB, "{" ; RCB, "}" ; ARROW, "->" ; SEMI, ";" ; COLON, ":" ;
   COMMA, "," ; LAMBDA, "\\" ; ASSIGN, "=" ; LT, "<" ; LEQ, "<=" ; GT, ">" ;
   GEQ, ">=" ; EQ, "==" ; NEQ, "/=" ; PLUS, "+" ; MINUS, "-" ; TIMES, "*" ;
   OR, "||" ; AND, "&&" ; EOF, "#"]

let print_tokens lb =
  let rec digere_lexer () = match Lexer.next_tokens lb with
    | EOF -> [EOF]
    | t -> t::digere_lexer () in
  let rec aux = function
    | [] -> printf "@."
    | t::q -> ( match t with
      | IDENT0 s -> printf "id0<%s> " s
      | IDENT1 s -> printf "id1<%s> " s
      | CST c -> ( match c with
        | Cint n -> printf "int<%d> " n
        | Cchar c -> printf "char<%s> " (Char.escaped c)
        | Cbool true -> printf "bool<True> "
        | Cbool false -> printf "bool<False> " )
      | STRING (Elist l) -> printf "list[" ;
        List.iter (function {expr = Ecst (Cchar c)} -> printf "%s,"
          (Char.escaped c) | _ -> ()) l ; printf "] "
      | t -> let s = try
            Hashtbl.find toks t
          with Not_found -> "_" in
        printf "%s " s ) ;
      aux q in
  aux (digere_lexer ()) ;
  printf "@."

let ops = Hashtbl.create 17
let () = List.iter (fun (o,s) -> Hashtbl.add ops o s )
  [ Badd, "+" ; Bsub, "-" ; Bmul, "*" ; Band, "&" ; Bor, "|" ; Bcol, ":" ;
    Blt, "<" ; Bleq, "<=" ; Bgt, ">" ; Bgeq, ">=" ; Beq, "=" ; Bneq, "/=" ]

let print_ast =
  let print_cte = function
    | Cint i -> print_int i
    | Cchar c -> printf "'%s'" (Char.escaped c)
    | Cbool true -> printf "True"
    | Cbool false -> printf "False" in
  let rec print_expr e = match e.expr with 
    | Eident s -> printf " %s" s
    | Ecst c -> printf " " ; print_cte c
    | Elist l -> printf " [" ; List.iter print_expr l ; printf " ]"
    | Eappli (f, args) -> print_expr f ; printf "(" ;
      List.iter (fun e -> print_expr e ; printf ",") args ; printf " )"
    | Elambda (args, e) -> printf " (\\" ;
      List.iter (fun {ident = s} -> printf " %s" s) args ; printf " ->" ;
      print_expr e ; printf ")"
    | Ebinop (o, e0, e1) -> printf " %s" (Hashtbl.find ops o) ;
      print_expr e0 ; print_expr e1
    | Eif (cdt, e1, e2) -> printf " if " ; print_expr cdt ; printf " then " ;
      print_expr e1 ; printf " else " ; print_expr e2
    | Elet (ld, e) -> printf "let " ;
      List.iter (fun d -> print_def d ; printf "\n") ld ; printf "in" ;
      print_expr e 
    | Ecase (e, e0, {ident = hd}, {ident = tl}, e1) -> printf "case " ;
      print_expr e ; printf " of\n | [] -> " ; print_expr e0 ;
      printf "\n | %s:%s -> " hd tl ; print_expr e1 ; printf "\n"
    | Edo l -> printf "{" ; List.iter (fun e -> printf "\n" ; print_expr e) l ;
      printf "\n}"
    | Ereturn -> printf " ()"
  and print_def ({ident = s}, l, e) = ( match l with
    | [] -> printf "%s=\n" s
    | _ -> printf "%s(" s ; List.iter (fun {ident = s}  -> printf "%s," s) l ;
      printf ")=\n" ) ;
    print_expr e in
  let rec print_file = function
    | [] -> ()
    | def0::q -> print_def def0 ; printf "\n@." ; print_file q in
  print_file

let print_uncurried_ast =
  let print_cte = function
    | Cint i -> print_int i
    | Cchar c -> printf "'%s'" (Char.escaped c)
    | Cbool true -> printf "True"
    | Cbool false -> printf "False" in
  let rec print_expr e = match e.uexpr with 
    | Uident s -> printf " %s" s
    | Ucst c -> printf " " ; print_cte c
    | Ulist l -> printf " [" ; List.iter print_expr l ; printf " ]"
    | Uappli (f, arg) -> print_expr f ; printf "(" ; print_expr arg ; printf " )"
    | Ulambda (args, e) -> printf " (\\" ;
    List.iter (fun s -> printf " %s" s) args ; printf " ->" ; print_expr e ;
      printf ")"
    | Ubinop (o, e0, e1) -> printf " %s" (Hashtbl.find ops o) ;
      print_expr e0 ; print_expr e1
    | Uif (cdt, e1, e2) -> printf " if " ; print_expr cdt ; printf " then " ;
      print_expr e1 ; printf " else " ; print_expr e2
    | Ulet (ld, e) -> printf "let " ;
      List.iter (fun d -> print_def d ; printf "\n") ld ; printf "in" ;
      print_expr e 
    | Ucase (e, e0, hd, tl, e1) -> printf "case " ; print_expr e ;
      printf " of\n | [] -> " ; print_expr e0 ; printf "\n | %s:%s -> " hd tl ;
      print_expr e1 ; printf "\n"
    | Udo l -> printf "{" ; List.iter (fun e -> printf "\n" ; print_expr e) l ;
      printf "\n}"
    | Ureturn -> printf " ()"
  and print_def (s, e) = printf "%s=\n" s ; print_expr e in
  let rec print_file = function
    | [] -> ()
    | def0::q -> print_def def0 ; printf "\n@." ; print_file q in
  print_file

let print_typed_ast =
  let var_names = Hashtbl.create 59 in
  let print_cte = function
    | Cint i -> print_int i
    | Cchar c -> printf "'%s'" (Char.escaped c)
    | Cbool true -> printf "True"
    | Cbool false -> printf "False" in
  let rec print_expr e = ( match e.texpr with 
      | Tident s -> printf " %s" s
      | Tcst c -> printf " " ; print_cte c
      | Tlist l -> printf " [" ; List.iter print_expr l ; printf " ]"
      | Tappli (f, arg) -> printf " (" ; print_expr f ; printf "(" ;
        print_expr arg ; printf " ))"
      | Tlambda (args, e) -> printf " (\\" ;
        List.iter (fun s -> printf " %s" s) args ; printf " ->" ; print_expr e ;
        printf ")"
      | Tbinop (o, e0, e1) -> printf " (%s" (Hashtbl.find ops o) ;
      print_expr e0 ; print_expr e1 ; printf ")"
      | Tif (cdt, e1, e2) -> printf " (if " ; print_expr cdt ; printf " then " ;
        print_expr e1 ; printf " else " ; print_expr e2 ; printf ")"
      | Tlet (ld, e) -> printf "(let " ;
        List.iter (fun d -> print_def d ; printf "\n") ld ; printf "in" ;
        print_expr e ; printf ")"
      | Tcase (e, e0, hd, tl, e1) -> printf "case " ; print_expr e ;
        printf " of\n | [] -> " ; print_expr e0 ; printf "\n | %s:%s -> " hd tl ;
        print_expr e1 ; printf "\n"
      | Tdo l ->
          printf "{" ; List.iter (fun e -> printf "\n" ; print_expr e) l ;
        printf "\n}"
      | Treturn -> printf " ()" ) ;
    printf "<%s>" (string_of_typ e.typ var_names)
  and print_def (s, e) = printf "%s=\n" s ; print_expr e in
  let rec print_file = function
    | [] -> ()
    | def0::q -> print_def def0 ; printf "\n@." ; print_file q in
  print_file

let print_free_vars_ast =
  let print_cte = function
    | Cint i -> print_int i
    | Cchar c -> printf "'%s'" (Char.escaped c)
    | Cbool true -> printf "True"
    | Cbool false -> printf "False" in
  let rec print_expr e = ( match e.vexpr with 
      | Vident s -> printf " %s" s
      | Vcst c -> printf " " ; print_cte c
      | Vemptylist -> printf " []"
      | Vappli (f, arg) -> printf " (" ; print_expr f ; printf "(" ;
        print_expr arg ; printf " ))"
      | Vlambda (arg, e) -> printf " (\\" ;
        printf " %s" arg ; printf " ->" ; print_expr e ;
        printf ")"
      | Vbinop (o, e0, e1) -> printf " (%s" (Hashtbl.find ops o) ;
      print_expr e0 ; print_expr e1 ; printf ")"
      | Vif (cdt, e1, e2) -> printf " (if " ; print_expr cdt ; printf " then " ;
        print_expr e1 ; printf " else " ; print_expr e2 ; printf ")"
      | Vlet (ld, e) -> printf "(let " ;
        List.iter (fun d -> print_def d ; printf "\n") ld ; printf "in" ;
        print_expr e ; printf ")"
      | Vcase (e, e0, hd, tl, e1) -> printf "case " ; print_expr e ;
        printf " of\n | [] -> " ; print_expr e0 ; printf "\n | %s:%s -> " hd tl ;
        print_expr e1 ; printf "\n"
      | Vdo l ->
          printf "{" ; List.iter (fun e -> printf "\n" ; print_expr e) l ;
        printf "\n}"
      | Vreturn -> printf " ()" ) ;
    printf "<" ;
    List.iter (fun x -> printf "%s," x) e.var_libres ;
    printf ">"
  and print_def (s, e) = printf "%s=\n" s ; print_expr e in
  let rec print_file = function
    | [] -> ()
    | def0::q -> print_def def0 ; printf "\n@." ; print_file q in
  print_file

let print_closure_ast =
  let print_cte = function
    | Cint i -> print_int i
    | Cchar c -> printf "'%s'" (Char.escaped c)
    | Cbool true -> printf "True"
    | Cbool false -> printf "False" in
  let rec print_expr = function
    | Fident s -> printf " %s" s
    | Fcst c -> printf " " ; print_cte c
    | Femptylist -> printf " []"
    | Fappli (f, arg) -> printf " (" ; print_expr f ; printf "(" ;
      print_expr arg ; printf " ))"
    | Fclos (f, fvars) -> printf " clos<" ;
      List.iter (fun x -> printf "%s," x) fvars ; printf "> %s" f
    | Fbinop (o, e0, e1) -> printf " (%s" (Hashtbl.find ops o) ;
      print_expr e0 ; print_expr e1 ; printf ")"
    | Fif (cdt, e1, e2) -> printf " (if " ; print_expr cdt ; printf " then " ;
      print_expr e1 ; printf " else " ; print_expr e2 ; printf ")"
    | Flet (ld, e) -> printf "(let " ;
      List.iter (fun d -> print_def d ; printf "\n") ld ; printf "in" ;
      print_expr e ; printf ")"
    | Fcase (e, e0, hd, tl, e1) -> printf "case " ; print_expr e ;
      printf " of\n | [] -> " ; print_expr e0 ; printf "\n | %s:%s -> " hd tl ;
      print_expr e1 ; printf "\n"
    | Fdo l ->
      printf "{" ; List.iter (fun e -> printf "\n" ; print_expr e) l ;
      printf "\n}"
    | Freturn -> printf " ()" ;
    | Fglacon e -> printf " §" ; print_expr e ; printf "§"
  and print_def (s, e) = printf "%s=\n" s ; print_expr e
  and print_decl = function
    | Fdef (s, e) -> print_def (s, e)
    | Ffun (s, clot, arg, e) -> printf "%s<" s ;
      List.iter (fun x -> printf "%s," x) clot ; printf "> %s ->" arg ;
      print_expr e
    | Fcodeglacon (s, clot, e) -> printf "G%s<" s ;
      List.iter (fun x -> printf "%s," x) clot ; printf "> _ ->" ; print_expr e
    | Fmain e -> print_def ("main", e) in
  let rec print_file = function
    | [] -> ()
    | decl0::q -> print_decl decl0 ; printf "\n@." ; print_file q in
  print_file

let print_allocated_ast =
  let print_var = function
    | Vglobale s -> printf "%s" s
    | Vlocale i -> printf "$%d" i
    | Vclos i -> printf "€%d" i
    | Varg -> printf "#" in
  let rec print_expr = function
    | CVar v -> printf " " ; print_var v
    | CCst c -> printf " %d" c
    | CEmptylist -> printf " []"
    | CAppli (f, arg) -> printf " (" ; print_expr f ; printf "(" ;
      print_expr arg ; printf " ))"
    | CClos (f, fvars) -> printf " clos<" ;
      List.iter (fun v -> print_var v ; printf ",") fvars ; printf "> %s" f
    | CBinop (o, e0, e1) -> printf " (%s" (Hashtbl.find ops o) ;
      print_expr e0 ; print_expr e1 ; printf ")"
    | CIf (cdt, e1, e2) -> printf " (if " ; print_expr cdt ; printf " then " ;
      print_expr e1 ; printf " else " ; print_expr e2 ; printf ")"
    | CLet (ld, e) -> printf "(let " ;
      List.iter (fun d -> print_def d ; printf "\n") ld ; printf "in" ;
      print_expr e ; printf ")"
    | CCase (e, e0, hd, tl, e1) -> printf "case " ; print_expr e ;
      printf " of\n | [] -> " ; print_expr e0 ;
      printf "\n | $%d:$%d -> " hd tl ; print_expr e1 ; printf "\n"
    | CDo l ->
      printf "{" ; List.iter (fun e -> printf "\n" ; print_expr e) l ;
      printf "\n}"
    | CReturn -> printf " ()" ;
    | CGlacon e -> printf " §" ; print_expr e ; printf "§"
  and print_def (i, e) = printf "$%d=\n" i ; print_expr e
  and print_decl = function
    | CDef (s, e, n) -> printf "%s[%d]=\n" s n ; print_expr e
    | CFun (s, e, n) -> printf "%s[%d] _ ->" s n ; print_expr e
    | CMain (e, n) -> printf "main[%d]=\n" n ; print_expr e in
  let rec print_file = function
    | [] -> ()
    | decl0::q -> print_decl decl0 ; printf "\n@." ; print_file q in
  print_file

(* redéfinition des fonctions du Main pour inclure les options de débogage *)

let file next_tokens lb =
  if !opt_print_tokens then (
    print_tokens lb ;
    exit 0 ) ;
  let ast = file next_tokens lb in
  if !opt_print_ast then
    print_ast ast ;
  ast

let uncurry_list_def ast primitives = 
  let uncurried_ast = uncurry_list_def ast primitives in
  if !opt_print_uncurried_ast then
    print_uncurried_ast uncurried_ast ;
  uncurried_ast

let type_ast uncurried_ast env =
  let typed_ast = type_ast uncurried_ast env in
  if !opt_print_typed_ast then
    print_typed_ast typed_ast ;
  typed_ast
        
let var_libre typed_ast =
  let free_vars_ast = var_libre typed_ast in
  if !opt_print_free_vars_ast then
    print_free_vars_ast free_vars_ast ;
  free_vars_ast

let ferm free_vars_ast =
  let closure_ast = ferm free_vars_ast in
  if !opt_print_closure_ast then
    print_closure_ast closure_ast ;
  closure_ast

let alloc closure_ast primitives =
  let allocated_ast = alloc closure_ast primitives in
  if !opt_print_allocated_ast then
    print_allocated_ast allocated_ast ;
  allocated_ast

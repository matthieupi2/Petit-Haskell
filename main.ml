
(* Programme principal *)

(* TODO Délocaliser les fonctions print_* *)

open Format
open Lexing
open Parser
open Ast
open UncurriedAst
open Error

let usage = "usage : petitghc [options] file.hs"

let opt_parse_only = ref false
let opt_uncurry_only = ref false
let opt_print_tokens = ref false
let opt_print_ast = ref false
let opt_print_uncurried_ast = ref false

let spec = [
  "--parse-only", Arg.Set opt_parse_only, " s'arrête après le parsing" ;
  "--uncurry-only", Arg.Set opt_uncurry_only,
      " s'arrête après la décurrification" ;
  "--print-tokens", Arg.Set opt_print_tokens, " affiche le résultat du lexing" ;
  "--print-ast", Arg.Set opt_print_ast, " affiche le résultat du parser" ;
  "--print-ast-uncurried", Arg.Set opt_print_uncurried_ast,
      " affiche le résultat de la décurrification" ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".hs") then
      raise (Arg.Bad "no .hs extension") ;
    file := Some s in
  Arg.parse spec set_file usage ;
  match !file with
    | Some f -> f
    | None -> Arg.usage spec usage ; exit 1

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
    List.iter (fun {ident = s} -> printf " %s" s) args ; printf " ->" ; print_expr e ;
      printf ")"
    | Ebinop (o, e0, e1) -> printf " %s" (Hashtbl.find ops o) ;
      print_expr e0 ; print_expr e1
    | Eif (cdt, e1, e2) -> printf " if " ; print_expr cdt ; printf " then " ;
      print_expr e1 ; printf " else " ; print_expr e2
    | Elet (ld, e) -> printf "let " ;
      List.iter (fun d -> print_def d ; printf "\n") ld ; printf "in" ;
      print_expr e 
    | Ecase (e, e0, {ident = hd}, {ident = tl}, e1) -> printf "case " ; print_expr e ;
      printf " of\n | [] -> " ; print_expr e0 ; printf "\n | %s:%s -> " hd tl ;
      print_expr e1 ; printf "\n"
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
    | def0::q -> print_def def0 ; printf "\n\n@." ; print_file q in
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
    | Uappli (f, arg) -> print_expr f ; printf "(" ; print_expr e ; printf " )"
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
    | def0::q -> print_def def0 ; printf "\n\n@." ; print_file q in
  print_file

(* TODO Délocaliser dans Error *)
let print_loc lb =
  let b = lexeme_start_p lb in
  let e = lexeme_end_p lb in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file b.pos_lnum
    (b.pos_cnum - b.pos_bol + 1) (e.pos_cnum - b.pos_bol + 1)

let () =
  try
    let c = open_in file in
    let lb = Lexing.from_channel c in 
    try
      if !opt_print_tokens then (
        print_tokens lb ;
        exit 0 ) ;
      let ast = Parser.file Lexer.next_tokens lb in
      close_in c ;
      try
        if !opt_print_ast then
          print_ast ast ;
        if !opt_parse_only then
          exit 0 ;
        let primitives =
            UncurriedAst.S.of_list ["div" ; "rem" ; "putChar" ; "error"] in
        let uncurried_ast = uncurry_list_def ast primitives in
        if !opt_print_uncurried_ast then
          print_uncurried_ast uncurried_ast ;
        if !opt_uncurry_only then
          exit 0 ;
        raise (CompilerError "compilateur inexistant")
      with e -> Error.error e
    with e -> Error.error_before_parsing lb e ;
  with exc -> eprintf "Anomaly: %s\n@." (Printexc.to_string exc) ;
      exit 2 (* TODO délocaliser ? *)

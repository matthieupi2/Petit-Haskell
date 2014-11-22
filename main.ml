
(* Programme principal *)

open Format
open Lexing
open Parser
(* TODO inutile ? *)
open Ast

let usage = "usage : petitghc [options] file.hs"

let opt_print_tokens = ref false
let opt_print_ast = ref false

let spec = [
  "--print-tokens", Arg.Set opt_print_tokens, " affiche le résultat du lexing" ;
  "--print-ast", Arg.Set opt_print_ast, " affiche le résultat du parser" ]

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
   OR, "||" ; AND, "&&" ; NEG, "-." ; EOF, "#"]

let print_tokens lb =
  let rec digere_lexer last_token = match Lexer.next_tokens last_token lb with
    | EOF -> [EOF]
    | t -> t::digere_lexer (Some t) in
  let rec aux = function
    | [] -> printf "@."
    | t::q -> ( match t with
      | IDENT0 s -> printf "id0<%s> " s
      | IDENT1 s -> printf "id1<%s> " s
      | CST c -> ( match c with
        | Cint n -> printf "int<%d> " n
        | Cchar c -> printf "char<%c> " c
        | Cbool true -> printf "bool<True> "
        | Cbool false -> printf "bool<False> "
        | Cstr s -> printf "string<%s> " s )
      | t -> let s = try
            Hashtbl.find toks t
          with Not_found -> "_" in
        printf "%s " s ) ;
      aux q in
  aux (digere_lexer None)

let ops = Hashtbl.create 17
let () = List.iter (fun (o,s) -> Hashtbl.add ops o s )
  [ Badd, "+" ; Bsub, "-" ; Bmul, "*" ; Band, "&" ; Bor, "|" ; Bcol, ":" ;
    Blt, "<" ; Bleq, "<=" ; Bgt, ">" ; Bgeq, ">=" ; Beq, "=" ; Bneq, "/=" ]

let print_ast =
  let print_cte = function
    | Cint i -> print_int i
    | Cchar c -> printf "'%c'" c
    | Cstr s -> printf "\"%s\"" s
    | Cbool true -> printf "True"
    | Cbool false -> printf "False" in
  let rec print_expr = function
    | Eident s -> printf "%s" s
    | Ecst c -> print_cte c
    | Ebinop (o, e0, e1) -> printf "%s " (Hashtbl.find ops o) ;
      print_expr e0 ;
      printf " " ;
      print_expr e1
    | _ -> printf "_" in
  let print_def (s, l, e) =
    printf "%s(" s ;
    List.iter (fun s  -> printf "%s," s) l ;
    printf ")=\n" ;
    print_expr e in
  let rec print_file = function
    | [] -> printf "#"
    | def0::q -> print_def def0 ; printf "\n\n" ; print_file q in
  print_file

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in 
  if !opt_print_tokens then
    print_tokens lb
  else (
    let ast = Parser.file (Lexer.next_tokens None) lb in
    if !opt_print_ast then
      print_ast ast ) ;
  close_in c ;
  exit 0 

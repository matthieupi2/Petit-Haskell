
(* Programme principal *)

open Format
open Lexing
open Parser
(* TODO inutile ? *)
open Ast

let usage = "usage : petitghc [options] file.hs"

let print_tokens = ref false

let spec = ["--print-tokens", Arg.Set print_tokens, " affiche le résultat du
lexing"]

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
   COMMA, "," ; LAMBDA, "\\" ; LT, "<" ; LEQ, "<=" ; GT, ">" ; GEQ, ">=" ;
   EQ, "==" ; NEQ, "/=" ; PLUS, "+" ; MINUS, "-" ; TIMES, "*" ; OR, "||" ;
   AND, "&&" ; NEG, "-." ; EOF, "#"]

let rec print_toks = function
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
    print_toks q

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in 
  let rec digere_lexer last_token = match Lexer.next_tokens last_token lb with
    | EOF -> [EOF]
    | t -> t::digere_lexer (Some t) in
  let lex = digere_lexer None in 
  close_in c ;
  if !print_tokens then
    print_toks lex ;
  exit 0 

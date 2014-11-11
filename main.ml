
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

let rec print_toks = function
  | [] -> printf "@."
  | IDENT0 s::q -> printf "id0<%s> " s ; print_toks q
  | IDENT1 s::q -> printf "id1<%s> " s ; print_toks q
  | CST c::q -> ( match c with
    | Cint n -> printf "int<%d> " n
    | Cchar c -> printf "char<%c> " c
    | Cbool true -> printf "bool<True> "
    | Cbool false -> printf "bool<False> "
    | _ -> printf "_ " ) ;
    print_toks q
  | EOF::q -> printf "#" ; print_toks q
  | _::q -> printf "_ " ; print_toks q

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in 
  let rec digere_lexer () = match Lexer.next_tokens lb with
    | EOF -> [EOF]
    | t -> t::digere_lexer () in
  let lex = digere_lexer () in 
  close_in c ;
  if !print_tokens then
    print_toks lex ;
  exit 0 


(* Analyseur lexical pour Petit Haskell *)

{
  open Lexing
  open Parser

  let pos_lnum =  ref 0
  let line_start = ref 0
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let car = ['\032'-'\126']#['\\' '"'] | '\\'['\\' '"' 'n' 't']
let ident = ['a'-'z'] (letter | digit | '_' | '\'')*

rule next_tokens = parse
  | '\n' { next_tokens lexbuf }
  | ident as s { if Lexing.lexeme_start lexbuf = !line_start then IDENT0 s  else IDENT1 s }
  | eof { EOF }
  | _ as c {Format.eprintf "illegal character : %c@." c ; exit 1 }

{

}

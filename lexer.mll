
(* Analyseur lexical pour Petit Haskell *)

{
  open Lexing
  open Parser
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let car = ['\032'-'\126']#['\\' '"'] | '\\'['\\' '"' 'n' 't']
let ident = ['a'-'z'] (letter | digit | '_' | '\'')*

rule next_tokens = parse
  | eof { exit 0 }
  | _ { Format.eprintf "illegal character@." ; exit 1 }

{

}

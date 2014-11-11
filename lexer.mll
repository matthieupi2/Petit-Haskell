
(* Analyseur lexical pour Petit Haskell *)

{
  type token =
    | Teof
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let car = ['\032'-'\126']#['\\' '"'] | '\\'['\\' '"' 'n' 't']
let ident = letter (letter | digit | '_')*

rule next_tokens = parse
  | eof { exit 0 }
  | _ { Format.eprintf "illegal character@." ; exit 1 }

{

}

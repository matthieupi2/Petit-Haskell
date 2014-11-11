
(* Analyseur lexical pour Petit Haskell *)

{
  open Lexing
  open Parser

  exception Error of string

  let id s lb =
    let pos = lb.lex_start_p in
    if pos.pos_cnum = pos.pos_bol then
      IDENT0 s
    else
      IDENT1 s
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let car = ['\032'-'\126']#['\\' '"'] | '\\'['\\' '"' 'n' 't']
let ident = ['a'-'z'] (letter | digit | '_' | '\'')*

rule next_tokens = parse
  | '\n' { new_line lexbuf ; next_tokens lexbuf }
  | '\t' | ' ' { next_tokens lexbuf }
  | "--" { comment lexbuf }
  | ident as s { id s lexbuf }
  | eof { EOF }
  | _ as c { raise (Error ("illegal character: " ^ String.make 1 c)) }

and comment = parse
  | '\n' { new_line lexbuf ; next_tokens lexbuf }
  | eof { raise (Error "unterminated comment") }
  | _ { comment lexbuf }

{

}

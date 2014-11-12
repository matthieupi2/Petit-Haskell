
(* Analyseur lexical pour Petit Haskell *)

{
  open Lexing
  open Ast
  open Parser

  exception Error of string

  let id s lb =
    let pos = lb.lex_start_p in
    if pos.pos_cnum = pos.pos_bol then
      IDENT0 s
    else
      IDENT1 s

  let unescape = function
    | "\\\\" -> '\\'
    | "\\\"" -> '"'
    | "\\n" -> '\n'
    | "\\t" -> '\t'
    | s -> s.[0]

  let string_of_list l =
    let s = Bytes.create (List.length l) in
    let rec aux n = function
      | [] -> Bytes.to_string s
      | c::q -> Bytes.set s n c ; aux (n+1) q in
    aux 0 l
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let integer = digit+
let car = ['\032'-'\126']#['\\' '"'] | '\\'['\\' '"' 'n' 't']
let ident = ['a'-'z'] (letter | digit | '_' | '\'')*

rule next_tokens last_token = parse
  | '\n'        { new_line lexbuf ; next_tokens last_token lexbuf }
  | '\t' | ' '  { next_tokens last_token lexbuf }
  | "--"        { comment last_token lexbuf }
  
  | ident as s  { id s lexbuf }

(* TODO gestion entier trop grand *)
  | integer as s        { CST (Cint (int_of_string s)) }
  | '\''(car as s)'\''  { CST (Cchar (unescape s)) }
  | '"'                 { CST (Cstr (string_of_list (string lexbuf))) }
  | "True"              { CST (Cbool true) }
  | "False"             { CST (Cbool false) }
  
  | '-' { match last_token with
    | Some (RP | RSB | RB | IDENT1 _ | CST _) -> MINUS
    | _ -> NEG }

  | eof     { EOF }
  | _ as c  { raise (Error ("illegal character: " ^ String.make 1 c)) }

and comment last_token = parse
  | '\n'  { new_line lexbuf ; next_tokens last_token lexbuf }
  | eof   { raise (Error "unterminated comment") }
  | _     { comment last_token lexbuf }

and string = parse
  | (car as s)     { unescape s::(string lexbuf) }
  | '"'     { [] }
  | eof     { raise (Error "unterminated string") }
  | _ as c  { raise (Error (
    "illegal character in a string: " ^ String.make 1 c)) }

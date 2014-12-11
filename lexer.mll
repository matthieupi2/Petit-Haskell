
(* Analyseur lexical pour Petit Haskell *)

{
  open Lexing
  open Ast
  open Parser
  open Error

  let kwd = Hashtbl.create 17
  let () = List.iter (fun (k,t) -> Hashtbl.add kwd k t)
    ["else", ELSE ; "if", IF ; "in", IN ; "let", LET ; "case", CASE ; "of", OF ;
     "then", THEN ; "return", RETURN ; "do", DO]

  let id s lb =
    try
      Hashtbl.find kwd s
    with Not_found -> (
      let pos = lb.lex_start_p in
      if pos.pos_cnum = pos.pos_bol then
        IDENT0 s
      else
        IDENT1 s )

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
let wrongEscape = '\\'[^'\\' '"' 'n' 't']
let ident = ['a'-'z'] (letter | digit | '_' | '\'')*

rule next_tokens = parse
  | '\n'        { new_line lexbuf ; next_tokens lexbuf }
  | '\t' | ' '  { next_tokens lexbuf }
  | "--"        { comment lexbuf }
  
  | ident as s  { id s lexbuf }

  | integer as s            { try
      CST (Cint (int_of_string s))
    with _ -> raise (CompilerError ("constant too large: " ^ s)) }
  | '\''(car as s)'\''      { CST (Cchar (unescape s)) }
  | '\'' (wrongEscape as s) { raise (LexerError
    ("'" ^ s ^ "' is not a escape character")) }
  | '\'' (_ as c) '\''             { raise (LexerError (
    "illegal character between ': " ^ Char.escaped c)) }
  | '\'' _ _                { raise (LexerError "missing \"'\"") }
  | '"'                     { STRING (Elist (string lexbuf)) }
  | "True"                  { CST (Cbool true) }
  | "False"                 { CST (Cbool false) }

  | '('   { LB }
  | ')'   { RB }
  | '['   { LSB }
  | ']'   { RSB }
  | '{'   { LCB }
  | '}'   { RCB }

  | "->"  { ARROW }
  | ';'   { SEMI }
  | ':'   { COLON }
  | ','   { COMMA }
  | '\\'  { LAMBDA }
  | '='   { ASSIGN }

  | '<'   { LT }
  | "<="  { LEQ }
  | '>'   { GT }
  | ">="  { GEQ }
  | "=="  { EQ }
  | "/="  { NEQ }

  | '-'   { MINUS }
  | '+'   { PLUS }
  | '*'   { TIMES }
  | "||"  { OR }
  | "&&"  { AND }

  | eof     { EOF }
  | _ as c  { raise (LexerError ("illegal character: " ^ Char.escaped c)) }

and comment = parse
  | '\n'  { new_line lexbuf ; next_tokens lexbuf }
  | eof   { raise (LexerError "unterminated comment") }
  | _     { comment lexbuf }

and string = parse
  | (car as s)        { Ecst (Cchar (unescape s))::(string lexbuf) }
  | wrongEscape as s  { raise (LexerError
    ("'" ^ s ^ "' is not a escape character")) }
  | '"'               { [] }
  | eof               { raise (LexerError "unterminated string") }
  | _ as c            { raise (LexerError (
    "illegal character in a string: " ^ Char.escaped c)) }

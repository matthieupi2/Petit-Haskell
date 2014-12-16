open Format
open Lexing

type location = Lexing.position * Lexing.position
type identError =
  | RedefPrimitive
  | RedefGlobal of location
  | RedefArg of location
  | RedefCase of location
  | Unbound

exception LexerError of string
exception ParserError of string
exception IdentError of string * location * identError
exception CompilerError of string

(* Pour ne pas avoir à localiser les expressions créées par le compilateur *)
let undef_pos = {pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}
let undef_loc = undef_pos, undef_pos

let print_loc (b, e) =
  eprintf "File \"%s\", line %d, characters %d-%d:@." b.pos_fname b.pos_lnum
      (b.pos_cnum - b.pos_bol + 1) (e.pos_cnum - b.pos_bol + 1)

let error_before_parsing lb e =
  let print_loc () =
    print_loc (lexeme_start_p lb, lexeme_end_p lb) in
  match e with
    | LexerError s -> print_loc () ; eprintf "lexical error: %s@." s ;
      exit 1
    | ParserError s -> print_loc () ; eprintf "syntax error: %s@." s ;
      exit 1
    | CompilerError s -> print_loc () ; eprintf "anomaly: %s@." s ;
      exit 2
    | e -> raise e

(* TODO revoir les messages d'erreurs *)
let error = function
  | IdentError (ident, loc, e) -> print_loc loc ; ( match e with
      | RedefPrimitive -> eprintf "%s is a primitive@." ident
      | RedefGlobal loc ->
        eprintf "%s is already a global variable defined at@." ident ;
        print_loc loc
      | RedefArg loc ->
        eprintf "%s is already a variable defined at@." ident ;
        print_loc loc
      | RedefCase loc ->
        eprintf "%s is already the name of the head of list defined at@." ident ;
        print_loc loc
      | Unbound -> eprintf "unbound variable \"%s\"@." ident ) ;
    exit 1
  | CompilerError s -> print_loc undef_loc ; eprintf "anomaly: %s@." s ;
    exit 2
  | e -> raise e


(* Gère l'ensemble des erreurs rencontrées par le compilateur *)

open Format
open Lexing

type location = Lexing.position * Lexing.position

(* Définition des différents types d'erreurs *)

type identError =
  | RedefPrimitive
  | RedefVar of location
  | RedefArg of location
  | RedefCase of location
  | Unbound
type typeError =
  | CantUnify
  | NotAFunction
  | FreeVar of string * string

exception LexerError of string
exception ParserError of string
exception IdentError of string * location * identError
exception TypeError of location * string * string * typeError
exception CompilerError of string

(* Pour ne pas avoir à localiser les expressions créées par le compilateur *)

let undef_pos = {pos_fname = ""; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}
let undef_loc = undef_pos, undef_pos

(* Impression des erreurs *)

let print_loc file (b, e) =
  eprintf "File \"%s\", line %d, characters %d-%d:@." file b.pos_lnum
      (b.pos_cnum - b.pos_bol + 1) (e.pos_cnum - b.pos_bol + 1)

let error_at_beginning e =
  eprintf "Anomaly: %s\n@." (Printexc.to_string e) ;
  exit 2

let error_before_parsing file lb e =
  let print_loc () =
    print_loc file (lexeme_start_p lb, lexeme_end_p lb) in
  match e with
    | LexerError s -> print_loc () ; eprintf "lexical error: %s@." s ;
      exit 1
    | ParserError s -> print_loc () ; eprintf "syntax error: %s@." s ;
      exit 1
    | CompilerError s -> print_loc () ; eprintf "anomaly: %s@." s ;
      exit 2
    | e -> raise e

let error file = function
  | IdentError (ident, loc, e) -> print_loc file loc ; ( match e with
      | RedefPrimitive ->
          eprintf "%s is already defined as a primitive@." ident
      | RedefVar loc ->
        eprintf "%s is already defined at@." ident ;
        print_loc file loc
      | RedefArg loc ->
        eprintf "%s is already a argument defined at@." ident ;
        print_loc file loc
      | RedefCase loc ->
        eprintf "%s is already the name of the head of list defined at@."
            ident ;
        print_loc file loc
      | Unbound -> eprintf "variable \"%s\" is undefined@." ident ) ;
    exit 1
  | TypeError (loc, t1, t2, e) -> print_loc file loc ; ( match e with
      | CantUnify ->
        eprintf "this expression has type %s but is expected to have type %s@."
            t1 t2
      | NotAFunction ->
        eprintf  "this expression is not a function, it cannot be applied@."
      | FreeVar (t3, t4) ->
        eprintf "this expression has type %s but is expected to have type %s.@."
            t1 t2 ;
        eprintf "%s occurs in %s@." t3 t4 ) ;
    exit 1
  | CompilerError s -> print_loc file undef_loc ; eprintf "anomaly: %s@." s ;
    exit 2
  | e -> raise e

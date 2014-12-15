type location = Lexing.position * Lexing.position

exception LexerError of string
exception ParserError of string
exception CompilerError of string

(* Pour ne pas avoir à localiser les expressions créées par le compilateur *)
let undef_pos = {Lexing.pos_fname = ""; Lexing.pos_lnum = 0 ;
    Lexing.pos_bol = 0 ; Lexing.pos_cnum = 0}
let undef_loc = undef_pos, undef_pos

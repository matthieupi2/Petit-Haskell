
(* Arbre de syntaxe abstraite de Petit Haskell *)

(* il ne semble pas a priori nécessaire de distinguer ident0 de ident1 *)
type ident = string

type binop =
  | Badd | Bsub | Bmul
  | Blt | Bleq | Bgt | Bgeq | Beq | Bneq
  | Band | Bor | Bcol

type constant =
  | Cint of int
  | Cchar of char
  | Cbool of bool

and ldef =  {def : def; loc : Lexing.position * Lexing.position}

and def = ident * ident list * lexpr

and lexpr = {expr : expr; loc : Lexing.position * Lexing.position}

and expr =
  | Eident of ident
  | Ecst of constant
  | Elist of lexpr list
  | Eappli of lexpr * lexpr list
  | Elambda of ident list * lexpr
  | Ebinop of binop * lexpr * lexpr
  | Eif of lexpr * lexpr * lexpr
  | Elet of ldef list * lexpr
  | Ecase of lexpr * lexpr * ident * ident * lexpr
  | Edo of lexpr list
  | Ereturn

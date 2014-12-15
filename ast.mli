
(* Arbre de syntaxe abstraite de Petit Haskell *)

(* il ne semble pas a priori nécessaire de distinguer ident0 de ident1 *)
type ident = string

and lident = {ident : ident; loci : Lexing.position * Lexing.position}

type binop =
  | Badd | Bsub | Bmul
  | Blt | Bleq | Bgt | Bgeq | Beq | Bneq
  | Band | Bor | Bcol

and lbinop = {binop : binop; locb : Lexing.position * Lexing.position}

type constant =
  | Cint of int
  | Cchar of char
  | Cbool of bool

and ldef =  {def : def; locd : Lexing.position * Lexing.position}

and def = lident * lident list * lexpr

and lexpr = {expr : expr; loce : Lexing.position * Lexing.position}

and expr =
  | Eident of lident
  | Ecst of constant
  | Elist of lexpr list
  | Eappli of lexpr * lexpr list
  | Elambda of lident list * lexpr
  | Ebinop of lbinop * lexpr * lexpr
  | Eif of lexpr * lexpr * lexpr
  | Elet of ldef list * lexpr
  | Ecase of lexpr * lexpr * lident * lident * lexpr
  | Edo of lexpr list
  | Ereturn

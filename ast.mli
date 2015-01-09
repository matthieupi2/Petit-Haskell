
(* Arbre de syntaxe abstraite de Petit Haskell *)

type ident = string

and lident = {ident : ident; loci : Error.location}

type binop =
  | Badd | Bsub | Bmul
  | Blt | Bleq | Bgt | Bgeq | Beq | Bneq
  | Band | Bor | Bcol

type constant =
  | Cint of int
  | Cchar of char
  | Cbool of bool

type def = lident * lident list * lexpr

and lexpr = {expr : expr; loce : Error.location}

and expr =
  | Eident of ident
  | Ecst of constant
  | Elist of lexpr list
  | Eappli of lexpr * lexpr list
  | Elambda of lident list * lexpr
  | Ebinop of binop * lexpr * lexpr
  | Eif of lexpr * lexpr * lexpr
  | Elet of def list * lexpr
  | Ecase of lexpr * lexpr * lident * lident * lexpr
  | Edo of lexpr list
  | Ereturn

type file = def list

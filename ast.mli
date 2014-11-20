
(* Arbre de syntaxe abstraite de Petit Haskell *)

type ident0 = string

type ident1 = string

type binop =
  | Badd | Bsub | Bmul
  | Blt | Bleq | Bgt | Bgeq | Beq | Bneq
  | Band | Bor | Bcol

type constant =
  | Cint of int
  | Cchar of char
  | Cstr of string
  | Cbool of bool

type def0 = ident0 * ident1 list * expr

and def = ident1 * ident1 list * expr

and expr =
  | Eident1 of ident1
  | Ecst of constant
  | Elist of expr list
  | Eappli of expr list
  | Elambda of ident1 list * expr
  | Ebinop of binop * expr * expr
  | Eif of expr * expr * expr
  | Elet of def list * expr
  | Ecase of expr * expr * ident1 * ident1 * expr
  | Edo of expr list
  | Ereturn

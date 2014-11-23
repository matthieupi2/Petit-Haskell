
(* Arbre de syntaxe abstraite de Petit Haskell *)

(* il ne semble pas a priori nécessaire de pouvoir distinguer ident0 de ident1 *)
type ident = string

type binop =
  | Badd | Bsub | Bmul
  | Blt | Bleq | Bgt | Bgeq | Beq | Bneq
  | Band | Bor | Bcol

type constant =
  | Cint of int
  | Cchar of char
  | Cstr of string
  | Cbool of bool

and def = ident * ident list * expr

(* TODO Ereturn <- Ecst Cunit *)
and expr =
  | Eident of ident
  | Ecst of constant
  | Elist of expr list
  | Eappli of expr * expr list
  | Elambda of ident list * expr
  | Ebinop of binop * expr * expr
  | Eif of expr * expr * expr
  | Elet of def list * expr
  | Ecase of expr * expr * ident * ident * expr
  | Edo of expr list
  | Ereturn

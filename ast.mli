
(* Arbre de syntaxe abstraite de Petit Haskell *)

type ident0 = Ident0 of string

type ident = string

type unop =
  | Uneg

type binop =
  | Badd | Bsub | Bmul
  | Blt | Bleq | Bgt | Bgeq | Beq | Bneq
  | Band | Bor | Bcol

type constant =
  | Cint of int
  | Cchar of char
  | Cstr of string
  | Cbool of bool

type def0 =
  ident0 * ident list * expr

and def =
  ident * ident list * expr

and expr =
  | Ecst of constant
  | Elist of expr list
  | Ecurr of expr list
  | Elambda of ident list * expr
  | Eif of expr * expr * expr
  | Elet of def list * expr
  | Ecase of expr * expr * ident * ident * expr
  | Edo of expr list
  | Ereturn

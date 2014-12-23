
(* Arbre de syntaxe abstraite de Petit Haskell *)

type ident = string

and lident = {ident : ident; loci : Error.location}

type binop =
  | Badd | Bsub | Bmul
  | Blt | Bleq | Bgt | Bgeq | Beq | Bneq
  | Band | Bor | Bcol

(* TODO lbinop ? 
and lbinop = {ddbinop : binop; locb : Lexing.position * Lexing.position} *)

type constant =
  | Cint of int
  | Cchar of char
  | Cbool of bool

(* TODO and ldef =  {def : def; locd : Lexing.position * Lexing.position} *)

type def = lident * lident list * lexpr

(* TODO desc *)
and lexpr = {expr : expr; loce : Error.location}

and expr =
  | Eident of ident
  | Ecst of constant
  | Elist of lexpr list
  | Eappli of lexpr * lexpr list    (* TODO lexpr list ? *)
  | Elambda of lident list * lexpr  (* TODO expr ? *)
  | Ebinop of binop * lexpr * lexpr
  | Eif of lexpr * lexpr * lexpr
  | Elet of def list * lexpr
  | Ecase of lexpr * lexpr * lident * lident * lexpr
  | Edo of lexpr list
  | Ereturn

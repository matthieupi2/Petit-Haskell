
(* Ast décurrifié et certifié sans répétition erronée d'identifiants *)

open Error
open Ast

type ident = string

and lident = {ident : ident; loci : Lexing.position * Lexing.position}

type binop =
  | Badd | Bsub | Bmul
  | Blt | Bleq | Bgt | Bgeq | Beq | Bneq
  | Band | Bor | Bcol

type constant =
  | Cint of int
  | Cchar of char
  | Cbool of bool

type def = lident * lexpr

and lexpr = {expr : expr; loce : Lexing.position * Lexing.position}

and expr =
  | Eident of ident
  | Ecst of constant
  | Elist of lexpr list
  | Eappli of lexpr * lexpr
  | Elambda of lident * lexpr
  | Ebinop of binop * lexpr * lexpr
  | Eif of lexpr * lexpr * lexpr
  | Elet of def * lexpr
  | Ecase of lexpr * lexpr * lident * lident * lexpr
  | Edo of lexpr list   (* TODO couple d'expressions ? *)
  | Ereturn


module S = Set.Make(struct type t = Ast.ident
    let compare = Pervasives.compare end)
module M = Map.Make(struct type t = Ast.ident
    let compare = Pervasives.compare end)

let are_different l =
  assert false

let rec uncurry_lambda args body =
  assert false

let rec uncurry_expr e =
  assert false

let uncurry ast =
  let primitives = S.of_list ["div" ; "rem" ; "putChar" ; "error" ] in
  let rec aux env = function
    | []  -> []
    | ({ident=name; loci=loc},_,_)::q when S.mem name primitives ->
        raise (IdentError (name, loc, RedefPrimitive))
    | ({ident=name; loci=loc}, args, body)::q -> try
        let first_def = M.find name env in
        raise (IdentError (name, loc, RedefGlobal first_def))
      with Not_found ->
        are_different args ;
        (name, uncurry_lambda args (uncurry_expr body))::
            (aux (M.add name loc env) q) in
  aux M.empty ast

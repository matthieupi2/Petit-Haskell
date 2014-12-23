
(* AST typé de Petit-Haskell *)

open Ast
open UncurriedAst


type typ =
  | Tbool
  | Tchar
  | Tint
  | Tio
  | Tarrow of typ * typ
  | Tlist of typ
  | Tvar of tvar

and tvar =
  { id : int; mutable def : typ option }

module V = struct
  type t = tvar
  let compare v1 v2 = Pervasives.compare v1.id v2.id
  let equal v1 v2 = v1.id = v2.id
  let create = let r = ref 0 in fun () -> incr r; { id = !r; def = None }
end


type texpr = { tdesc : tdesc; typ : typ }

(* décurrifier *)
and tdesc =
  | Tident of ident
  | Tcst of constant
  | Tlist of texpr list
  | Tappli of texpr * texpr
  | Tlambda of ident list * texpr
  | Tbinop of binop * texpr * texpr
  | Tif of texpr * texpr * texpr
  | Tlet of udef list * texpr
  | Tcase of texpr * texpr * ident * ident * texpr
  | Tdo of texpr list
  | Treturn

(* On suit le TD4 -> algorithme W avec unification destrucive *)

let rec head = function
  | Tvar {def = Some t} -> head t
  | t -> t

let rec canon t = match head t with
  | Tarrow (t1, t2) -> Tarrow (canon t1, canon t2)
  | Tlist t -> Tlist (canon t)
  | t -> t

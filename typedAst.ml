
(* AST typé de Petit-Haskell *)

(* TODO sauter plus de lignes *)
open Error
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

(* TODO Inutile ? *)
let rec canon t = match head t with
  | Tarrow (t1, t2) -> Tarrow (canon t1, canon t2)
  | Tlist t -> Tlist (canon t)
  | t -> t

let string_of_typ t =
  let var_names = Hashtbl.create 17 in
  let r = ref 0 in
  let fresh_name id =
    let s = String.make 1 (char_of_int (int_of_char 'a' + !r mod 26)) in
    let s' =
      if !r >= 26 then
        string_of_int (!r / 26)
      else
        "" in
    let s'' = "'" ^ s ^ s' in
    Hashtbl.add var_names id s'' ; 
    incr r ;
    s'' in
  let rec aux = function
    | Tbool -> "Bool"
    | Tchar -> "Char"
    | Tint -> "Integer"
    | Tio -> "IO ()"
    | Tarrow (t1, t2) -> let s1 = aux t1 in
      let s2 = aux t2 in
      s1 ^ " -> " ^ s2
    | Tlist t -> "[" ^ aux t ^ "]"
    | Tvar {def = Some t} -> aux t
    | Tvar {id = id} -> try
        Hashtbl.find var_names id
      with Not_found ->
        fresh_name id in
  aux t

let type_error loc t1 t2 =
  raise (TypeError (loc, string_of_typ t1, string_of_typ t2))


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

type ltexpr = { texpr : texpr; typ : typ; loct : location }

(* décurrifier *)
and texpr =
  | Tident of ident
  | Tcst of constant
  | Tlist of ltexpr list
  | Tappli of ltexpr * ltexpr
  | Tlambda of ident list * ltexpr
  | Tbinop of binop * ltexpr * ltexpr
  | Tif of ltexpr * ltexpr * ltexpr
  | Tlet of udef list * ltexpr
  | Tcase of ltexpr * ltexpr * ident * ident * ltexpr
  | Tdo of ltexpr list
  | Treturn

(* On suit le TD4 -> algorithme W avec unification destrucive *)

let rec head = function
  | Tvar { def = Some t } -> t
  | t -> t

(* TODO Inutile ? *)
let rec canon t = match head t with
  | Tarrow (t1, t2) -> Tarrow (canon t1, canon t2)
  | Tlist t -> Tlist (canon t)
  | t -> t

let string_of_typ t var_names =
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

(* TODO diversifier les erreurs *)
let type_error loc t1 t2 =
  let var_names = Hashtbl.create 17 in
  let s1 = string_of_typ t1 var_names in
  let s2 = string_of_typ t2 var_names in
  raise (TypeError (loc, s1, s2))

let rec occur v t = match head t with
  | Tvar v' -> V.equal v v'
  | Tarrow (t1, t2) -> occur v t1 || occur v t2
  | Tlist t -> occur v t
  | _ -> false

exception Cant_unify
let rec unify t1 t2 = match head t1, head t2 with
  | Tbool, Tbool | Tchar, Tchar | Tint, Tint | Tio, Tio -> ()
  | Tarrow (t1, t1'), Tarrow (t2, t2') -> ( try
      unify t1 t2 ; unify t1' t2'
    with Cant_unify ->    (* on simplifie avant de renvoyer l'erreur *)
      unify t1' t2' ; raise Cant_unify )
  | Tlist t1, Tlist t2 -> unify t1 t2
  | Tvar v, t | t, Tvar v -> if occur v t then
      raise Cant_unify
    else
      v.def <- Some t
  | _ -> raise Cant_unify

module Vset = Set.Make(V)

let rec fvars t = match head t with
  | Tvar v -> Vset.singleton v
  | Tarrow (t1, t2) -> Vset.union (fvars t1) (fvars t2)
  | Tlist t -> fvars t
  | _ -> Vset.empty

type schema = { vars : Vset.t; typ : typ }

module Smap = Map.Make(String)

type env = { bindings : schema Smap.t; fvars : Vset.t }

(* TODO inutile ? *)
let empty = { bindings = Smap.empty; fvars = Vset.empty }

let maj env =
  let fvars = Vset.fold (fun v fvars' -> Vset.union (fvars (Tvar v)) fvars')
      env.fvars Vset.empty in
  { bindings = env.bindings; fvars = fvars }

let add x t env =
  let env = maj env in
  let schema_x = { vars = Vset.empty; typ = t } in
  { bindings = Smap.add x schema_x env.bindings;
    fvars = Vset.union (fvars t) env.fvars }

let add_gen x t env =
  let env = maj env in
  let schema_x = { vars = Vset.diff (fvars t) env.fvars; typ = t } in
  { bindings = Smap.add x schema_x env.bindings;
    fvars = env.fvars }

module Vmap = Map.Make(V)

let find x env =
  let schema_x = Smap.find x env in
  let new_vars = Vset.fold (fun v new_vars -> Vmap.add v (V.create ()) new_vars)
      schema_x.vars Vmap.empty in
  let rec aux t = match head t with
    | Tvar v as t -> ( try
        Tvar (Vmap.find v new_vars)
      with Not_found ->
        t )
    | Tarrow (t1, t2) -> Tarrow (aux t1, aux t2)
    | Tlist t -> Tlist (aux t)
    | t -> t in
  aux schema_x.typ

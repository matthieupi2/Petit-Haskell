
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

type tdef = ident * ttexpr

and ttexpr = { texpr : texpr; typ : typ }

(* décurrifier *)
and texpr =
  | Tident of ident
  | Tcst of constant
  | Tlist of ttexpr list
  | Tappli of ttexpr * ttexpr
  | Tlambda of ident list * ttexpr
  | Tbinop of binop * ttexpr * ttexpr
  | Tif of ttexpr * ttexpr * ttexpr
  | Tlet of tdef list * ttexpr
  | Tcase of ttexpr * ttexpr * ident * ident * ttexpr
  | Tdo of ttexpr list
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

(* TODO parenthésage *)
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

type unificationError =
  | CantUnify
  | NotAFunction
  | FreeVar of typ * typ

let type_error loc t1 t2 e =
  let var_names = Hashtbl.create 17 in
  let s1 = string_of_typ t1 var_names in
  let s2 = string_of_typ t2 var_names in
  let e = match e with
    | CantUnify -> Error.CantUnify
    | NotAFunction -> Error.NotAFunction
    | FreeVar (t3, t4) -> let s3 = (string_of_typ t3 var_names) in
      let s4 = (string_of_typ t4 var_names) in
      Error.FreeVar (s3, s4) in
  raise (TypeError (loc, s1, s2, e))

let rec occur v t = match head t with
  | Tvar v' -> V.equal v v'
  | Tarrow (t1, t2) -> occur v t1 || occur v t2
  | Tlist t -> occur v t
  | _ -> false

exception UnificationFailure of unificationError

let rec unify t1 t2 = match head t1, head t2 with
  | Tbool, Tbool | Tchar, Tchar | Tint, Tint | Tio, Tio -> ()
  | Tarrow (t1, t1'), Tarrow (t2, t2') -> unify t1 t2 ; unify t1' t2'
  | Tlist t1, Tlist t2 -> unify t1 t2
  | Tvar v, t | t, Tvar v -> if occur v t then
      raise (UnificationFailure (FreeVar (Tvar v, t)))
    else
      v.def <- Some t
  | _ -> raise (UnificationFailure CantUnify)

module Vset = Set.Make(V)

let rec fvars t = match head t with
  | Tvar v -> Vset.singleton v
  | Tarrow (t1, t2) -> Vset.union (fvars t1) (fvars t2)
  | Tlist t -> fvars t
  | _ -> Vset.empty

type schema = { vars : Vset.t; styp : typ }

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
  let schema_x = { vars = Vset.empty; styp = t } in
  { bindings = Smap.add x schema_x env.bindings;
    fvars = Vset.union (fvars t) env.fvars }

let add_gen x t env =
  let env = maj env in
  let schema_x = { vars = Vset.diff (fvars t) env.fvars; styp = t } in
  { bindings = Smap.add x schema_x env.bindings;
    fvars = env.fvars }

module Vmap = Map.Make(V)

let find x env =
  let schema_x = Smap.find x env.bindings in
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
  aux schema_x.styp

let rec w env e = match e.uexpr with
  | Uident x -> ( try
      { texpr = Tident x; typ = find x env }
    with Not_found ->
      raise (IdentError (x, e.locu, Unbound)) )
  | Ucst c -> let typ = match c with
      | Cint _ -> Tint
      | Cchar _ -> Tchar
      | Cbool _ -> Tbool in
    { texpr = Tcst c; typ = typ }
  | Ulist [] -> { texpr = Tlist []; typ = Tlist (Tvar (V.create ())) }
  | Ulist (e::l) -> let e = w env e in
    let aux ue =
      let te = w env ue in
      try
        unify e.typ te.typ ;
        te
      with UnificationFailure error ->
        type_error ue.locu te.typ e.typ error in
    { texpr = Tlist (e::List.map aux l); typ = Tlist e.typ }
  | Uappli (ue1, ue2) -> ( let te1 = w env ue1 in
    let te2 = w env ue2 in
    let v = Tvar (V.create ()) in
    try
      unify te1.typ (Tarrow (te2.typ, v)) ;
      { texpr = Tappli (te1, te2) ; typ = v }
    with UnificationFailure e -> match head te1.typ with
      | Tarrow (t, _) -> type_error ue2.locu te2.typ t e (* TODO à vérifier *)
      | _ -> type_error ue1.locu te1.typ (Tarrow (te2.typ, v)) NotAFunction )
  | Ulambda (args, body) -> let rec aux env = function
      | [] -> let tbody = w env body in
        (tbody, tbody.typ)
      | x::q -> let v = Tvar (V.create ()) in
        let (tbody, tlambda_typ) = aux (add x v env) q in
        (tbody, Tarrow (v, tlambda_typ)) in
    let (tbody, tlambda_typ) = aux env args in
    { texpr = Tlambda (args, tbody); typ = tlambda_typ }
  | Ubinop (op, ue1, ue2) -> ( let (t1, t2, t3) = match op with
      | Badd | Bsub | Bmul -> (Tint, Tint, Tint)
      | Blt | Bleq | Bgt | Bgeq | Beq | Bneq -> (Tint, Tint, Tbool)
      | Band | Bor -> (Tbool, Tbool, Tbool)
      | Bcol -> let v = Tvar (V.create ()) in (v, Tlist v, Tlist v) in
    let te1 = w env ue1 in
    try
      unify t1 te1.typ ;
      let te2 = w env ue2 in
      try
        unify t2 te2.typ ;
        { texpr = Tbinop (op, te1, te2); typ = t3 }
      with UnificationFailure e -> type_error ue2.locu te2.typ t2 e
    with UnificationFailure e -> type_error ue1.locu te1.typ t1 e )
  | Uif (ucdt, ue1, ue2) -> ( let tcdt = w env ucdt in
    try
      unify tcdt.typ Tbool ;
      let te1 = w env ue1 in
      let te2 = w env ue2 in
      try
        unify te1.typ te2.typ ;
        { texpr = Tif (tcdt, te1, te2); typ = te1.typ }
      with UnificationFailure e -> type_error ue2.locu te2.typ te1.typ e
    with UnificationFailure e -> type_error ucdt.locu tcdt.typ Tbool e )
  | Ulet (udefs, ue) -> let new_vars =
      List.map (fun udef -> (udef, Tvar (V.create ()))) udefs in
    let env' =
      List.fold_left (fun env' ((x, _), t) -> add x t env') env new_vars in
    let unify_def ((x, ue), t) =
      let te = w env' ue in
      try
        unify te.typ t ;
        (x, te)
      with UnificationFailure e -> type_error ue.locu te.typ t e in
    let tdefs = List.map unify_def new_vars in
    let env_gen =
      List.fold_left (fun env (x, te) -> add_gen x te.typ env) env tdefs in
    let te = w env_gen ue in 
    { texpr = Tlet (tdefs, te); typ = te.typ }
  | Ucase _ -> assert false
  | Udo _ -> assert false
  | Ureturn -> assert false

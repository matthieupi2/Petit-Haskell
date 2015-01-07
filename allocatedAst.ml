(* phase 3 : allocation des variables *)

open Ast
open ClosureAst

type var =
  | Vglobal of string
  | Vlocal  of int

type cdecl =
  | CDef of ident * cexpr * int
  | CFun of ident * int * cexpr * int
  | CCodeglacon of ident * int * cexpr * int
  | CMain of cexpr * int

and cdef = int * cexpr

and cexpr =
  | CVar of var
  | CConst of int   (* ascii pour CChar, 0 pour False, sinon True pour CBool *)
  | CList of cexpr list
  | CAppli of cexpr * cexpr
  | CClos of ident * var list
  | CBinop of binop * cexpr * cexpr
  | CIf of cexpr * cexpr * cexpr
  | CLet of cdef list * cexpr
  | CCase of cexpr * cexpr * int * int * cexpr
  | CDo of cexpr list
  | CReturn
  | CGlacon of ident * var list


module Smap = Map.Make(String)

type local_env = int Smap.t

let find_var env x =
  try
    Vlocal (Smap.find x env)
  with Not_found ->
    Vglobal x

let rec alloc_expr env next = function
  | Fident x -> CVar (find_var env x), next
  | Fcst c -> let i = match c with 
      | Cint i -> i
      | Cchar c -> int_of_char c
      | Cbool b -> if b then 1 else 0 in
    CConst i, next
  | Flist l -> assert false
  | Fappli (e1, e2) ->
    let a1, n = alloc_expr env next e1 in
    let a2, _ = alloc_expr env next e2 in
    CAppli (a1, a2), n
  | Fclos (i, l) ->
    let l = List.map (find_var env) l in
    CClos (i, l), next
  | Fbinop (o, e1, e2) ->
    let a1, n1 = alloc_expr env next e1 in
    let a2, n2 = alloc_expr env next e2 in
    CBinop (o, a1, a2), max n1 n2
  | Fif (e1, e2, e3) ->
    let a1, n1 = alloc_expr env next e1 in
    let a2, n2 = alloc_expr env next e2 in
    let a3, n3 = alloc_expr env next e3 in
    CIf (a1, a2, a3), max (max n1 n2) n3
  | Flet (lfdef, e) ->
    let aux (adr, env) (x, _) =
      (adr - 4, Smap.add x adr env) in
    let (adr, env') = List.fold_left aux (-next - 4, env) lfdef in
    let next' = -adr - 4 in
    let rec aux adr = function
      | [] -> []
      | (_, e)::q -> let a, _ = alloc_expr env' next' e in
        (adr, a)::(aux (adr - 4) q) in
    let lcdef = aux (-next - 4) lfdef in
    let a, n = alloc_expr env' next' e in
    CLet (lcdef, a), n
  | Fcase (e1, e2, i1, i2, e3) ->
     let a1, n1 = alloc_expr env next e1 in
     let a2, n2 = alloc_expr env next e2 in
     let env1 = Smap.add i2 (-next-8) (Smap.add i1 (-next-4) env) in
     let a3, n3 = alloc_expr env1 (next+8) e3 in
     CCase (a1, a2, -next-4, -next-8, a3), max (max n1 n2) n3
  | Fdo l -> let aux (l1, n1) e =
      let a2, n2 = alloc_expr env next e in
      (a2::l1, max n1 n2) in
    let ll, nn = List.fold_left aux ([], next) l in
    CDo ll, nn
  | Freturn -> CReturn, next
  | Fglacon (i, l) ->
    let l1 = List.map (find_var env) l in
    CGlacon (i, l1), next
(* TODO fold_left *)
let rec ajout_liste_env env next = function
  | [] -> env, next
  | a::q -> let env1 = Smap.add a (-next-4) env in
            ajout_liste_env env1 (next+4) q

let alloc_def = function
  | Fdef (i, e) ->
    let a, n = alloc_expr Smap.empty 0 e in
    CDef (i, a, n)
  | Ffun (i1, l, i2, e) ->
    let env1, n1 = ajout_liste_env (Smap.singleton i2 (-4)) 4 l in
    let a, n = alloc_expr env1 n1 e in
    CFun (i1, List.length l, a, n)
  | Fcodeglacon (i, l, e) ->
    let env1, n1 = ajout_liste_env Smap.empty 0 l in
    let a, n = alloc_expr env1 n1 e in
    CCodeglacon (i, List.length l, a, n)
  | Fmain e ->
    let a, n = alloc_expr Smap.empty 0 e in
    CMain (a, n)

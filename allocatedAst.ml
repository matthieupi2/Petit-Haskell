
(* Nouvel ast avec :
  * l'explicitation de l'accès à chaque variable,
  * la taille nécessaire dans la pile pour stocker les variables locales,
  * la fusion des constructeurs Fcodeglacon et Ffun,
  * les constantes reçoivent une valeur entière *)

(* Les variables sont stockées de la même manière que vue en cours (mis à part
 * pour le choix des registres :
  * les variables globales dans .data étiquetées à leur nom,
  * les variables locales dans la pile à partir de l'adress $fp - 4,
  * les variables de la fermeture dans le tas, dans le bloc correspondant pointé
    par $t1 et à partir de $t1 + 8,
  * l'argument est contenu dans $t0 *)

open Ast
open ClosureAst



type var =
  | Vglobale of string
  | Vlocale  of int
  | Vclos of int
  | Varg

type cdecl =
  | CDef of ident * cexpr * int
  | CFun of ident * cexpr * int
  | CMain of cexpr * int

and cdef = int * cexpr

and cexpr =
  | CVar of var
  | CCst of int   (* valeur ascii pour CChar, 0 pour False, 1 pour True *)
  | CEmptylist
  | CAppli of cexpr * cexpr
  | CClos of ident * var list
  | CBinop of binop * cexpr * cexpr
  | CIf of cexpr * cexpr * cexpr
  | CLet of cdef list * cexpr
  | CCase of cexpr * cexpr * int * int * cexpr
  | CDo of cexpr list
  | CReturn
  | CGlacon of cexpr

(* Création du nouvel ast *)

module Smap = Map.Make(String)

type local_env = var Smap.t

(* next correspond au nombre d'octets à allouer sur la pile pour contenir les
 * variables locales de l'expression *)
let rec alloc_expr (env : local_env) next = function
  | Fident x -> CVar (Smap.find x env), next
  | Fcst c -> let i = match c with
      | Cint i -> i
      | Cchar c -> int_of_char c
      | Cbool b -> if b then 1 else 0 in
    CCst i, next
  | Femptylist -> CEmptylist, next
  | Fappli (e1, e2) ->
    let a1, n = alloc_expr env next e1 in
    let a2, _ = alloc_expr env next e2 in
    CAppli (a1, a2), n
  | Fclos (i, l) ->
    let l = List.map (fun vclos -> Smap.find vclos env) l in
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
      (adr - 4, Smap.add x (Vlocale adr) env) in
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
     let env1 = Smap.add i2 (Vlocale (-next-8))
        (Smap.add i1 (Vlocale (-next-4)) env) in
     let a3, n3 = alloc_expr env1 (next+8) e3 in
     CCase (a1, a2, -next-4, -next-8, a3), max (max n1 n2) n3
  | Fdo l -> let aux e (l1, n1) =
      let a2, n2 = alloc_expr env next e in
      (a2::l1, max n1 n2) in
    let ll, nn = List.fold_right aux l ([], next) in
    CDo ll, nn
  | Freturn -> CReturn, next
  | Fglacon fe ->
    CGlacon (fst (alloc_expr env next fe)), next

let rec alloc_def env = function
  | Fdef (i, e) ->
    let a, n = alloc_expr env 0 e in
    CDef (i, a, n)
  | Ffun (i, cloture, x, e) ->
    alloc_def (Smap.add x Varg env) (Fcodeglacon (i, cloture, e))
  | Fcodeglacon (i, cloture, e) ->
    let rec ajout_cloture adr = function
      | [] -> env
      | vclos::q -> let env' = ajout_cloture (adr + 4) q in
        Smap.add vclos (Vclos adr) env' in
    let env' = ajout_cloture 8 cloture in
    let a, n = alloc_expr env' 0 e in
    CFun (i, a, n)
  | Fmain e ->
    let a, n = alloc_expr env 0 e in
    CMain (a, n)

let alloc p prim_names =
  let rec create_genv = function
    | [] -> List.fold_left (fun env prim -> Smap.add prim (Vglobale prim) env)
        Smap.empty prim_names
    | t::q -> let env = create_genv q in
      let i = match t with
        | Fmain _ -> "main"
        | Fdef (i, _)
        | Ffun (i, _, _, _)
        | Fcodeglacon (i, _, _) -> i in
      Smap.add i (Vglobale i) env in
  let env = create_genv p in
  List.map (alloc_def env) p


open Format
open Ast
open FreeVarsAst
open Mips

(* phase 2 : construction explicite des fermetures  et des glacons *)

let numfun = ref 0
let numglacon = ref 0

type fdecl =
  | Fdef of ident * ident list * fexpr
  | Ffun of ident * ident list * ident * fexpr
  | Fcodeglacon of ident * ident list * fexpr
  | Fmain of ident list * fexpr

and fdef = ident * fexpr

and fexpr =
  | Fident of ident
  | Fcst of constant
  | Flist of fexpr list
  | Fappli of fexpr * fexpr
  | Fclos of ident * ident list
  | Fbinop of binop * fexpr * fexpr
  | Fif of fexpr * fexpr * fexpr
  | Flet of fdef list * fexpr
  | Fcase of fexpr * fexpr * ident * ident * fexpr
  | Fdo of fexpr list
  | Freturn
  | Fglacon of ident * ident list


let rec ferm_expr = function
  | {vexpr = Vident i} -> Fident i, []
  | {vexpr = Vcst c} -> Fcst c, []
  | {vexpr = Vlist l1} -> let aux (l3, lf) vv =
      let f1, f2 = ferm_expr vv in
      (f1::l3, f2@lf) in
    let l2, lf1 = List.fold_left aux ([], []) l1 in
      Flist l2, lf1
  | {vexpr = Vappli (v1, v2); var_libres = l} ->  
     numglacon := !numglacon + 1;
     let s = ("_glacon_" ^ (string_of_int (!numglacon))) in
     let f1d, f1f = ferm_expr v1 in
     let f2d, f2f = ferm_expr v2 in
     Fappli (f1d, Fglacon (s, v2.var_libres)),
     (Fcodeglacon (s, v2.var_libres, f2d))::(f1f@f2f)
  | {vexpr = Vlambda(i, vv); var_libres = l} -> 
    numfun := !numfun + 1;
    let f1, f2 = ferm_expr vv in
    let s = ("_fun_" ^ (string_of_int (!numfun))) in
    Fclos (s, l), Ffun (s, l, i, f1)::f2
  | {vexpr = Vbinop (o, v1, v2)} -> 
    let f1d, f1f = ferm_expr v1 in
    let f2d, f2f = ferm_expr v2 in
    Fbinop (o, f1d, f2d), f1f@f2f
  | {vexpr = Vif (v1, v2, v3)} -> 
     let f1d, f1f = ferm_expr v1 in
     let f2d, f2f = ferm_expr v2 in
     let f3d, f3f = ferm_expr v3 in
     Fif (f1d, f2d, f3d), f1f@f2f@f3f
  | {vexpr = Vlet (lvdef, v)} -> let aux (lfdef, lff) (x, v) =
      numglacon := !numglacon + 1;
      let s = ("_glacon_" ^ (string_of_int (!numglacon))) in
      let fd, ff = ferm_expr v in
      ((x, Fglacon (s, v.var_libres))::lfdef,
      Fcodeglacon (s, v.var_libres, fd)::ff@lff) in 
    let fd, ff = ferm_expr v in
    let lfdef, lff = List.fold_left aux ([], ff) lvdef in
    Flet (lfdef, fd), lff
  | {vexpr = Vcase (v1, v2, i1, i2, v3)} -> 
    let f1d, f1f = ferm_expr v1 in
    let f2d, f2f = ferm_expr v2 in (* TODO? glaçon *)
    let f3d, f3f = ferm_expr v3 in
    Fcase (f1d, f2d, i1, i2, f3d), f1f@f2f@f3f
  | {vexpr = Vdo l1} -> let aux (l3, lf) vv =
      let f1, f2 = ferm_expr vv in
      (f1::l3, f2@lf) in
    let l2, lf1 = List.fold_left aux ([], []) l1 in
    Fdo l2, lf1
  | {vexpr = Vreturn} -> Freturn, []


let ferm_def = function (* TODO? pas totues des fonctions *)
  | ("main" , v) -> let fmain, ffun = ferm_expr v in
    (Fmain (v.var_libres, fmain))::ffun
  | (i, v) -> let fmain, ffun = ferm_expr v in
    (Fdef (i, v.var_libres, fmain))::ffun

let ferm p = List.concat (List.map ferm_def p)

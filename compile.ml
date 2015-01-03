
open Format
open Ast
open TypedAst
open Mips


(* phase 1 : on calcule les variables libres *)

exception CompileError of string

type vdef = Vdef of ident * vvexpr

and vvexpr = {vexpr : vexpr; var_libres : ident list}

and vexpr =
  | Vident of ident
  | Vcst of constant
  | Vlist of vvexpr list
  | Vappli of vvexpr * vvexpr
  | Vlambda of ident * vvexpr
  | Vbinop of binop * vvexpr * vvexpr
  | Vif of vvexpr * vvexpr * vvexpr
  | Vlet of ident * vvexpr * vvexpr
  | Vcase of vvexpr * vvexpr * ident * ident * vvexpr
  | Vdo of vvexpr list
  | Vreturn


let rec union_list l1 l2 = match l1 with
  | [] -> l2
  | a::q -> if List.mem a l2 then union_list q l2 else union_list q (a::l2)

let rec var_libre_expr = function
  | Tident i -> {vexpr = Vident i; var_libres = i::[]}

  | Tcst c -> {vexpr = Vcst c; var_libres = []}
  
  | Tlist l -> List.fold_left (fun lv x -> let v = var_libre_expr x.texpr in
                                   (match lv.vexpr with
				       | Vlist l1 -> {vexpr = Vlist (v::l1); var_libres = union_list v.var_libres lv.var_libres}
                                       | _ -> raise (CompileError "1") ) )
                              {vexpr = Vlist []; var_libres = []} l

  | Tappli (t1, t2) -> 
      let v1 = var_libre_expr t1.texpr in
      let v2 = var_libre_expr t2.texpr in
      {vexpr = Vappli (v1, v2); var_libres = union_list v1.var_libres v2.var_libres}
  
  | Tlambda ([], t) -> var_libre_expr t.texpr
  | Tlambda ((a::q), t) -> let v = var_libre_expr (Tlambda(q, t)) in
                           {vexpr = Vlambda (a, v); var_libres = (List.filter (fun x -> x<>a) v.var_libres)}
  
  | Tbinop (o, t1, t2) -> 
      let v1 = var_libre_expr t1.texpr in
      let v2 = var_libre_expr t2.texpr in
      {vexpr = Vbinop (o, v1, v2); var_libres = union_list v1.var_libres v2.var_libres}

  | Tif (t1, t2, t3) ->
      let v1 = var_libre_expr t1.texpr in
      let v2 = var_libre_expr t2.texpr in
      let v3 = var_libre_expr t3.texpr in
      {vexpr = Vif (v1, v2, v3); var_libres = union_list v1.var_libres (union_list v2.var_libres v3.var_libres)}
  
  | Tlet ([], t) -> var_libre_expr t.texpr
  | Tlet (((a, t1)::q), t2) -> let v1 = var_libre_expr t1.texpr in
                              let v2 = var_libre_expr (Tlet(q, t2)) in
                              {vexpr = Vlet (a, v1, v2); var_libres = (List.filter (fun x -> x<>a) (union_list v1.var_libres v2.var_libres))}
  
  | Tcase (t1, t2, i1, i2, t3) ->
      let v1 = var_libre_expr t1.texpr in
      let v2 = var_libre_expr t2.texpr in
      let v3 = var_libre_expr t3.texpr in
      let v3var = List.filter (fun x -> (x<>i1 && x<>i2)) v3.var_libres in
      {vexpr = Vcase (v1, v2, i1, i2, v3); var_libres = union_list v1.var_libres (union_list v2.var_libres v3var)}

  | Tdo l -> List.fold_left (fun lv x -> let v = var_libre_expr x.texpr in
                                   (match lv.vexpr with
				       | Vdo l1 -> {vexpr = Vdo (v::l1); var_libres = union_list v.var_libres lv.var_libres}
                                       | _ -> raise (CompileError "2") ) )
                              {vexpr = Vdo []; var_libres = []} l
  
  | Treturn -> {vexpr = Vreturn; var_libres = []}
  
let var_libre_def (i, tt) = Vdef (i, var_libre_expr tt.texpr)

let var_libre = List.map var_libre_def

(************************************************************************************)
(* phase 2 : construction explicite des fermetures  et des glacons *)

let numfun = ref 0

type fdef =
  | Fdef of ident * ident list * fexpr
  | Ffun of ident * ident list * ident * fexpr
  | Fcodeglacon of ident * ident list * fexpr
  | Fmain of fexpr

and fexpr =
  | Fident of ident
  | Fcst of constant
  | Flist of fexpr list
  | Fappli of fexpr * fexpr
  | Fclos of ident * ident list
  | Fbinop of binop * fexpr * fexpr
  | Fif of fexpr * fexpr * fexpr
  | Flet of ident * fexpr * fexpr
  | Fcase of fexpr * fexpr * ident * ident * fexpr
  | Fdo of fexpr list
  | Freturn
  | Fglacon of ident


let rec ferm_expr = function
  | {vexpr = Vident i; var_libres = l} -> Fident i, []

  | {vexpr = Vcst c; var_libres = l} -> Fcst c, []

  | {vexpr = Vlist l1; var_libres = l} ->
       let l2, lf1 = List.fold_left (fun (l3,lf) vv -> let f1, f2 = ferm_expr vv in (f1::l3, f2@lf))
                                    ([], [])
                                    l1
                     in
       Flist l2, lf1

  | {vexpr = Vappli (v1, v2); var_libres = l} ->  
       numfun := !numfun + 1;
       let s = ("_fun" ^ (string_of_int (!numfun))) in
       let f1d, f1f = ferm_expr v1 in
       let f2d, f2f = ferm_expr v2 in
       Fappli (f1d, Fglacon s), (Fcodeglacon (s, v2.var_libres, f2d))::(f1f@f2f)

  | {vexpr = Vlambda(i, vv); var_libres = l} -> 
    numfun := !numfun + 1;
    let f1, f2 = ferm_expr vv in
    let s = ("_fun" ^ (string_of_int (!numfun))) in
    Fclos (s, l), Ffun (s, l, i, f1)::f2

  | {vexpr = Vbinop (o, v1, v2); var_libres = l} -> 
       let f1d, f1f = ferm_expr v1 in
       let f2d, f2f = ferm_expr v2 in
       Fbinop (o, f1d, f2d), f1f@f2f

  | {vexpr = Vif (v1, v2, v3); var_libres = l} -> 
       let f1d, f1f = ferm_expr v1 in
       let f2d, f2f = ferm_expr v2 in
       let f3d, f3f = ferm_expr v3 in
       Fif (f1d, f2d, f3d), f1f@f2f@f3f

  | {vexpr = Vlet (i, v1, v2); var_libres = l} -> 
       numfun := !numfun + 1;
       let s = ("_fun" ^ (string_of_int (!numfun))) in
       let f1d, f1f = ferm_expr v1 in
       let f2d, f2f = ferm_expr v2 in
       Flet (i, Fglacon s, f2d), (Fcodeglacon (s, v1.var_libres, f1d))::(f1f@f2f)

  | {vexpr = Vcase (v1, v2, i1, i2, v3); var_libres = l} -> 
       let f1d, f1f = ferm_expr v1 in
       let f2d, f2f = ferm_expr v2 in
       let f3d, f3f = ferm_expr v3 in
       Fcase (f1d, f2d, i1, i2, f3d), f1f@f2f@f3f

  | {vexpr = Vdo l1; var_libres = l} ->
       let l2, lf1 = List.fold_left (fun (l3,lf) vv -> let f1, f2 = ferm_expr vv in (f1::l3, f2@lf))
                                    ([], [])
                                    l1
                     in
       Fdo l2, lf1

  | {vexpr = Vreturn; var_libres = l} -> Freturn, []


let ferm_def = function
  | Vdef ("main" , v) -> let fmain, ffun = ferm_expr v in (Fmain fmain)::ffun
  | Vdef (i, v) -> let fmain, ffun = ferm_expr v in (Fdef (i, v.var_libres, fmain))::ffun

let ferm p = List.concat (List.map ferm_def p)


(************************************************************************************)
(* phase 3 : allocation des variables *)

type cdef =
  | CMain of ident * cexpr * int
  | CDef of ident * cexpr * int

and cexpr =
  | CLvar of int
  | CGvar of ident
  | CInt of int
  | CChar of char
  | CBool of bool
  | CList of cexpr list
  | CAppli of cexpr * cexpr
  | CLambda of int * cexpr
  | CBinop of binop * cexpr * cexpr
  | CIf of cexpr * cexpr * cexpr
  | CLet of int * cexpr * cexpr
  | CCase of cexpr * cexpr * int * int * cexpr
  | CDo of cexpr list
  | CReturn
  | CGlacon of cexpr


exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

type local_env = ident Smap.t

let rec alloc_expr env next = function
  | _ -> CReturn
(*
  | PCst i ->
    Cst i, next

  | PVar x ->
( try
  let num = Smap.find x env in
  LVar num, next
  with
        | Not_found -> if Hashtbl.mem genv x then GVar x, next else raise (VarUndef x) )

  | PBinop (o, e1, e2)->
  let a1, next1 = alloc_expr env next e1 in
  let a2, next2 = alloc_expr env next e2 in
  Binop (o, a1, a2), max next1 next2

    | PLetin (x, e1, e2) ->
    let a1, next1 = alloc_expr env next e1 in
    let a2, next2 = alloc_expr (Smap.add x (-next-4) env) (next+4) e2 in
    Letin (-next-4, a1, a2), max next1 next2

  | PCall (f, l) ->
  let n2, l2 = List.fold_left (fun x e -> let a, n = alloc_expr env next e in
				   let n1, l1 = x in
				   max n n1, a::l1) (next,[]) l
				   in
				   Call (f, List.rev l2), n2
*)

let alloc_stmt = function
  | _ -> CDef ("m", CReturn , 1)
(*
  | PSet (x, e) ->
    Hashtbl.replace genv x ();
    let a, n = alloc_expr Smap.empty 0 e in
    Set (x, a, n)

  | PFun (f, l, e) ->
  let env, ad = List.fold_right (fun l1 x -> let s,adr = x in
				     (Smap.add l1 adr s), (adr+4)) l (Smap.empty, 8) in
				     let a, n = alloc_expr env 0 e in
				     Fun (f, a, n)

				       | PPrint e ->
    let e, fpmax = alloc_expr Smap.empty 0 e in
    Print (e, fpmax)
*)
let alloc = List.map alloc_stmt

(******************************************************************************)
(* phase 2 : production de code *)

let pushn n = sub sp sp oi n

let rec compile_expr = function
| _ -> nop
(*
| Cst i ->
        li a0 i ++ push a0

    | LVar fp_x ->
        lw a0 areg (fp_x, fp) ++ push a0

| GVar x ->
        lw a0 alab x ++ push a0

    | Binop (o, e1, e2)->
      let op = match o with
| Add -> add
        | Sub -> sub
| Mul -> mul
        | Div -> div
      in
      let code_e1 = compile_expr e1 in
      let code_e2 = compile_expr e2 in
      code_e1 ++ code_e2 ++
      pop a1 ++ pop a0 ++ op a0 a0 oreg a1 ++ push a0

| Letin (ofs, e1, e2) ->
      let code_e1 = compile_expr e1 in
      let code_e2 = compile_expr e2 in
      code_e1 ++ pop a0 ++ sw a0 areg (ofs, fp) ++ code_e2

    | Call (f, l) ->
      let code = List.fold_left (fun x e -> let code_e = compile_expr e in
                                 x ++ code_e) nop l in
      code ++ jal f ++ pop a0 ++ popn (4*List.length l) ++ push a0
*)

let compile_stmt (codefun, codemain) = function
  | _ -> nop, nop
(*
  | Set (x, e, fpmax) ->
    let code = compile_expr e in
    let code =
      let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
      pre ++ code ++ pop a0 ++ sw a0 alab x ++ post in
    codefun, codemain ++ code

  | Fun (f, e, fpmax) ->
    let code = compile_expr e in
    let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
    let code =
      label f ++
      push fp ++ push ra ++
      move fp sp ++ pre ++
      code ++
      pop a0 ++ post ++
      pop ra ++ pop fp ++ push a0 ++ jr ra
    in
    code ++ codefun, codemain

| Print (e, fpmax) ->
    let code = compile_expr e in
    let code =
      let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
      pre ++ code ++ pop a0 ++ post ++ jal "print"
    in
    codefun, codemain ++ code
*)

let compile_program p ofile =
  let p = var_libre p in
  let p = alloc p in
  let codefun, code = List.fold_left compile_stmt (nop, nop) p in
  let p =
    { text =
        label "main" ++
        move fp sp ++
        code ++
        li v0 10 ++ (* exit *)
        syscall ++
        label "print" ++
        li v0 1 ++
        syscall ++
        li v0 4 ++
        la a0 alab "newline" ++
        syscall ++
        jr ra ++
        codefun;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dword [1] ++ l) genv
          (label "newline" ++ asciiz "\n")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  Mips.print_program fmt p;
  fprintf fmt "@?";
  close_out f

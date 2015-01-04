
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
let numglacon = ref 0

type fdef =
  | Fdef of ident * ident list * fexpr
  | Ffun of ident * ident list * ident * fexpr
  | Fcodeglacon of ident * ident list * fexpr
  | Fmain of ident list * fexpr

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
  | Fglacon of ident * ident list


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
       numglacon := !numglacon + 1;
       let s = ("_glacon_" ^ (string_of_int (!numglacon))) in
       let f1d, f1f = ferm_expr v1 in
       let f2d, f2f = ferm_expr v2 in
       Fappli (f1d, Fglacon (s, v2.var_libres)), (Fcodeglacon (s, v2.var_libres, f2d))::(f1f@f2f)

  | {vexpr = Vlambda(i, vv); var_libres = l} -> 
    numfun := !numfun + 1;
    let f1, f2 = ferm_expr vv in
    let s = ("_fun_" ^ (string_of_int (!numfun))) in
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
       numglacon := !numglacon + 1;
       let s = ("_glacon_" ^ (string_of_int (!numglacon))) in
       let f1d, f1f = ferm_expr v1 in
       let f2d, f2f = ferm_expr v2 in
       Flet (i, Fglacon (s, v1.var_libres), f2d), (Fcodeglacon (s, v1.var_libres, f1d))::(f1f@f2f)

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
  | Vdef ("main" , v) -> let fmain, ffun = ferm_expr v in (Fmain (v.var_libres, fmain))::ffun
  | Vdef (i, v) -> let fmain, ffun = ferm_expr v in (Fdef (i, v.var_libres, fmain))::ffun

let ferm p = List.concat (List.map ferm_def p)


(************************************************************************************)
(* phase 3 : allocation des variables *)

type cdef =
  | CDef of ident * cexpr * int
  | CFun of ident * int * cexpr * int
  | CCodeglacon of ident * int * cexpr * int
  | CMain of cexpr * int

and cexpr =
  | CLvar of int
  | CGvar of ident
  | CInt of int
  | CChar of char
  | CBool of bool
  | CList of cexpr list
  | CAppli of cexpr * cexpr
  | CClos of ident * int list
  | CBinop of binop * cexpr * cexpr
  | CIf of cexpr * cexpr * cexpr
  | CLet of int * cexpr * cexpr
  | CCase of cexpr * cexpr * int * int * cexpr
  | CDo of cexpr list
  | CReturn
  | CGlacon of ident * int list


exception VarUndef of string

let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

module Smap = Map.Make(String)

type local_env = ident Smap.t

let rec alloc_expr env next = function
  | Fident x -> (
      try
        let adr = Smap.find x env in CLvar adr
      with 
        | Not_found -> if Hashtbl.mem genv x then CGvar x else raise (VarUndef x)
                ), next

  | Fcst c -> (
      match c with 
	    | Cint i -> CInt i
            | Cchar c -> CChar c
            | Cbool b -> CBool b
              ), next

  | Flist l -> 
      let ll, nn = List.fold_left (fun (l1, n1) e -> let a2, n2 = alloc_expr env next e in
                                                      (a2::l1, max n1 n2) )
                                  ([], 0) l
                   in
      CList ll, (max nn next)

  | Fappli (e1, e2) ->
     let a1, n1 = alloc_expr env next e1 in
     let a2, n2 = alloc_expr env next e2 in
     CAppli (a1, a2), max n1 n2

  | Fclos (i, l) -> let l1 = List.fold_left (fun l2 x -> (Smap.find x env)::l2) [] l in
                    CClos (i, l1), next

  | Fbinop (o, e1, e2) ->
     let a1, n1 = alloc_expr env next e1 in
     let a2, n2 = alloc_expr env next e2 in
     CBinop (o, a1, a2), max n1 n2

  | Fif (e1, e2, e3) ->
     let a1, n1 = alloc_expr env next e1 in
     let a2, n2 = alloc_expr env next e2 in
     let a3, n3 = alloc_expr env next e3 in
     CIf (a1, a2, a3), max (max n1 n2) n3

  | Flet (i, e1, e2) ->
     let a1, n1 = alloc_expr env next e1 in
     let a2, n2 = alloc_expr (Smap.add i (-next-4) env) (next+4) e2 in
     CLet (-next-4, a1, a2), max n1 n2     

  | Fcase (e1, e2, i1, i2, e3) ->
     let a1, n1 = alloc_expr env next e1 in
     let a2, n2 = alloc_expr env next e2 in
     let env1 = Smap.add i2 (-next-8) (Smap.add i1 (-next-4) env) in
     let a3, n3 = alloc_expr env1 (next+8) e3 in
     CCase (a1, a2, -next-4, -next-8, a3), max (max n1 n2) n3

  | Fdo l -> 
      let ll, nn = List.fold_left (fun (l1, n1) e -> let a2, n2 = alloc_expr env next e in
                                                      (a2::l1, max n1 n2) )
                                  ([], 0) l
                   in
      CDo ll, (max nn next)

  | Freturn -> CReturn, next

  | Fglacon (i, l) -> let s = ("_adr" ^ i) in
                      Hashtbl.replace genv s ();
                      let l1 = List.fold_left (fun l2 x -> (Smap.find x env)::l2) [] l in
                      CGlacon (i, l1), next

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

let rec ajout_liste_env env next = function
  | [] -> env, next
  | a::q -> let env1 = Smap.add a (-next-4) env in
            let env2, n2 = ajout_liste_env env1 (next+4) q in
            env2, n2


let alloc_def = function
  | Fdef (i, l, e) -> Hashtbl.replace genv i ();
                      let a, n = alloc_expr Smap.empty 0 e in
                      CDef (i, a, n)

  | Ffun (i1, l, i2, e) -> let env1, n1 = ajout_liste_env (Smap.singleton i2 (-4)) 4 l in
                           let a, n = alloc_expr env1 n1 e in
                           CFun (i1, List.length l, a, n)

  | Fcodeglacon (i, l, e) -> let env1, n1 = ajout_liste_env Smap.empty 0 l in
                             let a, n = alloc_expr env1 n1 e in
                             CCodeglacon (i, List.length l, a, n)

  | Fmain (l, e) -> let a, n = alloc_expr Smap.empty 0 e in
                    CMain (a, n)

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

let alloc = List.map alloc_def

(******************************************************************************)
(* phase 4 : production de code *)

let numlbl = ref 0

let pushn n = sub sp sp oi n

let rec compile_expr = function
  | CLvar adr -> lw a0 areg(adr, fp)

  | CGvar i -> la a0 alab i

  | CInt i -> li v0 9 ++ li a0 8 ++ syscall ++
              sw zero areg(0, v0) ++ li a0 i ++ sw a0 areg(4, v0) ++ move a0 v0

  | CChar c -> li v0 9 ++ li a0 8 ++ syscall ++
               sw zero areg(0, v0) ++ la a0 alab ("_char_" ^ (string_of_int (int_of_char c))) ++
               sw a0 areg(4, v0) ++ move a0 v0

  | CBool b -> li v0 9 ++ li a0 8 ++ syscall ++
               sw zero areg(0, v0) ++ la a0 alab (if b then "_bool_True" else "_bool_False") ++
               sw a0 areg(4, v0) ++ move a0 v0

  | CList [] -> li v0 9 ++ li a0 8 ++ syscall ++
                sw zero areg(0, v0) ++ sw zero areg(4, v0) ++ move a0 v0

  | CList (a::q) -> let code = compile_expr (CList q) in
                    code ++ push a0 ++ compile_expr a ++ move a1 a0 ++ pop a2 ++
                    li v0 9 ++ li a0 12 ++ syscall ++ li a0 1 ++
                    sw a0 areg(0, v0) ++ sw a1 areg(4, v0) ++ sw a2 areg(8, v0) ++ move a0 v0

  | CAppli (e1, e2) -> let code_e1 = compile_expr e1 in
                       let code_e2 = compile_expr e2 in
                       code_e1 ++ push ra ++ jal "_force" ++ pop ra ++ push a0 ++
                       code_e2 ++ pop a1 ++ lw t0 areg(4, a1) ++ jalr t0

  | CClos (f, l) -> let c, n1 = List.fold_left (fun (code,n) y ->
                                                code ++ lw a0 areg(y, fp) ++ sw a0 areg(n, v0)
                                                , n+4 )
                                               (nop, 8) l in
                    li a0 (4*(List.length l) + 8) ++ li v0 9 ++ syscall ++
                    li a0 2 ++ sw a0 areg(0, v0) ++ la a0 alab f ++ sw a0 areg(4, v0) ++ c ++
                    move a0 v0

  | CBinop (o, e1, e2) -> 
      numlbl := !numlbl + 1;
      let s = ("_lbl_" ^ (string_of_int (!numlbl))) in
      let code_e1 = compile_expr e1 in
      let code_e2 = compile_expr e2 in
      code_e1 ++ push a0 ++ code_e2 ++ push ra ++ jal "_force" ++
      lw a1 areg (4, sp) ++ sw a0 areg (4, sp) ++ move a0 a1 ++ jal "_force" ++
      pop ra ++ pop a1 ++ lw a0 areg (4, a0) ++ lw a1 areg (4, a1) ++ (
      match o with
	| Badd -> add a2 a0 oreg a1
        | Bsub -> sub a2 a0 oreg a1
        | Bmul -> mul a2 a0 oreg a1
        | Band -> move a2 a0 ++ beq a2 a1 s ++ la a2 alab "_bool_False" ++ label s
        | Bor -> move a2 a0 ++ beq a2 a1 s ++ la a2 alab "_bool_True" ++ label s
        | Blt -> la a2 alab "_bool_True" ++ blt a0 a1 s ++ la a2 alab "_bool_False" ++ label s
        | Bleq -> la a2 alab "_bool_True" ++ ble a0 a1 s ++ la a2 alab "_bool_False" ++ label s
        | Bgt -> la a2 alab "_bool_True" ++ bgt a0 a1 s ++ la a2 alab "_bool_False" ++ label s
        | Bgeq -> la a2 alab "_bool_True" ++ bge a0 a1 s ++ la a2 alab "_bool_False" ++ label s
        | Beq -> la a2 alab "_bool_True" ++ beq a0 a1 s ++ la a2 alab "_bool_False" ++ label s
        | Bneq -> la a2 alab "_bool_True" ++ bne a0 a1 s ++ la a2 alab "_bool_False" ++ label s
        | Bcol -> nop
      ) ++
      li a0 8 ++ li v0 9 ++ syscall ++
      sw zero areg(0, v0) ++ sw a2 areg(4, v0) ++ move a0 v0
  
  | CIf (e1, e2, e3) ->
      numlbl := !numlbl + 2;
      let s1 = ("_lbl_" ^ (string_of_int (!numlbl-1))) in
      let s2 = ("_lbl_" ^ (string_of_int (!numlbl))) in
      let code_e1 = compile_expr e1 in
      let code_e2 = compile_expr e2 in
      let code_e3 = compile_expr e3 in
      code_e1 ++ push ra ++ jal "_force" ++ pop ra ++ lw a1 areg (4, a0) ++ la a2 alab "_bool_True"
      ++ beq a1 a2 s1 ++ code_e2 ++ beq zero zero s2 ++ label s1 ++ code_e3 ++ label s2

  | CLet (adr, e1, e2) -> let code_e1 = compile_expr e1 in
                          let code_e2 = compile_expr e2 in
                          code_e1 ++ sw a0 areg(adr, fp) ++ code_e2

  | CCase (e1, e2, adr1, adr2, e3) -> 
      numlbl := !numlbl + 2;
      let s1 = ("_lbl_" ^ (string_of_int (!numlbl-1))) in
      let s2 = ("_lbl_" ^ (string_of_int (!numlbl))) in
      let code_e1 = compile_expr e1 in
      let code_e2 = compile_expr e2 in
      let code_e3 = compile_expr e3 in
      code_e1 ++ push ra ++ jal "_force" ++ pop ra ++ lw a1 areg (0, a0) ++
      bne a1 zero s1 ++ code_e2 ++ beq zero zero s2 ++ label s1 ++
      lw a1 areg(4, a0) ++ lw a2 areg(8, a0) ++ sw a1 areg(adr1, fp) ++ sw a2 areg(adr2, fp) ++
      code_e3 ++ label s2
 
  | CDo l -> List.fold_left (fun c e -> compile_expr e ++ c) nop l

  | CReturn -> nop

  | CGlacon (f, l) -> let c, n1 = List.fold_left (fun (code,n) y ->
                                                  code ++ lw a0 areg(y, fp) ++ sw a0 areg(n, v0)
                                                  , n+4 )
                                                 (nop, 8) l in
                      li a0 8 ++ li v0 9 ++ syscall ++
                      sw v0 alab ("_adr" ^ f) ++
                      li a0 3 ++ sw a0 areg(0, v0) ++ move a2 v0 ++
                      li a0 (4*(List.length l) + 8) ++ li v0 9 ++ syscall ++ sw v0 areg(4, a2) ++
                      li a0 2 ++ sw a0 areg(0, v0) ++ la a0 alab f ++ sw a0 areg(4, v0) ++
                      push a2 ++ c ++
                      pop a0



let compile_def (codefun, codemain) = function
  | CDef (x, e, fpmax) ->
      let code = compile_expr e in
      let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
      let code =
        pre ++ code ++ post ++ sw a0 alab x
      in
      codefun, codemain ++ code

  | CFun (f, nvars, e, fpmax) ->
      let code = compile_expr e in
      let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
      let vars = ref (sw a0 areg(-4, fp)) in
      for i=2 to (nvars+1) do
        vars := !vars ++ lw t0 areg(4*i, fp) ++ sw t0 areg((-4)*i, fp)
      done;
      let code =
        label f ++
        push fp ++ push ra ++
        move fp sp ++ pre ++ !vars ++
        code ++
        post ++
        pop ra ++ pop fp ++ jr ra
      in
      code ++ codefun, codemain
  
  | CCodeglacon (f, nvars, e, fpmax) ->
      let code = compile_expr e in
      let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
      let vars = ref nop in
      for i=2 to (nvars+1) do
        vars := !vars ++ lw t0 areg(4*i, fp) ++ sw t0 areg((-4)*i+4, fp)
      done;
      let code =
        label f ++
        push fp ++ push ra ++
        move fp sp ++ pre ++ !vars ++
        code ++
        post ++
        push ra ++ jal "_force" ++ pop ra ++
        pop ra ++ pop fp ++
        move a2 a0 ++ li a0 8 ++ li v0 9 ++ syscall ++
        li a1 4 ++ sw a1 areg(0, v0) ++ sw a2 areg(4, v0) ++ sw v0 alab ("_adr" ^ f) ++
        move a0 a2 ++ jr ra
      in
      code ++ codefun, codemain

  | CMain (e, fpmax) ->
      let code = compile_expr e in
      let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
      let code =
        pre ++ code ++ post
      in
      codefun, codemain ++ code
  

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
  let p = ferm p in
  let p = alloc p in
  let codefun, code = List.fold_left compile_def (nop, nop) p in
  let p =
    { text =
        label "main" ++
        move fp sp ++
        code ++
        li v0 10 ++ (* exit *)
        syscall ++
        codefun ++
        (*********div*********)
        
        (*********rem*********)
        (*********putChar*********)
        (*********error*********)
        (*********force*********)
        label "_force" ++
        lw t0 areg(0, a0) ++
        li t1 2 ++
        bgt t0 t1 "_force_1" ++
        jr ra ++
        label "_force_1" ++
        li t1 3 ++
        beq t0 t1 "_force_2" ++
        lw a0 areg(4, a0) ++
        push ra ++ jal "_force" ++ pop ra ++
        jr ra ++
        label "_force_2" ++
        push ra ++
        lw a1 areg(4, a0) ++
        lw a2 areg(4, a1) ++
        jalr a2 ++
        pop ra ++
        jr ra;
      data =
        let chars = ref nop in
        for i=32 to 126 do
          if i<>34 && i<>92 then (
            chars := !chars ++ inline ("_char_" ^ (string_of_int i) ^ ":\n") ++ asciiz_c (char_of_int i) )
        done;
        Hashtbl.fold (fun x _ l -> label x ++ dword [1] ++ l) genv
          (label "newline" ++ asciiz "\n") ++ 
        !chars ++
        label "_char_9" ++ asciiz "\t" ++
        label "_char_10" ++ asciiz "\n" ++
        label "_char_34" ++ asciiz "\"" ++
        label "_char_92" ++ asciiz "\\" ++
        label "_bool_True" ++ asciiz "True" ++
        label "_bool_False" ++ asciiz "False"
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  Mips.print_program fmt p;
  fprintf fmt "@?";
  close_out f

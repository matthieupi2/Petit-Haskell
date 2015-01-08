(* phase 4 : production de code *)

(* conventions d'appel :
 - on passe l'argument dans t0 et la cloture dans t1
 - on renvoie un pointeur dans v0
 - la fonction force prend un pointeur dans v0 et renvoie un pointeur dans v0 (le meme)
*)


open Format
open Ast
open AllocatedAst
open Primitives
open Error
open Mips

let numlbl = ref 0

(* TODO tester si n > 0 *)
let pushn n = sub sp sp oi n


let compile_var = function
  | Vglobale i -> lw v0 alab i
  | Vlocale n -> lw v0 areg (n, fp)
  | Vclos n -> lw v0 areg (n, t1)
  | Varg -> move v0 t0

(* Ne change pas $t0, $t1, $ra *)
(* Registre de renvoi : $v0 *)
let rec compile_expr = function
  | CVar v -> compile_var v
  | CCst i -> li v0 9 ++ li a0 8 ++ syscall ++
    sw zero areg (0, v0) ++ li a0 i ++ sw a0 areg (4, v0)
  | CEmptylist -> li v0 9 ++ li a0 8 ++ syscall ++
    sw zero areg (0, v0) ++ sw zero areg(4, v0)
  | CAppli (e1, e2) ->
    let code_e1 = compile_expr e1 in
    let code_e2 = compile_expr e2 in
    push t0 ++ push t1 ++
    code_e1 ++ push ra ++ jal "_force" ++ pop ra ++ push v0 ++
    code_e2 ++ move t0 v0 ++ pop t1 ++ lw t2 areg (4, t1) ++ jalr t2 ++
    pop t1 ++ pop t0
  | CClos (f, l) ->
    let code, n1 = List.fold_left (fun (c,n) v ->
        c ++ compile_var v ++ sw v0 areg(n, t2), n+4 )
        (nop, 8) l in
    li a0 (4*(List.length l) + 8) ++ li v0 9 ++ syscall ++ move t2 v0 ++
    li a0 2 ++ sw a0 areg (0, t2) ++ la a0 alab ("_code" ^ f) ++ sw a0 areg (4, t2) ++
    code ++ move v0 t2
  | CBinop (o, e1, e2) ->
    (* TODO new_lbl () *)
    numlbl := !numlbl + 2;
    let s1 = ("_lbl_" ^ (string_of_int (!numlbl-1))) in
    let code_e1 = compile_expr e1 in
    let code_e2 = compile_expr e2 in
    ( match o with 
      | Bcol ->
        code_e1 ++ push v0 ++ code_e2 ++ pop a1 ++ move a2 v0 ++
        li a0 12 ++ li v0 9 ++ syscall ++ li a3 1 ++ sw a3 areg (0, v0) ++
        sw a1 areg (4, v0) ++ sw a2 areg (8, v0)
      | Band | Bor ->
        (* TODO force = ... *)
        code_e1 ++ push ra ++ jal "_force" ++ pop ra ++ lw a1 areg (4, v0) ++
        ( match o with
          | Band ->
            beq zero a1 s1
          | Bor ->
            bne zero a1 s1
          | _ ->
            raise (CompilerError "during production of code of Band and Bor") )
        ++ code_e2 ++ label s1
      | _ ->
        push ra ++ code_e1 ++ push v0 ++ code_e2 ++ jal "_force" ++
        move a1 v0 ++ pop v0 ++ jal "_force" ++ pop ra ++ lw a0 areg (4, v0) ++
        lw a1 areg (4, a1) ++
        ( match o with
          | Badd -> add a2 a0 oreg a1
          | Bsub -> sub a2 a0 oreg a1
          | Bmul -> mul a2 a0 oreg a1
          | Blt -> li a2 1 ++ blt a0 a1 s1 ++ li a2 0 ++ label s1
          | Bleq -> li a2 1 ++ ble a0 a1 s1 ++ li a2 0 ++ label s1
          | Bgt -> li a2 1 ++ bgt a0 a1 s1 ++ li a2 0 ++ label s1
          | Bgeq -> li a2 1 ++ bge a0 a1 s1 ++ li a2 0 ++ label s1
          | Beq -> li a2 1 ++ beq a0 a1 s1 ++ li a2 0 ++ label s1
          | Bneq -> li a2 1 ++ bne a0 a1 s1 ++ li a2 0 ++ label s1
          | _ -> raise (CompilerError "during production of code of Binop") )
        ++ li a0 8 ++ li v0 9 ++ syscall ++
        sw zero areg(0, v0) ++ sw a2 areg(4, v0) )
  | CIf (e1, e2, e3) ->
      numlbl := !numlbl + 2;
      let s1 = ("_lbl_" ^ (string_of_int (!numlbl-1))) in
      let s2 = ("_lbl_" ^ (string_of_int (!numlbl))) in
      let code_e1 = compile_expr e1 in
      let code_e2 = compile_expr e2 in
      let code_e3 = compile_expr e3 in
      code_e1 ++ push ra ++ jal "_force" ++ pop ra ++ lw a1 areg (4, v0) ++
      bne a1 zero s1 ++ code_e2 ++ b s2 ++ label s1 ++ code_e3 ++ label s2
  | CLet (l, e) ->
    let aux (i1,e1) c =
      let c1 = compile_expr e1 in
      c1 ++ sw v0 areg (i1, fp) ++ c in
    let code = List.fold_right aux l nop in
    code ++ compile_expr e
  | CCase (e1, e2, adr1, adr2, e3) -> 
    numlbl := !numlbl + 2;
    let s1 = ("_lbl_" ^ (string_of_int (!numlbl-1))) in
    let s2 = ("_lbl_" ^ (string_of_int (!numlbl))) in
    let code_e1 = compile_expr e1 in
    let code_e2 = compile_expr e2 in
    let code_e3 = compile_expr e3 in
    code_e1 ++ push ra ++ jal "_force" ++ pop ra ++ lw a1 areg (0, v0) ++
    bne a1 zero s1 ++ code_e2 ++ b s2 ++ label s1 ++
    lw a1 areg(4, v0) ++ lw a2 areg(8, v0) ++ sw a1 areg(adr1, fp) ++
    sw a2 areg(adr2, fp) ++ code_e3 ++ label s2
  | CDo l -> List.fold_right (fun e c -> compile_expr e ++ c) l nop
  | CReturn -> nop
  | CGlacon e ->
    let code = compile_expr e in
    code ++ move a1 v0 ++ li a0 8 ++ li v0 9 ++ syscall ++
    li a0 3 ++ sw a0 areg(0, v0) ++ sw a1 areg(4, v0)

let compile_decl = function
  | CDef (x, e, fpmax) ->
    let code = compile_expr e in
    let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
    pre ++ code ++ post ++ sw v0 alab x
  | CFun (f, e, fpmax) ->
    let code = compile_expr e in
    let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
    label ("_code" ^ f) ++
    push fp ++ push ra ++
    move fp sp ++ pre ++
    code ++
    post ++
    pop ra ++ pop fp ++ jr ra
  | CMain (e, fpmax) ->
    let code = compile_expr e in
    let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
    pre ++ code ++ post

let force =
  label "_force" ++
  lw a0 areg(0, v0) ++
  li a1 2 ++
  bgt a0 a1 "_force_1" ++
  jr ra ++
  label "_force_1" ++
  li a1 3 ++
  beq a0 a1 "_force_2" ++
  lw v0 areg(4, v0) ++
  jr ra ++
  label "_force_2" ++
  push ra ++ push t1 ++ push v0 ++
  lw t1 areg(4, v0) ++
  lw t2 areg(4, t1) ++
  jalr t2 ++ jal "_force" ++ move a1 v0 ++
  pop v0 ++ pop t1 ++
  li a0 4 ++ sw a0 areg(0, v0) ++ sw a1 areg(4, v0) ++
  pop ra ++
  jr ra
  
let compile_program p primitives =
  let rec aux = function
    | [] -> (nop, nop, [])
    | (CMain _ as main)::q -> let codefun, _, data = aux q in
      codefun, compile_decl main, data
    | (CDef (i, _, _) | CFun (i, _, _)) as decl::q ->
      let codefun, code, data = aux q in
      ((compile_decl decl) ++ codefun, code, i::data) in
  let codefun, code, data = aux p in
  { text =
      label "main" ++
      move fp sp ++
      (* Création des fermetures des primitives *)
      List.fold_left (fun code prim ->
        la t0 alab prim.name ++
        la t1 alab ("_prim_" ^ prim.name) ++
        li a0 8 ++
        li v0 9 ++
        syscall ++
        li a0 2 ++
        sw a0 areg(0, v0) ++
        sw t1 areg(4, v0) ++
        sw v0 areg(0, t0) ++
        code ) nop primitives ++
      code ++
      li a0 0 ++
      li v0 17 ++ (* exit 0 *)
      syscall ++
      codefun ++
      List.fold_left (fun code prim ->
        code ++
        label ("_prim_" ^ prim.name) ++
        prim.body ) nop primitives ++
      force;
    data =
      label "_newline" ++ asciiz "\n" ++
      List.fold_left (fun data lbl -> label lbl ++ dword [0] ++ data)
          nop data ++
      List.fold_left (fun code prim -> code ++ label prim.name ++ dword [0])
          nop primitives ++
      List.fold_left (fun data prim -> data ++ prim.pdata) nop primitives
  }

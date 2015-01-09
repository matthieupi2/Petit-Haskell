
(* Contient la définition des primitives utilisées par le compilateur
 * ainsi que des outils pour pouvoir utiliser cette définition *)

open TypedAst
open Mips

type primitive = { name : string; typ : typ; generalized : bool;
    body : Mips.text; pdata : Mips.data } 

(* TODO compléter les body *)
(* TODO ajouter ces fermetures dans le tas pour rester coherent avec *)
let primitives = [
  { name = "div";
    typ = Tarrow (Tint, Tarrow (Tint, Tint));
    generalized = false;
    body = push fp ++ push ra ++ move fp sp ++
    li a0 12 ++ li v0 9 ++ syscall ++
    li a1 2 ++ sw a1 areg(0, v0) ++
    la a1 alab "_div_1" ++ sw a1 areg(4, v0) ++ sw t0 areg(8, v0) ++
    pop ra ++ pop fp ++ jr ra ++

    label "_div_1" ++ push fp ++ push ra ++ push t1 ++ move fp sp ++
    move v0 t0 ++ jal "_force" ++ pop t1 ++ push v0 ++
    lw v0 areg(8, t1) ++ jal "_force" ++ pop a1 ++
    lw a1 areg(4, a1) ++ lw a0 areg(4, v0) ++
    bne a1 zero "_div_pas_par_0" ++ la a0 alab "_error_div_by_0" ++ li v0 4 ++
    syscall ++ li v0 17 ++ li a0 1 ++ syscall ++ label "_div_pas_par_0" ++
    div a2 a0 oreg a1 ++ li a0 8 ++ li v0 9 ++ syscall ++
    sw zero areg(0, v0) ++ sw a2 areg(4, v0) ++ pop ra ++ pop fp ++ jr ra
	; pdata = label "_error_div_by_0" ++ asciiz "error: divide by 0" } ;
  { name = "rem";
    typ = Tarrow (Tint, Tarrow (Tint, Tint));
    generalized = false;
    body = push fp ++ push ra ++ move fp sp ++
    li a0 12 ++ li v0 9 ++ syscall ++
    li a1 2 ++ sw a1 areg(0, v0) ++
    la a1 alab "_rem_1" ++ sw a1 areg(4, v0) ++ sw t0 areg(8, v0) ++
    pop ra ++ pop fp ++ jr ra ++

    label "_rem_1" ++ push fp ++ push ra ++ push t1 ++ move fp sp ++
    move v0 t0 ++ jal "_force" ++ pop t1 ++ push v0 ++
    lw v0 areg(8, t1) ++ jal "_force" ++ pop a1 ++
    lw a1 areg(4, a1) ++ lw a0 areg(4, v0) ++
    bne a1 zero "_rem_pas_par_0" ++ la a0 alab "_error_rem_by_0" ++ li v0 4 ++
    syscall ++ li v0 17 ++ li a0 1 ++ syscall ++ label "_rem_pas_par_0" ++
    rem a2 a0 oreg a1 ++ li a0 8 ++ li v0 9 ++ syscall ++
    sw zero areg(0, v0) ++ sw a2 areg(4, v0) ++ pop ra ++ pop fp ++ jr ra
	 ; pdata = label "_error_rem_by_0" ++ asciiz "error: divide by 0" } ;
  { name = "putChar";
    typ = Tarrow (Tchar, Tio); generalized = false; 
    body = move v0 t0 ++ push ra ++ jal "_force" ++ pop ra ++
      lw a0 areg(4, v0) ++ li v0 11 ++ syscall ++ jr ra;
		pdata = nop } ;
  { name = "error";
    typ = Tarrow (Tlist Tchar, Tvar (V.create ()));
    generalized = true;
    body = move v0 t0 ++ jal "_force" ++ move t0 v0 ++
      la a0 alab "_error_text" ++ li v0 4 ++ syscall ++ move v0 t0 ++
      jal "_force" ++ label "_error_1" ++ lw a0 areg(0, v0) ++
      beq a0 zero "_error_2" ++ lw a0 areg(8, v0) ++ push a0 ++
      lw v0 areg(4, v0) ++ jal "_force" ++ lw a0 areg(4, v0) ++ li v0 11 ++
      syscall ++ pop v0 ++ jal "_force" ++ b "_error_1" ++ label "_error_2" ++
      la a0 alab "_newline" ++ li v0 4 ++ syscall ++ li a0 1 ++ li v0 17 ++
      syscall
    ; pdata = label "_error_text" ++ asciiz "error: " } ]

(* Utilisée par UncurriedAst pour vérifier la non redéfinition *)
let getNames =
  let rec aux = function
    | [] -> []
    | t::q -> t.name::(aux q) in
  aux primitives

(* Utilisée par TypedAst comme environnement de base *)
let getEnv =
  let rec aux = function
    | [] -> empty_env
    | t::q -> let add = if t.generalized then add_gen else TypedAst.add in
      add t.name t.typ (aux q) in
  aux primitives


(* Contient la définition des primitives utilisées par le compilateur
 * ainsi que des outils pour pouvoir utiliser cette définition *)

open TypedAst
open Mips

type primitive = { name : string; typ : typ; generalized : bool;
    body : Mips.text; pdata : Mips.data } 

(* TODO compléter les body *)
(* TODO ajouter ces fermetures dans le tas pour rester coherent avec *)
let primitives = [
  { name = "div"; typ = Tarrow (Tint, Tarrow (Tint, Tint)); generalized = false;
    body = nop
(* _div : [a] x -> x/a
   div : y -> _div [y] *)
					; pdata = nop} ;
  { name = "rem"; typ = Tarrow (Tint, Tarrow (Tint, Tint)); generalized = false;
    body = nop ; pdata = nop } ;
  { name = "putChar"; typ = Tarrow (Tchar, Tio); generalized = false; 
    body = move v0 t0 ++ push ra ++ jal "_force" ++ pop ra ++ lw a0 areg(4, v0) ++
           li v0 11 ++ syscall ++ jr ra
					; pdata = nop } ;
  { name = "error"; typ = Tarrow (Tlist Tchar, Tvar (V.create ()));
    generalized = true;
    body = move v0 t0 ++ push ra ++ jal "_force" ++ pop ra ++ label "_error_1" ++
    lw a0 areg(0, v0) ++ beq a0 zero "_error_2" ++
    lw a0 areg(8, v0) ++ push a0 ++ lw v0 areg(4, v0) ++ push ra ++ jal "_force" ++ pop ra ++
    move t0 v0 ++ push ra ++ jal "putChar" ++ pop ra ++ pop v0 ++
    beq zero zero "_error_1" ++ label "_error_2" ++ li a0 1 ++ li v0 17 ++ syscall
					; pdata = nop } ]

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

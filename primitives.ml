
(* Contient la définition des primitives utilisées par le compilateur
 * ainsi que des outils pour pouvoir utiliser cette définition *)

open TypedAst
open Mips

type primitive = { name : string; typ : typ; generalized : bool;
    body : Mips.text;  data : Mips.data } 

(* TODO compléter les body *)
let primitives = [
  { name = "div"; typ = Tarrow (Tint, Tarrow (Tint, Tint)); generalized = false;
    body = nop ; data = nop} ;
  { name = "rem"; typ = Tarrow (Tint, Tarrow (Tint, Tint)); generalized = false;
    body = nop ; data = nop } ;
  { name = "putChar"; typ = Tarrow (Tchar, Tio); generalized = false;
    body = nop ; data = nop } ;
  { name = "error"; typ = Tarrow (Tlist Tchar, Tvar (V.create ()));
    generalized = true; body = nop; data = nop } ]

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

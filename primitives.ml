
open TypedAst
open Mips

type primitive = { name : string; typ : typ; generalized : bool;
    body : Mips.text } 

let primitives = [
  { name = "div"; typ = Tarrow (Tint, Tarrow (Tint, Tint)); generalized = false;
    body = nop } ;
  { name = "rem"; typ = Tarrow (Tint, Tarrow (Tint, Tint)); generalized = false;
    body = nop } ;
  { name = "putChar"; typ = Tarrow (Tchar, Tio); generalized = false;
    body = nop } ;
  { name = "error"; typ = Tarrow (Tlist Tchar, Tvar (V.create ()));
    generalized = false; body = nop } ]

let getNames () =
  let rec aux = function
    | [] -> []
    | t::q -> t.name::(aux q) in
  aux primitives
 
let getEnv () =
  let rec aux = function
    | [] -> empty_env
    | t::q -> let add = if t.generalized then add_gen else TypedAst.add in
      add t.name t.typ (aux q) in
  aux primitives

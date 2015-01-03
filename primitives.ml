
open TypedAst

type primitive = { name : string; typ : typ; generalize : bool;
    body : Mips.text } 

let primitives = []

let getNames () =
  let rec aux = function
    | [] -> []
    | t::q -> t.name::(aux q) in
  aux primitives
 
let getEnv () =
  let rec aux = function
    | [] -> empty_env
    | t::q -> let add = if t.generalize then add_gen else add in
      add t.name t.typ (aux q) in
  aux primitives

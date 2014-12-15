
(* Ast : -applications décurrifiées
 * -certifié sans répétition erronée d'identifiants *)

open Error
open Ast

(* TODO localiser les initialisations ? *)
type udef = ident * luexpr

and luexpr = {uexpr : uexpr; locu : Error.location}

and uexpr =
  | Uident of ident
  | Ucst of constant
  | Ulist of luexpr list
  | Uappli of luexpr * luexpr
  | Ulambda of ident list * luexpr
  | Ubinop of binop * luexpr * luexpr
  | Uif of luexpr * luexpr * luexpr
  | Ulet of udef list * luexpr
  | Ucase of luexpr * luexpr * ident * ident * luexpr
  | Udo of luexpr list
  | Ureturn

(*****************************************************************)

module S = Set.Make(struct type t = Ast.ident
    let compare = Pervasives.compare end)
module M = Map.Make(struct type t = Ast.ident
    let compare = Pervasives.compare end)

(* Renvoie une liste d'identifiants non localisés (si pas d'erreurs) *)
let are_different l =
  let rec aux prev = function
    | [] -> []
    | {ident=name; loci=loc}::q -> try
        let first_def = M.find name prev in
        raise (IdentError (name, loc, RedefArg first_def))
      with Not_found ->
        name::aux (M.add name loc prev) q in
  aux M.empty l

let rec uncurry_expr e =
  assert false

let uncurry ast =
  let primitives = S.of_list ["div" ; "rem" ; "putChar" ; "error" ] in
  let rec aux env = function
    | []  -> []
    | ({ident=name; loci=loc},_,_)::q when S.mem name primitives ->
        raise (IdentError (name, loc, RedefPrimitive))
    | ({ident=name; loci=loc}, args, body)::q -> try
        let first_def = M.find name env in
        raise (IdentError (name, loc, RedefGlobal first_def))
      with Not_found ->
        (name, Ulambda (are_different args, uncurry_expr body))::
            (aux (M.add name loc env) q) in
  aux M.empty ast

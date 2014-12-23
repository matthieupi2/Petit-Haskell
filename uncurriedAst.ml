
(* Ast : -applications décurrifiées
 * -certifié sans répétition erronée d'identifiants *)

open Error
open Ast

(* TODO localiser les initialisations ? *)
type udef = ident * luexpr

(* TODO desc *)
and luexpr = {uexpr : uexpr; locu : Error.location}

and uexpr =
  | Uident of ident
  | Ucst of constant
  | Ulist of luexpr list
  | Uappli of luexpr * luexpr (* TODO localiser ? *)
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

(* Vérifie que les arguments ont des identifiants différents *)
let uncurry_args l =
  let rec aux prev = function
    | [] -> []
    | {ident=name; loci=loc}::q -> try
        let first_def = M.find name prev in
        raise (IdentError (name, loc, RedefArg first_def))
      with Not_found ->
        name::aux (M.add name loc prev) q in
  aux M.empty l

let rec uncurry_appli f = function
  | [] -> raise (CompilerError "function without argument")
  | fst_arg::args -> let create_uappli f a =
      let a = uncurry_expr a in
      {uexpr = Uappli (f, a); locu = (fst f.locu, snd a.locu)} in
    let rec uncurry uncurried_expr = function
      | [] -> uncurried_expr.uexpr  (* uncurry_expr retrouve locu *)
      | t::q -> uncurry (create_uappli uncurried_expr t) q in
    uncurry (create_uappli (uncurry_expr f) fst_arg) args

and uncurry_expr e =
  let ue = match e.expr with
    | Eident ident -> Uident ident
    | Ecst c  -> Ucst c
    | Elist l -> Ulist (List.map uncurry_expr l)
    | Eappli (f, args) -> uncurry_appli f args
    | Elambda (args, body) -> Ulambda (uncurry_args args, uncurry_expr body)
    | Ebinop (o, e1, e2) -> Ubinop (o, uncurry_expr e1, uncurry_expr e2)
    | Eif (cdt, e1, e2) ->
        Uif (uncurry_expr cdt, uncurry_expr e1, uncurry_expr e2)
    | Elet (defs, e) -> Ulet (uncurry_list_def defs S.empty, uncurry_expr e)
    | Ecase (_,_, hd, tl,_) when hd.ident = tl.ident ->
        raise (IdentError (tl.ident, tl.loci, RedefCase hd.loci))
    | Ecase (e0, e1, hd, tl, e2) -> Ucase (uncurry_expr e0, uncurry_expr e1,
        hd.ident, tl.ident, uncurry_expr e2)
    | Edo l -> Udo (List.map uncurry_expr l)
    | Ereturn -> Ureturn in
  {uexpr = ue; locu = e.loce}

(* Vérifie que les variables définies ont des identifiants différents *)
and uncurry_list_def ast primitives =
  let rec aux env = function
    | []  -> []
    | ({ident=name; loci=loc},_,_)::q when S.mem name primitives ->
        raise (IdentError (name, loc, RedefPrimitive))
    | ({ident=name; loci=loc}, args, body)::q -> try
        let first_def = M.find name env in
        raise (IdentError (name, loc, RedefVar first_def))
      with Not_found ->
        (name, {uexpr = Ulambda (uncurry_args args, uncurry_expr body);
            locu = loc})::(aux (M.add name loc env) q) in
  aux M.empty ast

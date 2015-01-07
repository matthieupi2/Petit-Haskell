
open Ast
open TypedAst
open Error

(* phase 1 : on calcule les variables libres *)

type vdef = ident * vvexpr

and vvexpr = {vexpr : vexpr; var_libres : ident list}

and vexpr =
  | Vident of ident
  | Vcst of constant
  | Vemptylist
  | Vappli of vvexpr * vvexpr
  | Vlambda of ident * vvexpr
  | Vbinop of binop * vvexpr * vvexpr
  | Vif of vvexpr * vvexpr * vvexpr
  | Vlet of vdef list * vvexpr
  | Vcase of vvexpr * vvexpr * ident * ident * vvexpr
  | Vdo of vvexpr list
  | Vreturn

let rec union_list l1 l2 = match l1 with
  | [] -> l2
  | a::q -> if List.mem a l2 then union_list q l2 else union_list q (a::l2)

let rec var_libre_expr = function
  | Tident i -> {vexpr = Vident i; var_libres = [i]}
  | Tcst c -> {vexpr = Vcst c; var_libres = []}
  | Tlist [] -> {vexpr = Vemptylist; var_libres = []}
  | Tlist l -> let a = Tvar (V.create ()) in
    let aux t q =
      {texpr = Tbinop (Bcol, t, q); typ = a} in
    let te'= List.fold_right aux l {texpr = Tlist []; typ = a} in
    var_libre_expr te'.texpr
  | Tappli (t1, t2) -> 
    let v1 = var_libre_expr t1.texpr in
    let v2 = var_libre_expr t2.texpr in
    { vexpr = Vappli (v1, v2);
      var_libres = union_list v1.var_libres v2.var_libres}
  | Tlambda ([], t) -> var_libre_expr t.texpr
  | Tlambda ((a::q), t) -> let v = var_libre_expr (Tlambda(q, t)) in
    { vexpr = Vlambda (a, v);
      var_libres = (List.filter (fun x -> x<>a) v.var_libres)}
  | Tbinop (o, t1, t2) -> 
    let v1 = var_libre_expr t1.texpr in
    let v2 = var_libre_expr t2.texpr in
    { vexpr = Vbinop (o, v1, v2); 
      var_libres = union_list v1.var_libres v2.var_libres}
  | Tif (t1, t2, t3) ->
      let v1 = var_libre_expr t1.texpr in
      let v2 = var_libre_expr t2.texpr in
      let v3 = var_libre_expr t3.texpr in
      { vexpr = Vif (v1, v2, v3);
        var_libres = union_list v1.var_libres
            (union_list v2.var_libres v3.var_libres)}
  | Tlet (ltdef, t) -> let v = var_libre_expr t.texpr in
    let aux = fun (lvdef, var_libres, new_vars) (x, t) ->
      let v = var_libre_expr t.texpr in
      ((x, v)::lvdef, union_list v.var_libres var_libres, x::new_vars) in
    let (lvdef, var_libres, new_vars) = 
      List.fold_left aux ([], v.var_libres, []) ltdef  in
    { vexpr = Vlet (lvdef, v) ;
      var_libres = List.filter (fun x -> not (List.mem x new_vars)) var_libres }
  | Tcase (t1, t2, i1, i2, t3) ->
      let v1 = var_libre_expr t1.texpr in
      let v2 = var_libre_expr t2.texpr in
      let v3 = var_libre_expr t3.texpr in
      let v3var = List.filter (fun x -> (x<>i1 && x<>i2)) v3.var_libres in
      { vexpr = Vcase (v1, v2, i1, i2, v3);
        var_libres = union_list v1.var_libres (union_list v2.var_libres v3var)}
  | Tdo l -> let aux lv x =
      let v = var_libre_expr x.texpr in
      match lv.vexpr with
			  | Vdo l1 -> { vexpr = Vdo (v::l1);
                      var_libres = union_list v.var_libres lv.var_libres}
        | _ -> raise (CompilerError "during transformation from Tdo to Vdo") in
    List.fold_left aux {vexpr = Vdo []; var_libres = []} l
  | Treturn -> {vexpr = Vreturn; var_libres = []}
  
let var_libre_def (i, tt) = (i, var_libre_expr tt.texpr)

let var_libre ltdef =
  List.map var_libre_def ltdef

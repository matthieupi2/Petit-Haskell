
(* Programme principal :
 * prend un argument un fichier <name_of_file>.hs contenant du code Petit
 * Haskell et le compile en créant (ou modifie si existant) un fichier
 * <name_of_file>.s contenant le code MIPS du programme compilé *)

(* Les lignes commençant par (**) sont utilisés pour le débogage et inutiles
 * pour le bon fonctionnement du compilateur. *)

open Format
open Lexing
open Parser
open Ast
open UncurriedAst
open TypedAst
open FreeVarsAst
open ClosureAst
open AllocatedAst
open Compile
open Error
(**) open Print

(* Définition des options proposées pour l'exécution *)

let usage = "usage : petitghc [options] file.hs"

let opt_parse_only = ref false
let opt_type_only = ref false

let spec = [
  "--parse-only", Arg.Set opt_parse_only, " stop after parsing" ;
  "--type-only", Arg.Set opt_type_only, " stop after typing" ]
(**) @Print.spec

(* Ouverture du fichier à compiler *)
let rfile =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".hs") then
      raise (Arg.Bad "no .hs extension") ;
    file := Some s in
  Arg.parse spec set_file usage ;
  match !file with
    | Some f -> f
    | None -> Arg.usage spec usage ; exit 1

 (* Fichier d'écriture *)
let wfile = Filename.chop_suffix rfile ".hs" ^ ".s"

(* Ecriture du code Mips *)
let write_code code_mips =
  let f = open_out wfile in
  let fmt = formatter_of_out_channel f in
  Mips.print_program fmt code_mips; 
  fprintf fmt "@?"; 
  close_out f

(* Fonction principale *)
(* Il y a plusieurs niveaux d'erreurs car l'impression de celles-ci nécessite
 * des données créées durant la compilation *)
let () =
  try
    let c = open_in rfile in
    let lb = from_channel c in 
    try
      let ast = file Lexer.next_tokens lb in
      close_in c ;
      try
        if !opt_parse_only then
          exit 0 ;
        let uncurried_ast = uncurry_list_def ast Primitives.getNames in
        let typed_ast = type_ast uncurried_ast Primitives.getEnv in
        if !opt_type_only then
          exit 0 ;
        let free_vars_ast = var_libre typed_ast in
        let closure_ast = ferm free_vars_ast in
        let allocated_ast = alloc closure_ast Primitives.getNames in
        let code_mips = compile_program allocated_ast Primitives.primitives in
        write_code code_mips
      with e -> Error.error rfile e
    with e -> Error.error_before_parsing rfile lb e ;
  with e -> Error.error_at_beginning e

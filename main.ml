
(* Programme principal *)

open Format
open Lexing

let usage = "usage : petitghc [options] file.hs"

let spec = []

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".hs") then
      raise (Arg.Bad "no .hs extension") ;
    file := Some s in
  Arg.parse spec set_file usage ;
  match !file with
    | Some f -> f
    | None -> Arg.usage spec usage ; exit 1

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in 
  let rec digere_lexer () = match Lexer.next_tokens lb with
    | _ -> digere_lexer () in
  digere_lexer () ;
  close_in c ;
  exit 0

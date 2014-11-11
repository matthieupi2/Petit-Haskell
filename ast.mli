
(* Arbre de syntaxe abstraite de Petit Haskell *)

type constant =
  | Cint of int
  | Cchar of char
  | Cstr of string
  | Cbool of bool

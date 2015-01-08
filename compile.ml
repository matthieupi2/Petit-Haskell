
open Format
(*open Ast*)
open AllocatedAst
open Mips


(******************************************************************************)
(* phase 4 : production de code *)

let numlbl = ref 0

let pushn n = sub sp sp oi n

let rec compile_expr = function
  | _ ->nop
(*| CVar adr -> lw a0 areg(adr, fp)
 
  (*| CGvar i -> la a0 alab i*)

  | CInt i -> li v0 9 ++ li a0 8 ++ syscall ++
              sw zero areg(0, v0) ++ li a0 i ++ sw a0 areg(4, v0) ++ move a0 v0

  | CChar c -> li v0 9 ++ li a0 8 ++ syscall ++
               sw zero areg(0, v0) ++ li a0 (int_of_char c) ++ sw a0 areg(4, v0) ++ move a0 v0

  | CBool b -> li v0 9 ++ li a0 8 ++ syscall ++
              sw zero areg(0, v0) ++ li a0 (if b then 1 else 0) ++ sw a0 areg(4, v0) ++ move a0 v0
(*
  | CChar c -> li v0 9 ++ li a0 8 ++ syscall ++
               sw zero areg(0, v0) ++ la a0 alab ("_char_" ^ (string_of_int (int_of_char c))) ++
               sw a0 areg(4, v0) ++ move a0 v0

  | CBool b -> li v0 9 ++ li a0 8 ++ syscall ++
               sw zero areg(0, v0) ++ la a0 alab (if b then "_bool_True" else "_bool_False") ++
               sw a0 areg(4, v0) ++ move a0 v0
*)
  | CList [] -> li v0 9 ++ li a0 8 ++ syscall ++
                sw zero areg(0, v0) ++ sw zero areg(4, v0) ++ move a0 v0

  | CList (a::q) -> let code = compile_expr (CList q) in
                    code ++ push a0 ++ compile_expr a ++ move a1 a0 ++ pop a2 ++
                    li v0 9 ++ li a0 12 ++ syscall ++ li a0 1 ++
                    sw a0 areg(0, v0) ++ sw a1 areg(4, v0) ++ sw a2 areg(8, v0) ++ move a0 v0

  | CAppli (e1, e2) -> let code_e1 = compile_expr e1 in
                       let code_e2 = compile_expr e2 in
                       code_e1 ++ push ra ++ jal "_force" ++ pop ra ++ push a0 ++
                       code_e2 ++ pop a1 ++ lw t0 areg(4, a1) ++ jalr t0

  | CClos (f, l) -> let c, n1 = List.fold_left (fun (code,n) y ->
                                                code ++ lw a0 areg(y, fp) ++ sw a0 areg(n, v0)
                                                , n+4 )
                                               (nop, 8) l in
                    li a0 (4*(List.length l) + 8) ++ li v0 9 ++ syscall ++
                    li a0 2 ++ sw a0 areg(0, v0) ++ la a0 alab f ++ sw a0 areg(4, v0) ++ c ++
                    move a0 v0

  | CBinop (o, e1, e2) -> 
      numlbl := !numlbl + 2;
      let s1 = ("_lbl_" ^ (string_of_int (!numlbl-1))) in
      let s2 = ("_lbl_" ^ (string_of_int (!numlbl))) in
      let code_e1 = compile_expr e1 in
      let code_e2 = compile_expr e2 in
      ( match o with 
        | Band | Bor | Bcol -> (
        if o = Bcol then (
          code_e1 ++ push a0 ++ code_e2 ++ pop a1 ++ move a2 a0 ++
          li a0 12 ++ li v0 9 ++ syscall ++ li t0 1 ++ sw t0 areg(0, v0) ++ sw a1 areg(4, v0) ++ sw a2 areg(8, v0) ++ move a0 v0
         )
        else (
          code_e1 ++ push ra ++ jal "_force" ++ pop ra ++ lw a1 areg (4, a0) ++(
          match o with
            | Band -> li a2 0 ++ beq a2 a1 s1 ++ code_e2 ++ beq zero zero s2 ++ label s1 ++ li a0 8 ++ li v0 9 ++ syscall ++ sw zero areg(0, v0) ++ sw a2 areg(4, v0) ++ move a0 v0 ++ label s2
            | Bor -> li a2 1 ++ beq a2 a1 s1 ++ code_e2 ++ beq zero zero s2 ++ label s1 ++ li a0 8 ++ li v0 9 ++ syscall ++ sw zero areg(0, v0) ++ sw a2 areg(4, v0) ++ move a0 v0 ++ label s2
            | _ -> failwith "Erreur 2"
          ))
        )
        | _ -> (
        code_e1 ++ push a0 ++ code_e2 ++ push ra ++ jal "_force" ++
        lw a1 areg (4, sp) ++ sw a0 areg (4, sp) ++ move a0 a1 ++ jal "_force" ++
        pop ra ++ pop a1 ++ lw a0 areg (4, a0) ++ lw a1 areg (4, a1) ++ (
        match o with
          | Badd -> add a2 a0 oreg a1
          | Bsub -> sub a2 a0 oreg a1
          | Bmul -> mul a2 a0 oreg a1
          | Blt -> li a2 1 ++ blt a0 a1 s ++ li a2 0 ++ label s1
          | Bleq -> li a2 1 ++ ble a0 a1 s ++ li a2 0 ++ label s1
          | Bgt -> li a2 1 ++ bgt a0 a1 s ++ li a2 0 ++ label s1
          | Bgeq -> li a2 1 ++ bge a0 a1 s ++ li a2 0 ++ label s1
          | Beq -> li a2 1 ++ beq a0 a1 s ++ li a2 0 ++ label s1
          | Bneq -> li a2 1 ++ bne a0 a1 s ++ li a2 0 ++ label s1
          | _ -> failwith "Erreur 1"
      ) ++
      li a0 8 ++ li v0 9 ++ syscall ++
      sw zero areg(0, v0) ++ sw a2 areg(4, v0) ++ move a0 v0 ) )
  
  | CIf (e1, e2, e3) ->
      numlbl := !numlbl + 2;
      let s1 = ("_lbl_" ^ (string_of_int (!numlbl-1))) in
      let s2 = ("_lbl_" ^ (string_of_int (!numlbl))) in
      let code_e1 = compile_expr e1 in
      let code_e2 = compile_expr e2 in
      let code_e3 = compile_expr e3 in
      code_e1 ++ push ra ++ jal "_force" ++ pop ra ++ lw a1 areg (4, a0) ++ la a2 alab "_bool_True"
      ++ beq a1 a2 s1 ++ code_e2 ++ beq zero zero s2 ++ label s1 ++ code_e3 ++ label s2

  | CLet (adr, e1, e2) -> let code_e1 = compile_expr e1 in
                          let code_e2 = compile_expr e2 in
                          code_e1 ++ sw a0 areg(adr, fp) ++ code_e2

  | CCase (e1, e2, adr1, adr2, e3) -> 
      numlbl := !numlbl + 2;
      let s1 = ("_lbl_" ^ (string_of_int (!numlbl-1))) in
      let s2 = ("_lbl_" ^ (string_of_int (!numlbl))) in
      let code_e1 = compile_expr e1 in
      let code_e2 = compile_expr e2 in
      let code_e3 = compile_expr e3 in
      code_e1 ++ push ra ++ jal "_force" ++ pop ra ++ lw a1 areg (0, a0) ++
      bne a1 zero s1 ++ code_e2 ++ beq zero zero s2 ++ label s1 ++
      lw a1 areg(4, a0) ++ lw a2 areg(8, a0) ++ sw a1 areg(adr1, fp) ++ sw a2 areg(adr2, fp) ++
      code_e3 ++ label s2
 
  | CDo l -> List.fold_left (fun c e -> compile_expr e ++ c) nop l

  | CReturn -> nop

  | CGlacon (f, l) -> let c, n1 = List.fold_left (fun (code,n) y ->
                                                  code ++ lw a0 areg(y, fp) ++ sw a0 areg(n, v0)
                                                  , n+4 )
                                                 (nop, 8) l in
                      li a0 8 ++ li v0 9 ++ syscall ++
                      sw v0 alab ("_adr" ^ f) ++
                      li a0 3 ++ sw a0 areg(0, v0) ++ move a2 v0 ++
                      li a0 (4*(List.length l) + 8) ++ li v0 9 ++ syscall ++ sw v0 areg(4, a2) ++
                      li a0 2 ++ sw a0 areg(0, v0) ++ la a0 alab f ++ sw a0 areg(4, v0) ++
                      push a2 ++ c ++
                      pop a0

*)

let compile_def (codefun, codemain) = function
  | _ -> nop, nop
(*| CDef (x, e, fpmax) ->
      let code = compile_expr e in
      let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
      let code =
        pre ++ code ++ post ++ sw a0 alab x
      in
      codefun, codemain ++ code

  | CFun (f, nvars, e, fpmax) ->
      let code = compile_expr e in
      let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
      let vars = ref (sw a0 areg(-4, fp)) in
      for i=2 to (nvars+1) do
        vars := !vars ++ lw t0 areg(4*i, fp) ++ sw t0 areg((-4)*i, fp)
      done;
      let code =
        label f ++
        push fp ++ push ra ++
        move fp sp ++ pre ++ !vars ++
        code ++
        post ++
        pop ra ++ pop fp ++ jr ra
      in
      code ++ codefun, codemain
  
  | CCodeglacon (f, nvars, e, fpmax) ->
      let code = compile_expr e in
      let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
      let vars = ref nop in
      for i=2 to (nvars+1) do
        vars := !vars ++ lw t0 areg(4*i, fp) ++ sw t0 areg((-4)*i+4, fp)
      done;
      let code =
        label f ++
        push fp ++ push ra ++
        move fp sp ++ pre ++ !vars ++
        code ++
        post ++
        push ra ++ jal "_force" ++ pop ra ++
        pop ra ++ pop fp ++
        move a2 a0 ++ li a0 8 ++ li v0 9 ++ syscall ++
        li a1 4 ++ sw a1 areg(0, v0) ++ sw a2 areg(4, v0) ++ sw v0 alab ("_adr" ^ f) ++
        move a0 a2 ++ jr ra
      in
      code ++ codefun, codemain

  | CMain (e, fpmax) ->
      let code = compile_expr e in
      let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
      let code =
        pre ++ code ++ post
      in
      codefun, codemain ++ code
  

(*
  | Set (x, e, fpmax) ->
    let code = compile_expr e in
    let code =
      let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
      pre ++ code ++ pop a0 ++ sw a0 alab x ++ post in
    codefun, codemain ++ code

  | Fun (f, e, fpmax) ->
    let code = compile_expr e in
    let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
    let code =
      label f ++
      push fp ++ push ra ++
      move fp sp ++ pre ++
      code ++
      pop a0 ++ post ++
      pop ra ++ pop fp ++ push a0 ++ jr ra
    in
    code ++ codefun, codemain

| Print (e, fpmax) ->
    let code = compile_expr e in
    let code =
      let pre, post = if fpmax > 0 then pushn fpmax, popn fpmax else nop, nop in
      pre ++ code ++ pop a0 ++ post ++ jal "print"
    in
    codefun, codemain ++ code
*)
*)
let compile_program p ofile =
  let codefun, code = List.fold_left compile_def (nop, nop) p in
  let p =
    { text =
        label "main" ++
        move fp sp ++
        code ++
        li v0 10 ++ (* exit *)
        syscall ++
        codefun ++
        (*********div*********)
        
        (*********rem*********)
        (*********putChar*********)
        (*********error*********)
        (*********force*********)
        label "_force" ++
        lw t0 areg(0, a0) ++
        li t1 2 ++
        bgt t0 t1 "_force_1" ++
        jr ra ++
        label "_force_1" ++
        li t1 3 ++
        beq t0 t1 "_force_2" ++
        lw a0 areg(4, a0) ++
        push ra ++ jal "_force" ++ pop ra ++
        jr ra ++
        label "_force_2" ++
        push ra ++
        lw a1 areg(4, a0) ++
        lw a2 areg(4, a1) ++
        jalr a2 ++
        pop ra ++
        jr ra;
      data =
        let chars = ref nop in
        for i=32 to 126 do
          if i<>34 && i<>92 then (
            chars := !chars ++ inline ("_char_" ^ (string_of_int i) ^ ":\n") ++ asciiz_c (char_of_int i) )
        done;
        (*Hashtbl.fold (fun x _ l -> label x ++ dword [1] ++ l) genv
          (label "newline" ++ asciiz "\n") ++*) 
        !chars ++
        label "_char_9" ++ asciiz "\t" ++
        label "_char_10" ++ asciiz "\n" ++
        label "_char_34" ++ asciiz "\"" ++
        label "_char_92" ++ asciiz "\\" ++
        label "_bool_True" ++ asciiz "True" ++
        label "_bool_False" ++ asciiz "False"
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  Mips.print_program fmt p;
  fprintf fmt "@?";
  close_out f

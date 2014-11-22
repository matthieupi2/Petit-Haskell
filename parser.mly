%{
  open Ast
%}

%token <string> IDENT0 IDENT1
%token <Ast.constant> CST
%token ELSE IF IN LET CASE OF THEN RETURN DO
%token LB RB LSB RSB LCB RCB
%token ARROW SEMI COLON COMMA LAMBDA ASSIGN UNIT
%token LT LEQ GT GEQ EQ NEQ
%token PLUS MINUS TIMES OR AND
%token NEG
%token EOF

%nonassoc IN
%nonassoc ELSE
%nonassoc ARROW
%left OR
%left AND
%left LT LEQ GT GEQ EQ NEQ
%right COLON
%left PLUS MINUS
%left TIMES
%nonassoc NEG
%left fun_appli

%start file

%type <Ast.def list> file

%%

file:
  | ldef0=def0* EOF { ldef0 } ;

def0:
  | s0=IDENT0 ls1=IDENT1* ASSIGN e=expr { (s0, ls1, e) } ;

(* TODO implémenter list *)
simple_expr:
  | LB e=expr RB  { e }
  | s1=IDENT1     { Eident s1 }
  | c=CST         { Ecst c } ;

(* TODO simple_expr *)
(* TODO lambda prend un seul argument *)
(* TODO Ereturn = Ecst Cunit *)
expr:
  | se=simple_expr                        { se }
  | e0=expr o=op e1=expr                  { Ebinop (o, e0, e1) }
  | NEG e=expr                            { Ebinop (Bsub, Ecst (Cint 0), e) }
  | LAMBDA args=IDENT1* ARROW e=expr      { Elambda (args, e) } 
  | IF cdt=expr THEN e1=expr ELSE e2=expr { Eif (cdt, e1, e2) }
  | RETURN UNIT                           { Ereturn } ;

%inline op:
  | PLUS  { Badd }
  | MINUS { Bsub }
  | TIMES { Bmul }
  | AND   { Band }
  | OR    { Bor  }
  | COLON { Bcol }
  | LT    { Blt  }
  | LEQ   { Bleq }
  | GT    { Bgt  }
  | GEQ   { Bgeq }
  | EQ    { Beq  }
  | NEQ   { Bneq } ;

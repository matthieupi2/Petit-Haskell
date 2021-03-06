
/* Analyseur syntaxique pour Petit Haskell */

%{
  open Ast
  open Error
%}

%token <string> IDENT0 IDENT1
%token <Ast.constant> CST
%token <Ast.expr> STRING
%token ELSE IF IN LET CASE OF THEN RETURN DO
%token LB RB LSB RSB LCB RCB
%token ARROW SEMI COLON COMMA LAMBDA ASSIGN
%token LT LEQ GT GEQ EQ NEQ
%token PLUS MINUS TIMES OR AND
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
%nonassoc neg
/* %left fun_appli (inutile d'après menhir) */

%start file

%type <Ast.file> file

%%

file:
  | ldef0=def0* EOF { ldef0 }
  | error           { raise (ParserError "unknown error") } ;

def0:
  | s0=lident0 ls1=lident1* ASSIGN e=lexpr { (s0, ls1, e) } ;

def:
  | s1=lident1 ls1=lident1* ASSIGN e=lexpr { (s1, ls1, e) } ;

lident0:
  | s=IDENT0 { {ident = s; loci = $startpos, $endpos} } ;

lident1:
  | s=IDENT1 { {ident = s; loci = $startpos, $endpos} } ;

lsimple_expr:
  | se=simple_expr  { {expr = se; loce = $startpos, $endpos} } ;

simple_expr:
  | LB e=expr RB                            { e }
  | s1=IDENT1                               { Eident s1 }
  | c=CST                                   { Ecst c }
  | e=STRING                                { e }
  | LSB l=separated_list(COMMA, lexpr) RSB  { Elist l } ;

lexpr:
  | e=expr { {expr = e; loce = $startpos, $endpos} } ;

expr:
  | se=simple_expr                           { se }
  | se=lsimple_expr args=lsimple_expr+ /* %prec fun_appli */
                                             { Eappli (se, args) }
  | LAMBDA args=lident1+ ARROW e=lexpr       { Elambda (args, e) } 
  | MINUS e=lexpr %prec neg                  { Ebinop (Bsub,
      {expr = Ecst (Cint 0); loce = undef_loc}, e) }
  | e0=lexpr o=op e1=lexpr                   { Ebinop (o, e0, e1) }
  | IF cdt=lexpr THEN e1=lexpr ELSE e2=lexpr { Eif (cdt, e1, e2) }
  | LET l=liaisons IN e=lexpr                { Elet (l, e) }
  | CASE e=lexpr OF LCB LSB RSB ARROW e0=lexpr SEMI hd=lident1 COLON tl=lident1
    ARROW e1=lexpr SEMI? RCB                 { Ecase (e, e0, hd, tl, e1) }
  | DO LCB ld=do_list RCB                    { Edo ld }
  | RETURN LB RB                             { Ereturn } ;

liaisons:
  | d=def                 { [d] }
  | LCB ld=def_list RCB   { ld } ;

def_list:
  | d=def SEMI ld=def_list  { d::ld }
  | d=def SEMI?             { [d] } ;

do_list:
  | e=lexpr SEMI le=do_list  { e::le }
  | e=lexpr SEMI?            { [e] } ;

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

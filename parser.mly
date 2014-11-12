%{
  open Ast
%}

%token <string> IDENT0
%token <string> IDENT1
%token <Ast.constant> CST
%token ELSE IF IN LET CASE OF THEN RETURN DO
%token LP RP LB RB LSB RSB ARROW SEMI COLON COMMA
%token LT LEQ GT GEQ EQ NEQ
%token PLUS MINUS TIMES OR AND
%token NEG
%token EOF

%start file

%type <unit> file

%%

file:
 | EOF {()};

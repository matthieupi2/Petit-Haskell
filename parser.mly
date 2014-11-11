%{

%}

%token CINT CCAR CSTR CTRUE CFALSE
%token ELSE FI IN LET CASE OF THEN RETURN DO
%token LP RP LB RB LSB RSB ARROW SEMI COLON COMMA
%token LT LEQ GT GEQ EQ NEQ
%token PLUS MINUS TIMES OR AND
%token EOF

%start file

%type <unit> file

%%

file:
 | EOF {()};

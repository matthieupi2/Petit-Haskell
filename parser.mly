%{

%}

%token EOF

%start file

%type <unit> file

%%

file:
 | EOF {()};

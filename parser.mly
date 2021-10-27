%token <int> INT
%token <string> VAR
%token EOF LPAR RPAR COMMA COLON SEMICOLON ARROW

%start start

%%

start:
  |expr EOF

%{
  open Ast
%}

%token <int> INT
%token <string> STRING IDENT
%token LPAREN RPAREN
%token ADD SUB MUL DIV
%token COMMA HASH EQUAL SEMI
%token TRUE FALSE IF THEN ELSE PRINT LET
%token EOF

%type <Ast.program> program
%type <Ast.expr> expr

%left ADD SUB
%left MUL DIV

%start program

%%

program:
  | expr EOF { {expr = $1} }

seq:
  | expr { [$1] }
  | seq SEMI expr { $1 @ [$3] }

tuple:
  | expr { [$1] }
  | tuple COMMA expr { $1 @ [$3] }

primary:
  | INT { INT $1 }
  | TRUE { BOOL true }
  | FALSE { BOOL false }
  | STRING { STRING $1 }
  | IDENT { VAR $1 }
  | LET IDENT EQUAL expr { LET ($2, $4) }
  | PRINT expr { PRINT $2 }
  | IF expr THEN expr ELSE expr { IF ($2, $4, $6) }
  | LPAREN expr RPAREN { $2 }
  | LPAREN tuple RPAREN { TUPLE $2 }
  | LPAREN seq RPAREN { SEQ $2 }
  | LPAREN RPAREN { UNIT }

field:
  | primary { $1 }
  | HASH INT primary { FIELD ($2, $3) }

binop:
  | field { $1 }
  | expr ADD expr { BINOP (ADD, $1, $3) }
  | expr SUB expr { BINOP (SUB, $1, $3) }
  | expr MUL expr { BINOP (MUL, $1, $3) }
  | expr DIV expr { BINOP (DIV, $1, $3) }

expr:
  | binop { $1 }

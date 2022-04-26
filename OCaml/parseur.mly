%{
	open AST
%}

%token <int> NOMBRE
%token <bool> BOOLEAN
%token PLUS MOINS FOIS GPAREN DPAREN EOL MOD GR_ST LE_ST EQ 
%left PLUS MOINS
%left FOIS
%left MOD
%left EQ 
%left GR_ST LE_ST
%left BOOL_NEG
%nonassoc UMOINS
%type <unit> main expression
%start main
%type <AST.expression_a> main expression
%%

main:
  expression { $1 }
;
expression:
    expression PLUS expression { Plus ($1,$3) }
  | expression MOINS expression { Moins($1,$3) }
  | expression FOIS expression { Mult ($1,$3) }
  | expression GR_ST expression { GrSt ($1, $3)}
  | expression LE_ST expression {LeSt ($1, $3)}
  | GPAREN expression DPAREN { $2 }
  | BOOLEAN { Bool $1 }
  | BOOL_NEG { BoolNeg ($)}
  | MOINS expression %prec UMOINS { Neg $2 }
  | NOMBRE { Num $1 }
;
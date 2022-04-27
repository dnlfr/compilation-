%{
    open AST
%}

%token <float> NOMBRE
%token <bool> BOOLEAN
%token PLUS MOINS FOIS GPAREN DPAREN EOL MOD PT_VIRG GR_ST LE_ST EQ BOOL_NEG 
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
  expression PT_VIRG { $1 }
;
expression:
  expression PLUS expression { Plus ($1,$3) }
  | PT_VIRG { Pt_Virg () }
  | expression MOINS expression { Moins($1,$3) }
  | expression FOIS expression { Mult ($1,$3) }
  | expression MOD expression { Mod ($1, $3) }
  | expression GR_ST expression { GrSt ($1, $3)}
  | expression LE_ST expression {LeSt ($1, $3)}
  | expression EQ expression {Eq ($1, $3)}
  | GPAREN expression DPAREN { $2 }
  | BOOLEAN { Bool $1 }
  | BOOL_NEG expression { BoolNeg ($2)}
  | MOINS expression %prec UMOINS { Neg $2 }
  | NOMBRE { Num $1 }
;
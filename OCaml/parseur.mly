%{
    open AST
%}

%token <float> NOMBRE
%token PLUS MOINS FOIS GPAREN DPAREN EOL MOD PT_VIRG
%left PLUS MOINS
%left FOIS
%left MOD
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
  | GPAREN expression DPAREN { $2 }
  | expression MOD expression { Mod ($1, $3) }
  | MOINS expression %prec UMOINS { Neg $2 }
  | NOMBRE { Num $1 }
;
%{
    open AST
%}

%token <float> NOMBRE
%token <bool> BOOLEAN
%token <string> VAR
%token PLUS MOINS FOIS GPAREN DPAREN EOL MOD GR_ST GR_EQ LE_EQ LE_ST EQ BOOL_NEG PT_VIRG TERC TERS AND OR AFFECT IF ELSE 
      GBRACKET DBRACKET DO WHILE FOR NOT_EQ
%token <string> NAN
%left PLUS MOINS
%left FOIS
%left MOD
%left EQ NOT_EQ
%left AND OR 
%left AFFECT
%left GR_ST LE_ST LE_EQ GR_EQ
%left BOOL_NEG
%left TERC
%nonassoc UMOINS
%start main commande
%type <AST.programme_a> main programme
%type <AST.commande_a> commande 
%type <AST.expression_a> expression
%%

main:
  commande programme { Prog ($1, $2, 0) }
  | commande { Cmd ($1, 0)}
;
programme: 
  commande programme { Prog ($1, $2, 0) }
  | commande { Cmd ($1, 0) }
;
commande: 
  expression PT_VIRG { Expr ($1, 0) }
  | PT_VIRG { Pt_Virg ((), 0) }
  | GBRACKET programme DBRACKET { Seq ($2, 0) }
  | IF GPAREN expression DPAREN commande ELSE commande { IfElse ($3, $5, $7, 0) }
  | DO commande WHILE expression { DoWhile($2, $4, 0) }
  | WHILE GPAREN expression DPAREN commande { While ($3, $5, 0) }
  | FOR GPAREN expression PT_VIRG expression PT_VIRG expression DPAREN commande { For ($3, $5, $7, $9, 0) }
;
expression:
    expression PLUS expression { Plus ($1,$3, 0) }
  | expression MOINS expression { Moins($1,$3, 0) }
  | expression FOIS expression { Mult ($1,$3, 0) }
  | expression MOD expression { Mod ($1, $3, 0) }
  | expression GR_ST expression { GrSt ($1, $3, 0)}
  | expression LE_ST expression { LeSt ($1, $3, 0)}
  | expression LE_EQ expression { LeEq ($1, $3, 0)}
  | expression GR_EQ expression { GrEq ($1, $3, 0)}
  | expression EQ expression {Eq ($1, $3, 0)}
  | expression NOT_EQ expression {NotEq ($1, $3, 0) }
  | expression TERC expression TERS expression {Ter ($1, $3, $5, 0) } /* ternary operator */
  | expression AND expression { And ($1, $3, 0) }
  | expression OR expression { Or ($1, $3, 0) }
  | VAR AFFECT expression { Affect ($1, $3, 0)}
  | GPAREN expression DPAREN { $2 } /* Parenthesage, pas besoin de definir la taille */
  | NAN { NaN ($1, 0) }
  | BOOLEAN { Bool ($1, 0) }
  | BOOL_NEG expression { BoolNeg ($2, 0)}
  | MOINS expression %prec UMOINS { Neg ($2, 0) }
  | VAR { Var ($1, 0)}
  | NOMBRE { Num ($1, 0) }
;
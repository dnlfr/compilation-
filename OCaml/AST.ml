type expression_a =
  | Plus    of expression_a * expression_a * int
  | Moins   of expression_a * expression_a * int
  | Mult    of expression_a * expression_a * int
  | Div     of expression_a * expression_a * int
  | Neg     of expression_a * int
  | Num     of float * int
  | Mod     of expression_a * expression_a * int
  | Eq      of expression_a * expression_a  * int
  | GrSt    of expression_a * expression_a * int
  | LeSt    of expression_a * expression_a * int
  | BoolNeg of expression_a * int
  | Bool    of bool * int
  | NaN     of string  * int
  | Ter     of expression_a * expression_a * expression_a * int
  | And     of expression_a * expression_a * int
  | Or      of expression_a * expression_a * int
  | Var     of string * int
  | Affect  of string * expression_a * int

and commande_a = 
  | Expr    of expression_a * int 
  | Pt_Virg of unit * int
  | IfElse  of expression_a * commande_a * commande_a * int

and programme_a = 
  | Prog    of commande_a * programme_a * int
  | Cmd     of commande_a * int
;;


(* Ajout de la taille *)
let rec sizeOf_exp exp = match exp with 
  | Plus    (g,d,t) -> t
  | Moins   (g,d,t) -> t
  | Mult    (g,d,t) -> t
  | Div     (g,d,t) -> t
  | Mod     (g,d,t) -> t
  | Neg     (e,t)   -> t
  | Num     (n,t)   -> 1
  | Eq      (g,d,t) -> t
  | GrSt    (g,d,t) -> t
  | LeSt    (g,d,t) -> t
  | BoolNeg (e,t)   -> t
  | Bool    (b,t)   -> 1
  | NaN     (n,t)   -> 1
  | And     (g,d,t) -> t
  | Or      (g,d,t) -> t
  | Var     (v,t) -> 1 
  | Affect  (v,e,t) -> t   
  | Ter     (exp1,exp2,exp3, t) -> t

and sizeOf_cmd cmd = match cmd with 
  | Expr    (e,t) -> t
  | Pt_Virg (u,t) -> t
  | IfElse  (e,cmd1,cmd2,t) -> t

and sizeOf_prog prog = match prog with 
  | Prog    (c,p,t) -> t
  | Cmd     (c,t) -> t
;;

(* Fonctions d'affichage *)
let rec print_binaire form s g d t = Format.fprintf form "@[<2>%s%s@ %a%s@ %a%s%i@]" s "(" print_AST_exp g " ," print_AST_exp d " ) taille = " t

and print_AST_exp form = let open Format in function
  | Plus  (g,d,_) -> print_binaire form "Plus" g d (sizeOf_exp g + sizeOf_exp d + 1)
  | Moins (g,d,_) -> print_binaire form "Moins" g d (sizeOf_exp g + sizeOf_exp d + 1)
  | Mult  (g,d,_) -> print_binaire form "Mult" g d (sizeOf_exp g + sizeOf_exp d + 1)
  | Div   (g,d,_) -> print_binaire form "Div" g d (sizeOf_exp g + sizeOf_exp d + 1)
  | Mod   (g,d,_) -> print_binaire form "Mod" g d (sizeOf_exp g + sizeOf_exp d + 1)
  | Num   (n,t)   -> fprintf form "@[<2>%s@ %f%s%i@]" "Num" n " taille Num: " 1 
  | Neg   (e,_)   -> fprintf form "@[<2>%s@ %a%s%i@]" "Neg" print_AST_exp e " taille Neg: " (sizeOf_exp e)
  | Eq    (g,d,_) -> print_binaire form "Eq" g d (sizeOf_exp g + sizeOf_exp d + 1)
  | GrSt  (g,d,_) -> print_binaire form "GrSt" g d (sizeOf_exp g + sizeOf_exp d + 1)
  | LeSt  (g,d,_) -> print_binaire form "LeSt" g d (sizeOf_exp g + sizeOf_exp d + 1)
  | BoolNeg (e,t) -> fprintf form "@[<2>%s@ %a%s%i@]" "BoolNeg" print_AST_exp e " tailleNegBool: " (sizeOf_exp e)
  | Bool  (b,t)   -> fprintf form "@[<2>%s@ %b%s%i@]" "CsteBo" b " taille Bool: " 1
  | NaN   (n,t)   -> fprintf form "@[<2>%s@ %s%s%i@]" "Nan" n " taille NaN: " 1
  | And  (g,d,t) -> print_binaire form "And" g d (sizeOf_exp g + sizeOf_exp d + 1)
  | Or  (g,d,t) -> print_binaire form "Or" g d (sizeOf_exp g + sizeOf_exp d + 1)
  | Var (v,t) -> fprintf form "@[<2>%s@ %s%s%i@]" "Var" v " taille Var: " 1
  | Affect (s,e,t) -> fprintf form "@[<2>%s%s%s@ %a%s%i@]"  "SetVar " s " = " print_AST_exp e " tailleAffect = " (sizeOf_exp e + 1)
  | Ter (exp1, exp2, exp3, t) -> fprintf form "@[<2> %s%a%s%s%a%s%a%s%i%s%i%s%i%s@]"
                                "Ter: " print_AST_exp exp1 ")" "?" print_AST_exp exp2 ":" print_AST_exp exp3 "\n taille Ter = " (sizeOf_exp exp1 + sizeOf_exp exp2 + sizeOf_exp exp3 + 2)
                                " (ConJump : " (sizeOf_exp exp1 + 1)
                                " Jump : " (sizeOf_exp exp2) ")\n"
and print_AST_cmd form = let open Format in function
  | Expr (e,t) -> fprintf form "@[<2>%a %s%i@]" print_AST_exp e " taille Expr = " (sizeOf_exp e)
  | Pt_Virg (u, t) -> fprintf form "[<2>%s@]" "; \n" 
  | IfElse (e,cmd1,cmd2,t) -> fprintf form "@[<2>%s%a%s@ %a@ %s@ %a@ %s%i@ %s%i@ %s%i%s@]"
                                "IF (" print_AST_exp e ") \n"
                                print_AST_cmd cmd1
                                "\n ELSE \n"
                                print_AST_cmd cmd2
                                "\n taille IfElse = " (sizeOf_exp e + sizeOf_cmd cmd1 + sizeOf_cmd cmd2 + 2)
                                " (ConJump : " (sizeOf_cmd cmd1 + 1)
                                " Jump : " (sizeOf_cmd cmd2) ")\n"

and print_AST_prog form = let open Format in function
  | Prog (c,p,t) -> fprintf form "@[<2>%a@ %s@ %a@ %s%i%s@]" print_AST_cmd c "\n" print_AST_prog p " tailleProg = " t "\n"
  | Cmd (c,t) -> fprintf form "@[<2>%a@ %s%i@]" print_AST_cmd c " taille Commande: " t  
;; 

let rec print_post_fixe form g d s = Format.fprintf form "@[<2>%s@ %a%s@ %a%s@ %s@ %s@]" "\n" code_exp g "\n" code_exp d "\n" s "\n"

and code_exp form = let open Format in function
  | Plus  (g,d,t) -> print_post_fixe form g d "AddiNb" 
  | Mult  (g,d,t) -> print_post_fixe form g d "MultNb" 
  | Moins (g,d,t) -> print_post_fixe form g d "SubiNb" 
  | Mod   (g,d,t) -> print_post_fixe form g d "ModuNb" 
  | Div   (g,d,t) -> print_post_fixe form g d "DivNb" 
  | Num   (n,t) -> fprintf form "@[<2>%s@ %f@ %s@]" "CsteNb" n "\n"
  | Neg   (e,t) ->  fprintf form "@[<2>%a@ %s@]" code_exp e "NegaNb\n"
  | Eq    (g,d,t) -> print_post_fixe form g d "Eq"
  | GrSt  (g,d,t) -> print_post_fixe form g d "GrSt"
  | LeSt  (g,d,t) -> print_post_fixe form g d "LeSt"
  | BoolNeg (e,t) -> fprintf form "@[<2>%a@ %s@]" code_exp e "BoolNeg" 
  | Bool  (b,t)   -> fprintf form "@[<2>%s@ %b@]" "CsteBo" b 
  | NaN   (n,t)   -> fprintf form "@[<2>%s%s@]" "Nan" n 
  | And  (g,d,t) -> print_post_fixe form g d "And" 
  | Or   (g,d,t) -> print_post_fixe form g d "Or" 
  | Var (v,t) ->  fprintf form "@[<2>%s@ %s@]" "GetVar" v
  | Affect (s,e,t) -> fprintf form "@[<2>%a@ %s%s%@]" code_exp   e "\n SetVar " s 
  | Ter (exp1, exp2, exp3, t) -> fprintf form "@[<2>%a%s%i@ %s@ %a@ %s%i%s@ %a%s@]"
                              code_exp exp1 "\nConJump " (sizeOf_exp exp2 + 1) "\n"
                              code_exp exp1 
                              "\n Jump " (sizeOf_exp exp3) "\n"
                              code_exp exp3 "\n"
and code_cmd form = let open Format in function 
  | Expr (e,t) -> fprintf form "@[<2>%s%a%s@]" code_exp e "\n"
  | Pt_Virg (u,t) -> fprintf form "@[<2>%s@]" "\n"
  | IfElse (e,cmd1,cmd2,t) -> fprintf form "@[<2>%a%s%i@ %s@ %a@ %s%i%s@ %a%s@]"
                              code_exp e "\nConJump " (sizeOf_cmd cmd1 + 1) "\n"
                              code_cmd cmd1
                              "\n Jump " (sizeOf_cmd cmd2) "\n"
                              code_cmd cmd2 "\n"

and code_prog form = let open Format in function 
  | Prog (c,p,t) -> fprintf form "@[<2>%a@ %s@ %a@ %s@]" code_cmd c "\n" code_prog p "\n" 
  | Cmd (c,t) -> fprintf form "@[<2>%a@ %s@]" code_cmd c "\n"
;;
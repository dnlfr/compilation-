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
  | Seq     of programme_a * int
  | IfElse  of expression_a * commande_a * commande_a * int
  | DoWhile of commande_a * expression_a * int
  | While   of expression_a * commande_a * int
  
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
  | Seq     (p,t) -> t
  | IfElse  (e,cmd1,cmd2,t) -> t
  | DoWhile (c,e,t) ->  t
  | While  (e,c,t) -> t
and sizeOf_prog prog = match prog with 
  | Prog    (c,p,t) -> t
  | Cmd     (c,t) -> t

(* On instaure la taille de toutes les commandes -> Calcul des offsets du cours *)

and caloffset_exp e = match e with 
  | Plus    (g,d,_) -> let e1 = caloffset_exp g in let e2 = caloffset_exp d in Plus(e1, e2, (sizeOf_exp e1 + sizeOf_exp e2 + 1))
  | Moins   (g,d,_) -> let e1 = caloffset_exp g in let e2 = caloffset_exp d in Moins(e1, e2, (sizeOf_exp e1 + sizeOf_exp e2 + 1))
  | Mult    (g,d,_) -> let e1 = caloffset_exp g in let e2 = caloffset_exp d in Mult(e1, e2, (sizeOf_exp e1 + sizeOf_exp e2 + 1))
  | Div     (g,d,_) -> let e1 = caloffset_exp g in let e2 = caloffset_exp d in Div(e1, e2, (sizeOf_exp e1 + sizeOf_exp e2 + 1))
  | Mod     (g,d,_) -> let e1 = caloffset_exp g in let e2 = caloffset_exp d in Mod(e1, e2, (sizeOf_exp e1 + sizeOf_exp e2 + 1))
  | Neg     (e,_)   -> let e1 = caloffset_exp e in Neg(e1, (sizeOf_exp e1 + 1))
  | Num     (n,_)   -> Num(n, 1)
  | Eq      (g,d,_) -> let e1 = caloffset_exp g in let e2 = caloffset_exp d in Eq(e1, e2, (sizeOf_exp e1 + sizeOf_exp e2 + 1))
  | GrSt    (g,d,_) -> let e1 = caloffset_exp g in let e2 = caloffset_exp d in GrSt(e1, e2, (sizeOf_exp e1 + sizeOf_exp e2 + 1))
  | LeSt    (g,d,_) -> let e1 = caloffset_exp g in let e2 = caloffset_exp d in LeSt(e1, e2, (sizeOf_exp e1 + sizeOf_exp e2 + 1))
  | BoolNeg (e,_)   -> let e1 = caloffset_exp e in BoolNeg(e, (sizeOf_exp e1 +1))
  | Bool    (b,_)   -> Bool(b,1)
  | NaN     (n,_)   -> NaN(n,1)
  | And     (g,d,_) -> let e1 = caloffset_exp g in let e2 = caloffset_exp d in And(e1, e2, (sizeOf_exp e1 + sizeOf_exp e2 + 1))
  | Or      (g,d,_) -> let e1 = caloffset_exp g in let e2 = caloffset_exp d in Or(e1, e2, (sizeOf_exp e1 + sizeOf_exp e2 + 1))
  | Var     (v,_) -> Var(v,1)
  | Affect  (v,e,_) -> let e1 = caloffset_exp e in Affect(v, e1, sizeOf_exp e1 +1)
  | Ter     (exp1,exp2,exp3, _) -> let e1 = caloffset_exp exp1 in let e2 = caloffset_exp exp2 in let e3 = caloffset_exp exp3 in Ter(e1, e2, e3, (sizeOf_exp e1 + sizeOf_exp e2 + sizeOf_exp e3 +2))

and caloffset_cmd c = match c with 
  | Expr    (e,_) -> let e1 = caloffset_exp e in Expr(e1, sizeOf_exp e1)
  | Pt_Virg (u,t) -> Pt_Virg(u, 0)
  | Seq     (p,t) -> let p1 = caloffset_prog p in Seq(p1, sizeOf_prog p1)
  | IfElse  (e,cmd1,cmd2,t) -> let e1 = caloffset_exp e in let c1 = caloffset_cmd cmd1 in let c2 = caloffset_cmd cmd2
                              in IfElse(e1,c1,c2,(sizeOf_exp e1 + sizeOf_cmd c1 + sizeOf_cmd c2 + 2)) 
  | DoWhile (c,e,t) ->  let c1 = caloffset_cmd c in let e1 = caloffset_exp e in DoWhile(c1,e1, (sizeOf_cmd c1 + sizeOf_exp e1 +1))
  | While  (e,c,t) -> let e1 = caloffset_exp e in let c1 = caloffset_cmd c in While(e1,c1, (sizeOf_cmd c1 + sizeOf_exp e1 +1))

and caloffset_prog p = match p with 
  | Prog    (c,p,_) -> let c1 = caloffset_cmd c in let p1 = caloffset_prog p in Prog(c1, p1, sizeOf_cmd c1 + sizeOf_prog p1)
  | Cmd     (c,_) -> let c1 = caloffset_cmd c in Cmd(c1, sizeOf_cmd c1)

;;

(* Fonctions d'affichage *)
let rec print_binaire form s g d t = Format.fprintf form "@[<2>%s%s@ %a%s@ %a%s%i@]" s "(" print_AST_exp g " ," print_AST_exp d " ) taille = " t

and print_AST_exp form = let open Format in function
  | Plus  (g,d,t) -> print_binaire form "Plus" g d t
  | Moins (g,d,t) -> print_binaire form "Moins" g d t
  | Mult  (g,d,t) -> print_binaire form "Mult" g d t
  | Div   (g,d,t) -> print_binaire form "Div" g d t
  | Mod   (g,d,t) -> print_binaire form "Mod" g d t
  | Num   (n,t)   -> fprintf form "@[<2>%s@ %f%s%i@]" "Num" n " taille Num: " t
  | Neg   (e,t)   -> fprintf form "@[<2>%s@ %a%s%i@]" "Neg" print_AST_exp e " taille Neg: " t
  | Eq    (g,d,t) -> print_binaire form "Eq" g d t
  | GrSt  (g,d,t) -> print_binaire form "GrSt" g d t
  | LeSt  (g,d,t) -> print_binaire form "LeSt" g d t
  | BoolNeg (e,t) -> fprintf form "@[<2>%s@ %a%s%i@]" "BoolNeg" print_AST_exp e " tailleNegBool: " t
  | Bool  (b,t)   -> fprintf form "@[<2>%s@ %b%s%i@]" "CsteBo" b " taille Bool: " t
  | NaN   (n,t)   -> fprintf form "@[<2>%s@ %s%s%i@]" "Nan" n " taille NaN: " t
  | And  (g,d,t) -> print_binaire form "And" g d t
  | Or  (g,d,t) -> print_binaire form "Or" g d t
  | Var (v,t) -> fprintf form "@[<2>%s@ %s%s%i@]" "Var" v " taille Var: " t
  | Affect (s,e,t) -> fprintf form "@[<2>%s%s%s@ %a%s%i@]"  "SetVar " s " = " print_AST_exp e " tailleAffect = " t
  | Ter (exp1, exp2, exp3, t) -> fprintf form "@[<2> %s%a%s%s%a%s%a%s%i%s%i%s%i%s@]"
                                "Ter: " print_AST_exp exp1 ")" "?" print_AST_exp exp2 ":" print_AST_exp exp3 "\n taille Ter = " t
                                " (ConJump : " (sizeOf_exp exp1 + 1)
                                " Jump : " (sizeOf_exp exp2) ")\n"
and print_AST_cmd form = let open Format in function
  | Expr (e,t) -> fprintf form "@[<2>%a %s%i@]" print_AST_exp e " taille Expr = " t
  | Pt_Virg (u, t) -> fprintf form "[<2>%s@]" "; \n" 
  | Seq (p,t) -> fprintf form "@[<2>%s@ %a%s%i@ ]" "{\n" print_AST_prog p "\n } taille Seq = " t
  | IfElse (e,cmd1,cmd2,t) -> fprintf form "@[<2>%s%a%s@ %a@ %s@ %a@ %s%i@ %s%i@ %s%i%s@]"
                                "IF (" print_AST_exp e ") \n"
                                print_AST_cmd cmd1
                                "\n ELSE \n"
                                print_AST_cmd cmd2
                                "\n taille IfElse = " t
                                " (ConJump : " (sizeOf_cmd cmd1 + 1)
                                " Jump : " (sizeOf_cmd cmd2) ")\n"
  | DoWhile (c,e,t) -> fprintf form "@[<2>%s@ %a%s@ %a%s%i@]" "do \n" print_AST_cmd c "\n while" print_AST_exp e "\n taille DoWhile = " t 
  | While  (e,c,t) ->  fprintf form "@[<2>%s@ %a@ %s@ %a@ %s@ %i@ %s@]" "while \n" print_AST_exp e "\n" print_AST_cmd c "\n taille While = " t "\n"                        

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
  | Affect (s,e,t) -> fprintf form "@[<2>%a@ %s%s@]" code_exp   e "\n SetVar " s 
  | Ter (exp1, exp2, exp3, t) -> fprintf form "@[<2>%a%s%i@ %s@ %a@ %s%i%s@ %a%s@]"
                              code_exp exp1 "\nConJump " (sizeOf_exp exp2 + 1) "\n"
                              code_exp exp1 
                              "\n Jump " (sizeOf_exp exp3) "\n"
                              code_exp exp3 "\n"
and code_cmd form = let open Format in function 
  | Expr (e,t) -> fprintf form "@[<2>%a%s@]" code_exp e "\n"
  | Pt_Virg (u,t) -> fprintf form "@[<2>%s@]" "\n"
  | Seq (p,t) -> fprintf form "@[<2> %a%s@]" code_prog p "\n"
  | IfElse (e,cmd1,cmd2,t) -> fprintf form "@[<2>%a%s%i%s%a%s%i%s%a%s@]"
                              code_exp e "\nConJump " (sizeOf_cmd cmd1 + 1) "\n"
                              code_cmd cmd1
                              "\n Jump " (sizeOf_cmd cmd2) "\n"
                              code_cmd cmd2 "\n"
  | DoWhile (c,e,t) -> fprintf form "@[<2>%s%a%s%a@ %s@ %i%s@]" "\n" code_cmd c "\n" code_exp e "ConJump " (-1 * sizeOf_cmd c + sizeOf_exp e + 1) "\n"
  | While  (e,c,t) -> fprintf form "@[<2> %s%a%s%i%s%a%s%i%s]" "\n" code_exp e "\nConJump" (sizeOf_cmd c +1) "\n" code_cmd c "\n Jump " (-1 * t) "\n"
and code_prog form = let open Format in function 
  | Prog (c,p,t) -> fprintf form "@[<2>%a@ %s@ %a@ %s@]" code_cmd c "\n" code_prog p "\n" 
  | Cmd (c,t) -> fprintf form "@[<2>%a@ %s@]" code_cmd c "\n"
;;
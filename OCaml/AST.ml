type expression_a =
  | Plus  of expression_a * expression_a * int
  | Moins of expression_a * expression_a * int
  | Mult  of expression_a * expression_a * int
  | Div   of expression_a * expression_a * int
  | Neg   of expression_a * int
  | Num   of float * int
  | Mod   of expression_a * expression_a * int
  | Eq      of expression_a * expression_a  * int
  | GrSt    of expression_a * expression_a * int
  | LeSt    of expression_a * expression_a * int
  | BoolNeg of expression_a * int
  | Bool    of bool * int
  | NaN of string  * int
  | Ter of expression_a * expression_a * expression_a * int
;;


(* Fonctions d'affichage *)
(* Ajout de la taille *)
let rec print_binaire form s g d t = Format.fprintf form "@[<2>%s%s@ %a%s@ %a%s@]" s "(" print_AST g " ," print_AST d " ) taille: " t 

and print_AST form = let open Format in function
  | Plus  (g,d,t) -> print_binaire form "Plus" g d t
  | Moins (g,d,t) -> print_binaire form "Moins" g d t
  | Mult  (g,d,t) -> print_binaire form "Mult" g d t
  | Div   (g,d,t) -> print_binaire form "Div" g d t
  | Mod   (g,d,t) -> print_binaire form "Mod" g d t
  | Neg   (e,t)   -> fprintf form "@[<2>%s@ %a@%i@]" "Neg" print_AST e "tailleNeg: " t 
  | Num   (n,t)   -> fprintf form "@[<2>%s@ %f@%i@]" "Num" n "tailleNum:  " t
  | Eq    (g,d,t) -> print_binaire form "Eq" g d t
  | GrSt  (g,d,t) -> print_binaire form "GrSt" g d t
  | LeSt  (g,d,t) -> print_binaire form "LeSt" g d t
  | BoolNeg (e,t) -> fprintf form "@[<2>%s@ %a@%i@]" "BoolNeg" print_AST e "tailleNegBool: " t
  | Bool  (b,t)   -> fprintf form "@[<2>%s@ %b@%i@]" "CsteBo" b "taille Bool: " t
  | NaN   (n,t)   -> fprintf form "@[<2>%s@ %a@%i@]" "Nan" n "taille NaN: " t
  (* | Ter (g, d, ) *)
;; 

let rec print_post_fixe form g d s = Format.fprintf form "@[<2>%s@ %a%s@ %a%s@ %s@ %s@]" "\n" code g "\n" code d "\n" s "\n"

and code form = let open Format in function
  | Plus  (g,d,t) -> print_post_fixe form g d "AddiNb" 
  | Mult  (g,d,t) -> print_post_fixe form g d "MultNb" 
  | Moins (g,d,t) -> print_post_fixe form g d "SubiNb" 
  | Mod   (g,d,t) -> print_post_fixe form g d "ModuNb" 
  | Num   (n,t) -> fprintf form "@[<2>%s@ %f@ %s@]" "CsteNb" n "\n"
  | Neg   (e,t) ->  fprintf form "@[<2>%a@ %s@]" code e "NegaNb\n"
  | Eq    (g,d,t) -> print_post_fixe form g d "Eq"
  | GrSt  (g,d,t) -> print_post_fixe form g d "GrSt"
  | LeSt  (g,d,t) -> print_post_fixe form g d "LeSt"
  | BoolNeg (e,t) -> fprintf form "@[<2>%a@ %s@]" "BoolNeg" print_AST e "tailleNegBool: " t
  | Bool  (b,t)   -> fprintf form "@[<2>%s@ %b@]" "CsteBo" b "taille Bool: " t
  | NaN   (n,t)   -> fprintf form "@[<2>%s@ %f@ %s@]" "Nan" n "taille NaN: " t
;
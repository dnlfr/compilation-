type expression_a =
  | Plus  of expression_a * expression_a
  | Moins of expression_a * expression_a
  | Mult  of expression_a * expression_a
  | Div   of expression_a * expression_a
  | Neg   of expression_a
  | Num   of float 
  | Mod   of expression_a * expression_a
  | Eq      of expression_a * expression_a 
  | GrSt    of expression_a * expression_a 
  | LeSt    of expression_a * expression_a 
  | BoolNeg of expression_a 
  | Bool    of bool
;;


(* Fonctions d'affichage *)

let rec print_binaire form s g d = Format.fprintf form "@[<2>%s%s@ %a%s@ %a%s@]" s "(" print_AST g " ," print_AST d " )" 

and print_AST form = let open Format in function
  | Plus  (g,d) -> print_binaire form "Plus" g d
  | Moins (g,d) -> print_binaire form "Moins" g d
  | Mult  (g,d) -> print_binaire form "Mult" g d
  | Div   (g,d) -> print_binaire form "Div" g d
  | Mod   (g,d) -> print_binaire form "Mod" g d
  | Neg    e    -> fprintf form "@[<2>%s@ %a@]" "Neg" print_AST e 
  | Num    n    -> fprintf form "@[<2>%s@ %f@]" "Num" n
;; 

let rec print_post_fixe form g d s = Format.fprintf form "@[<2>%s@ %a%s@ %a%s@ %s@ %s@]" "\n" code g "\n" code d "\n" s "\n"

<<<<<<< HEAD
and code form = let open Format in function
=======
and code form = let open Format in function 
>>>>>>> 0d9e2a48a1781779ff37ebc22c1cf5e2c8edb9bf
  | Plus  (g, d) -> print_post_fixe form g d "AddiNb" 
  | Mult  (g, d) -> print_post_fixe form g d "MultNb" 
  | Moins (g, d) -> print_post_fixe form g d "SubiNb" 
  | Mod (g,d) -> print_post_fixe form g d "ModuNb" 
  | Num   n -> fprintf form "@[<2>%s@ %f@ %s@]" "CsteNb" n "\n"
<<<<<<< HEAD
  | Neg   e ->  fprintf form "@[<2>%a@ %s@]" code e "NegaNb\n"
;;
=======
  | Neg   e ->  fprintf form "@[<2>%a@ %s@]" code e "NegaNb\n"
>>>>>>> 0d9e2a48a1781779ff37ebc22c1cf5e2c8edb9bf

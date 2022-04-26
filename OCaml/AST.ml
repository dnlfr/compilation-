type expression_a =
  | Plus  of expression_a * expression_a
  | Moins of expression_a * expression_a
  | Mult  of expression_a * expression_a
  | Div   of expression_a * expression_a
  | Neg   of expression_a
  | Num   of int * float 
  | Mod   of expression_a * int
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
  | Num    n    -> fprintf form "@[<2>%s@ %i@]" "Num" n
;; 

let rec print_post_fixe form g d s = Format.fprintf form "@[<2>%s@ %a%s@ %a%s@ %s@ %s@]" "\n" code g "\n" code d "\n" s "\n"

and code form = let open Format in function 
  | Num n -> fprintf "@[<2>%s@ %f@ %s@]" "CsteNb" n "\n"
  | Plus  (g, d) -> print_post_fixe form "AddiNb" g d 
  | Mult  (g, d) -> print_post_fixe form "MultNb" g d 
  | Moins (g, d) -> print_post_fixe form "SubiNb" g d
  | Mod (g,d) -> print_post_fixe form "Mod" g d 
  | Neg   n ->  fprintf form "@[<2>%s@ %f@ %s@]" n "NegaNb\n"
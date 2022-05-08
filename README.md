# Projet de compilation 2021-2022

## Progression actuelle: fragment 3.1 + FOR loop 

# Comment compiler notre projet 

`ocamllex lexeur.mll`
`ocamlyacc parseur.mly`
`ocamlc -c AST.ml parseur.mli lexeur.ml parseur.ml main.ml`
`ocamlc -o main AST.cmo lexeur.cmo parseur.cmo main.cmo`

# Jeu de test pour chacun des fragments:

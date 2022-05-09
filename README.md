# Projet de compilation 2021-2022

## Progression actuelle: fragment 3.1 + FOR loop 

### Comment compiler notre projet 

```
ocamllex lexeur.mll
ocamlyacc parseur.mly
ocamlc -c AST.ml parseur.mli lexeur.ml parseur.ml main.ml
ocamlc -o main AST.cmo lexeur.cmo parseur.cmo main.cmo
```

### Jeu de test pour chacun des fragments:

----------------

#### Fragment 0:

```
AddiNb, MultNb, NegNb, Halt : -2*(3+2);

SubsNb: 1+(3-2); 

ModuNb: 1%2; 

Test du float: 0.2;

Equals: (1+4) == (2+7);

NotEql: (1+4) != (2+7);

LoEqNb, GrEqNb, LoStNb, GrStNb: (1+2) < (2+4); (juste le signe à changer)

Not: !true;
```
#### Fragment 1 

```
Écriture scientifique: 1.215e25;

Commentaire: /* test */ 1+2;

Opérateur Ternaire: (1<3)?2;

And: (1+2) && (1+3); 
Or:  (1+2) ||(1+3); 
```
#### Fragment 2 + Ajout de la taille

```
GetVar: x; 
SetVar: x=2;
IfElse: if(x<2) x+1; else x=0;

```
#### Fragment 3 

```
DoWhile: do x+1; while x<2;
While: while(x>5) x-1;
For: for(i=0;i<5;i+1)

```

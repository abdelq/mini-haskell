--- 
title: TP 1 
Rapport author:
    - Abdelhakim Qbaich
    - David Boivin 
date: \today header-includes:
    -- \usepackage[french]{babel} ...
    
# Notre expérience

Le travail consistait à créer un langage dérivé du Lisp en
Haskell. L'implémentation de cette réplique miniature nous a mené à
mettre en pratique des notions vues en classes. Cela nous a été utile
pour mieux comprendre le paradigme fonctionnel.

Par ailleurs, cela nous a permis de mieux comprendre le concept
d'arbre de syntaxe abstrait, la portée de variables, le << typage >>
et l'évaluation.

Dans l'ensemble, c'était une expérience positive et très
enrichissante, qui nous a permis d'approfondir nos connaissances des
langages de programmation.

# Difficultés rencontrées

Le travail dans son ensemble s'est bien déroulé. Cependant, nous avons
tout de même eut quelques difficultés.

La première fut le faux départ qu'on a effectué. Nous avions commencé
notre travail assez tôt et nous avions mal implémentées certaines
fonctions de `eval`.  Donc nous avons recommencé la plupart de ces
fonctions pour renforcer la fiabilité de nos fonctions déjà écrites.

Une autre difficulté fut l'implémentation du `let`. Nous avions pris
du temps avant de trouver une fonction simple qui traduirait cette
expression et qui permettait d'être mutuellement récursive. Il a fallu
comprendre que `ELet` devait construire une liste de tuples.  Nous
avons aussi passé beaucoup de temps à construire son `eval` et son
`typecheck`.

# Les tests unitaires

Nous avons passé les tests unitaires et pour une meilleur fiabilité
nous avons rajouté des tests unitaires. Les tests qui sont échoués
sont les expressions qui manque des éléments. Comme par exemple quand
il n'y a aucun élément entre parenthèses. Malheureusement ces erreurs
doivent être gérées avant l'execution de *sexp2Exp*.

# Modification aux _datatypes_ fournis

## _Data Exp_
Une des étapes du travail fut de transformer les expressions fournies
en structure de données. Dans cette étape nous avons utilisé les
notions d'arbre de syntaxe abstraite pour les structures de
données. Nous avons seulement ajouté des *data* aux programmes sans
modifiées ceux fournies. Pour le *ELet* nous avons une liste de tuple à
trois éléments qui sont les variables de l'environement et il finit
avec une *exp* . C'est cette expression qui permettra d'imbriquer
plusieurs *let* ou de mettre un *lambda* à l'interieur de celui-ci.

``` ELet [(Sym, type, exp)] exp

``` Pour le sexp2Exp qui transformera les expressions en structure de
données nous avons utiliser une fonction récursive , mais qui utilise
une autre fonction pour créer et remplir la liste de tuples.


## EData
TODO


### _Data Type_ et _Value_ 

TODO Nous avons rajouté un nouveau type pour les _data_. Elle est
utile pour constuire notre _typecheck_ . Nous avons aussi rajouter des
valeurs pour les data des structures de données pour


## La logique derrière nos implémentations

Nous avons decidé de simplifier au maximum nos fonctions pour une
meilleur visibilité et compréhension. Pour ce faire nous avons utilisé
la récursion et le *pattern matching* à notre avantage. Ces méthodes
ont été beaucoup utilisés pour construire nos *sexp2Exp* notre *eval*
et le *typecheck*. Chaque fonctions *sexp2Exp* consistent à chacune
des possibilités de notre langages donc aucune de ces fonctions
*overlapped* les autres.

## Appréciation générale

Ce travail fut un travail intéressant qui nous as montré la difficulté
de comprendre un langage de programmation et de l'implémenté. Même si
à certain moment les contraintes du langages Haskell étaient frustante
, le travail s'est bien déroulé dans son ensemble. Nous étions content
de trouver les solutions au fur et à mesure du travail.

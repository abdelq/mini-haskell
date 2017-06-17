---
title: TP 1 -- Rapport
author:
    - Abdelhakim Qbaich
    - David Boivin
date: \today
header-includes:
    - \usepackage[french]{babel}
...

# Notre expérience

Le travail consistait à créer un langage dérivé du Lisp en
Haskell. L'implémentation de cette réplique miniature nous a mené
à mettre en pratique des notions vues en classes. Cela nous a été
utile pour mieux comprendre le paradigme fonctionnel.

Par ailleurs, cela nous a permis de mieux comprendre le concept
d'arbre de syntaxe abstrait, la portée de variables, le « typage
» et l'évaluation.

Dans l'ensemble, c'était une expérience positive et très
enrichissante, qui nous a permis d'approfondir nos connaissances
des langages de programmation.

# Difficultés rencontrées

Le travail dans son ensemble s'est bien déroulé. Cependant, nous
avons tout de même eut quelques difficultés.

La première fut le faux départ qu'on a effectué. Nous avions
commencé notre travail assez tôt et nous avions mal implémentées
certaines fonctions de `eval`. Donc nous avons recommencé la
plupart de ces fonctions pour renforcer la fiabilité de nos
fonctions déjà écrites.

Une autre difficulté fut l'implémentation du `let`. Nous avions
pris du temps avant de trouver une fonction simple qui traduirait
cette expression et qui permettait d'être mutuellement
récursive. Il a fallu comprendre que `ELet` devait construire une
liste de tuples. Nous avons aussi passé beaucoup de temps à
construire son `eval` et son `typecheck`.

# Les tests unitaires

Nous avons passé les tests unitaires et pour une meilleur
fiabilité nous avons rajouté des tests unitaires. Les tests qui
sont échoués sont les expressions qui manque des éléments. Comme
par exemple quand il n'y a aucun élément entre
parenthèses. Malheureusement ces erreurs doivent être gérées
avant l'exécution de `sexp2Exp`. Les test comportant
l'implémentation de `case` ont aussi échoués puisque nous n'avons
pas eu le temps de l'implémenter.

# Modification aux _datatypes_ fournis

## `Data Exp`
Une des étapes du travail fut de transformer les expressions
fournies en structure de données. Dans cette étape nous avons
utilisé les notions d'arbre de syntaxe abstraite pour les
structures de données. Nous avons seulement ajouté des `data` aux
programmes sans modifiées ceux fournies. Pour le `ELet` nous
avons une liste de tuple à trois éléments qui sont les variables
de l'environnement et il finit avec une `exp` . C'est cette
expression qui permettra d'imbriquer plusieurs *let* ou de mettre
un *lambda* à l'intérieur de celui-ci.

`ELet [(Sym, type, exp)] exp`

Pour le sexp2Exp qui transformera les expressions en structure de
données nous avons utiliser une fonction récursive , mais qui
utilise une autre fonction pour créer et remplir la liste de
tuples.

Pour implémenter une façon de définir nos propre structure de
données il nous a fallu rajouter au `datatypes` le `EData`. Voici
la `data Exp` que nous avons rajouter.

`EData [Value] Exp`

Donc avec le `sexp2Exp`on transforme la structure de données avec
un tableau de valeurs et une expression . La difficulté de cela
étais surtout de transformer l'entrée pour pouvoir l'utiliser
efficacement avec `eval` et `typecheck`.  Pour cela nous avons
rajouter un nouveau `type` et une nouvelle `value`.

Nous avons rajouté un nouveau `type` pour les `data`. Elle est
utile pour construire notre `typecheck` . Nous avons aussi
rajouter des valeurs pour les  des structures de données pour
pouvoir évaluer et retourner une valeur quand on retourne une
structure de données que nous avons déclaré.  Voici les deux
ajouts aux structures de données `Type` et `Value`.

`TData TData Symbol`
`VData Type [Symbol]`

Nous avons aussi ajouter une façon dont l'interpréteur
afficherait les valeur `VData`.

`show (TData sym) = sym`

Malheuresement notre implémentation de `data` n'est pas complète pour
certain cas. Elle ne regrade pas si un type est déjà définie dans son
`data` . De plus notre `typeCheck` est non récusif pour le reste de la
liste. Le `typeCheck` fonctionne mal avec les tuples aussi.

## Le `case`

Nous avons pas eu le temps d'implémenter le `case` malheureusement.

## La logique derrière nos implémentations

Nous avons decidé de simplifier au maximum nos fonctions pour une
meilleur visibilité et compréhension. Pour ce faire nous avons
utilisé la récursion et le *pattern matching* à notre
avantage. Ces méthodes ont été beaucoup utilisés pour construire
nos `sexp2Exp`, notre `eval` et le `typecheck`. Chaque fonctions
`sexp2Exp` consistent à chacune des possibilités de notre
langages donc aucune de ces fonctions `overlapped` les autres.
Nous nous sommes assurés que toutes les fonctions que nous
écrivions étaient correcte et marchaient avec le plus grand
nombre de cas.

## Appréciation générale

Ce travail fut un travail intéressant qui nous as montré la
difficulté de comprendre un langage de programmation et de
l'implémenter. Même si à certain moment les contraintes du
langages Haskell étaient frustrante, le travail s'est bien
déroulé dans son ensemble. Une de nos déceptions fut le manque de
temps que nous avions pour implémenter le `case`. Nous étions
content de trouver les solutions au fur et à mesure du travail
malgré les difficultés. 

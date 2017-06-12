--- title: TP 1 -- Rapport author:
    - Abdelhakim Qbaich
    - David Boivin date: \today header-includes:
    - \usepackage[french]{babel} ...

## Notre experience 
Le travail demandé consistait à créer un _mini-haskell_ en
haskell. L'implémentation de cette réplique miniature nécessitait
certaines notions vue en classe. La mise en pratique de ces notions
nous ont été utile pour la compréhention des langages fonctionnels. Il
nous à permis de mieux comprendre les arbres de syntax abstrait , la
portées des variables statique, le typage sous-jacent au langages de
programmation et aux étapes consistant à l'interprétation d'un langage
comme haskell. Ce fut pour nous une expérience positive et
constructive pour nos connaissances des langages de programmation.


## Difficultés rencontrées
Le travail dans son ensemble s'est bien déroulé , mais cepandant nous
avons eut quelques difficultés. La première fut le faux départ qu'on a
effectué. Nous avions commencé notre travail assez tôt et nous avions
mal implémentées certaines fonctions de _eval_. Donc nous avons
recommencé la plupart de ces fonctions pour renforcer la fiabilité de
nos fonctions déjà écrites. Une autre difficulté fut l'expression
_ELet_ . Nous avions pris du temps avant de trouver une fonction
simple qui traduirait cette expression et qui permettait d'être
mutuellement récursive.Il a fallu comprendre que _ELet_ devait
construire une liste de tuples. Nous avons aussi passé beaucoups de
temps à construire son _eval_ et son _typecheck_. 


## Les test unitaires

Nous avons passer les tests unitaires et pour une meilleur fiabilité
nous avons rajouter des tests unitaires.  Les tests qui sont échoués
sont les expressions qui manque des éléments. Comme par exemple quand
il n'y a aucun élément entre parenthèses. Malheureusement ces erreurs
doivent être gérées avant que nous utilisions _sexp2Exp_.



## Modification aux _datatypes_ fournis

### _Data Exp_
Une des étapes du travail fut de transformer les expressions fournis
en structure de données.  Dans cette étape nous avons utilisé les
notions d'arbre de syntaxe abstraite pour les structures de données.
Nous avons seulement ajouté des _data_ aux programmes sans modifiées
ceux fournis. Pour le _ELet_ nous avons une liste de tuple à trois
éléments qui sont les variables de l'environement et il finit avec une
_exp_ . C'est cette expression qui permettra d'imbriquer plusieurs
_let_ ou de mettre un _lambda_ à l'interieur de celui-ci.

```
ELet [(Sym, type, exp)] exp

```
Pour le sexp2Exp qui transformera les expression en structure de
données nous avons utiliser une fonction récursive , mais qui utilise
une autre fonction pour créer et remplir la liste de tuples.


TODO EData



### _Data Type_ et _Value_ 

TODO
Nous avons rajouter un nouveau type pour les _data_. Elle est utile
pour constuire notre _typecheck_ . Nous avons aussi rajouter des valeurs
pour les data des structures de données pour


## La logique derrière nos implémentations

Nous avons decidé de simplifier au maximum nos fonctions pour une
meilleur visibilité et compréhension. Pour ce faire nous avons
utilisé la récursion et le _pattern matching_ à notre avantage. Ces
méthodes ont été beaucoup utiliser pour construire nos _sexp2Exp_
notre _eval_ et le _typecheck_. Chaque fonctions _sexp2Exp_ consistent
à chacune des possibilités de notre langages donc aucune de ces
fonctions _overlapped_ les autres..




## Appréciation générale

Ce travail fût un travail intéressant qui nous as montré la difficulté
de comprendre un langage de programmation et de l'implémenté. Même si
à certain moment les contraintes du lanagages haskell était frustante
, le travail s'est bien déroulé dans son ensemble. Nous étions content
de trouver les solutions au fur et à mesure du travail. 

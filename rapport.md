---
title: TP 1 -- Rapport
author:
    - Abdelhakim Qbaich
    - David Boivin
date: \today
header-includes:
    - \usepackage[french]{babel}
...

* Décrire notre travail et notre expérience
* Difficultés rencontrées
* Dire pourquoi le programme échoue certains tests unitaires
* Expliquer et donner des pistes de solutions

* Indiquer les modifications effectuées aux datatypes fournis

``` haskell
data Type = TInt
          | TArrow Type Type
          | TData Symbol
          deriving (Eq)
```

``` haskell
data Exp = EInt Int
         | EVar Symbol
         | EApp Exp Exp
         | ELam Symbol Type Exp
         | ELet [(Symbol, Type, Exp)] Exp
         | EData [Value] Exp
         deriving (Show, Eq)
```

``` haskell
data Value = VInt Int
           | VLam Symbol Exp Env
           | VPrim (Value -> Value)
           | VData Type [Symbol]
```

* Indiquer la logique derrière nos implémentations
* Appréciation générale

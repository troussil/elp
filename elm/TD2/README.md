# Deuxième séance en autonomie (1h50)

L'objectif de cette deuxième séance est de savoir créer ses propres types et de se familiariser avec l'architecture elm pour créer une application web monopage.

## Types

La première chose que tu dois faire, c'est de lire attentivement la [section du guide consacré aux types](https://guide.elm-lang.org/types/).

Je te propose ci-dessous rappel et une série d'exercices pour que tu t'entraines à créer tes propres types. C'est très important pour la suite.

### Rappel

Les types algébriques, appelés `custom types` dans le guide, sont une combinaison de : 
- type somme (ou encore union, enum, etc.), comme `type Couleur = Rouge | Noir`, 
- type produit (qui ne sont rien d'autres que des tuples personnalisés), comme `type Point = DonnePoint Float Float`,  
- et récursivité. 

Avant de donner plus d'exemples, mettons-nous d'accord sur les mots. A gauche du `=`, c'est le monde des types. On a un constructeur de type. A droite du `=`, on a le monde des valeurs. On a une liste de constructeurs de valeurs séparés par des `|`. Un constructeur de valeur peut avoir zéro paramètre (`Rouge`, `Noir`) ou plusieurs paramètres (`Point Float Float` a deux paramètres, chacun de type `Float`). Pour obtenir une valeur, on appelle le constructeur de valeur : 
```elm
> type Couleur = Rouge | Noir
> type Point = DonnePoint Float Float
> Rouge
Rouge : Couleur
> DonnePoint 3.2 5.4
DonnePoint 3.2 5.4 : Point
```

Tous les constructeurs commencent par une majuscule. Comme il y a des espaces de noms séparés pour les constructeurs de types et de valeurs, on peut aussi écrire :
```elm
> type Point = Point Float Float
> Point 3.2 5.4
Point 3.2 5.4 : Point
```

Par ailleurs, les types algébriques peuvent être paramétrés par des types variables, toujours notés en minuscules. Le constructeur de type peut avoir zéro paramètre (comme pour `Couleur`, `Point`), ou plusieurs comme ci-dessous : 
```elm
type Maybe a = Just a | Nothing
```
```elm
type Result error value = Ok value | Err error
```
Ces types sont déjà définis dans elm et sont fondamentaux pour la [gestion des erreurs](https://guide.elm-lang.org/error_handling/). Dans le premier cas, il n'y a qu'un seul type variable, noté `a` (on choisit souvent `a` pour un type variable), dans le second, il y en deux, nommés `error` et `value`. Pour construire des valeurs pour ces types, il suffit d'appeler les constructeurs de valeurs : 

```elm
> Just 'c'
Just 'c' : Maybe Char
> Just 5.2
Just 5.2 : Maybe Float
> Nothing
Nothing : Maybe a
> Ok "it works!"
Ok ("it works!") : Result error String
> Err "it fails!"
Err ("it fails!") : Result String value
```

Enfin, les types algébriques peuvent être récursifs : le type qu'on définit (à gauche) apparait comme paramètre d'un constructeur de valeur (à droite) comme dans l'exemple ci-dessous : 

```elm
> type StackInt = Empty | Push Int StackInt 
> s = Push 5 Empty
Push 5 Empty : StackInt
> Push 3 s
Push 3 (Push 5 Empty) : StackInt
```

### Exercices

- Donne, pour chacun des cinq types précédents (`Couleur`, `Point`, `Maybe`, `Result`, `StackInt`), le constructeur de type, ses paramètres, la liste des constructeurs de valeurs et leurs paramètres. 
- Propose un type `CouleurCarte` pour modéliser la couleur (ou enseigne) d'une carte à jouer. 
- Propose un type `ValeurCarte` pour modéliser la valeur d'une carte à jouer. 
- Propose un type `Carte` pour modéliser une carte à jouer. 
- Crée la carte correspondant à l'as de Trèfle, puis crée la liste des quatre as. 
- Le type `StackInt` ne peut traiter que des `Int`. Pour lever cette contrainte, proppose un type paramétré `Stack`. Puis, crée une valeur de type `Stack Char` et contenant au moins deux caractères. 
- Propose un type paramétré `Tree` pour modéliser un arbre binaire. Puis, crée un arbre vide, ainsi qu'un arbre contenant au moins trois nombres flottants. 

## Architecture elm

Maintenant, je t'invite à lire attentivement la [section du guide consacré à l'architecture elm](https://guide.elm-lang.org/architecture/)
et à faire les exercices associés. Ces exercices ne sont pas corrigés, mais pas trop difficiles. Si tu es bloqué, demande conseil à quelqu'un ou contacte l'enseignant. 

## Pour terminer, un petit effort de rappel

- Qu'est-ce que le pattern matching ?
- Quelle est la différence entre un tuple, un enregistrement, une liste ?
- Qu'est-ce qu'un type algébrique ?
- Quels sont les types et fonctions nécessaires pour `browser.sandbox` ?

Compare tes réponses avec ce document et celui de la [séance précédente](../TD1/). 

## Pour aller plus loin. 

Si tu as déjà fini et qu'il reste du temps, tu peux regarder les [exemples](https://elm-lang.org/examples), lire les sections [error handling](https://guide.elm-lang.org/error_handling/) et [commands and subscriptions](https://guide.elm-lang.org/effects/).

## Correction de la première partie

Bientôt. 

# Deuxième séance en autonomie (1h50)

L'objectif de cette deuxième séance est de savoir créer ses propres types et de se familiariser avec l'architecture elm pour créer une application web monopage.

## Types

La première chose que tu dois faire, c'est de lire attentivement la [section du guide consacré aux types](https://guide.elm-lang.org/types/).

Je te propose ci-dessous rappel et une série d'exercices pour que tu t'entraines à créer tes propres types. C'est très important pour la suite.

### Rappel

Les types algébriques, appelés *custom types* dans le guide, sont une combinaison de : 
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

Par ailleurs, les types algébriques peuvent être paramétrés par des types variables, toujours notés en minuscules. Le constructeur de type peut avoir zéro paramètre (comme pour `Couleur`, `Point`), ou plusieurs, comme pour ces types, déjà définis dans elm et fondamentaux pour la [gestion des erreurs](https://guide.elm-lang.org/error_handling/): 
```elm
type Maybe a = Just a | Nothing
```
```elm
type Result error value = Ok value | Err error
```
 Dans le premier cas, il n'y a qu'un seul type variable, noté `a` (on choisit souvent `a` pour un type variable), dans le second, il y en deux, nommés `error` et `value`. Pour construire des valeurs pour ces types, il suffit d'appeler les constructeurs de valeurs : 

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

Le type `StackInt` ne peut traiter que des `Int`. Pour lever cette contrainte, il suffit d'opter pour un type paramétré : 
```elm
type Stack a = Empty | Push a (Stack a)
```

On peut ainsi créé une valeur de type `Stack Char` ou une valeur de type `Stack Int` :
```
> aStack = Push 'b' (Push 'a' Empty)
Push 'b' (Push 'a' Empty)
    : Stack Char
> aStack = Push 2 (Push 1 Empty)
Push 2 (Push 1 Empty)
    : Stack number
```

N'est-ce pas grisant de définir en une ligne une structure de données représentant des piles d'éléments de type arbitraire ?

### Exercices

- Donne, pour chacun des cinq types précédents (`Couleur`, `Point`, `Maybe`, `Result`, `StackInt`), le constructeur de type, ses paramètres, la liste des constructeurs de valeurs et leurs paramètres. 
- Propose un type `CouleurCarte` pour modéliser la couleur (ou enseigne) d'une carte à jouer. 
- Propose un type `ValeurCarte` pour modéliser la valeur d'une carte à jouer. 
- Propose un type `Carte` pour modéliser une carte à jouer. 
- Crée la carte correspondant à l'as de Trèfle, puis crée la liste des quatre as. 
- Propose un type paramétré `Tree` pour modéliser un arbre binaire. Puis, crée un arbre vide, ainsi qu'un arbre contenant au moins trois nombres flottants. Enfin, écris une fonction qui retourne la hauteur d'une arbre. 

## Architecture elm

Maintenant, je t'invite à lire attentivement la [section du guide consacré à l'architecture elm](https://guide.elm-lang.org/architecture/)
et à faire les exercices associés. Ces exercices ne sont pas corrigés, mais pas trop difficiles. Si tu es bloqué, demande conseil à quelqu'un ou contacte l'enseignant. 

## Pour terminer, un petit effort de rappel

- Qu'est-ce que le pattern matching ?
- Quelle est la différence entre un tuple, un enregistrement, une liste ?
- Qu'est-ce qu'un type algébrique ?
- Quels sont les types et fonctions nécessaires pour `browser.sandbox` ?

Compare tes réponses avec ce document, celui de la [séance précédente](../TD1/) et le [guide](https://guide.elm-lang.org/).

## Pour aller plus loin. 

Si tu as déjà fini et qu'il reste du temps, tu peux regarder les [exemples](https://elm-lang.org/examples), lire la section [error handling](https://guide.elm-lang.org/error_handling/) et te pencher sur les [parseurs](../parser/README.md). S'il te reste encore du temps après ça, tu peux regarder la section [commands and subscriptions](https://guide.elm-lang.org/effects/) et commencer à réfléchir au [projet](../projet/README.md).

## Correction de la première partie

### Premier item 

```elm
type Couleur = Rouge | Noir
```
- un constructeur de type sans paramètre `Couleur`
- deux constructeurs de valeur sans paramètre : `Rouge` et `Noir`

```elm
type Point = Point Float Float
```
- un constructeur de type sans paramètre `Point` (celui de gauche)
- un constructeur de valeur `Point` (celui de droite) prenant deux paramètres de type `Float` en entrée. 

```elm
type Maybe a = Just a | Nothing
```
- un constructeur de type `Maybe` prenant en paramètre un type `a`, généralement appelé *type variable*. 
- deux constructeurs de valeur : 
  - un sans paramètre : `Nothing`, 
  - et `Just` prenant un paramètre de type `a`

On peut rapprocher `Maybe` de `List` : tous deux représentent des conteneurs paramétrés par un autre type.  

```elm
type Result error value = Ok value | Err error
```
- un constructeur de type `Result` prenant deux types en paramètre `error` et `value`
- deux constructeurs de valeur : 
  - `Ok` prend en paramètre `value`,  
  - `Err` prend en paramètre `error`. 

```elm
type StackInt = Empty | Push Int StackInt 
```
- un constructeur de type sans paramètre `StackInt` (celui de gauche)
- deux constructeurs de valeurs : 
  - `Empty` est sans paramètre, 
  - `Push` prend deux paramètres, un de type `Int` et un de type `StackInt`.

Le type `StackInt` ne peut traiter que des `Int`. On peut lever cette contrainte à l'aide d'un type paramétré `type Stack a = Empty | Push a (Stack a)`. On remplace `Int` par un type variable appelé `a` et `StackInt` par `Stack a`. C'est d'ailleurs la réponse à l'une des questions suivantes.

### Items suivants

Voir le fichier [Test.elm](src/Test.elm)

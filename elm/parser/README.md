# Parseur

## Introduction aux parseurs en elm

Un parseur prend une chaîne de caractères en entrée et retourne soit un succès (avec un résultat), soit une erreur. Par exemple, un parseur d'entier peut prendre "123" comme entrée et retourner le nombre 123.

En elm, ce type de fonctionnalités se trouve dans la bibliothèque [elm/parser](https://package.elm-lang.org/packages/elm/parser/latest).
Un parseur est une valeur de type `Parser a`, où `a` est le type du résultat attendu. Par exemple `Parser Int` est le type utilisé pour un parseur d'entier. Pour utiliser un parseur sur une chaîne de caractères, on appelle la fonction `run` en lui donnant en entrée à la fois le parseur et la chaîne. Par exemple :
```
> import Parser exposing (..)
> run int "123"
Ok 123 : Result (List DeadEnd) Int
> run int "123xxx"
Ok 123 : Result (List DeadEnd) Int
> run int "xxx123"
Err [{ col = 1, problem = ExpectingInt, row = 1 }]
    : Result (List DeadEnd) Int
```

Notez que le type de retour est `Result` (revoir si besoin la [gestion des erreurs](https://guide.elm-lang.org/error_handling/)). 

## Pipelines

On peut enchaîner les parseurs avec `succeed` et les opérateurs `|.` et `|=` pour construire une valeur d'un type personnalisé.

Imaginons qu'on ait défini le type suivant :
```elm
type alias Point = { x : Float, y : Float }
```

On peut construire un point en parsant une chaîne de caractères à l'aide du parseur suivant : 
```elm
extraitPoint : Parser Point
extraitPoint =
  succeed Point
    |. symbol "("
    |. spaces
    |= float
    |. spaces
    |. symbol ","
    |. spaces
    |= float
    |. spaces
    |. symbol ")"
```

Le parseur `spaces` ignore les espaces blancs (" ", tabulations, sauts de ligne, etc.), tandis que le parseur `symbol` est utilisé pour vérifier qu'une chaîne de caractères commence par une chaîne de caractères littérale spécifique. 
Le premier argument de `succeed` est `Point`, c'est-à-dire le constructeur de valeur pour le type `Point`. 
L'opérateur `|.` est utilisé pour écarter le résultat du parseur qui suit, tandis que `|=` est utilisé pour conserver le résultat du parseur qui suit. Ainsi, on détecte "(", on ignore les espaces suivants, on détecte un flottant qu'on passe en premier argument au constructeur `Point`, on détecte ",", on ignore les espaces suivants, on détecte un flottant qu'on passe en second argument au constructeur `Point`, on détecte ")". Si tout s'est bien passé le résultat contient une valeur de type 'Point'.

```
> run extraitPoint "(2.5, 6.2)"
Ok { x = 2.5, y = 6.2 }
    : Result (List DeadEnd) Point
> run extraitPoint "(2.5 6.2)"
Err [{ col = 6, problem = ExpectingSymbol ",", row = 1 }]
    : Result (List DeadEnd) Point
> run extraitPoint "(2.5, 6.2)xxx"
Ok { x = 2.5, y = 6.2 }
    : Result (List DeadEnd) Point
```

### oneOf

Pour construire la valeur d'un type `union`, il est nécessaire d'utiliser
le parseur `oneOf` qui permet d'essayer plusieurs parseurs dans l'ordre jusqu'à ce qu'un réussisse :

```elm
type CouleurCarte = Trefle | Carreau | Coeur | Pique
extraitCouleur : Parseur CouleurCarte
extraitCouleur = oneOf [ succeed (\_ -> Trefle) |= symbol "Trefle"
                       , succeed (\_ -> Carreau) |= symbol "Carreau"
                       , succeed (\_ -> Coeur) |= symbol "Coeur"
                       , succeed (\_ -> Pique) |= symbol "Pique"
                       ]
```

Notez l'utilisation des lambdas comme `(\_ -> Trefle)` quand les constructeurs de valeurs n'ont aucun paramètre comme `Trefle`. 

```
> run extraitCouleur "Trefle"
Ok Trefle : Result (List DeadEnd) EmblemeCarte
> run extraitCouleur "Carreau"
Ok Carreau : Result (List DeadEnd) EmblemeCarte
> run extraitCouleur "Spad"
Err [{ col = 1, problem = ExpectingSymbol "Trefle", row = 1 },{ col = 1, problem = ExpectingSymbol "Carreau", row = 1 },{ col = 1, problem = ExpectingSymbol "Coeur", row = 1 },{ col = 1, problem = ExpectingSymbol "Pique", row = 1 }]
    : Result (List DeadEnd) EmblemeCarte
```

### lazy

Pour construire la valeur d'un type récursif, il est nécessaire d'utiliser le parseur `lazy`. Par exemple, pour parser des expressions imbriquées comme `(1+(2+3))` :

```elm
type Expr
    = Number Int
    | Sum Expr Expr
```

```elm
parseSum : Parser Expr
parseSum = succeed Sum 
           |. symbol "("
           |= parseExpr
           |. symbol "+"
	   |= parseExpr
	   |. symbol ")"
		
parseExpr : Parser Expr
parseExpr = oneOf [ succeed Number |= int
                  , lazy (\_ -> parseSum)
		  ]
```

```
> run parseExpr "(1+(2+3))"
Ok (Sum (Number 1) (Sum (Number 2) (Number 3)))
    : Result (List DeadEnd) Expr
```

# Première séance en autonomie (1h50)

L'objectif de cette première séance est de se familiariser avec les types structurés existants (tuples/enregistrements et listes) et d'écrire des fonctions.

## Installation et prise en main

Une première chose à faire est d'installer [elm](https://guide.elm-lang.org/install/elm.html). Si tu travailles sur une machine du département, sache que l'exécutable `elm` pèse environ 28 Mo. Si tu n'as pas beaucoup de place dans ton répertoire personnel, déplace-le dans le répertoire `/tmp` pour éviter de dépasser ton quota. 

Tu n'utiliseras qu'un REPL (read-eval-print-loop) pour cette première séance. Pour cela, il suffit de taper `elm repl` dans un shell. Tu peux alors écrire des expressions elm qui seront lues (read), puis évaluées (eval). Le résultat sera affiché (print) et tu pourras recommencer (loop). 

Voici trois références utiles pour faire connaissance avec elm et écrire des expressions syntaxiquement correctes : 
- [https://guide.elm-lang.org/core_language.html](https://guide.elm-lang.org/core_language.html)
- [https://elm-lang.org/docs/syntax](https://elm-lang.org/docs/syntax)

## Types structurés

Outre les types de base comme `Bool`, `Int`, `Float`, `Char`, `String`, il est utile de savoir qu'il existe aussi trois classes de types - `number`, `appendable`, `concatenable` - que le système de type utilise pour assigner un type quand il n'est pas donné explicitement. Quand tu tapes `42`, tu obtiens `42 : number`. Explication : le système de type refuse de choisir entre `Int` et `Float` en optant pour `number` qui regroupe les deux. 

Par ailleurs, il est fondamental de savoir manipuler les types structurés prédéfinis : les tuples, les enregistrements et les listes. 

- un tuple contient un nombre fixe de champs pouvant être de type distinct. Les tuples sont bridés à au plus trois champs pour nous obliger à structurer le code. Par exemple, `reponse = (False, "un message d'erreur")`. 
- un enregistrement ressemble à un tuple dont les champs sont nommés. Les noms de champs ajoutent de la sémantique et sont utilisés comme accesseurs. Il n'y a pas de contrainte sur le nombre de champs. Par exemple, `person = { name="me", age="22" }`.
- une liste contient zéro, une ou plusieurs valeurs toutes de même type. Par exemple, `names = ["Alice", "Bob", "Chuck"]` ou `numbers = [1,2,3,4]` ou `[]` (liste vide). 

Assure-toi que tu sache définir un tuple, un enregistrement, une liste. Aide-toi du [guide](https://guide.elm-lang.org/core_language.html) si besoin. 

## Pattern matching

*Pattern matching* désigne le mécanisme par lequel les types structurés peuvent être *déstructurés* dans trois cas : 
- `let ... in ...`, qui sert à poser des définitions comme en math, 
- `case ... of ... -> ...`, qui sert habituellement à distinguer des cas, 
- la définition de fonctions.

Imaginons qu'on ait définit un tuple `couleur = (128, 255, 0)`. Voici deux exemples de pattern matching dans lesquels on extrait les valeurs des trois champs en les associant à `r,v,b`, puis on n'utilise que le premier `r` : 
```
> let (r,v,b) = couleur in r
128 : number
> case couleur of (r,v,b) -> r
128 : number
```

Voici un troisième exemple dans lequel on définit une fonction retournant la valeur du premier champs et on l'appelle avec `couleur en entrée : 
```
> getRedChannel (r,v,b) = r
<function> : ( a, b, c ) -> a
> getRedChannel couleur
128 : number
```

Assure-toi que tu comprends les exemples précédents. Si besoin, consulte les références mentionnées plus haut, lis le paragraphe sur le [pattern matching](https://elm-lang.org/docs/records#pattern-matching), demande à un ami ou à l'enseignant.

En s'inspirant du guide et de l'exemple précédent, trouve cinq façons d'extraire la valeur du champs `name` de l'enregistrement suivant : 

```
> person = { name="me", age="22" }
{ age = "22", name = "me" }
    : { age : String, name : String }
```

Si besoin, la correction se trouve en bas de document, mais essaie d'abord de faire seul. 

Les listes sont construites soit explicitement, soit avec l'opérateur `::` de sorte que ces trois expressions sont équivalentes :
```elm
[1,2,3,4]
1 :: [2,3,4]
1 :: 2 :: 3 :: 4 :: []
```

Pour déconstuire une liste, on utilise donc naturellement l'opérateur `::`, mais seulement dans un `case` car il est nécessaire de prendre en compte le fait qu'une liste peut être vide :
```elm
case lst of
  [] -> ...
  (x :: xs) -> ...
```  
Dans le premier cas, `lst` est vide et égale à `[]`, tandis que dans le second cas, `lst` contient au moins un élément, l'élément en tête de liste est désigné par `x` et la liste des autres éléments (possiblement vide) est désignée par `xs`.
  
Tu utiliseras beaucoup ce mécanisme dans la prochaine partie pour définir récursivement des fonctions manipulant des listes. Cela permettra de distinguer le cas général (au moins un élément), du cas terminal (liste vide). 

## Fonctions

Les fonctions sont définies soit en nommant une *lambda*, soit sous forme équationnelle : 
`inc = \x -> x + 1` (lambda à droite) est équivalent à `inc x = x + 1` (équation). 

Toutes les fonctions ne sont pas récursives. 

```elm
sign x = if x > 0 then 1 else if x == 0 then 0 else -1
```

```elm
estVide lst = case lst of
   [] -> True
   (x :: xs) -> False
```

Cependant, comme il n'y a pas de structure de contrôle itérative, les traitements répétés sont naturellement codés sous la forme de fonctions récursives. Par exemple : 

```elm
len lst = case lst of
   [] -> 0
   (x :: xs) -> 1 + len xs
```

Assure-toi que tu comprends les exemples précédents. Si besoin, consulte les références mentionnées plus haut, demande à un ami ou à l'enseignant. 

### Fonctions récursives manipulant des listes
  
Cette section est composée d'une série d'exercices. Tu peux tout écrire dans le REPL, mais il est plus commode de définir tes fonctions dans un fichier séparé et de n'utiliser le REPL que pour les appels. Pour cela, suis la procédure suivante :
- Quitte le REPL avec la commande `:exit`.
- Crée un nouveau projet en tapant `elm init` dans le shell. Un répertoire `src` et un fichier `elm.json` sont créés. 
- Ajoute dans `src` un fichier appelé `Test.elm` et contenant en première ligne `module Test exposing (..)`. C'est là que tu définiras tes fonctions. 
- Relance le REPL en tapant `elm repl`, puis tape `import Test exposing (..)` pour accéder aux définitions. 
  
1. Définis la fonction `addElemInList` qui ajoute un élément donné, un nombre de fois donné, dans une liste donnée. 

```
> addElemInList 1 3 []
[1,1,1] : List number
> addElemInList 'a' 3 ['b']
['a','a','a','b'] : List Char
```

Suggestion : le cas terminal survient quand on veut ajouter un élément zéro fois. Utilise un `if` pour détecter le cas terminal. 

2. Définis la fonction `dupli` qui duplique les éléments d'une liste donnée. 

```
> dupli [1,2,3]
[1,1,2,2,3,3]
    : List number
> dupli ['a','b','c']
['a','a','b','b','c','c']
    : List Char
```
 
Suggestion : le cas terminal survient quand on a une liste vide. Utilise un `case` pour distinguer le cas terminal, du cas général. 

3. Définis la fonction `compress` qui supprime les copies consécutives des éléments d'une liste.
      
```
> compress [2,2,2,3,4,4,2,2,5,6,6,6]
[2,3,4,2,5,6]
    : List number
```
Remarque 1: en réalité on ne supprime rien, on recrée une liste dans laquelle on omet les éléments redondants. 

Remarque 2: on peut imbriquer les `case` et `if`. 

### Package List

Le package [List](https://package.elm-lang.org/packages/elm/core/latest/List) est disponible par défaut et contient un grand nombre de fonctions très utiles. Très souvent les appeler nous dispensent d'écrire une fonction récursive. `List.map`, `List.filter`, `List.foldr`, `List.foldl` sont typiques des langages fonctionnels. Lis la documentation de ces fonctions pour comprendre à quoi elles servent et comment les utiliser. 

1. Ecris des variantes non récursives pour `addElemInList` et `dupli`. Tu devras utiliser l'opérateur `++` de concaténation, les fonctions `List.repeat` et `List.concatMap`.

2. Ecris une variante non récursive pour `compress` à l'aide de `List.foldr` et de la fonction suivante :
```elm
compressHelper x partialRes = case partialRes of
  [] -> [x]
  (y :: ys) -> if x == y
               then partialRes
               else x :: partialRes 
```

## Pour terminer, un petit effort de rappel

Le plus efficace pour garder en mémoire quelque chose c'est d'essayer de s'en rappeler. C'est pourquoi je te propose de répondre aux questions suivantes : 

- Quelle est la différence entre un tuple, un enregistrement, une liste ?
- Qu'est-ce que le pattern matching ?
- Que font `List.map`, `List.filter`, `List.foldr`, `List.foldl` ?
- Explique en quoi elm est un langage fonctionnel.
- Explique en quoi elm est un langage fortement typé.

Compare tes réponses avec ce document, les ressources que tu as consulté et le [cours](https://perso.liris.cnrs.fr/tristan.roussillon/ens/elm/). 

## Correction

Tu peux regarder la correction et comparer avec ton propre code. 

### Pattern matching

```
> person = { name="me", age="22" }
> person.name  --nom du champs comme accesseur, style objet
"me" : String
> .name person --nom du champs comme accesseur, style fonctionnel
"me" : String
> let { name } = person in name
"me" : String
> case person of { name } -> name
"me" : String
> getName { name } = name
<function> : { b | name : a } -> a
> getName person
"me" : String
```

### Fonctions récursives et package List

Voir le fichier [Test.elm](src/Test.elm)


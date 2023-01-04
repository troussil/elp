# Première séance en autonomie (1h50)

L'objectif de cette première séance est de se familiariser avec les types structurées existants (tuples/enregistrements et listes) et d'écrire des fonctions.

## Installation et prise en main

Une première chose à faire est d'installer [elm](https://guide.elm-lang.org/install/elm.html). Si tu travailles sur une machine du département, sache que l'exécutable `elm` pèse environ 28 Mo. Si tu n'as pas beaucoup de place dans ton répertoire personnel, déplace-le dans le répertoire `/tmp` pour éviter de dépasser ton quota. 

Tu n'utiliseras qu'un REPL (read-eval-print-loop) aujourd'hui. Pour cela, il suffit de taper `elm repl` dans un shell. Tu peux alors écrire des expressions elm qui seront lues (read), puis évaluées (eval). Le résultat sera affiché (print) et tu pourras recommencer (loop). 

Voici trois références utiles pour faire connaissance avec elm et écrire des expressions syntaxiquement correctes : 
- [https://guide.elm-lang.org/core_language.html](https://guide.elm-lang.org/core_language.html)
- [https://elm-lang.org/docs/syntax](https://elm-lang.org/docs/syntax)
- [https://learnxinyminutes.com/docs/elm/](https://learnxinyminutes.com/docs/elm/) 

## Types structurés

Outre les types de base comme `Bool`, `Int`, `Float`, `Char`, `String`, il est utile de savoir qu'il existe aussi trois classes de types - `number`, `appendable`, `concatenable` - que le système de type utilise pour assigner un type quand il n'est pas donné explicitement. Quand tu tapes `42`, tu obtiens `42 : number`. Explication : le système de type refuse de choisir entre `Int` et `Float` en optant pour `number` qui regroupe les deux. 

Par ailleurs, il est fondamental de savoir manipuler les types structurés prédéfinis : les tuples, les enregistrements et les listes. 

- un tuple contient un nombre fixe de champs pouvant être de type distinct. Les tuples sont bridés à au plus trois champs pour nous obliger à structurer le code. 
- un enregistrement ressemble à un tuple dont les champs sont nommés. Les noms de champs ajoutent de la sémantique et sont utilisés comme accesseurs. Il n'y a pas de contrainte sur le nombre de champs. 
- une liste contient zéro, une ou plusieurs valeurs toutes de même type. 

En parcourant les références fournies plus haut, assure-toi que tu puisse définir un tuple, un enregistrement, une liste. 

## Pattern matching

*Pattern matching* désigne le mécanisme par lequel les types structurés peuvent être *déstructurés* dans trois cas : 
- `let ... in ...`, qui sert à poser des définitions comme en math, 
- `case ... of ...`, qui sert habituellement à distinguer des cas, 
- la définition de fonctions.

Voici des exemples de pattern matching pour un tuple : 

```
> couleur = (128, 255, 0)
(128,255,0) : ( number, number1, number2 )
> let (r,v,b) = couleur in r
128 : number
> case couleur of (r,v,b) -> r
128 : number
> getRedChannel (r,v,b) = r
<function> : ( a, b, c ) -> a
> getRedChannel couleur
128 : number
```

Après avoir lu le paragraphe sur le [pattern matching](https://elm-lang.org/docs/records#pattern-matching)
et en s'inspirant de l'exemple précédent, trouve trois façons d'extraire la valeur du champs `name` de l'enregistrement suivant : 

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

4. Définis la fonction `encode` qui encode une liste donnée de façon à ce que toute suite
de `n` éléments égaux à `x` soit remplaçée par le tuple `(n,x)`.
	   
```
> encode [2,2,2,3,4,4,2,2,5,6,6,6]
[(3,2),(1,3),(2,4),(2,2),(1,5),(3,6)]
    : List ( Int, number )
```

Si tu es bloqué ici, passe à la suite. Tu demanderas de l'aide quand l'enseignant sera disponible. 

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

3. Même question pour `encode`, mais je te laisse le soin d'écrire toi-même la fonction auxiliaire.

Si tu es arrivé à cette question à la fin de la séance et même si tu n'as pas terminé d'y répondre, c'est déjà bien. Tu peux te féliciter. 

## Pour terminer, un petit effort de rappel

Le plus efficace pour garder en mémoire quelque chose c'est d'essayer de s'en rappeler. C'est pourquoi je te propose de répondre aux questions suivantes : 

- Quelle est la différence entre un tuple, un enregistrement, une liste ?
- Qu'est-ce que le pattern matching ?
- Que font `List.map`, `List.filter`, `List.foldr`, `List.foldl` ?
- Explique en quoi ELM est un langage fonctionnel.
- Explique en quoi ELM est un langage fortement typé.
- Comment appelle-t-on le fait qu'une expression soit toujours évaluée en un même résultat ?
- Quelle est la différence entre évaluation stricte et paresseuse ?

Compare tes réponses avec ce document, les ressources que tu as consulté et le [cours](https://perso.liris.cnrs.fr/tristan.roussillon/ens/elm/). 

## Pour aller plus loin, exercices facultatifs sans correction. 

1. A l'aide de `List.repeat` et `List.ConcatMap`, définis une fonction `decode : List (Int,a) -> List a` telle que :

```
> decode [(3,2),(1,3),(2,4),(2,2),(1,5),(3,6)]
[2,2,2,3,4,4,2,2,5,6,6,6]
    : List number
```

2. Définis une fonction récursive `power` telle que `power 2 3` renvoie `8` et `power 2 (-3)` renvoie `-8`.
   
3. Définis une fonction récursive qui supprime le dernier élément d'une liste, puis une fonction qui supprime le premier et le dernier éléments d'une liste. Par exemple, `supprimeDernier [1,2,3]` retourne `[1,2]` et `supprimeExtremites [1,2,3]` retourne `[2]`.

4. Définis une fonction qui prend en entrée une fonction de combinaison binaire et une liste, puis retourne une nouvelle liste. Elle a pour effet d'intercaler le résultat de la combinaison de chaque paire d'éléments consécutifs entre eux. Par exemple, `intercale (++) ["insa","lyon"]` retourne `["insa","insalyon","lyon"]` et `intercale (+) [1,2,3]` retourne `[1,3,2,5,3]`.

5. Définis une fonction récursive `getLast : (List a) -> (Maybe a)` qui prend en entrée une liste et retourne son dernier élément. L'appel `getLast [1,2,3]` retourne `Just 3`, tandis que l'appel `getLast []` retourne `Nothing`. En savoir plus sur [Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe).


## Correction

Tu peux regarder la correction et comparer avec ton propre code. 

### Pattern matching

```
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

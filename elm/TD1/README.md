# Première séance en autonomie (2h)

L'objectif de cette première séance est de se familiariser avec les types structurées existants (tuples/enregistrements et listes) et d'écrire des fonctions.

## Installation

Une première chose à faire est d'installer [elm](https://guide.elm-lang.org/install/elm.html). 
Tu n'utiliseras qu'un REPL (read-eval-print-loop) aujourd'hui. Pour cela, il suffit de taper 
`elm repl` dans un shell. 

Tu peux commencer à taper des [valeurs](https://elm-lang.org/docs/syntax#literals) et les combiner avec des opérateurs. 

## Types structurés

url syntaxe

liste, fonctions de base
tuple/record
pattern matching : Patterns for destructuring records can appear in let expressions, lambda expressions, and case expressions. 

- Nous avons `couleur = (0, 255, 0)`. Comment extraire les différents champs de la variable `couleur` ?
- Nous avons `msg = "bonjour"`. Comment extraire la première lettre d'une part et toutes les suivantes d'autre part de la chaîne de caractères assignée à la variable `msg` ?
  
  
# Fonctions

if/case/let

sign x
  | x > 0 = 1
  | x == 0 = 0
  | otherwise = -1

myLen :: [a] -> Integer
myLen lst = case lst of
              [] -> 0
              (x:xs) -> 1 + myLen xs


## fonctions récursives

- Coder une fonction récursive `power` telle que `power 2 3` renvoie `8` et `power 2 (-3)` renvoie `-8`.

- Codez une fonction *récursive* `divInt :: (Ord a, Num a, Num b) => a -> a -> b` qui prend en entrée deux entiers positifs p, q et retourne le quotient de la division euclidienne de p par q par soustractions successives. Par exemple, l'appel `divInt 7 2` retourne `3`, tandis que l'appel `divInt 2 7` retourne `0`. 


              
1. 

A l'aide de gardes, définissez la fonction

.. literalinclude:: code/addElemInList.hs
   :language: haskell
   :lines: 1

qui ajoute un élément donné, un nombre de fois donné, dans une liste donnée.

.. code-block:: none
	   
    *Main> addElemInList 1 3 []
    [1,1,1]
    *Main> addElemInList 'a' 2 "bb"
    "aabb"
    *Main> 

addElemInList :: a -> Int -> [a] -> [a]
addElemInList x c lst
  | c <= 0 = lst
  | c > 0 = addElemInList x (c-1) (x:lst)

2.

A l'aide d'une case expression, définissez la fonction

.. literalinclude:: code/duplicate.hs
   :language: haskell
   :lines: 1

qui duplique les éléments d'une liste donnée. 

.. code-block:: none
	   
    *Main> dupli [1,2,3]
    [1,1,2,2,3,3]
    *Main> dupli "abc"
    "aabbcc"
    
dupli :: [a] -> [a]
dupli lst = case lst of
              (x:xs) -> x:x:(dupli xs)
              [] -> []
    
3.

Définissez la fonction

.. literalinclude:: code/compress.hs
   :language: haskell
   :lines: 1

qui supprime les copies consécutives des éléments d'une liste.
      
.. code-block:: none

    *Main> compress "aaaabccaadeeee"
    "abcade"

compress :: (Eq a) => [a] -> [a]
compress []  = []
compress [x] = [x]
compress (x:y:xs)
  | x == y    = compress (y:xs)
  | otherwise = x:(compress (y:xs))

4.


Définissez la fonction

.. literalinclude:: code/encode.hs
   :language: haskell
   :lines: 1

qui encode une liste donnée de façon à ce que toute suite
de ``n`` éléments égaux à ``x`` soit remplaçée par le tuple ``(n,x)``.
	   
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] =  []
encode (x:xs) = case res of
    [] -> [(1,x)]
    ( (nb,elem) : ys) 
      | x == elem -> (nb+1,x):(ys)
      | otherwise -> (1,x):res
  where res = encode xs 	   
	   
.. code-block:: none

   *Main> encode "aaaabccaadeeee"
   [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

Astuce : on suppose qu'on connaît le résultat pour une liste ``xs``.
Comment construire incrémentalement le résultat pour ``x:xs`` ?
   
2. Codez une fonction *récursive* `getLast :: [a] -> Maybe a` qui prend en entrée une liste et retourne son dernier élément. L'appel `getLast "azerty"` retourne `Just 'y'`, tandis que l'appel `getLast []` retourne `Nothing`. 

2. Codez une fonction récursive `getAt :: Int -> [a] -> a` qui prend en entrée un indice, une liste et retourne l'élément de la liste se trouvant à cet indice. L'appel `getAt 0 "azerty"` retourne `'a'`, l'appel `getAt 4 "azerty"` retourne `'t'`. (Vous pouvez ignorer les cas particuliers). 


   
4. Définissez une fonction *récursive* qui supprime le dernier élément d'une liste, puis une fonction qui supprime le premier et le dernier éléments d'une liste. Par exemple, `supprimeDernier [1,2,3]` retourne `[1,2]` et `supprimeExtremites [1,2,3]` retourne `[2]`. (2 points)

5. Définissez une fonction qui prend en entrée une fonction de combinaison binaire et une liste, puis retourne une nouvelle liste. Elle a pour effet d'intercaler le résultat de la combinaison de chaque paire d'éléments consécutifs entre eux. Par exemple, `intercale (++) ["insa","lyon"]` retourne `["insa","insalyon","lyon"]` et `intercale (+) [1,2,3]` retourne `[1,3,2,5,3]`. (3 points)
   
   
# Autre

## Introduction: effort de rappel

- Expliquez en quoi Haskell est un langage fonctionnel.
- Expliquez en quoi Haskell est un langage fortement typé.
- Expliquez ce qu'est l'évaluation paresseuse.
- Comment appelle-t-on le fait qu'une expression soit toujours évaluée en un même résultat ?
- Quelle est la différence entre une liste et un tuple ?
- Que font les opérateurs `(:)`, `(++)`, et les fonctions `length`, `head`, `take`, `repeat`, `map`,  `foldr` ?
- Qu'est-ce qu'une monade ? Citez trois exemples de types monadiques. 

Comparez vos réponses avec le cours et les vidéos. 




 
- Coder une fonction récursive `grouperParPaires :: [a] -> [(a,a)]` telle que `grouperParPaires [1..7]` retourne `[(1,2),(3,4),(5,6)]` (le dernier élément est ignoré dans le cas d'un nombre impair d'éléments). 
- Coder une fonction récursive `decode :: [(Int,a)] -> [a]` telle que `decode [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]` retourne la chaîne `"aaaabccaadeeee"`.
- Coder une fonction récursive `encode :: (Eq a) => [a] -> [(Int, a)]` telle que `encode "aaaabccaadeeee"` retourne `[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]`. 





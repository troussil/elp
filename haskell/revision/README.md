# Révision

## Introduction: effort de rappel

- Expliquez en quoi Haskell est un langage fonctionnel.
- Expliquez en quoi Haskell est un langage fortement typé.
- Expliquez ce qu'est l'évaluation paresseuse.
- Comment appelle-t-on le fait qu'une expression soit toujours évaluée en un même résultat ?
- Quelle est la différence entre une liste et un tuple ?
- Que font les opérateurs `(:)`, `(++)`, et les fonctions `length`, `head`, `take`, `repeat`, `map`,  `foldr` ?
- Qu'est-ce qu'une monade ? Citez trois exemples de types monadiques. 

Comparez vos réponses avec le cours et les vidéos. 

## Programmer

Une correction de cette section est proposée en fin de document. 

### Types

- Nous avons `couleur = (0, 255, 0)`. Comment extraire les différents champs de la variable `couleur` ?
- Nous avons `msg = "bonjour"`. Comment extraire la première lettre d'une part et toutes les suivantes d'autre part de la chaîne de caractères assignée à la variable `msg` ?
- Donner les noms des types et les noms des constructeurs de valeurs pour chacune des déclarations suivantes : 
  a) `data Bool = True | False`
  b) `data Inst = Forward Int | Left Int | Right Int | Repeat Int [Inst]` 
  c) `data Resultat a b = Erreur a | Valide b`
  d) `data Point2D a = Point2D a a`
- Définir un type de données `Jour` représentant les jours de la semaine. Ecrire une fonction `estWeekend :: Jour -> Bool` qui renvoie faux si et seulement si le jour donné est le samedi ou le dimanche. 
- Définir un type de données `Contact` permettant de représenter le nom et l'adresse mail d'une personne, tous deux de type `String`. Ecrire une fonction `chercher :: String -> [Contact] -> Bool` qui indique si un nom donné se trouve dans une liste de contact donné. 
- Définir un type `Pile` comme un synonyme du type liste, puis écrire les fonctions suivantes : 
  - `empiler :: a -> Pile -> Pile`,
  - `depiler :: Pile -> Pile`,
  - `haut :: Pile -> a`,
  - `estVide :: Pile -> Bool`. 
- Définir un type `Arbre` dont les noeuds ont un nombre quelconque de fils et sont porteurs d'une donnée (toutes les données sont de même type).
  
### Fonctions

#### Récursivité

- Coder une fonction récursive `repeat' :: x -> [x]` qui crée une liste contenant une infinité de `x`. 
- Coder une fonction récursive `power` telle que `power 2 3` renvoie `8` et `power 2 (-3)` renvoie `-8`. 
- Coder une fonction récursive `grouperParPaires :: [a] -> [(a,a)]` telle que `grouperParPaires [1..7]` retourne `[(1,2),(3,4),(5,6)]` (le dernier élément est ignoré dans le cas d'un nombre impair d'éléments). 
- Coder une fonction récursive `decode :: [(Int,a)] -> [a]` telle que `decode [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]` retourne la chaîne `"aaaabccaadeeee"`.
- Coder une fonction récursive `encode :: (Eq a) => [a] -> [(Int, a)]` telle que `encode "aaaabccaadeeee"` retourne `[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]`. 

#### take, repeat, map, foldr, bind

- A l'aide de `foldr`, coder les fonctions `length' :: [a] -> Int`, `sum' :: Num a => [a] -> a` et `product' :: Num a => [a]` qui calculent respectivement le nombre, la somme et le produit des éléments d'une liste donnée.
- A l'aide de `foldr` et `map`, coder la fonction `dupli :: [a] -> [a]` qui duplique les éléments d’une liste donnée. 
- A l'aide de l'opérateur `(>>=)`, coder la même fonction `dupli :: [a] -> [a]`. 
- A l'aide des fonctions `foldr`, `map`, `take` et `repeat`, coder la fonction `decode :: [(Int,a)] -> [a]` de la section précédente. 
- A l'aide de l'opérateur `(>>=)` et des fonctions `take` et `repeat`, coder la même fonction `decode :: [(Int,a)] -> [a]`.
- Implémenter `map'`, une version de `map`, à l'aide de `foldr`. 
- Implémenter `filter'`, une version de `filter`, à l'aide de `foldr`. 

## Lire un programme

Dans le programme suivant, expliquez :

- à quoi sert la clause `deriving (Eq, Read, Show)`, 
- pourquoi il n'est pas nécessaire de préciser le type de retour de la fonction `read`, 
- pourquoi il y a un deuxième `do` après le `else`, alors qu'il n'y en a pas après le `then`, 
- comment on pourrait remplacer la construction `if-then-else` par un `case`, 
- comment exécuter le programme dans ghci, 
- comment compiler le programme avec ghc. 

Comparez vos explications avec celles d'un camarade. 

```
data CouleurCarte = Trefle | Carreau | Coeur | Pique
  deriving (Eq, Read, Show)
data ValeurCarte = Deux | Trois | Quatre | Cinq | Six | Sept |
                   Huit | Neuf | Dix | Valet | Dame | Roi | As
  deriving (Eq, Read, Show)
data Carte = Carte ValeurCarte CouleurCarte 
  deriving (Eq, Read, Show)
  
jeu carteATrouver =
  do
    line <- getLine
    if (read line) == carteATrouver
      then putStrLn "Vous avez gagne!"
      else do
        putStrLn "Non, reessayez!"
        jeu carteATrouver

main = jeu (Carte As Trefle)
```

## Correction de la section "Programmer"

`(r, g, b) = couleur`

`(x:xs) = msg`

a) `Bool` est le type à définir, `True` et `False` sont des constructeurs de valeurs sans paramètre (donc des valeurs).
b) `Inst`, `Int` et `[Int]` sont des types, tandis que `Forward`, `Left`, `Right`, `Repeat` sont des constructeurs de valeurs. 
c) `Resultat`, `a`, `b`, sont des types (`Resultat` est le type défini dans la déclaration et il est paramétré par deux autres types : `a` et `b`). `Erreur` et `Valide` sont des constructeurs de valeurs. 
d) `Point2D` (celui écrit à gauche du `=`) et `a` désignent des types et en même temps, `Point2D` (à droite du `=`) est aussi le nom d'un constructeur de valeur.
 
  ```
  data Jour = Lundi | Mardi | Mercredi | Jeudi | Vendredi | Samedi | Dimanche
  estWeekend unJour = case unJour of
    Samedi -> True
    Dimanche -> True
    _ -> False
  ```
 
  ```
  data Contact = Contact {nom::String, mail::String}
  chercher unNom uneListe = length (filter (\c -> nom c == unNom) uneListe) > 0
  ```
   
  ```
  type Pile a = [a]
  empiler x pile = x:pile
  depiler (x:xs) = xs --precondition: pile non vide
  haut (x:xs) = x     --precondition: pile non vide
  estVide pile = case pile of
    [] -> True
    _ -> False
  ```
  
`data Arbre a = Empty | Arbre a [Arbre a]`. Où sont les deux constructeurs de valeurs ?

`repeat' x = x : repeat' x` 
 
  ```
  power x e
    | e == 0 = 1
    | e < 0  = power (1/x) (-e)
    | e > 0  = x * (power x (e-1))
  ```
  Le type de `power` donné par ghci est `power :: (Ord a1, Fractional a2, Num a1) => a2 -> a1 -> a2`. Comprenez-vous pourquoi ?
 
  ```
  grouperParPaires (x0 : x1 : xs) = (x0, x1) : (grouperParPaires xs)
  grouperParPaires  _ = []
  ```
 
  ```
  decode :: [(Int, a)] -> [a]
  decode [] = []
  decode ( (nb,elt) : sublst )
    | nb > 0   = elt : ( decode ( (nb-1,elt) : sublst ) )
    | otherwise = decode sublst
  ```
 
  ```
  encode :: (Eq a) => [a] -> [(Int, a)]
  encode [] = []
  encode [x] = [(1,x)]
  encode (x0:x1:xs) = case res of
      [] -> [(1,x)]
      ( (nb,elt) : ys) 
        | x == elt -> (nb+1,x):(ys)
        | otherwise -> (1,x):res
    where res = encode xs 
  ```
 
  ```
  length' = foldr (\_ y -> y+1) 0
  sum' = foldr (+) 0
  product' = foldr (*) 1
  ```

`dupli lst = foldr (++) [] (map (\x -> [x,x]) lst)`

`dupli lst = lst >>= (\x -> [x,x])` 

`decode lst = foldr (++) [] (map (\(nb,elt) -> take nb (repeat elt)) lst)`

`decode lst = lst >>= \(nb,elt) -> take nb (repeat elt)`

`map' func lst = foldr (\elt partialLst -> (func elt) : partialLst) [] lst`
 
  ``` 
  filter' pred lst = foldr construction [] lst 
    where construction elt sublst
            | (pred elt) = elt : sublst
            | otherwise  = sublst
  ```
  Comme on ne peut utiliser de garde sur une lambda-fonction, on passe par une clause `where`. Pouvez-vous rendre plus lisible la correction des cinq questions précédentes en utilisant une clause `where` ?


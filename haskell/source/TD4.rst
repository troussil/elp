============================================
Foncteur, applicatif, monade
============================================

Classes de types polymorphes
====================================

Kind
------------------------------------

Le système de typage associe un genre (*kind*) à tout type et toute classe.
Dans GHCi, on peut l'obtenir avec la commande ``:kind``.

- Les types concrets comme ``Int``, ``Int -> Int`` ou ``[Int]`` sont de genre ``*``.

- Les types d'ordre supérieur, paramétrés par un autre type, comme ``Maybe`` ou ``[]`` (liste) sont de genre ``* -> *``.

- Il y a d'autres genres que nous ne détaillons pas comme ``* -> * -> *``, ``(* -> *) -> *`` ou ``* -> Constraint``.
  

Classes
-------------------------------------

Foncteur, applicatif et monade sont des types de genre ``* -> *`` et
pour lesquels sont définies des opérations qui respectent certaines lois. 

Une monade est un foncteur applicatif qui est lui-même un foncteur.
Autrement dit, la monade enrichit l'ensemble des opérations que définit
le foncteur applicatif, lui-même plus grand que celui que définit le foncteur.


Foncteur
===================================

map
-------------------------------------

Intuitivement, un foncteur est un conteneur pour lequel on peut appliquer une fonction à son contenu.
Un exemple typique est la liste, dont la fonction ``map`` permet d'appliquer une fonction à tous ses éléments.

.. code-block:: none

   Prelude> map (*2) [1,2,3]
   [2,4,6]

Classe ``Functor`` et ``fmap``
-------------------------------------

Le concept de foncteur généralise ``map`` à toutes les instances de la classe ``Functor``,
qui exige la définition d'une fonction ``fmap`` :

.. code-block:: haskell

   class Functor f where
     fmap :: (a -> b) -> f a -> f b

Cette fonction doit être implémentée de façon à ce que ces deux lois soient respectées : 
     
.. code-block:: haskell

   fmap id == id
   fmap (f . g) == fmap f . fmap g
   
Une liste est un foncteur
---------------------------------------

Une liste est un foncteur dont la fonction ``fmap`` correspond exactement à ``map`` :

.. code-block:: none

   Prelude> fmap (*2) [1,2,3]
   [2,4,6]
   Prelude> fmap (*2) []
   []

Rappel sur ``Maybe``
---------------------------------------------

``Maybe`` est défini dans le Prélude comme :

.. code-block:: haskell

   data Maybe a = Just a | Nothing


On peut l'utiliser ainsi : 
   
.. literalinclude:: code/minimum.hs
   :language: Haskell
   :lines: 1-3

.. code-block :: none

    *Main> minimum' [1,3,-2,5]
    Just (-2)
    *Main> minimum' []
    Nothing

``Maybe`` est un foncteur
---------------------------------------------

Le typage nous empêche d'utiliser directement la valeur.

.. code-block :: none

    *Main> 2 * minimum' [1,3,-2,5]

    <interactive>:16:1:
      Non type-variable argument in the constraint: Num (Maybe a)
      (Use FlexibleContexts to permit this)
      When checking that ‘it’ has the inferred type
      it :: forall a. (Num a, Num (Maybe a), Ord a) => Maybe a

On peut néanmoins manipuler ces valeurs grâce à ``fmap`` : 

.. code-block :: none

   *Main> fmap (*2) (Just 5)
   Just 10
   *Main> fmap (*2) Nothing
   Nothing  

..
   L'opérateur ``<$>``
   -------------------------------

   L'opérateur ``<$>`` est un synonyme de ``fmap`` :

   .. code-block:: haskell

      (<$>) = fmap

   Il inclut le symbole ``$`` pour rappeler l'opérateur d'évaluation :

   .. code-block:: none

      Prelude> (*2) $ 5
      10
      Prelude> (*2) <$> (Just 5)
      Just 10
      Prelude> (*2) <$> Nothing
      Nothing

   
Foncteur applicatif
===================================

Fonction ``pure`` et opérateur ``<*>``
------------------------------------------

Intuitivement, les foncteurs applicatifs rendent possible non seulement
l'application d'une fonction à un paramètre sur leur contenu,
mais aussi l'application d'une fonction à *plusieurs paramètres*
sur autant de foncteurs applicatifs.

.. code-block:: none

   Prelude> (*) 2 5
   10
   Prelude> pure (*) <*> (Just 2) <*> (Just 5)
   Just 10

Comme cet exemple le suggère, ``Maybe`` est un foncteur applicatif. 
   
Classe ``Applicative``
-----------------------------------
   
Ce concept est implémenté par la classe ``Applicative`` qui hérite de ``Functor``.
Un type qui instancie ``Applicative`` doit donc instancier également ``Functor``,
et définir la fonction ``pure``, ainsi que l'opérateur ``<*>`` : 

.. code-block:: haskell

   class Functor f => Applicative f where
     pure :: a -> f a
     (<*>) :: f (a -> b) -> f a -> f b

Comme pour ``fmap``, ces fonctions doivent être implémentées de façon à ce que
plusieurs lois soient respectées, notamment :

.. code-block:: haskell

   pure f <*> x = fmap f x


Une liste est un foncteur applicatif
---------------------------------------

Une liste est un foncteur applicatif pour lequel ``pure`` construit une liste contenant l'élément donné
en argument et l'opérateur ``<*>`` prend une liste de fonctions et une liste de valeurs et construit une liste
où chacune des fonctions est appliquée sur chacune des valeurs.

.. code-block:: none

   Prelude> [(*2), (+1)] <*> [1,2,3]
   [2,4,6,2,3,4]
   Prelude> pure (*) <*> [2, 1] <*> [1,2,3]
   [2,4,6,1,2,3]
   Prelude> pure (*)
   [(*)]
   Prelude> [(*)] <*> [2,1]
   [(*2),(*1)]
   Prelude> [(*2), (*1)] <*> [1,2,3]
   [2,4,6,1,2,3]


Types monadiques
============================

Chaînage
---------------------------------------------

Intuitivement, les monades sont des conteneurs, comme les foncteurs,
qui offrent une manière d'enchaîner des fonctions encapsulant leur valeur
de retour dans une monade.

.. literalinclude:: code/minimum.hs
   :language: Haskell
   :lines: 1-3

.. code-block:: none

    *Main> minimum' [1,3,-2,5]
    Just (-2)
    *Main> Just [1,3,-2,5] >>= minimum' 
    Just (-2)
    *Main> Nothing >>= minimum' 
    Nothing
    
Comme le suggère cet exemple ``Maybe`` est une monade. 

Classe ``Monad``
---------------------------------------------

Ce concept est implémenté par la classe ``Monad`` qui hérite de ``Applicative``.
Un type qui instancie ``Monad`` doit donc instancier également ``Applicative``,
et définir l'opérateur ``>>=`` (*bind*), le reste étant implémenté par défaut.  

.. code-block:: haskell

   class Applicative m => Monad m where
     return :: a -> f a
     return = pure
     
     (>>=) :: m a -> (a -> m b) -> m b

     (>>) :: m a -> m b -> m b
     m >> n = m >>= (\_ -> n)
     

*Bind*
-----------------------------------------------

L'opérateur ``>>=`` prend une monade ``m a``, contenant une valeur de type ``a``,
applique une fonction ``(a -> m b)`` sur celle-ci et retourne la monade
qui en résulte ``m b``. L'opérateur ``>>`` est une version particulière où la valeur
de la première monade n'est pas transmise à la seconde.
  
La sémantique de ces opérateurs doit respecter les lois monadiques : 

.. code-block:: haskell

   return a >>= k           = k a
   m >>= return             = m
   xs >>= return . f        = fmap f xs
   m >>= (\x -> k x >>= h)  = (m >>= k) >>= h

défi 1 : opérations monadiques
-----------------------------------

Que donnent les expressions suivantes ?

- ``return 'a' :: [Char]`` et ``return 4 :: Maybe Int``
- ``"abc" >>= (\x -> [x,x])``  et ``"" >>= (\x -> [x,x])``
- ``[(4,'a'),(2,'b'),(1,'a')] >>= (\(nb,elt) -> take nb (repeat elt))``
- ``Just 5 >>= (\x -> Just (x*2))``
- ``Nothing >>= (\x -> Just (x*2))``
- ``Just 5 >>= (\x -> return (x*2))`` 
- ``Just 5 >>= return . (*2)``
- ``Just 5 >> Just 6`` et ``Nothing >> Just 6``
- ``[2,5,0] >> "bc"`` et ``"bc" >> [2,5,0]``
  

Sucre syntaxique : notation ``do``
------------------------------------

Il y a une notation qui évite l'usage explicite de ``>>`` et ``>>=`` : 

.. code-block:: haskell

   do e1 ; e2     = e1 >> e2
   do p <- e1; e2 = e1 >>= \p -> e2

Par exemple :
   
.. code-block:: none

   *Main> Just [1,3,-2,5] >>= minimum' >>= (\x -> return (x-10))
   Just (-12)
   *Main> do lst<- (Just [1,3,-2,5]); min<- minimum' lst; return (min-10)
   Just (-12)

Notez qu'en général on passe à la ligne plutôt qu'utiliser un point-virgule.  
   
défi 2 : notation ``do``
----------------------------

Que donnent les expressions suivantes ?

- ``do x <- "abc"; [x,x]``
- ``do (nb,elt) <- [(4,'a'),(2,'b')]; take nb (repeat elt))``
- ``do x <- Just 5; Just (x*2)``
- ``do x <- Nothing; Just (x*2)``
- ``do x <- Just 5; return (x*2)``
- ``do Just 5; Just 6``
- ``do [2,5,0]; "bc"``
- ``do [2,5,0]; "bc"; [0]``

   
``String`` et ``IO``
==============================

``show``, ``read``
-------------------------------

Les fonctions ``show`` and ``read`` permettent de transformer
une valeur d'une instance des classes ``Show`` and ``Read``
en ``String`` et inversement. 

.. code-block:: none

   *Main> show (Carte Trois Carreau)
   "Carte Trois Carreau"
   *Main> read "Carte Deux Pique" :: Carte
   Carte Deux Pique

Notez qu'on indique le type de la valeur que doit retourner ``read``
car le système de type n'a aucun moyen de le deviner. 

.. code-block:: none

   *Main> read "Carte Deux Pique"
   *** Exception: Prelude.read: no parse
   *Main> read "n'importe quoi" :: Carte
   *** Exception: Prelude.read: no parse		

``getLine``, ``putStrLn``
-------------------------------

Tout action d'entrée-sortie retourne une valeur qui est étiquettée par le type monadique ``IO``.

Par exemple, ``getLine`` réalise une action de lecture et retourne une chaîne de caractère.

.. code-block:: Haskell

   getLine :: IO String

Les actions qui ne retournent aucune valeur utile prennent le type ``IO ()``.
Par exemple, ``putStrLn`` prend une chaîne pour l'afficher mais ne retourne rien. 

.. code-block:: Haskell

   putStrLn :: String -> IO ()

Remarque : ``print = putStrLn . show``. 
   
bind
-------------------------------

Les actions sont séquencées avec l'opérateur ``>>=``.

.. code-block:: Haskell

   >>= :: IO a -> (a -> IO b) -> IO b

Ce programme réalise deux actions d'entrée-sortie à la suite : il lit
une chaîne, puis l'affiche sur la sortie standard. 
   
.. literalinclude:: code/getput.hs
   :language: Haskell

	      
Notation ``do``
-------------------------------

Le mot-clé ``do`` introduit une séquence d'instructions
implicitement enchaînées. 

Ces instructions sont soit des actions, soit des définitions locales
avec des *let-statement* où des ``<-`` pour récupérer les valeurs provenant
d'entrées-sorties.  

Ce programme est l'équivalent du précédent :

.. literalinclude:: code/getput2.hs
   :language: Haskell

Autre exemple
---------------------------

.. literalinclude:: code/main2.hs
   :language: Haskell

.. literalinclude:: code/main.hs
   :language: Haskell

Autre exemple avec ``let``
----------------------------

.. literalinclude:: code/main4.hs
   :language: Haskell

.. literalinclude:: code/main3.hs
   :language: Haskell

Autre exemple avec ``return``
--------------------------------

.. literalinclude:: code/getName.hs
   :language: Haskell
   :lines: 1-8
	   
.. literalinclude:: code/getNameDo.hs
   :language: Haskell
   :lines: 1-6
 
.. literalinclude:: code/getNameDo.hs
   :language: Haskell
   :lines: 8-9
  
      
Rappel de compilation
---------------------------

- La commande ``ghc fichier.hs -o executable`` produit
  directement un exécutable : ``./executable``, à partir
  du moment où ``fichier.hs`` contient un ``main``. 

- D'autres options sont utiles, notamment ``-outputdir build``
  pour mettre les fichiers intermédiaires dans un répertoire séparé ``build``
  (voir ``man ghc`` ou ``ghc --help``). 
	      
..
   return
   -------------------------------

   La fonction ``return`` crée une action, qui ne fait rien d'autre
   que d'encapsuler une valeur dans une action : 

   .. code-block:: Haskell

      return :: a -> IO a

   Cette fonction lit un caractère et retourne ``IO True`` si le caractère
   est un ``y``.  Une erreur classique est d'oublier le ``return``. 

   .. literalinclude:: code/ready.hs
      :language: Haskell

   (Notez que le type est connu grâce à la signature de la fonction). 


défi 3 : jeu de devinette
-------------------------------

A partir d'un type ``Carte`` modélisant une carte à jouer, 
définissez la fonction :

.. literalinclude:: code/io.hs
   :language: Haskell
   :lines: 11

appelée ci-dessous : 
	   
.. literalinclude:: code/io.hs
   :language: Haskell
   :lines: 21-26

L'utilisateur tape le nom d'une carte.
Si c'est celle mémorisée dans le programme, il a gagné, sinon
il a le droit de proposer une nouvelle carte. 


Conclusion
==============================


Capacités/Connaissances
-----------------------------------------------

- Donnez des exemples de monades: ``Maybe``, ``[]``, ``IO``.
- Expliquez ce qu'est une monade. 
- Utilisez à bon escient les principales fonctions d'entrée-sorties:
  ``read``, ``show``, ``getLine``, ``putStrLn``. 
- Traduire une opération *bind* par un *do* et inversement.
- Ecrire un petit programme en définissant la fonction ``main``

  

défi 1 : Opérations habituelles
----------------------------------------------
	      
.. code-block:: none

   Prelude> return 'a' :: [Char] 
   "a"
   Prelude> return 4 :: Maybe Int
   Just 4
   Prelude> "abc" >>= (\x -> [x,x]) 
   "aabbcc"
   Prelude> "" >>= (\x -> [x,x])
   ""
   Prelude> [(4,'a'),(2,'b'),(1,'a')] >>=
		(\(nb,elt) -> take nb (repeat elt))
   "aaaabba"

défi 1 : Opérations habituelles (suite)
----------------------------------------------
	      
.. code-block:: none

   Prelude> Just 5 >>= (\x -> Just (x*2))
   Just 10
   Prelude> Nothing >>= (\x -> Just (x*2))
   Nothing
   Prelude> Just 5 >>= (\x -> return (x*2))
   Just 10
   Prelude> Just 5 >>= return . (*2))
   Just 10
   Prelude> Just 5 >> Just 6
   Just 6
   Prelude> Nothing >> Just 6
   Nothing
   Prelude> [2,5,0] >> "bc"
   "bcbcbc"
   Prelude> "bc" >> [2,5,0]
   [2,5,0,2,5,0]
   
défi 2 : Notation ``do``
----------------------------------------------

.. code-block:: none

   Prelude> do x <- "abc"; [x,x]
   "aabbcc"
   Prelude> do (nb,elt) <- [(4,'a'),(2,'b')]; take nb (repeat elt)
   "aaaabb"
   Prelude> do x <- Just 5; Just (x*2)
   Just 10
   Prelude> do x <- Nothing; Just (x*2)
   Nothing
   Prelude> do x <- Just 5; return (x*2)
   Just 10
   Prelude> do Just 5; Just 6
   Just 6
   Prelude> do [2,5,0]; "bc"
   "bcbcbc"
   Prelude> do [2,5,0]; "bc"; [0]
   [0,0,0,0,0,0]

défi 3 : Jeu de devinette (types)
-----------------------------------

.. literalinclude:: code/io.hs
   :language: Haskell
   :lines: 1-8

défi 3 : Jeu de devinette (suite)
-----------------------------------

.. literalinclude:: code/io.hs
   :language: Haskell
   :lines: 11-26

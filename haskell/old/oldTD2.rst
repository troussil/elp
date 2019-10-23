================================================
Fonctions récursives et modèles de traitement
================================================

Avant-propos
========================

Pourquoi Haskell ?
------------------------

- Haskell est très formateur car purement fonctionnel : on ne peut faire autrement que coder dans un style fonctionnel. 
- La programmation fonctionnelle, ancienne, explose de nouveau dans le web (python, js). 
- Les concepts de Haskell sont en train de faire leur chemin dans d'autres langages plus populaires
  (list comprehension de python, generics de java, etc.).

  
Fonctions récursives
========================


Défi 1. ``compress``
-----------------------

Définissez la fonction

.. literalinclude:: code/compress.hs
   :language: haskell
   :lines: 1

qui supprime les copies consécutives des éléments d'une liste.
      
.. code-block:: none

    *Main> compress "aaaabccaadeeee"
    "abcade"

    
``Let-expression``
---------------------------

Parfois on a besoin de localement se référer à une valeur intermédiaire.

Une ``let-expression`` (``let var = expr1 in expr2``)  peut 
être utilisée partout où une expression est requise. 

.. code-block:: haskell

   (let x = 2 in x*2) + 3

Dans certains cas (dans le REPL par exemple), un
``let-statement`` (``let var = expr``)  peut 
être utilisé.  

.. code-block:: none

   Prelude> let x = 2
   Prelude> x*2 + 3
   7


Clause ``where``
--------------------------

La clause ``where`` permet de partager une variable entre des parties
d'une définition qui ne forme pas syntaxiquement une expression.  

.. code-block:: haskell

   f x
       | cond x   = a
       | otherwise = g a
     where
       a = w x
       
   
Défi 2. ``encode``
-----------------------

Définissez la fonction

.. literalinclude:: code/encode.hs
   :language: haskell
   :lines: 1

qui encode une liste donnée de façon à ce que toute suite
de ``n`` éléments égaux à ``x`` soit remplaçée par le tuple ``(n,x)``.
	   
.. code-block:: none

   *Main> encode "aaaabccaadeeee"
   [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]

Astuce : on suppose qu'on connaît le résultat pour une liste ``xs``.
Comment construire incrémentalement le résultat pour ``x:xs`` ?
   


Modèles de traitement
========================

Récursion
------------------

- Vous imaginez peut-être qu'on définit toujours une fonction de manière récursive en Haskell. 
- Mais en fait, les programmeurs expérimentés ne le font que rarement.
- En pratique, il y a des *modèles de traitement* très fréquents qu'implémentent certaines fonctions.
- Le programmeur peut alors rester sur un plus haut niveau en combinant ces fonctions avec
  l'opérateur de composition ``.`` (``f (g x)`` est équivalent à ``(f . g) x``). 

.. head (reverse "qmlsdjf") et (head . reverse) "qmslkdfj"

  
Prélude
-------------------------

Le Prélude est un module doté d'un grand nombre de fonctions standards.
Il est implicitement importé dans chaque programme Haskell. 

- Certaines fonctions ont déjà été mentionnées : 
  ``length, head, tail, take, drop, init, last, reverse, elem``, etc.

- Nous allons en voir d'autres maintenant : 
  ``map``, ``filter``, ``foldl``, ``foldr``, ``foldl'``, etc. 
  

  
``filter``
-------------------------

Un premier modèle de traitement consiste à ne conserver d'une liste que les éléments vérifiant une certaine condition. 

.. code-block:: haskell
		
   filter :: (a -> Bool) -> [a] -> [a] 	
		
Le premier argument est un *prédicat*, c'est-à-dire une fonction qui
prend un ``a`` et qui répond oui ou non. Le second argument est une
liste d'élément de type ``a``. 
Tous les éléments pour lesquels la réponse est positive sont 
dans la liste résultante (de taille plus petite ou égale).

Par exemple, l'expression ``filter (\x -> x < 10) [9,10,11,12]`` est évaluée
à ``[9]``. 

Défi 3 : ``filter``
-------------------------

En utilisant les fonctions ``filter`` et ``length``, donnez l'expression
qui retourne le nombre de 'a' dans la chaîne de caractère "aaaabccaadeeee".  

``map``
-------------------------

Un deuxième modèle de traitement consiste à appliquer une même opération à tous les éléments d'une liste. 

.. code-block:: haskell
		
    map :: (a -> b) -> [a] -> [b] 

Le premier argument est une fonction qui prend un ``a`` et retourne un ``b``.
Celle-ci est appliquée à tous les éléments de type ``a`` d'une liste,
ce qui donne une liste d'éléments de type ``b`` (de même taille). 
    
En ayant défini ``inc x = x + 1``, l'expression ``map inc [1,2,3]`` est évaluée
en ``[2,3,4]``. 

Applications partielles 
---------------------------

.. code-block:: haskell

   add :: Integer -> Integer -> Integer
   add x y = x + y

``add`` peut être évaluée avec deux arguments *ou un seul*,
dans ce dernier cas, il s'agit d'une *application partielle*.  

.. code-block:: haskell

   add 2 3 :: Integer -- resultat de l'addition 2 + 3
   add 1 :: Integer -> Integer -- fonction qui ajoute 1

Donc on peut aussi écrire ``map (add 1) [1,2,3]``

Sections
-------------------------

En Haskell, une *section* est l'application partielle d'un opérateur infixe
(qui s'écrit entre les deux opérandes). Par exemple :

- ``(+)`` :math:`\equiv` ``\x y -> x+y``
- ``(x+)`` :math:`\equiv` ``\y -> x+y``
- ``(+y)`` :math:`\equiv` ``\x -> x+y``
- ``inc = (+1)``
- ``add = (+)``
    
Donc on peut aussi écrire ``map (+1) [1,2,3]``


Défi 4 : ``map``
-------------------------

En utilisant la fonction ``addElemInList`` et ``map``, donnez l'expression qui
transforme le codage ``[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]`` en
``["aaaa","b","cc","aa","d","eeee"]``. 

   
Définition de liste en extension
----------------------------------

Il existe une syntaxe particulière permettant de définir des listes en extension (appelée *list comprehension*)  
et qui remplace avantageusement l'utilisation de ``map`` et ``filter`` dans un grand nombre de cas.  

.. code-block:: haskell

    [ f x | x <- xs, x < k ]

signifie "la liste de tous les ``f x`` telle que ``x`` vienne de la liste ``xs`` et soit inférieur à ``k``" 
et s'inspire de la notation mathématique 

.. math::

   \{ f(x) | x \in xs, x < k \}.


Défi 5 : ``comprehension list``
---------------------------------

En utilisant la notation précédente, traduisez en Haskell l'algorithme suivant :

.. code-block:: none

   quicksort(l)
     soit r une liste vide
     si la liste l contient au moins un élément e (le premier), alors
       soit l- (resp. l+) la liste contenant tous les éléments de l
       étant inférieurs (resp. supérieurs ou égaux) à e
       r <- concaténation de quicksort(l-), (e), quicksort(l+) 
     return r
       
``foldr`` and co.
----------------------

Un troisième modèle de traitement consiste à combiner les éléments d'une liste.

Il est connu sous le nom *fold* (ou *reduce* dans d'autres langages). Il y a
plusieurs variantes en Haskell : `foldr, foldl or foldl' <https://wiki.haskell.org/Foldr_Foldl_Foldl%27>`_ 

A partir d'une fonction binaire ``f`` et d'une valeur initiale ``z``,
les fonctions ``foldr`` et ``foldl`` vont combiner les valeurs d'une liste,
en partant de la droite (r pour right) ou de la gauche (l pour left). 

- ``foldr f z [a,b,c]`` :math:`\equiv` ``a `f` (b `f` (c `f` z))``
- ``foldl f z [a,b,c]`` :math:`\equiv` ``((z `f` a) `f` b) `f` c``
- ``foldl'`` est une variante plus efficace de ``foldl``

``foldr`` en détail
-----------------------

Soit la définition (légèrement différente de celle du Prélude)

.. code-block:: Haskell

   foldr :: (a -> b -> b) -> b -> [a] -> b

La premier argument est une fonction qui combine un ``a`` et un ``b`` et retourne une valeur de type ``b``.
Le deuxième est une valeur initiale de type ``b``. Le troisième une liste de ``a``.


La fonction de combinaison va être d'abord appliquée à la valeur initiale ``z``
et à l'élément de fin de liste (le plus à droite). 
Le résultat sera ensuite combiné avec l'élément précédent et ainsi de suite.
La dernière application de la fonction de combinaison retournera la valeur finale. 

Défi 6 : ``foldr`` 
----------------------------

En utilisant la fonction ``foldr``, donnez l'expression qui transforme la liste
``["aaaa","b","cc","aa","d","eeee"]`` en la chaîne ``"aaaabccaadeeee"``



Point-free style
-----------------------

Grâce à l'application partielle, comme pour ``inc = (+1)``,
il est possible de définir ``mySum`` sans donner aucun argument :

.. literalinclude:: code/mySum2.hs
   :language: haskell
   :lines: 1-2

au lieu de

.. literalinclude:: code/mySum.hs
   :language: haskell
   :lines: 2

	   
C'est le "point-free style" (où "point" réfère aux arguments)
qu'adopte les développeurs expérimentés parce qu'il décrit
ce qu'*est* une fonction, plutôt que ce qu'elle *fait*. 

Défi bonus : définir quelques fonctions
-------------------------------------------

Comme dans le Prélude, définissez les fonctions suivantes à l'aide de ``foldr`` : 

- ``length :: [a] -> Int``
- ``sum :: Num a => [a] -> a``
- ``product :: Num a => [a] -> a``
- ``and :: [Bool] -> Bool``
- ``or :: [Bool] -> Bool``
- ``any :: (a -> Bool) -> [a] -> Bool``
- ``all :: (a -> Bool) -> [a] -> Bool``

  
Conclusion
=====================

Capacités/Connaissances
-----------------------------

- Citer des exemples où une fonction est passée en argument à une autre fonction.   
- Utiliser à bon escient les fonctions ``filter``, ``map``, ``foldr`` ou ``foldl``, etc.
- Définir une fonction sur les listes par une syntaxe en extension ou par des fonctions usuelles.   
  

Défi 1. ``compress``
-----------------------


.. literalinclude:: code/compress.hs
   :language: haskell

Défi 2. ``encode``
-----------------------

.. literalinclude:: code/encode.hs
   :language: haskell


Défi 3 : ``filter``
------------------------

.. code-block:: none

   Prelude> length (filter (\x -> x == 'a') "aaaabccaadeeee")
   6
	       
.. code-block:: none

   Prelude> ( length . filter (\x -> x == 'a') ) "aaaabccaadeeee"
   6

   
Défi 4 : ``map``
---------------------------------

.. code-block:: none

   *Main> :l addElemInList.hs 
   [1 of 1] Compiling Main             ( addElemInList.hs, interpreted )
   Ok, modules loaded: Main.
   *Main> let f t = case t of (nb, elem) -> addElemInList elem nb []
   *Main> map f [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
   ["aaaa","b","cc","aa","d","eeee"]

Défi 5 : ``comprehension list``
---------------------------------

.. literalinclude:: code/quicksort.hs
   :language: haskell
	      
Défi 6 : ``foldr``
---------------------

.. code-block:: none

   Prelude> foldr (++) [] ["aaaa","b","cc","aa","d","eeee"]
   "aaaabccaadeeee"

		
Défi bonus : définir quelques fonctions
-------------------------------------------

.. literalinclude:: code/withFoldr.hs
   :language: haskell


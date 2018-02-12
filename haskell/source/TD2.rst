================================================
Evaluation paresseuse et modèles de traitement
================================================


Evaluation paresseuse
========================

Qu'est-ce que la paresse ? 
----------------------------

En Haskell, les expressions ne sont pas évaluées tant que leur valeur n'est pas requise.
Quand une expression est donnée en argument à une fonction, celle-ci est simplement mémorisée
dans une structure dédiée aux expressions non évaluées, sans aucun traitement supplémentaire. 

- Les expressions sont évaluées lors d'une mise en correspondance avec un motif. 
- Mais elles ne sont pas évaluées complètement, seulement du minimum requis pour la mise en correspondance.

Exemple
-------------------------

Soit la fonction suivante :

.. code-block:: haskell

    f x _ = x + 2		

NB. ``_`` est un *joker* qui évite de conserver la valeur associée
quand on en n'a pas besoin dans la partie droite.

Les évaluations suivantes seront immédiates car le second argument
ne sera jamais évalué : 
    
.. code-block:: haskell

    f 5 (29^35792)		
    f 5 (1/0)		

Paresse implique pureté
---------------------------
    
Un langage pur est un langage sans effet de bord.
Les effets de bord sont des effets de l'évaluation d'une expression sur l'extérieur. 
Or ces effets surviennent dans un ordre temporel qui a de l'importance
pour le bon fonctionnement du programme.

Par exemple : 

- le moment auquel on modifie une variable globale importe, car cela pourrait affecter l'évaluation d'autres expressions.
- le moment auquel on écrit à l'écran importe, car cela pourrait affecter l'ordre d'affichage des messages.  

La paresse, qui gomme l'ordre temporel des traitements, requière donc l'absence d'effet de bord. 

Défi 1 : tracer une évaluation
-------------------------------------

Soient les fonctions suivantes : 

.. literalinclude:: code/take-repeat.hs
   :language: haskell

Tracez (sur papier) l'évaluation de : 

.. code-block:: haskell

    take 3 (repeat 7)

Complexité
---------------------------

Soit la fonction suivante

.. literalinclude:: code/reverse1.hs
   :language: haskell

où

.. literalinclude:: code/concat.hs
   :language: haskell

Pour avoir une idée de la complexité
de ce code, nous allons tracer l'évaluation
de ``myReverse [1,2,3,4]``.  
	      
Trace 
---------------------------

.. code-block:: none

    myReverse [1,2,3,4] {second motif de myReverse}
    = myReverse [2,3,4] ++ [1]
    {le premier argument de (++) est évalué pour le matching}
    = (myReverse [3,4] ++ [2]) ++ [1] {même chose...}
    = ((myReverse [4] ++ [3]) ++ [2]) ++ [1]		
    = (((myReverse [] ++ [4]) ++ [3]) ++ [2]) ++ [1] {premier motif}		
    = ((([] ++ [4]) ++ [3]) ++ [2]) ++ [1] {premier motif de (++)}
    = (([4] ++ [3]) ++ [2]) ++ [1] {second motif de (++)}
    = ((4 : [] ++ [3]) ++ [2]) ++ [1] 
    {le second motif de (++) est immédiatement choisi,
    car (4 : [] ++ [3]) correspond clairement à (x:xs)}
    = (4 : ([] ++ [3]) ++ [2]) ++ [1] {même chose...}
    = 4 : (([] ++ [3]) ++ [2]) ++ [1] 
    {il reste à évaluer le second argument de (:) pour obtenir la liste,
    donc pour chaque élément on remonte la suite de concaténations...}

    
Défi 2 : complexité
----------------------------

Pour la version suivante, tracez (sur papier) l'évaluation de ``myReverse [1,2,3,4]``.
D'après vous, quelle est la complexité de cette version de ``myReverse`` ?

.. _reversecode:

.. literalinclude:: code/reverse2.hs
   :language: haskell


Conséquence 1 : structures de contrôle 
----------------------------------------

.. code-block:: haskell

    myBinaryAnd e1 e2 = case (e1, e2) of
		(False, _) -> False
		(True, x) -> x
		
.. code-block:: haskell

    myIf cond e1 e2 = case (cond, e1, e2) of
		       (True, e1, _) -> e1
		       (False, _, e2) -> e2

Grâce à la paresse, les deux appels suivants retournent immédiatement.  

.. code-block:: haskell

    myBinaryAnd False (head [] == 'x')
    myIf True 2 (34^9784346 > 34987345) 		



Conséquence 2 : structures de données infinies
-----------------------------------------------

.. literalinclude:: code/numsFrom.hs
   :language: haskell
		
.. code-block:: none

   *Main> numsFrom 1
   [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,
   27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,
   50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,
   73,74,75,76,77,78,79,80,81Interrupted.

.. code-block:: none

   *Main> take 5 (numsFrom 1)
   [1,2,3,4,5]

   
Conséquence 3 : pipeline par composition
------------------------------------------

.. literalinclude:: code/pipelines.hs
   :language: haskell

Ce programme d'une ligne compte et affiche sur la sortie standard la taille des lignes lues sur l'entrée standard.
A ce stade, vous n'avez pas besoin de comprendre chaque fonction (cf. `Hoogle <https://www.haskell.org/hoogle/>`_
pour en savoir plus), seulement que l'opérateur ``.`` est l'opérateur
de composition : :math:`f \circ g` s'écrit ``f . g`` en Haskell. 

Bien que le programme ne soit qu'une suite de compositions, l'affichage commence avant que toutes les lignes soient lues.
En effet, grâce à l'évaluation paresseuse, dès qu'une ligne est lue, sa taille est aussitôt affichée. A essayer!



Modèles de traitement
========================

Récursion
------------------

- Vous imaginez peut-être qu'on définit toujours une fonction de manière récursive en Haskell. 
- Mais comme le suggère le dernier exemple, les programmeurs expérimentés ne le font que rarement.
- En pratique, il y a des *modèles de traitement* très fréquents qu'implémentent certaines fonctions.
- Le programmeur peut alors rester sur un plus haut niveau en combinant ces fonctions.   

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

En utilisant la fonction ``filter``, ainsi que les opérateurs ``==`` (égalité)
et ``!=`` (différence), définissez récursivement la fonction  

.. literalinclude:: code/group2.hs
   :language: haskell
   :lines: 1

qui retourne une liste regroupant tous les éléments égaux dans des sous-listes. 

.. code-block:: none

   *Main> group "banana"
   ["b","aaa","nn"]

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
---------------------------------

En utilisant votre fonction précédente ``group``,
ainsi que la fonction ``map``, définissez la fonction  

.. literalinclude:: code/encode2.hs
   :language: haskell
   :lines: 1

qui retourne la liste des différents éléments et de leur répétition
dans une liste donnée.
	   
.. code-block:: none

   *Main> encode "banana"
   [('b',1),('a',3),('n',2)]

..  Défi 4 : ``map``
    -------------------------

    En utilisant la fonction ``map``,

    - définissez une fonction qui, à partir d'une liste de chaîne de caractères,
      renvoie la liste de leur longueur.
    - définissez une fonction qui, à partir d'une liste de chaîne de caractères non vides,
      renvoie la liste de leur première lettre. 

    Astuce : pensez aussi aux fonctions ``length`` et ``head``. 

   
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

.. code-block:: Haskell

   foldr f z []     = z
   foldr f z (x:xs) = f x (foldr z f xs)

La fonction de combinaison va être d'abord appliquée à la valeur initiale ``z``
et à l'élément de fin de liste (le plus à droite). 
Le résultat sera ensuite combiné avec l'élément précédent et ainsi de suite.
La dernière application de la fonction de combinaison retournera la valeur finale. 

Défi 6 : reverse par foldl 
----------------------------

Définissez la fonction

.. literalinclude:: code/reverse3.hs
   :language: haskell
   :lines: 1

en utilisant la fonction ``foldl`` (une ligne de code).


.. Défi 6 : sommation 
   -----------------------

   Définissez la fonction

   .. literalinclude:: code/mySum.hs
      :language: haskell
      :lines: 1

   en utilisant la fonction ``foldr``.

   .. code-block:: none

      *Main> mySum [1..10]
      55


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

- Expliquer le mécanisme de l'évalution paresseuse
- Tracer l'évaluation d'expressions données
- Citer des exemples où une fonction est passée en argument à une autre fonction.   
- Utiliser à bon escient les fonctions ``filter``, ``map``, ``foldr`` ou ``foldl``, etc.
- Définir une fonction sur les listes par une syntaxe en extension ou par des fonctions usuelles.   
  

Défi 1 : tracer une évaluation (aussi `là <http://www.cis.upenn.edu/~cis194/spring13/lectures/06-laziness.html>`_)
---------------------------------------------------------------------------------------------------------------------

.. code-block:: none


      take 3 (repeat 7)
          { 3 <= 0 is False, so we proceed to the second clause, which
	    needs to match on the second argument. So we must expand
	    repeat 7 one step. }
    = take 3 (7 : repeat 7)
          { the second clause does not match but the third clause
            does. Note that (3-1) does not get evaluated yet! }
    = 7 : take (3-1) (repeat 7)
          { In order to decide on the first clause, we must test (3-1)
            <= 0 which requires evaluating (3-1). }
    = 7 : take 2 (repeat 7)
          { 2 <= 0 is False, so we must expand repeat 7 again. }
    = 7 : take 2 (7 : repeat 7)
          { The rest is similar. }
    = 7 : 7 : take (2-1) (repeat 7)
    = 7 : 7 : take 1 (repeat 7)
    = 7 : 7 : take 1 (7 : repeat 7)
    = 7 : 7 : 7 : take (1-1) (repeat 7)
    = 7 : 7 : 7 : take 0 (repeat 7)
    = 7 : 7 : 7 : [] = [7,7,7]
		

Défi 2 : complexité en :math:`O(n)`
---------------------------------------

.. code-block:: none

    myReverse [1,2,3,4] 
    = rev [1,2,3,4] [] {motif 2 de rev...}
    = rev [2,3,4] 1:[]
    = rev [3,4] 2:1:[]
    = rev [4] 3:2:1:[]
    = rev [] 4:3:2:1:[] {motif 1 de rev}
    = 4:3:2:1:[] {(:) est associatif à droite}
    = 4:3:2:[1]
    = 4:3:[2,1]
    = 4:[3,2,1]
    = [4,3,2,1]

Défi 3 : ``filter``
------------------------

.. literalinclude:: code/group2.hs
   :language: haskell

Défi 4 : ``map``
---------------------------------

.. literalinclude:: code/encode2.hs
   :language: haskell

Défi 5 : ``comprehension list``
---------------------------------

.. literalinclude:: code/quicksort.hs
   :language: haskell
	      
Défi 6 : reverse par foldl 
----------------------------

.. literalinclude:: code/reverse3.hs
   :language: haskell

Défi bonus : définir quelques fonctions
-------------------------------------------

.. literalinclude:: code/withFoldr.hs
   :language: haskell


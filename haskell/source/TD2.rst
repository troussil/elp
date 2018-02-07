=========================================
Evaluation paresseuse et 
=========================================


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

NB. ``_`` est un *joker*, évite de conserver la valeur associée
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

Tracez l'évaluation de : 

.. code-block:: haskell

    take 3 (repeat 7)

    
Défi 2 : complexité
----------------------------

Pour ces deux versions, tracez (sur papier) l'évaluation de ``myReverse [1,2,3,4]``.
D'après vous, quelle est la complexité de ces deux versions de ``myReverse`` ?

.. literalinclude:: code/reverse1.hs
   :language: haskell

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

Le Prélude est un module doté d'un grand nombre de fonctions standards,
implicitement importé dans chaque programme Haskell. 

- Certaines fonctions ont déjà été mentionnées : 
  ``length, head, tail, take, drop, init, last, reverse, elem``, etc.

- Nous allons en voir d'autres maintenant : 
  ``map``, ``fmap``, ``filter``, ``foldl``, ``foldr``, ``foldl'``, etc. 
  

map, fmap
-------------------------

Un premier modèle de traitement consiste à appliquer une même opération à tous les éléments d'une liste. 

exemple

filter
-------------------------

Un deuxième modèle de traitement consiste à ne conserver d'une liste que les éléments vérifiant une certaine condition. 

exemple

foldl
-------------------------

Un troisième modèle de traitement consiste à "résumer" les éléments d'une liste

exemple

foldl vs foldl'
-------------------------

zip ?
-------------------------

list comprehension
--------------------------

..
  Tacit programming, also called point-free style

Conclusion
=====================

Capacités/Connaissances
-----------------------------

- Expliquer le mécanisme de l'évalution paresseuse
- Tracer l'évaluation d'expressions données
- Citer les fonctions usuelles ``filter``, ``map``, ``foldl``, etc.
- Définir une fonction sur les listes par une syntaxe en extension ou par composition des fonctions usuelles.   
  

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
		
Défi 2 : version 1 en :math:`O(n^2)`
---------------------------------------

.. literalinclude:: code/concat.hs
   :language: haskell

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

..		
    = 4 : (([] ++ [3]) ++ [2]) ++ [1] {premier motif de (++)}
    = 4 : ([3] ++ [2]) ++ [1] 
    = 4 : (3 : [] ++ [2]) ++ [1] 
    = 4 : 3 : ([] ++ [2]) ++ [1] 

Défi 2 : version 2 en :math:`O(n)`
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

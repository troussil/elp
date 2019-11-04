==============================
Fonctions
==============================


Définition et appel de fonctions
====================================

Définitions
--------------------------

Dans un premier temps, on écrit les fonctions dans un fichier ``.hs`` qu'on charge avec ``:load``
après avoir lancé le REPL en tapant ``ghci``. 

On utilise deux sortes de définition: l'équation et la signature.

- Les fonctions sont définies par une *équation* comme dans
  ``inc n = n + 1``.
- On peut aussi explicitement typer la fonction (*signature*) : 
  ``inc :: Integer -> Integer``

  plus généralement ``inc :: Num a => a -> a``
- En général, il n'est pas nécessaire de donner cette signature car elle est
  automatiquement déduite, mais c'est une forme de documentation utile. 

Lambda-fonctions
--------------------------

Les équations sont en fait une manière élégante de définir des fonctions

.. code-block:: haskell

    inc x = x + 1
    add x y = x + y

équivalente à l'utilisation de *lambda-fonctions* (fonctions anonymes,
où la barre oblique rappelle le plus long des deux traits d'un lambda). 
    
.. code-block:: haskell

    inc = \x -> x + 1
    add = \x y -> x + y 

Appels de fonctions
------------------------

En Haskell, il n'y a pas besoin de parenthèses pour appeler une fonction.
Tous les arguments sont listés après la fonction.

Schéma général : ``func arg1 arg2 arg3 ...``


Exemple 1 : ``func arg1 (add 2 5) arg ...``.
Le deuxième argument est ``add 2 5``.

Exemple 2 : ``func arg1 add 2 5 arg ...``.
Les arguments 2, 3, 4 sont respectivement ``add, 2`` et ``5``.


Opérateurs et fonctions
----------------------------

Les opérateurs binaires tels que ``:`` ou ``++`` ne sont rien d'autre que des fonctions à deux paramètres.

Ce sont généralement des signes qui sont placés entre les deux arguments :
``1 : [2,3]``. Mais on peut tout aussi bien les entourer de parenthèses pour les utiliser comme des fonctions :
``(:) 1 [2,3]``.

Inversement, les fonctions peuvent être placées entre quotes penchées pour les utiliser comme des opérateurs infixes : 
``take 2 "abc"`` est equivalent à ``2 `take` "abc"`` .

Premières définitions
------------------------------

- Ajoutez les définitions de ``inc`` et ``add`` dans un fichier appelé ``ex.hs``. 
- Dans GHCI, chargez votre fichier avec ``:load ex.hs``.
- Que font les appels suivants ?

  - ``add 2 5``
  - ``3 `add` 6``
  - ``4 + 7``
  - ``(+) 4 7``
  - ``inc 3``
  - ``(add 1) 3``
  - ``add (1 3)``
  
Distinction des cas
================================
    
Stratégie récursive
-----------------------------

- De nombreuses fonctions nécéssitent la répétition d'un traitement.
- Comme il n'y a pas de boucles, on les définit récursivement.
- Il est donc nécessaire de distinguer le cas de base, du cas récursif.
    
  - soit par des gardes (*guards*) pour distinguer des intervalles de valeurs.
  - soit par une *case-expression* pour brancher selon la structure d'une valeur.

Gardes
----------------

Les équations peuvent contenir des *gardes*. 

- Etant donnée ``f arg = expr``,
  l'appel ``f val`` sera toujours evalué à ``expr``,
  
- tandis qu'avec ``f arg | cond = expr``,
  l'appel ``f val`` est évalué à ``expr`` si et seulement si ``cond`` est vrai,
  où ``cond`` est une expression booléenne impliquant l'argument, comme par exemple ``arg > 0``.

Gardes multiples
--------------------

Plusieurs gardes peuvent être chaînés. Dans ce cas, ils sont testés les uns après les autres.

.. literalinclude:: code/sign1line.hs
   :language: haskell

.. literalinclude:: code/sign.hs
   :language: haskell
	      
sont la traduction Haskell de :

.. math::
   
   \text{sign}(x) = \left\{
          \begin{array}{ll}
            1 & \qquad \mathrm{si}\quad x > 0 \\
            0 & \qquad \mathrm{si}\quad x = 0 \\
            -1 & \qquad \mathrm{sinon} \\
          \end{array}
   \right.
	
Remarque : *layout*
--------------------------

Haskell utilise l'alignement en colonne pour accroître la lisibilité. 
Une ligne indentée plus à droite que la précédente est la suite de la ligne précédente.

Ci-dessous, il y a une erreur car aucune expression ne peut commencer par ``|``.

.. literalinclude:: code/layout.hs
   :language: haskell
	      
.. code-block:: none
			      
   2:1: parse error on input ‘|’

Remarque : commentaires
--------------------------------

.. code-block:: haskell

   -- Un commentaire en une ligne commence avec deux tirets.
   {- Un commentaire sur plusieurs lignes peut être contenu dans
   un bloc de cette façon.
   -}

Notez que dans les fichiers d'extension ``.lhs``, seules les lignes
précédées par ``>`` et espace (*Bird style*) sont considérées comme
du code Haskell.
   
défi 1. ``addElemInList`` 
---------------------------

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

Case expression 
------------------------

Une *case-expression* fournit le moyen de distinguer plusieurs cas
selon une valeur et sa structure. 

.. literalinclude:: code/myLen.hs
   :language: haskell

- dans le cas où ``lst`` est une liste vide, ``myLen lst`` est évaluée en ``0``,

- dans le cas où ``lst`` est composé d'un élément ``x`` en tête d'une liste ``xs`` (possiblement vide)
  ``myLen lst`` est évaluée en ``1 + myLen xs``.

Tous les membres de gauche (devant les flèches) et tous les membres de droite (derrière) doivent avoir le même type,
respectement ``[a]`` et ``Integer`` dans cet exemple. 

Pattern matching
-----------------------

Dans une *case-expression* les membres de gauche sont des motifs (*patterns*)
avec lesquels est mise en correspondance (*matching*) la valeur associée au paramètre.

Une mise en correspondance avec un motif peut soit

- *produire une erreur*, si la valeur contient une erreur ;
- *échouer*, si la valeur ne correspond pas au motif (``[1,2,3] != []``).
  Dans ce cas, le motif suivant est essayé et si tout échoue, une erreur se produit ; 
- *réussir* sinon (``[1,2,3] = 1:[2,3]``, correspondant au motif ``x:xs`` où ``x=1`` et ``xs=[2,3]``).
  Dans ce cas, le membre de droite est évalué et retourné comme résultat de l'appel.
  

défi 2. ``dupli`` 
--------------------------

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

Sucre syntaxique
--------------------

Dans un programme Haskell, une fonction est assez souvent définie par une série d'équations car

.. code-block:: haskell
	   
   f x1 x2 ... xk = case (x1, ..., xk) of
		     (p11, ..., p1k) -> e1
		     ...
		     (pn1, ..., pnk) -> en

est équivalent à 
		   
.. code-block:: haskell

    f p11 ... p1k = e1
    ...
    f pn1 ... pnk = en

Mais une case expression peut être utilisée
aussi dans une expression plus complexe.
    

Fonctions récursives
========================


défi 3. ``compress``
-----------------------

Définissez la fonction

.. literalinclude:: code/compress.hs
   :language: haskell
   :lines: 1

qui supprime les copies consécutives des éléments d'une liste.
      
.. code-block:: none

    *Main> compress "aaaabccaadeeee"
    "abcade"

    
Let-expression
---------------------------

Parfois on a besoin de localement se référer à une valeur intermédiaire.

Une *let-expression* (``let var = expr1 in expr2``)  peut 
être utilisée partout où une expression est requise. 

.. code-block:: haskell

   (let x = 2 in x*2) + 3

Dans certains cas (REPL ou notation ``do``), un
*let-statement* (``let var = expr``)  peut 
être utilisé.  

.. code-block:: none

   Prelude> let x = 2
   Prelude> x*2 + 3
   7


Clause ``where``
--------------------------

La clause *where* permet de partager une variable entre des parties
d'une définition qui ne forme pas syntaxiquement une expression.  

.. code-block:: haskell

   f x
       | cond x   = a
       | otherwise = g a
     where
       a = w x
       
   
défi 4. ``encode``
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

défi 5 : ``filter``
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


défi 6 : ``map``
-------------------------

En utilisant la fonction ``addElemInList`` et ``map``, donnez l'expression qui
transforme le codage ``[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]`` en
``["aaaa","b","cc","aa","d","eeee"]``. 

   
Définition de liste en intention
----------------------------------

Il existe une syntaxe particulière permettant de définir des listes en intention (appelée *list comprehension*)  
et qui remplace avantageusement l'utilisation de ``map`` et ``filter`` dans un grand nombre de cas.  

.. code-block:: haskell

    [ f x | x <- xs, x < k ]

signifie "la liste de tous les ``f x`` telle que ``x`` vienne de la liste ``xs`` et soit inférieur à ``k``" 
et s'inspire de la notation mathématique 

.. math::

   \{ f(x) | x \in xs, x < k \}.

       
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

.. code-block:: Haskell

   foldr :: (a -> b -> b) -> b -> [a] -> b

La premier argument est une fonction qui combine un ``a`` et un ``b`` et retourne une valeur de type ``b``.
Le deuxième est une valeur initiale de type ``b``. Le troisième une liste de ``a``.


La fonction de combinaison va être d'abord appliquée à la valeur initiale ``z``
et à l'élément de fin de liste (le plus à droite). 
Le résultat sera ensuite combiné avec l'élément précédent et ainsi de suite.
La dernière application de la fonction de combinaison retournera la valeur finale. 

défi 7 : ``foldr`` 
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
===================


Capacités/Connaissances
--------------------------

- Définir une fonction récursive sur des listes.
- Expliquer le mécanisme du *pattern matching* et de l'évalution paresseuse
- Tracer l'évaluation d'expressions données
- Citer des exemples où une fonction est passée en argument à une autre fonction.   
- Utiliser à bon escient les fonctions ``filter``, ``map``, ``foldr`` ou ``foldl``, etc.
- Définir une fonction sur les listes par une syntaxe en extension ou par des fonctions usuelles.   


défi 1. ``addElemInList`` 
---------------------------


.. literalinclude:: code/addElemInList.hs
   :language: haskell

défi 2. ``dupli`` 
--------------------------


.. literalinclude:: code/duplicate.hs
   :language: haskell


défi 3. ``compress``
-----------------------


.. literalinclude:: code/compress.hs
   :language: haskell

défi 4. ``encode``
-----------------------

.. literalinclude:: code/encode.hs
   :language: haskell


défi 5 : ``filter``
------------------------

.. code-block:: none

   Prelude> length (filter (\x -> x == 'a') "aaaabccaadeeee")
   6
	       
.. code-block:: none

   Prelude> ( length . filter (\x -> x == 'a') ) "aaaabccaadeeee"
   6

   
défi 6 : ``map``
---------------------------------

.. code-block:: none

   *Main> :l addElemInList.hs 
   [1 of 1] Compiling Main             ( addElemInList.hs, interpreted )
   Ok, modules loaded: Main.
   *Main> let f t = case t of (nb, elem) -> addElemInList elem nb []
   *Main> map f [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
   ["aaaa","b","cc","aa","d","eeee"]

     
défi 7 : ``foldr``
---------------------

.. code-block:: none

   Prelude> foldr (++) [] ["aaaa","b","cc","aa","d","eeee"]
   "aaaabccaadeeee"

		
Défi bonus : définir quelques fonctions
-------------------------------------------

.. literalinclude:: code/withFoldr.hs
   :language: haskell


========================================
Prise en main, définition des fonctions
========================================

Caractéristiques
=========================

Carte d'identité
------------------------

- Haskell a été créé en 1990 par un ensemble d'universitaires.
- Il est défini dans un document appelé *Haskell Report*, dont la dernière révision date de 2010.
- C'est un langage de programmation
    
  - fonctionnel
  - pur
  - paresseux
  - statiquement typé
  
    
Langage fonctionnel
------------------------

- Toutes les actions sont faites par l'évaluation d'*expressions* menant à des *valeurs*.
  
  Par exemple ``inc (inc 3)`` donne ``5``. 
- Les valeurs sont des entités abstraites comme ``5, 'a', [1,2,3], ('b',4), inc``. 
- Les fonctions sont définies sous forme équationnelle

  ``inc n = n + 1``.     
- Toutes les valeurs (y compris les fonctions) sont *first-class*. 
  Elles peuvent être passés comme arguments à des fonctions,
  retournées comme résultats, placées dans des structures de données, etc.     

Pureté
------------------------

- Les structures de données sont toujours figées (*immutable*).  
- Les expressions n'ont jamais d'effets de bord (comme modifier des variables globales ou effectuer des entrées-sorties).
  Leur évaluation mènera toujours au même résultat. 

- Le parallélisme est plus direct quand l'évaluation d'une expression est indépendante des autres.
- La forme équationnelle permet de mieux raisonner sur les programmes.
  Comme en algèbre, on peut toujours remplacer la partie gauche d'une equation par
  sa partie droite dans une seconde équation :

  ``inc (inc 3) = inc (3 + 1) = (3 + 1) + 1 = 5``. 

Paresse
--------------------------

En Haskell, les expressions ne sont pas évaluées tant que leur valeur n'est pas requise: 

``inc (3 + 1)`` est évalué en ``(3 + 1) + 1`` et  non ``inc 4``! 

Par exemple, ``v = 1/0`` ne produit aucune erreur, car cela signifie `v est défini comme étant 1/0`,
plutôt que `calculer 1/0 et stocker le résultat dans v`. Ce n'est que lorsque ``v`` sera utilisé
(test d'égalité, affichage, etc.) que l'erreur surviendra.

Par conséquent, il est possible :
- de définir ses propres structures de contrôle (`if` par exemple)
- de manipuler des structures de données infinies ou de taille exponentielle.
    
Typage statique fort
---------------------------      
      
- Chaque valeur à un type associé.
- Un type est intuitivement un ensemble de valeurs. 
- Les types sont vérifiés à la compilation.
    
- De nombreuses erreurs seront détectées à la compilation.
- Le code généré par le compilateur sera optimisé. 
- C'est aussi une aide pour raisonner sur les programmes.    
    


Compilation
=========================

GHC
---------------------------------

- GHC (Glasgow Haskell Compiler) est bien connu pour compiler
  un programme Haskell (fichier d'extension ``.hs``).

- La commande ``ghc fichier.hs -o executable`` produit
  directement un exécutable : ``./executable``.

- D'autres options sont utiles : ``-c`` pour compiler sans édition de lien et   
  ``-outputdir build`` pour mettre les fichiers intermédiaires dans un répertoire séparé ``build``
  (voir ``man ghc`` ou ``ghc --help``). 


Mode interactif
--------------------------

GHCi est un REPL (Read-Eval-Print-Loop) qui accompage GHC.

Vous pouvez évaluer des expressions,
demander le type d'une expression avec ``:type`` (``:t``),
charger des fichiers Haskell avec ``:load`` (``:l``) et ``:reload`` (``:r``),
et bien plus encore (``:?`` pour une liste des commandes disponibles).

Comme les entrées-sorties ne sont pas évidentes en Haskell
(à cause de la paresse et de la pureté du langage),
nous allons d'abord surtout utiliser ce mode interactif en tapant ``ghci``. 

Types prédéfinis
==========================

Divers types
----------------

- types atomiques :

  - ``Char``, ``Bool``, ``Int``, ``Integer``, ``Float``, ``Double``, etc. 
  - *function mappings* comme ``Integer -> Bool``, ``Integer -> Integer -> Integer``, etc. 
  
- types structurés :

  - tuples comme ``(Char, Integer)``, etc. 
  - listes comme ``[Integer]``, ``[[Integer]]``, etc. 

Listes
--------------------

Les listes sont très souvent utilisées dans les langages fonctionnels
et sont de bons exemples pour expliquer divers concepts de ces langages.

- ``[]`` est une liste vide,
- ``:`` est l'opérateur infixe qui ajoute un élément en tête de liste,
- ``,`` sépare les élements d'une liste ; ``[1,2,3] = 1:(2:(3:[])) = 1:2:3:[]``.    
- ``++`` est l'opérateur infixe qui concatène deux listes. 
- Diverses fonctions du `Prelude` sont chargées par défaut :
  ``length, head, tail, take, drop, init, last, reverse, elem``, etc. 

Inspection des types
------------------------

Par la commande ``:t`` de GHCi, donnez le type de :

- ``"azerty"``
- ``[]``
- ``(:)``
- ``(++)``
- ``head``
- ``reverse``
- ``[1,2,3]``
    
La réponse est de la forme : 

valeur ``::`` [ contexte ``=>`` ] type

Types polymorphes
--------------------

Les types composés sont *polymorphes* quand ils décrivent une famille de types :

``[a]`` désigne la famille des listes d'éléments de type ``a``
(le :math:`(\forall a)` est implicite).  

Exemple : ``[1,2,3], ['a','b','c'], ["azerty", "qwerty"]`` sont de type ``[a]``, mais pas ``[2,'b']``. 

Le *contexte* qui précède le type indique quelles opérations on peut effectuer avec les valeurs de type ``a``:

- ``Eq a`` (``==``, ``/=``)
- ``Ord a`` (``<``, ``<=``, ``>``, ``>=``, ``min``, ``max``)
- ``Show a`` (conversion en chaîne de caractères ``show``, ``shows``)
- ``Num a`` (opérations arithmétiques...)


Messages d'erreur
--------------------------

N'ayez pas peur des messages d'erreur. Ils sont longs car ils contiennent beaucoup d'information.
Voici un exemple :

.. code-block:: none

    Prelude> 'x' ++ "foo"

    <interactive>:1:1:
      Couldn't match expected type `[a0]' with actual type `Char'
      In the first argument of `(++)', namely 'x'
      In the expression: 'x' ++ "foo"
      In an equation for `it': it = 'x' ++ "foo"

Quelque chose devait être une liste (``[a0]``) mais est en fait un caractère (``Char``).
Cette chose est le premier argument de ``(++)``, c'est-à-dire ``'x'``.
Les lignes suivantes donnent un peu plus de contexte (``it`` désigne la valeur de la
dernière expression tapée dans GHCi). 


.. Résumé: types et typage
   --------------------------

   .. figure:: figs/type.svg
      :width: 80%
      :alt: types et typage
      :align: center


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
Tous les arguments sont juste listés après la fonction.

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
  - soit par une *case expression* pour brancher selon la structure d'une valeur.

Gardes
----------------

Les équations peuvent contenir des *gardes*. 
Etant donnée une equation de la forme ``func arg | predicat = expr``, l'appel ``func value`` est évalué à ``expr`` si ``predicat`` est évalué à vrai. 

Un prédicat est une expression de type booléen comme ``arg > 0``.

Gardes multiples
--------------------

Quand il y a plusieurs gardes, ils sont testés les uns à la suite des autres.

.. literalinclude:: code/sign.hs
   :language: haskell

est la traduction Haskell de :

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

Haskell évite l'utilisation de point-virgule
en utilisant l'alignement en colonne. 

- Le caractère suivant un mot-clé ou un caractère spécial comme ``|``
  détermine la colonne par où commence la déclaration.
- Cette colonne doit être plus à droite que la colonne de départ du bloc environnant.

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

Une *case expression* fournit le moyen de distinguer plusieurs cas
selon la structure d'une valeur, c'est-à-dire selon la façon dont elle est construite. 

.. literalinclude:: code/myLen.hs
   :language: haskell

Tous les membres de gauche bien sûr, mais aussi tous les membres de droite doivent avoir le même type. 	      

Pattern matching
-----------------------

Dans une *case expression* les membres de gauche sont des motifs (*patterns*)
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

Mais c'est une bonne habitude de choisir la première version, car une case expression peut être utilisée
aussi en dehors du contexte de la définition d'une fonction. 
    
défi 3. ``repli`` 
--------------------------

A l'aide de la fonction ``addElemInList``, définissez la fonction    
    
.. literalinclude:: code/replicate.hs
   :language: haskell
   :lines: 1

qui réplique un nombre de fois donné, les éléments d'une liste donnée. 

.. code-block:: none
		
    *Main> repli "azerty" 3
    "aaazzzeeerrrtttyyy"

NB. Ecrire les deux fonctions dans le même fichier. 

défi 4 : tracer une évaluation
-------------------------------------

Soient les fonctions suivantes : 

.. literalinclude:: code/take-repeat.hs
   :language: haskell

Tracez (sur papier) l'évaluation de : 

.. code-block:: haskell

    take 3 (repeat 7)


Conclusion
===================


Capacités/Connaissances
--------------------------

- Classer Haskell parmi les langages fonctionnels purs.
- Classer Haskell parmi les langages compilés à typage statique.
- Définir une fonction récursive sur des listes.
- Expliquer le mécanisme du *pattern matching* et de l'évalution paresseuse
- Tracer l'évaluation d'expressions données

défi 1. ``addElemInList`` 
---------------------------


.. literalinclude:: code/addElemInList.hs
   :language: haskell

défi 2. ``dupli`` 
--------------------------


.. literalinclude:: code/duplicate.hs
   :language: haskell

défi 3. ``repli`` 
--------------------------

    
.. literalinclude:: code/replicate.hs
   :language: haskell

défi 4 : tracer une évaluation (aussi `là <http://www.cis.upenn.edu/~cis194/spring13/lectures/06-laziness.html>`_)
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


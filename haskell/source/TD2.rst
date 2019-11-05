========================================
Types et classes
========================================

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

Type défini par le développeur
=================================

Synonymes
---------------------------------

Il est possible de définir des synonymes de types grâce à la déclaration ``type`` :

- ``type String = [Char]``
- ``type PhoneNumber = String``  
- ``type Name = String``  
- ``type PhoneBook = [(Name,PhoneNumber)]`` 
- ``type AssociationList a b = [(a,b)]``


Déclaration ``data``
----------------------------------

En Haskell, on peut définir des types *algébriques* par des déclarations ``data`` :

.. code-block:: haskell

   data constructeurDeType [params] = constructeurDeValeur [params]
                           [ | constructeurDeValeur2 [params] ... ] 

Ce sont des unions (récursives) de types produits :

.. figure:: figs/typeDeType.svg
   :width: 350pt 
   :alt: différentes sortes de type
   :align: center

.. _carte-label:
  
Type somme vs type produit 
--------------------------------

- Pour créer un *type somme*, on utilise ``|`` pour faire
  l'union des ensembles de valeur.
- Pour créer un *type produit*, on utilise simplement l'espace
  pour faire un produit cartésien des ensembles de valeur. 

.. literalinclude:: code/carte.hs
   :language: haskell
   :lines: 1-4

``CouleurCarte`` et ``ValeurCarte`` sont deux types sommes,
tandis que ``Carte`` est un type produit.
Par exemple, ``DonneCarte As Trefle`` est une valeur de type ``Carte``. 	   

Enregistrements
----------------------------------

Un enregistrement est un type produit dans lequel on nomme les champs ; ces noms pouvant servir d'accesseur.

.. literalinclude:: code/enregistrement.hs
   :language: haskell
   :lines: 1-4

.. code-block:: none
		
    *Main> nom p1
    "Curry"
    *Main> nom p2
    "Church"

Types récursifs
----------------------   

Il est possible de se référer au type que l'on crée
dans un des constructeurs de valeurs. 

.. literalinclude:: code/myList.hs
   :language: haskell
   :lines: 1

Autrement dit, une valeur de type ``IntList`` est soit la valeur ``Empty`` soit la combinaison
d'une valeur de type ``Int`` et d'une valeur de type ``IntList``, ce qui permet de représenter
une série de valeurs entières de taille quelconque.       

.. literalinclude:: code/myList.hs
   :language: haskell
   :lines: 3-5

Types polymorphes
-----------------------

Il est possible de paramétrer le constructeur de type afin de définir un type polymorphe.

.. literalinclude:: code/myList2.hs
   :language: haskell
   :lines: 1

On obtient ainsi une série contenant un nombre quelconque de valeurs de type arbitraire (mais identique pour toutes les valeurs). 

.. literalinclude:: code/myList2.hs
   :language: haskell
   :lines: 3-7

Encore un type polymorphe
----------------------------

.. code-block:: haskell

   data Point a = MakePt a a

Par exemple, à partir de tout type ``a``, le constructeur de type ``Point``
définit ``Point a``, le type des points cartésiens ayant ``a`` comme coordonnées. 

Le constructeur de valeur ``MakePt :: a -> a -> Point a`` est une manière d'obtenir
des valeurs de type ``Point a``. Par exemple, ``MakePt 2.0 3.0`` est le point :math:`(2,3)`.

Avertissement
-------------------------

Les constructeurs de type comme ``Point`` et les constructeurs de valeurs comme ``MakePt``
se trouvent dans des espaces de noms séparés. Il est donc possible de donner le même nom
aux deux constructeurs, afin de rendre plus évident le lien entre un type et le constructeur
de valeur associé. 

.. literalinclude:: code/shape.hs
   :language: haskell
   :lines: 1

	   
Liste revisitée
-------------------

``[]`` désigne à la fois un constructeur de valeur (liste vide) et un
constructeur de type qui, à partir du type ``a``, crée le type ``[] a``
(habituellement noté ``[a]``), c-à-d. une liste de ``a``.  

La définition d'une liste est récursive :

.. code-block:: haskell

   data [a] = [] | a : [a]

Une liste de ``a`` est soit une liste vide, soit une liste ayant au moins
une valeur de type ``a`` en tête de liste. 

Tuple revisité
-------------------

Bien sûr, il est possible de définir un type polymorphe par plusieurs paramètres.

.. code-block:: haskell

   data Pair a b = MakePair a b
   aPair = MakePair 5 'a' 
		
C'est de cette manière que sont définis les tuples pour lesquels 
``(,)`` désigne à la fois un constructeur de valeur (à paramètres)
et un constructeur de type qui, à partir des types ``a`` et ``b``,
crée le type ``(,) a b`` (habituellement noté ``(a,b)``).

.. code-block:: haskell

   data (,) a b = (,) a b


Autres types habituels
----------------------------------

Type ``unit``

.. code-block:: haskell

   data () = ()

Type ``Maybe``

.. code-block:: haskell

   data Maybe a = Nothing | Just a

Type ``Either``

.. code-block:: haskell

   data Either a b = Left a | Right b
   
   
Derniers types pour la route
-----------------------------------   

Comparez

.. code-block:: haskell

    data Tree a = Leaf a | Node (Tree a) (Tree a)

et

.. code-block:: haskell

    data Tree a = Empty | Node (Tree a) a (Tree a)

Dessinez et construisez des valeurs pour ces types. 


Classe et surcharge
==========================

Surcharge
---------------------------

La *surcharge* désigne le fait qu'une même opération
puisse être appliquée à des types différents *tout en* menant à des
comportements différents.

- Les opérateurs numériques comme ``+`` fonctionnent sur différents types de nombres (``Int``, ``Float``, etc.)
- Les opérateurs de comparaison comme ``<`` fonctionnent sur de très nombreux types (nombre, caractère, liste, etc.),
  mais pas tous. 

Les déclarations ``class`` et ``instance`` vont nous permettre de regrouper
les types en classe selon les opérations qu'on peut leur appliquer. 

Déclaration ``class``
--------------------------

Intuitivement, une *classe* correspond à un ensemble de type pour lesquels certaines opérations sont définies.
Par exemple, la classe ``Eq`` du Prélude correspond à tous les types ``a``
pour lesquels l'opération ``(==)`` est définie.

.. literalinclude:: code/Eq.hs
   :language: haskell

Notez que l'opérateur ``(/=)`` est implémenté directement à partir de l'opérateur ``(==)``, qui est le point de variation.
L'implémentation de cet opérateur devra être fourni par les types se déclarant comme instance de ``Eq``. 


Exemple
--------------------------

Soit le type suivant : 

.. literalinclude:: code/carte.hs
   :language: haskell
   :lines: 1

On ne peut pas faire par défaut de test d'égalité,
car l'opérateur ``(==) :: Eq a => a -> a -> Bool``
ne prend en paramètre que des types qui sont des instances de ``Eq``.

.. code-block:: none

   *Main> Trefle == Pique
	      
   <interactive>:6:8:
   No instance for (Eq CouleurCarte) arising from a use of ‘==’
   In the expression: Trefle == Pique
   In an equation for ‘it’: it = Trefle == Pique
	   

Déclaration ``instance``
--------------------------

Un type déclaré comme *instance* d'une classe doit surcharger les opérations associées à la classe.
Par exemple, pour pouvoir comparer deux couleurs de carte à jouer, il faut explicitement déclarer
le type ``CouleurCarte`` comme instance de ``Eq`` et surcharger ``(==)`` : 


.. literalinclude:: code/carte.hs
   :language: haskell
   :lines: 6-12

.. code-block:: none

   *Main> Trefle == Pique
   False
   *Main> Trefle /= Pique
   True

Classes du Prélude
-------------------------

Le Prélude a de nombreuses classes : 

- ``Eq`` (``==``, ``/=``)
- ``Ord`` (``compare``, ``<``, ``<=``, ``>``, ``>=``, ``min``, ``max``)
- ``Enum`` (facilité d'énumération ``toEnum``, ``fromEnum``, ``enumFrom``, etc.)  
- ``Show``, ``Read`` (conversion en chaîne de caractères ``show``, ``read``, etc.)
- ``Num`` (nombres ``+``, ``-``, etc.)
- ...
  
Obtenez la liste complète des opérations d'une classe et de ses instances
avec la commande ``:i`` suivi du nom de la classe dans GHCi. 

Hiérarchie de classe
----------------------------

En Haskell, il est possible d'étendre une classe. Par exemple, la classe ``Ord`` *hérite*
des opérations ``==`` et ``/=`` de ``Eq`` et possède *en plus* des opérations de comparaison,
ainsi que les fonctions ``min`` et ``max``. On dit que ``Ord`` est une *sous-classe* de ``Eq``. 

.. literalinclude:: code/Ord.hs
   :language: haskell

Une fonction qui utilise les opérations de ``Eq`` et ``Ord`` peut utiliser le contexte ``(Ord a)``
plutôt que ``(Eq a, Ord a)`` car ``Ord`` "implique" ``Eq``.

Exemple des nombres
---------------------------

.. figure:: figs/nombres.svg
   :width: 800pt 
   :alt: différentes classes de nombre
   :align: center

Conversion
----------------------

La fonction usuelle pour convertir une type entier (instance de ``Integral``)
en un type numérique (instance de ``Num``) est :

.. code-block:: haskell

   fromIntegral :: (Num b, Integral a) => a -> b

Si ``n`` est de type ``Int``, la conversion est nécessaire. 
		
.. code-block:: none

   *Main> sqrt n

   <interactive>:32:1:
      No instance for (Floating Int) arising from a use of ‘sqrt’
      In the expression: sqrt n
      In an equation for ‘it’: it = sqrt n

.. code-block:: none

   *Main> sqrt (fromIntegral n)


Clause ``deriving``
--------------------------

La déclaration de ``CouleurCarte`` comme instance de ``Eq`` est simple mais ennuyante à écrire.
Heureusement, l'instance peut être déduite *automatiquement* d'une déclaration ``data`` si on
spécifie la clause ``deriving`` comme dans l'exemple suivant :

.. literalinclude:: code/carte2.hs
   :language: haskell
   :lines: 1-5

Les instances de ``Ord``, ``Enum``, ``Read``, ``Show`` peuvent aussi être générées automatiquement
dans la même clause.  

Opérations habituelles
---------------------------------

Que donnent les expressions suivantes ?

- ``show Coeur``
- ``read "Coeur" :: CouleurCarte``   
- ``Valet /= Dix``
- ``Valet <= Dix`` et ``Deux < As``
- ``pred As``, ``succ As``
- ``fromEnum Trefle`` et  ``fromEnum Dame``
- ``enumFrom Valet`` et  ``enumFrom Trefle``
- ``enumFromTo Cinq Dix``
- ``[Trefle ..]`` et ``[Cinq .. Dix]``
- ``toEnum 12 :: ValeurCarte``
  

Conclusion
================

Capacités/Connaissances
---------------------------------

- Définir un type à l'aide des déclarations ``data`` et ``type``.
- Distinguer constructeur de valeur et de type.
- Connaître et utiliser les types et classes habituels. 

Comparaison avec d'autres langages
-----------------------------------

- Les types ne sont pas des objets. En Haskell, il n'y a pas de notion d'un état interne modifiable
  comme dans les langages orientés objets. 
- Cependant, les classes en Haskell sont similaires aux `concepts <http://en.cppreference.com/w/cpp/language/constraints>`_
  en programmation générique ou encore aux interfaces en Java :
  elles spécifient un protocole d'utilisation sous forme d'une liste d'opérations. 
- En Haskell, la cohérence des types est vérifiée à la compilation, il n'y a pas de liaison dynamique 
  à l'exécution comme dans les langages orientés objets.
- En Haskell, le type d'une valeur ne peut pas être changé.
  Il n'y a pas de classe racine comme ``Object`` en Java.  


Opérations habituelles
----------------------------------

.. code-block:: none

   *Main> show Coeur
   "Coeur"
   *Main> :t read
   read :: Read a => String -> a
   *Main> read "Coeur" :: CouleurCarte
   Coeur
   *Main> pred As
   Roi
   *Main> succ As
   *** Exception: succ{ValeurCarte}: tried to take `succ' of last tag
   in enumeration
   *Main> fromEnum Trefle
   0
   *Main> fromEnum Dame
   10

Opérations habituelles (suite)
----------------------------------------

.. code-block:: none
		
   *Main> enumFrom Valet
   [Valet,Dame,Roi,As]
   *Main> enumFrom Trefle
   [Trefle,Carreau,Coeur,Pique]
   *Main> enumFromTo Cinq Dix
   [Cinq,Six,Sept,Huit,Neuf,Dix]
   *Main> [Trefle ..]
   [Trefle,Carreau,Coeur,Pique]
   *Main> [Cinq ..]
   [Cinq,Six,Sept,Huit,Neuf,Dix,Valet,Dame,Roi,As]
   *Main> [Cinq .. Dix]
   [Cinq,Six,Sept,Huit,Neuf,Dix]
   *Main> toEnum 12 :: ValeurCarte
   As
   

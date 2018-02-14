====================================
Définition des types et des classes
====================================

Plan
=========================

.. Typage statique
   ---------------------------      
      
   - Chaque valeur à un type associé.
   - Les types sont vérifiés à la compilation.

   .. figure:: figs/type.svg
      :width: 80%
      :alt: types et typage
      :align: center


Déclarations
-------------------

On a vu pour l'instant deux sortes de déclarations :

- *equation*
- *type signature*

On va maintenant en voir d'autres : 
  
- ``data`` declaration (créer un type)
- ``type`` declaration (créer un synonyme de type)

  .. - ``newtype`` declaration (créer un type à partir d'un autre)

- ``class`` declaration (créer un groupe de type)
- ``instance`` declaration (déclarer un type comme appartenant à un groupe donné) 
    
 
Type défini par le développeur
=================================

Déclaration ``data``
----------------------------------

En Haskell, on peut définir nos propres types par des déclarations ``data`` :

.. code-block:: haskell

   data constructeurDeType [params] = constructeurDeValeur [params]

Il est ainsi possible de définir plusieurs sortes de type :

.. figure:: figs/typeDeType.svg
   :width: 350pt 
   :alt: différentes sortes de type
   :align: center



Type énuméré vs type tuple 
--------------------------------

- Pour créer un *type énuméré*, on utilise ``|`` pour faire
  l'union des ensembles de valeur.
- Pour créer un *type tuple*, on utilise simplement l'espace
  pour faire un produit cartésien des ensembles de valeur. 

.. literalinclude:: code/carte.hs
   :language: haskell
   :lines: 1-4

``CouleurCarte`` et ``ValeurCarte`` sont deux types énumérés,
tandis que ``Carte`` est un type tuple.
Par exemple, ``Cte As Trefle`` est une valeur de type ``Carte``. 	   


Type tuple polymorphe
-------------------------

Un *type polymorphe* est composé d'au moins une famille de type.

.. code-block:: haskell

   data Point a = Pt a a

Par exemple, à partir de tout type ``a``, le constructeur de type ``Point``
définit ``Point a``, le type des points cartésiens ayant ``a`` comme coordonnées. 

Le constructeur de valeur ``Pt :: a -> a -> Point a`` est une manière d'obtenir
des valeurs de type ``Point a``. Par exemple, ``Pt 2.0 3.0`` est le point :math:`(2,3)`.

Avertissement
-------------------------

Les constructeurs de type comme ``Point`` et les constructeurs de valeurs comme ``Pt``
se trouvent dans des espaces de noms séparés. Il est donc possible de donner le même nom
aux deux constructeurs, afin de rendre plus évident le lien entre un type et le constructeur
de valeur associé. 

.. literalinclude:: code/shape.hs
   :language: haskell
   :lines: 1

Type énuméré polymorphe
--------------------------

.. literalinclude:: code/shape.hs
   :language: haskell
   :lines: 3-4

Autrement dit, le type ``Shape a`` correspond soit à un cercle
(défini par un point de type ``Point a`` et un rayon de type ``a``),
soit à un rectangle (défini par deux points de type ``Point a``).  
	   
.. literalinclude:: code/shape.hs
   :language: haskell
   :lines: 7-12

	   
Types récursifs
----------------------   

Il est possible de se référer au type que l'on crée
dans un des constructeurs de valeurs. 

.. literalinclude:: code/tree2.hs
   :language: haskell
   :lines: 1

Autrement dit, une valeur de type ``Tree a`` est un arbre binaire polymophique
dont les éléments sont soit une feuille (contenant une valeur de type ``a``),
soit un noeud interne reliant deux sous-arbres.      

.. literalinclude:: code/tree2.hs
   :language: haskell
   :lines: 3-9


Liste revisitée
-------------------

``[a]`` est en fait une notation particulière pour ``[] a``, car 
``[]`` est un *constructeur de type* :
à partir du type ``a``, il crée le type ``[] a``, c'est-à-dire
une liste de ``a``. 

La définition d'une liste est récursive :

.. code-block:: haskell

   data [a] = [] | a : [a]

Une liste de ``a`` est soit une liste vide, soit une liste ayant au moins
une valeur de type ``a`` en tête de liste. 

Défi 1 : construire un arbre
---------------------------------

A l'aide des définitions précédentes,
construisez l'arbre représenté ci-dessous,
et passez-le comme argument à la fonction ``flatten`` dans GHCi. 

.. figure:: figs/arbre.svg
   :width: 250pt 
   :alt: arbre
   :align: center

Défi 2 : hauteur d'un arbre binaire
--------------------------------------

Définissez la fonction

.. literalinclude:: code/treeHeight.hs
   :language: haskell
   :lines: 1

qui, pour un arbre donné, renvoie la longueur du plus long chemin entre
la racine et une feuille, c'est-à-dire la hauteur de l'arbre :

.. code-block:: none

   *Main> treeHeight (Leaf 4)
   1
   *Main> treeHeight (Node (Leaf 4) (Leaf 5))
   2
   *Main> treeHeight (Node (Leaf 4) (Node (Leaf 5) (Leaf 6)))
   3

		
.. Défi 2 : aplatir un arbre binaire
   -----------------------------------

   Proposez une version plus efficace de ``flatten``
   en utilisant l'opérateur ``(:)`` au lieu de l'opérateur ``(++)``. 

   .. code-block:: none

      *Main> flatten (Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)) 
      [1,2,3]

   Astuce : comme pour :ref:`MyReverse<reversecode>`, définissez une fonction intermédiaire
   qui prend en entrée l'arbre et une liste temporaire d'accumulation. 

   .. code-block:: Haskell

      flatten' :: Tree a -> [a] -> [a] 

	   
Défi 3 : liste imbriquée
-----------------------------------

Après avoir défini le type adéquat, écrivez la fonction

.. literalinclude:: code/NestedList.hs
   :language: haskell
   :lines: 3

qui aplatit une liste d'élément
pouvant contenir des listes comme élément.  

.. code-block:: none

   *Main> flatten (Elem 5)
   [5]
   *Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4]]])
   [1,2,3,4]
   *Main> flatten (List [])
   []

	   


Synonymes
---------------------------------

Il est possible de définir des synonymes de types grâce à la déclaration ``type`` :

- ``type String = [Char]``
- ``type AssociationList a b = [(a,b)]``
- ``type PhoneNumber = String``  
- ``type Name = String``  
- ``type PhoneBook = [(Name,PhoneNumber)]`` 

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

Défi 4 : opérations habituelles
---------------------------------

Que donnent les expressions suivantes ?

- ``show Coeur``
- ``read "Coeur" :: CouleurCarte``   
- ``Valet /= Dix``
- ``Valet <= Dix``, ``Deux < As``
- ``pred As``, ``succ As``
- ``fromEnum Trefle``, ``fromEnum Dame``
- ``enumFrom Valet``,  ``enumFrom Trefle``
- ``enumFromTo Cinq Dix``
- ``[Trefle ..]``, ``[Valet ..]``, ``[Cinq .. Dix]``
- ``toEnum 12 :: ValeurCarte``
  
Défi 5 : carte à jouer
------------------------

- Définissez le type d'une carte à jouer.
- Faites en une instance de ``Eq`` et de ``Ord``, de façon à ce que seulement la valeur compte
  dans une comparaison.

.. code-block:: none

   *Main> Carte As Trefle == Carte As Pique
   True
   *Main> Carte As Trefle == Carte Roi Trefle
   False
   *Main> Carte As Trefle > Carte Roi Trefle
   True
   *Main> min (Carte As Trefle) (Carte Roi Trefle)
   Carte Roi Trefle

Défi 6 : tas de cartes
------------------------

- Définissez le type d'un tas de carte.
- Créez le tas de carte à l'aide d'une *list comprehension*. 
- Donnez l'expression indiquant le nombre de cartes se trouvant dans le tas. 


Défi bonus. ``toIntList``
---------------------------

- Créez la classe ``IntListable`` dotée de la fonction

.. code-block:: haskell

   toIntList :: IntListable a => a -> [Int]

- Faites de

  - ``Int``,
  - ``Bool``,
  - ``[Int]``,
  - une paire d'élément ``IntListable``,
  
  des instances de cette classe. 
   
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


Conclusion
================

Capacités/Connaissances
---------------------------------

- Définir un type à l'aide des déclarations ``data`` et ``type``.
- Distinguer constructeur de valeur et de type. 
- Définir une classe à l'aide de la déclaration ``class`` et
  surcharger une opération à l'aide de la déclaration ``instance``.
- Citer les classes ``Eq``, ``Ord``, ``Enum``, ``Show``, ``Read``,
  ``Num`` et leurs opérations. 
  
Défi 1 : construire un arbre
---------------------------------

.. figure:: figs/arbre.svg
   :width: 250pt
   :alt: arbre
   :align: center

.. code-block:: none

   *Main> flatten (Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c'))
   "abc"

     
Défi 2 : hauteur d'un arbre binaire
--------------------------------------

.. literalinclude:: code/treeHeight.hs
   :language: haskell

Défi 3 : liste imbriquée
-----------------------------------

.. literalinclude:: code/NestedList.hs
   :language: haskell

Défi 4 : opérations habituelles
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

Défi 4 : opérations habituelles (suite)
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

Défi 5 : carte à jouer
------------------------

.. literalinclude:: code/carte2.hs
   :language: haskell
   :lines: 1-12

(Inspiré de ce `tutoriel <https://wiki.haskell.org/Type>`_)
	   
Défi 6 : tas de cartes
------------------------

.. literalinclude:: code/carte2.hs
   :language: haskell
   :lines: 14-15

.. code-block:: none

   *Main> length leTas
   52

(Inspiré de ce `tutoriel <https://wiki.haskell.org/Type>`_)
   
Défi bonus. ``toIntList``
---------------------------

.. literalinclude:: code/toIntList.hs
   :language: haskell

(Inspiré de cette `leçon <http://www.cis.upenn.edu/~cis194/spring13/lectures/05-type-classes.html>`_)

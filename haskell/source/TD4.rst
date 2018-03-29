====================================
Monades : ``Maybe``, ``[]``, ``IO``
====================================

``Maybe``
====================================

Example
------------------------------------

(Exemple tiré de `cette présentation <http://aramis.resinfo.org/ateliers/slides-haskell/presentation.html#1>`_)

Une fonction partielle peut ne pas être définie sur tout son domaine.

Quel est le minimum d'une liste vide ?

.. code-block:: Haskell

   minimum []

Différentes approches :

- Exception
- Valeur sentinelle arbitraire
- Valeur de retour par référence
- Type spécifique

Fonction partielle - Exception
---------------------------------------------

.. code-block :: python

      >>> min([1,2,3])
      1
      >>> min([])
      Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
      ValueError: min() arg is an empty sequence

Bugs : cas non gérés, erreur à l'exécution

Fonction partielle - Valeur sentinelle
---------------------------------------------

.. code-block :: c++

   float minimum(const std::vector<float> &v)
   {
       if(v.size() == 0)
            return -std::numeric_limits<float>::infinity();

       float ret = v[0];
       for(size_t i = 1; i < v.size(); ++i)
       {
           ret = std::min(v[i], ret);
       }
       return ret;
   }

Bugs : la valeur sentinelle exclue des données,
on ne teste pas la valeur de retour ou pas la bonne valeur
   
Fonction partielle - *Flag* de réussite
---------------------------------------------

.. code-block :: cpp

   bool minimum(const std::vector<float> &v, float &ret)
   {
       if(v.size() == 0)
            return false;

       ret = v[0];
       for(size_t i = 1; i < v.size(); ++i)
           ret = std::min(v[i], ret);

       return true;
   }

Bugs :

.. code-block :: cpp

   float res;
   minimum(maListe, res);
   std::cout << res << std::endl; // Non initialisé

Solution : Type spécifique
---------------------------------------------

Utilisation du type polymorphe ``Maybe a``.

.. literalinclude:: code/minimum.hs
   :language: Haskell
   :lines: 3-9

.. code-block :: none

    *Main> minimum [1,3,-2,5]
    Just (-2)
    *Main> minimum []
    Nothing


``Maybe a`` : usage
---------------------------------------------

Le typage nous empêche d'utiliser directement la valeur.

.. code-block :: none

    *Main> 2 * minimum [1,3,-2,5]

    <interactive>:16:1:
      Non type-variable argument in the constraint: Num (Maybe a)
      (Use FlexibleContexts to permit this)
      When checking that ‘it’ has the inferred type
      it :: forall a. (Num a, Num (Maybe a), Ord a) => Maybe a


On peut néanmoins manipuler ces valeurs par des opérateurs avancés
(``>>=``, ``return``, ``fmap``) que nous détaillerons plus loin :

.. code-block :: none

   *Main> Just 5 >>= return . (*2)
   Just 10
   *Main> Nothing >>= return . (*2)
   Nothing

Retour sur les listes
---------------------------------------------

On pourrait utiliser ``[a]`` au lieu de ``Maybe a`` où

- ``[]`` représente ``Nothing``,
- ``[x]`` représente ``Just x``.

  Il vaut mieux utiliser ``Maybe a`` quand il n'y a qu'une valeur à mémoriser,
  mais ``[a]``, quand il y en a plusieurs. 

Défi 1 : ``safeHead``
----------------------------------------------

Définissez la fonction :

.. literalinclude:: code/safeHead.hs
   :language: Haskell
   :lines: 1

qui retourne l'élément en tête d'une liste,
encapsulé dans un ``Maybe a``

.. code-block:: none

   *Main> safeHead []
   Nothing
   *Main> safeHead [5,2,4]
   Just 5
   *Main> 

``String`` et ``IO a``
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

``getChar``, ``putChar``
-------------------------------

Tout action d'entrée-sortie retourne une valeur qui est étiquettée par le type ``IO a``.

Par exemple, ``getChar`` réalise une action de lecture et retourne un caractère.

.. code-block:: Haskell

   getChar :: IO Char

Les actions qui ne retournent aucune valeur utile prennent le type ``IO ()``.
Par exemple, ``putChar`` prend un caractère pour l'afficher mais ne retourne rien. 

.. code-block:: Haskell

   putChar :: Char -> IO ()

bind
-------------------------------

Les actions sont séquencées avec l'opérateur ``>>=`` (appelé bind).

.. code-block:: Haskell

   >>= :: IO a -> (a -> IO b) -> IO b

Ce programme réalise deux actions d'entrée-sortie à la suite : il lit
un caractère, puis l'affiche sur la sortie standard. 
   
.. literalinclude:: code/getput.hs
   :language: Haskell

	      
do notation
-------------------------------

Le mot-clé ``do`` introduit une séquence d'instructions
implicitement enchaînées. 

Ces instructions sont soit des actions, soit des définitions locales
avec ``let`` et surout ``<-`` pour récupérer les valeurs provenant
d'entrées-sorties.  

Ce programme est équivalent au précédent
(l'opération *bind* étant implicite) :

.. literalinclude:: code/getput2.hs
   :language: Haskell


return
-------------------------------

La fonction ``return`` crée une action, qui ne fait rien d'autre
que d'encapsuler une valeur dans une action : 

.. code-block:: Haskell

   return :: a -> IO a

Cette fonction lit un caractère et retourne ``IO True`` si le caractère
est un ``y``.  
   
.. literalinclude:: code/ready.hs
   :language: Haskell

Notez que le type est connue grâce à la signature de la fonction. 

``getLine``, ``putStrLn``
-------------------------------

Alors que ``getChar`` et ``putChar`` opérent sur des caractères (``Char``),
``getLine`` et  ``putStrLn`` opérent sur des chaînes de caractères (``String``).

Par exemple, ``getLine`` réalise une action de lecture et retourne une chaîne.

.. code-block:: Haskell

   getLine :: IO String

A l'inverse, ``putStrLn`` prend une chaîne et l'affiche sans rien retourner.
   
.. code-block:: Haskell

   putStrLn :: String -> IO ()


Défi 2 : jeu
-------------------------------

A partir du type :ref:`Carte <carte-label>`,
définissez la fonction :

.. literalinclude:: code/io.hs
   :language: Haskell
   :lines: 18

appelée ci-dessous. L'utilisateur tape le nom d'une carte.
Si c'est celle choisie par le programme, il a gagné, sinon
il a le droit de proposer une nouvelle carte. 
	   
.. literalinclude:: code/io.hs
   :language: Haskell
   :lines: 28-37



Conclusion
==============================


Monades
---------------------------------------------

Les monades sont des types polymorphes qui définissent ces deux opérations:

- ``return :: a -> m a``
  Cette opération injecte une valeur dans la monade.   

- ``>>= :: m a -> (a -> m b) -> m b`` (appelé bind)
  Cette opération combine une valeur monadique ``m a``, contenant une valeur de type ``a``
  à une fonction qui prend une valeur de type ``a`` et retourne la valeur monadique ``m b``.

  
  Bien sûr la sémantique de ces opérations dépendent de la monade;
  le résultat ne sera pas le même s'il s'agit de ``Maybe``, ``[]`` ou ``IO``. 

Remarque
--------------------------------------------

La notation ``do`` n'est pas réservée aux entrées-sorties,
mais est utilisable avec toutes les monades, puisque c'est
une manière implicite d'utiliser ``>>=``.

.. code-block:: none

   *Main> safeHead [5,2,4] >>= return . (*2)
   Just 10
   *Main> safeHead [5,2,4] >>= (\x -> return (x*2))
   Just 10
   *Main> do x <- safeHead [5,2,4] ; return (x*2)
   Just 10


Capacités/Connaissances
-----------------------------------------------

- Donnez des exemples de monades: ``Maybe``, ``[]``, ``IO``.
- Expliquez ce qu'est une monade. 
- Utilisez à bon escient les principales fonctions d'entrée-sorties:
  ``read``, ``show``, ``getLine``, ``putStrLn``. 
- Traduire une opération *bind* par un *do* et inversement.
- Ecrire un petit programme en définissant la fonction ``main``

  
Défi 1 : ``safeHead``
----------------------------------------------


.. literalinclude:: code/safeHead.hs
   :language: Haskell


Défi 2 : Jeu
-------------------------------

.. literalinclude:: code/io.hs
   :language: Haskell
   :lines: 13-27

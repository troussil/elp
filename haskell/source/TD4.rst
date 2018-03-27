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

.. code-block :: pycon

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

Bugs : valeur sentinelle exclue des données
   
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

Utilisation du type polymorphique ``Maybe a``.

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


IO
==============================

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
implicitement enchaînées par ``>>=``.

Ces instructions sont soit des actions, soit des définitions locales
avec ``let`` et surout ``<-`` pour récupérer les valeurs provenant
d'entrées-sorties.  

Ce programme est équivalent au précédent:

.. literalinclude:: code/getput2.hs
   :language: Haskell


return
-------------------------------

La fonction ``return`` crée une action, qui ne fait rien d'autre
que d'encapsuler une valeur pour assurer la cohérence des types:

.. code-block:: Haskell

   return :: a -> IO a

Cette fonction lit un caractère et retourne ``True`` si le caractère
est un ``y``.  
   
.. literalinclude:: code/ready.hs
   :language: Haskell

.. code-block :: none

    *Main> readY
    yTrue
    *Main> readY
    zFalse


Conclusion
==============================


Monades
---------------------------------------------

Les monades sont des types polymorphes qui définissent deux opérations:

- ``return :: a -> m a``
  Cette opération injecte une valeur dans la monade.   

- ``>>= :: m a -> (a -> m b) -> m b`` (appelé bind)
  Cette opération combine une valeur monadique ``m a``, contenant une valeur de type ``a``
  à une fonction qui prend une valeur de type ``a`` et retourne la valeur monadique ``m b``.

  
  Bien sûr la sémantique de ces opérations dépendent de la monade;
  le résultat ne sera pas le même s'il s'agit de ``Maybe``, ``[]`` ou ``IO``. 

Lois
---------------------------------------------



Capacités/Connaissances
-----------------------------------------------

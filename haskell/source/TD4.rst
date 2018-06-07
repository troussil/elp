====================================
Monades : ``Maybe``, ``[]``, ``IO``
====================================

Avant-propos
====================================

Qui utilise Haskell ?
------------------------------------

Qui utilise un langage dans lequel les variables ne varient pas,
dans lequel on ne peut rajouter une instruction d'affichage, sans
changer le type de la fonction ? 

- Facebook (`Haxl <https://github.com/facebook/Haxl>`_),
- Microsoft (`Bond <https://github.com/Microsoft/bond>`_),

Consultez par exemple cette `liste <https://github.com/erkmos/haskell-companies>`_
qui répertorie des entreprises qui utilisent Haskell. 

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

- Valeur sentinelle arbitraire
- Valeur de retour par référence
- Exception
- Type spécifique


Fonction partielle - Valeur sentinelle
---------------------------------------------

.. code-block :: cpp

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

   Bugs : la valeur sentinelle exclue des données.
   Le client peut ne pas tester la valeur de retour
   ou ne pas tester la bonne valeur. 
   
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

Fonction partielle - Exception
---------------------------------------

.. code-block:: python

      >>> min([1,2,3])
      1
      >>> min([])
      Traceback (most recent call last):
      File "<stdin>", line 1, in <module>
      ValueError: min() arg is an empty sequence

Bugs : cas non géré, erreur à l'exécution

Solution : Type spécifique
---------------------------------------------

Utilisation du type polymorphe défini dans le Prélude comme :

.. code-block:: haskell

   data Maybe a = Just a | Nothing


.. literalinclude:: code/minimum.hs
   :language: Haskell
   :lines: 3-9

.. code-block :: none

    *Main> minimum [1,3,-2,5]
    Just (-2)
    *Main> minimum []
    Nothing

Inconvénient (relatif)
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
(``fmap``, ``<*>``, ``>>=``, ``return``) : 

.. code-block :: none

   *Main> fmap (*2) (Just 5)
   Just 10
   *Main> fmap (*2) Nothing
   Nothing

Retour sur les listes
---------------------------------------------

On pourrait utiliser ``[a]`` au lieu de ``Maybe a`` où

- ``[]`` représente ``Nothing``,
- ``[x]`` représente ``Just x``.

``Maybe a`` est adapté quand il n'y a qu'une valeur à mémoriser,
``[a]``, quand il y en a plusieurs. 

Dans le module ``import Data.Maybe``, il y a d'ailleurs
les fonctions ``listToMaybe``, ``maybeToList``. 

Défi 1 : ``safeHead``
----------------------------------------------

Définissez la fonction :

.. literalinclude:: code/safeHead.hs
   :language: Haskell
   :lines: 1

qui retourne l'élément en tête d'une liste,
encapsulé dans une valeur de type ``Maybe a``

.. code-block:: none

   *Main> safeHead []
   Nothing
   *Main> safeHead [5,2,4]
   Just 5
   *Main> 

Types monadiques
============================

Monades
---------------------------------------------

Les monades sont des types polymorphes pour lesquels ces opérations
sont disponibles:

- ``return :: a -> m a``. Injecte une valeur dans la monade.   

- ``>>= :: m a -> (a -> m b) -> m b`` (appelé *bind*).
  Combine une valeur monadique ``m a``, contenant une valeur de type ``a``
  à une fonction ``a -> m b`` et retourne la valeur monadique ``m b``.

- ``>> :: m a -> m b -> m b``, utilisée quand on n'a pas besoin de la
  première valeur monadique. 
  
Bien sûr la sémantique de ces opérations dépend de la monade;
le résultat ne sera pas le même s'il s'agit de ``Maybe``, ``[]`` ou ``IO``,
qui sont tous trois des monades. 

Défi 2 : opérations monadiques
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
  
Lois monadiques
------------------------------------

La sémantique des opérateurs ``>>=`` et ``return`` doit
respecter les lois suivantes : 

.. code-block:: none

   return a >>= k           = k a
   m >>= return             = m
   xs >>= return . f        = fmap f xs
   m >>= (\x -> k x >>= h)  = (m >>= k) >>= h

Sucre syntaxique : notation ``do``
------------------------------------

Il y a une notation qui évite l'usage explicite de ``>>`` et ``>>=`` : 

.. code-block:: haskell

   do e1 ; e2     = e1 >> e2
   do p <- e1; e2 = e1 >>= \p -> e2

Par exemple :
   
.. code-block:: none

   *Main> safeHead [5,2,4] >>= (\x -> return (x*2))
   Just 10
   *Main> do x <- safeHead [5,2,4] ; return (x*2)
   Just 10

(Notez qu'en général on passe à la ligne plutôt qu'utiliser un point-virgule).  
   
Défi 3 : notation ``do``
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

Rappel de compilation
---------------------------

- La commande ``ghc fichier.hs -o executable`` produit
  directement un exécutable : ``./executable``, à partir
  du moment où ``fichier.hs`` contient un ``main``. 

- D'autres options sont utiles, notamment ``-outputdir build``
  pour mettre les fichiers intermédiaires dans un répertoire séparé ``build``
  (voir ``man ghc`` ou ``ghc --help``). 
	      
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


Défi 4 : jeu
-------------------------------

A partir du type :ref:`Carte <carte-label>`,
définissez la fonction :

.. literalinclude:: code/io.hs
   :language: Haskell
   :lines: 11

appelée ci-dessous. L'utilisateur tape le nom d'une carte.
Si c'est celle choisie par le programme, il a gagné, sinon
il a le droit de proposer une nouvelle carte. 
	   
.. literalinclude:: code/io.hs
   :language: Haskell
   :lines: 21-26



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

  
Défi 1 : ``safeHead``
----------------------------------------------


.. literalinclude:: code/safeHead.hs
   :language: Haskell


Défi 2 : Opérations habituelles
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

Défi 2 : Opérations habituelles (suite)
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
   
Défi 3 : Notation ``do``
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
		
Défi 4 : Jeu
-------------------------------

.. literalinclude:: code/io.hs
   :language: Haskell
   :lines: 11-26

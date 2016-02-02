======================
Hiérarchie de classes
======================


Héritage
==========================


Attention
----------------------------

L'héritage possède deux caractéristiques pouvant parfois être en contradiction: 

- l'**extension de code**, qui consiste à dériver une nouvelle classe d'une
  classe déjà existante, pour hériter du code de la classe existante et en 
  ajouter sans avoir à tout réécrire. 
- le **polymorphisme** qui modélise la relation dans laquelle un objet 
  peut être utilisé à la place d'un autre objet. 

Syntaxe et vocabulaire
---------------------------

.. code-block:: java 

	public class B extends A { ... }

.. code-block:: java 

	public class C extends A { ... }

================ ==============
``A``            ``B``, ``C``
================ ==============
base             dérivée
mère, parente    fille
super-classe     sous-classe
qui est hérité   qui hérite
qui est étendue  qui étend
type, sur-type   sous-type
================ ==============

Extension de code
==========================

Principe
----------------------------

La classe dérivée hérite des membres non marqués **private** de la classe 
de base (mis à part les constructeurs). Cette classe peut elle-même posséder des champs
et méthodes supplémentaires. C'est le principe de l'extension, suggéré par le 
mot-clé ``extends``.   

Le mot-clé **protected** au-devant des membres de la classe de base permet 
d'élargir leur accès à toutes les classes dérivées. 

Ex.1. Point/Vecteur (5 min)
----------------------------

Observez les fichiers :download:`Point.java <download/Point.java>` 
et :download:`Vecteur.java <download/Vecteur.java>`. 

- Que fait ce code ? 
- A quoi correspondent ``x`` et ``y`` dans chacun des fichiers ?
- A quoi sert le mot-clé ``super`` ?
- Voyez-vous un cas de **surcharge** (= *overloading*), qui permet à plusieurs
  méthodes de partager le même *nom*, à condition que leur *signature* 
  (nombre et type des paramètres) soit différente ?
  
Ex.2. Complexe (15 min)
----------------------------

Faites une classe ``Complexe``, qui étend ``Vecteur``, par les méthodes: 
 - ``double obtenirNorme()``
 - ``Complexe obtenirConjugue()``
 - ``void multiplier(Complexe c)``
 - ``void diviser(Complexe c)``

Ce qu'il faut retenir
----------------------------------


Polymorphisme
============================

Principe
----------------------------

L'héritage modélise aussi la relation dans laquelle 
un objet de la classe dérivée peut être utilisé comme un objet 
de la classe de base, c'est ce qui fait de l'héritage 
un mécanisme complexe.  

.. code-block:: java 

	public class B extends A { ... }

.. code-block:: java 

        A objet = new B(); //transtypage ascendant implicite

Un objet de la classe B *est un* objet de la classe A et peut
être utilisé comme tel. 

NB. Cette relation n'est pas *symétrique*. 

Appel de méthode
----------------------------

- Si une méthode ``methodeA`` est définie dans la classe ``A``, 
  on peut l'appeler à partir de la variable ``objet``: 

.. code-block:: java 

        objet.methodeA(); //compile

- Si une méthode ``methodeB`` n'est définie que dans la classe ``B``, 
  on ne peut l'appeler: 

.. code-block:: java 

        objet.methodeB(); //ne compile pas

- Et si une même méthode est définie à la fois dans la classe ``A`` 
  et dans la classe ``B`` ?

Redéfinition
----------------------------

La classe dérivée peut aussi redéfinir certaines méthodes dont elle hérite, 
c'est-à-dire les implémenter d'une autre manière. 

Ne pas confondre **redéfinition** (= *overriding*), même signature, mais corps différent entre 
la classe de base et la classe dérivée, et **surcharge** (= *overloading*), même nom, 
mais liste de paramètres différente, au sein d'une même classe.  

NB. Depuis Java 5, en utilisant l'annotation ``@Override``, le compilateur vérifie que 
vous redéfinissez une méthode d'une classe de base et n'en créez pas une nouvelle 
(cf. `annotations prédéfinies <http://docs.oracle.com/javase/tutorial/java/annotations/predefined.html>`_). 


Appel de méthode redéfinie
----------------------------------

.. literalinclude:: code/TD3/Base.java
   :language: java

.. literalinclude:: code/TD3/Derivee.java
   :language: java

.. literalinclude:: code/TD3/Demo.java
   :language: java
   :lines: 3-4

Quelle méthode est appelée ?

Liaison dynamique
----------------------------

A l'exécution, la machine virtuelle regarde quel est l'objet référencé par la variable 
à partir de laquelle l'appel est effectué. C'est toujours la méthode de cet objet qui 
est appelée : c'est le principe de la **liaison dynamique**. 

Dans l'exemple précédent, la variable ``unObjet`` référence un objet de type ``Derivee``. 
C'est donc la méthode implémentée dans la classe ``Derivee`` qui est exécutée. 


Hiérarchie de classes 
----------------------------

Rien n'empêche de dériver une classe, elle-même dérivée d'une autre classe et 
ainsi de suite. 
L'héritage est *transitif*: si ``B`` hérite de ``A`` et si ``D`` hérite de ``B``,
alors ``D`` hérite aussi de ``A`` via ``B``.  

En Java, toutes les classes dérivent par défaut de ``java.lang.Object`` (cf. 
`l'API standard <http://docs.oracle.com/javase/7/docs/api/>`_).
Cette classe possède quelques méthodes pouvant être redéfinies comme 
``toString`` qui retourne une représentation textuelle de type ``String`` de l'objet
(nom de la classe, arobase, hash code par défaut). 


Ce qu'il faut retenir
----------------------------------


Exceptions 
============================


Erreurs et exceptions
------------------------------------

Contrairement aux erreurs qui causent un dysfonctionnement irréversible menant à l'arrêt du programme
(``java.lang.OutOfMemoryError``), les exceptions désignent les situations où l'exécution peut se poursuivre, 
généralement de façon différente (``java.lang.ArithmeticException``). 

En Java, erreurs et exceptions sont matérialisées par des instances de classes dérivant de 
``java.lang.Error`` et ``java.lang.Exception``, toutes deux dérivant de ``java.lang.Throwable``. 

Pensez à consulter `l'API standard <http://docs.oracle.com/javase/7/docs/api/>`_
ou les `tutoriaux <http://docs.oracle.com/javase/tutorial/essential/exceptions/index.html>`_
qui traitent le sujet.  



Le client qui traite les exceptions
------------------------------------

Le bloc d'instructions principal est mis dans un bloc ``try``, 
tandis que la gestion des exceptions est répartie, selon la 
nature de l'exception, dans des blocs ``catch`` successifs. 

.. code-block:: java 

        try {
            /* code */
        } catch(ExceptionDeTypeA e) {
	    /* gestion des exceptions de type A */
	} catch(ExceptionDeTypeB e) {
	    /* gestion des exceptions de type B */
 	} finally {
	    /* tout fermer et nettoyer */
	}

Le bloc optionnel ``finally`` s'exécute toujours. 

Celui qui n'en fait pas assez
------------------------------

Ne jamais écrire un code qui masque les exceptions. 

.. code-block:: java 

        try {
          unCodeQuiLeveUneException();
        } catch(Exception e) {
          /* Aucune action, ce qui masque les erreurs */
        }

Préférez au moins: 

.. code-block:: java 

        try {
          unCodeQuiLeveUneException();
        } catch(Exception e) {
          /* affiche l'empilement des appels qui ont mené à l'erreur */
	  e.printStackTrace();
        }


Celui qui en fait trop 
-----------------------------

N'entourez pas chaque instruction d'un bloc ``try``/``catch``:  
ça ne sert à rien et va à l'encontre de l'objectif qui est de 
**séparer** le bloc d'instructions principal, des instructions 
relevant de la gestion des exceptions pouvant survenir dans ce bloc, 
afin d'obtenir un code plus lisible et plus facile à réutiliser.    

.. code-block:: java 

        try {
          unCodeQuiLeveUneExceptionA();
        } catch(ExceptionA e) {
	  e.printStackTrace();
        }
        try {
          unCodeQuiLeveUneExceptionB();
        } catch(ExceptionB e) {
	  e.printStackTrace();
        }


Le développeur
-------------------------

Le développeur d'une classe peut indiquer aux clients qu'une méthode est susceptible de lever une exception
avec le mot-clé ``throws`` et peut effectivement **lever une exception** au moment voulu avec le mot-clé ``throw``. 

.. code-block:: java 
   :emphasize-lines: 3,5

        import java.lang.Exception; 
        ...
        public int pop() throws Exception {
            if ( myNode == null ) 
                throw new Exception();
            else
                myNode = myNode.next(); 
        }


Créer sa propre classe d'exception
----------------------------------

.. code-block:: java 

        import java.lang.Exception; 
        ...
        public class EmptyStackException extends Exception {
           ...
        }


.. code-block:: java 

        public int pop() throws EmptyStackException {
            if ( myNode == null ) 
                throw new EmptyStackException();
            else
                myNode = myNode.next(); 
        }

Propager une exception
--------------------------

.. code-block:: java 

        private static void oneMove(Stack src, Stack dest) 
          throws EmptyStackException {
            try {
	        dest.push( src.top() ); 
	        src.pop();
            } catch (EmptyStackException e) {
                throw new EmptyStackException("empty stack");
            }
        }

Plutôt que d'attraper et lever la même exception, il est possible de la **propager**.
 
.. code-block:: java 

        private static void oneMove(Stack src, Stack dest) 
          throws EmptyStackException {
            dest.push( src.top() ); 
	    src.pop();
        }

Ex.3. Exceptions (10 min)
---------------------------


Ce qu'il faut retenir
-------------------------

- Les exceptions sont des instances de classes dérivant de ``java.lang.Exception``.

- La levée d'une exception provoque une remontée dans l'appel des
  méthodes jusqu'à ce qu'un bloc ``catch`` acceptant cette exception
  soit trouvé.

- L'appel à une méthode susceptible de lever une exception doit :

  - soit être contenu dans un bloc ``try`` / ``catch``
  - soit être situé dans une méthode propageant cette classe d'exception (``throws``) 

- Un bloc ``finally`` peut suivre les blocs ``catch``. Son contenu est toujours exécuté 
  (avec ou sans exception, et même en cas de ``break``, ``continue``, ``return`` dans le bloc ``try``). 

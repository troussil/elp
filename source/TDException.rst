===========================
Mécanisme d'exception 
===========================

Exceptions
===========================

Vous saurez distinguer exceptions vérifiées et non vérifiées,
choisir entre propager et traiter une exception.

Erreurs et exceptions
------------------------------------

Les **exceptions** désignent les situations où l'exécution peut se poursuivre, 
généralement de façon différente. Elles sont matérialisées en Java  par des instances
de classes dérivant de ``java.lang.Exception``, elle-même dérivant de ``java.lang.Throwable``. 

     java.lang.Object
        java.lang.Throwable
            java.lang.Exception

N'hésitez pas à lire les `tutoriaux <http://docs.oracle.com/javase/tutorial/essential/exceptions/index.html>`_
qui traitent le sujet.  


Le développeur
-------------------------

Le développeur d'une classe peut **lever une exception** au moment opportun avec le mot-clé ``throw``
et peut indiquer aux clients que la méthode concernée est susceptible de lever une exception
avec le mot-clé ``throws``. 

.. code-block:: java 
   :emphasize-lines: 1,3

        public int pop() throws RuntimeException {
            if ( myNode == null ) 
                throw new RuntimeException();
            else
                myNode = myNode.next(); 
        }

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


Propager une exception
--------------------------

.. code-block:: java 

        private static void oneMove(Stack src, Stack dest) 
          throws RuntimeException {
            try {
	        dest.push( src.top() ); 
	        src.pop();
            } catch (RuntimeException e) {
                throw new RuntimeException();
            }
        }

Plutôt que d'attraper et lever la même exception, il est possible de la **propager**:
 
.. code-block:: java 

        private static void oneMove(Stack src, Stack dest) 
          throws RuntimeException {
            dest.push( src.top() ); 
	    src.pop();
        }

Deux types d'exceptions
------------------------------------

En général, le compilateur s'assure qu'un bloc de code susceptible de lever 
une exception propage l'exception avec ``throws`` ou l'attrape avec ``try``/``catch``
et affiche un message d'erreur si ce n'est pas le cas. Les exceptions qui déclenchent 
un tel comportement sont vérifiées (*checked exceptions*).

Il existe cependant des exceptions, matérialisées par des instances de ``Error``, 
de ``RuntimeException`` et de leurs classes dérivées, qui ne sont pas vérifiées
(*unchecked exceptions*). Elles doivent être réservées aux événements après lesquels 
le programme ne peut plus continuer à fonctionner normalement (bug ou ressource externe 
nécessaire manquante). 


Personnaliser l'exception
---------------------------------------

Avoir sa propre classe d'exception, même vide, permet de distinguer
cette exception des autres par différents blocs ``catch``. 

.. code-block:: java 

        public class EmptyStackException extends RuntimeException {}


.. code-block:: java 

        public int pop() throws EmptyStackException {
            if ( myNode == null ) 
                throw new EmptyStackException();
            else
                myNode = myNode.next(); 
        }

Celui qui n'en fait pas assez
------------------------------

Ne jamais écrire un code qui masque les exceptions. 

.. code-block:: java 

        //PAS BIEN
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

        //PAS BIEN
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



Ex. Combinaisons (30 min)
-------------------------------------------

Écrire une classe exécutable qui affiche le coefficient binomial
:math:`C_n^p = C_{n-1}^p + C_{n-1}^{p-1}`. Les entiers n et p doivent être
lus à partir des deux premiers arguments passés au programme.

Attraper les exceptions suivantes :

a) aucun argument n'est donné à l'appel du programme et le tableau des
   arguments est vide (``ArrayIndexOutOfBoundsException``)
b) les arguments passés ne sont pas des entiers (``NumberFormatException``)
c) les arguments passés ne sont pas positifs et d. p > n

Dans les derniers cas, c) et d), vous devrez créer vos propres
exceptions et les lever vous-mêmes.

Ce qu'il faut retenir
-------------------------

- Les exceptions sont des instances de classes dérivant de ``java.lang.Exception``.

- La levée d'une exception provoque une remontée dans l'appel des
  méthodes jusqu'à ce qu'un bloc ``catch`` acceptant cette exception
  soit trouvé.

- L'appel à une méthode susceptible de lever une exception vérifiée doit :

  - soit être contenu dans un bloc ``try`` / ``catch``
  - soit être situé dans une méthode propageant cette classe d'exception (``throws``) 

- Un bloc ``finally`` peut suivre les blocs ``catch``. Son contenu est toujours exécuté. 

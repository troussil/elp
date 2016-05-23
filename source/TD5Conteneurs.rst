===========================================
Conteneurs et exceptions
===========================================

Conteneurs
==========================

Résumé du package
---------------------------------------

Le package ``java.util`` regroupe des classes relatives à la gestion du temps, 
à la gestion des événements, ainsi qu'aux conteneurs (collections et tableaux associatifs). 

Un **conteneur** est un ensemble générique d’objets 
(jeu de cartes, répertoire de fichiers, sac de perles, annuaire téléphonique, dictionnaire, etc.). 

L'efficacité du codage particulier d'une collection dépend de ses caractéristiques (ordre, doublons, etc.) 
et des opérations envisagées (parcours, ajout, suppression, concaténation, etc.). 


Conteneurs
---------------------------------------

Le package ``java.util`` propose les conteneurs suivants:
 
- ``ArrayList``, ``ArrayDeque``
- ``LinkedList``
- ``HashMap``, ``HashSet``
- ``TreeMap``, ``TreeSet``. 

(Cette liste n'est pas exhaustive).

On utilise le plus souvent ``ArrayList``, ``HashSet``, ``HashMap``. 

Ex.1. Conteneurs (10 min)
--------------------------------

- Parcourez la `documentation <http://docs.oracle.com/javase/7/docs/api/>`_ relative aux conteneurs citées. 

- Quelle est la différence entre ``Collection``, ``List``, ``Set``, ``Map``?

- Quelle est la différence entre ``ArrayList`` et ``LinkedList``?

- Quelle est la différence entre ``HashMap`` et ``TreeMap``?

.. acces aléatoire sur un tableau de taille variable, liste doublement chainée sans acces aléatoire

.. collection ordonnée - ensemble sans doublons - ensemble de paires clé-valeurs, les clés étant uniques -

.. table de hachage : acces constant en moyenne, pas d'ordre fixe - arbre rouge et noir : acces en O(log n) + ordre sur les clefs

L'interface ``Collection``
-----------------------------------

Les interfaces ``List`` et ``Set`` dérivent de ``Collection``, 
dérivant elle-même de ``Iterable``. 

Elle exige la présence, entre autres, des méthodes suivantes:  

- ``contains(Object o)``
- ``isEmpty()``
- ``toArray()``
- ``iterator()``

La méthode ``iterator()``, requise par ``Iterable``, renvoie un objet issu d'une classe implémentant 
l'interface ``Iterator``, avec lequel on peut parcourir la collection. 

L'interface ``Iterator``
-----------------------------------
  
Un objet de type ``Iterator`` offre un moyen de parcourir la collection à laquelle il réfère:  

.. code-block:: java 

	Collection<Integer> c = new ArrayList<Integer>(); 
	...
	for (Iterator<Integer> it = c.iterator(); it.hasNext(); ) {
	    System.out.println( it.next() ); 
	}

Depuis java 5, vous pouvez utiliser ce raccourci: 

.. code-block:: java 

	for (Integer i: c) {
	    System.out.println( i ); 
	}


Généricité
-----------------------------------

Comme vous le voyez, depuis java 5, les classes et les interfaces peuvent être paramétrées par un type. 

.. code-block:: java 

	public class StackByLinkedList<E> { 
	    ...
	}

Lors de la déclaration, le type formel est remplacé par le type effectif (non primitif). 

.. code-block:: java 

	StackByLinkedList<Integer> s = new StackByLinkedList<Integer>(); 

Le fonctionnement est le même que celui du passage de paramètres pour une méthode. 

L'interface ``Map``
-----------------------------------

L'interface ``Map`` décrit des objets qui mettent en correspondance des clés et des valeurs 
(à une clé étant associé au plus une valeur). 

En plus de la méthode ``get()`` renvoyant la valeur associée à une clé donnée, elle offre 
trois vues de type ``Collection``: 

- l'ensemble de clés est renvoyé par la méthode ``keySet()``, 
- la collection de valeurs est renvoyé par la méthode ``values()``, 
- l'ensemble de paires clé-valeur est renvoyé par la méthode ``entrySet()``.

L'interface ``Map.Entry``
-----------------------------------

.. code-block:: java 

	Map<Integer,String> annuaire = new HashMap<Integer,String>(); 

L'interface ``Map.Entry`` représente une paire clé-valeur: 
``getKey()`` retourne la clé, tandis que ``getValue()`` retourne la valeur. 

.. code-block:: java 

        Iterator<Map.Entry<Integer,String> > it;  
	for (it = annuaire.entrySet().iterator(); it.hasNext(); ) {
	    Map.Entry<Integer,String> e = it.next();
            System.out.println(e.getKey() + ": " + e.getValue());  
	}

.. code-block:: java 

	for (Map.Entry<Integer,String> e: annuaire.entrySet()) {
            System.out.println(e.getKey() + ": " + e.getValue());  
	}

Ex.2. Etudiants (30 min)
-----------------------------------

- Téléchargez la classe :download:`Etudiant <Etudiant.java>`.  

- Créez une classe ``GroupeEtudiant`` qui possède la méthode ``void ajout(Etudiant e)``. 
  Les étudiants sont stockés dans un ``ArrayList``. 

- Dans le ``main`` d'une classe ``ClientEtudiant``, ajoutez des objets de la
  classe ``Etudiant`` à la classe ``GroupeEtudiant``. 
  Au moins un de ces objets aura comme nom `Toto`. 

- Dans la classe ``ClientEtudiant``, rechercher `Toto`, en appelant la méthode
  ``List<Etudiant> recherche(String nom)`` codée dans ``GroupeEtudiant``. 
  Quelle est la complexité de la méthode de recherche ?


Ce qu'il faut retenir
------------------------------------

- Un conteneur est un ensemble générique d’objets.  

- On distingue deux types: 

  - ``Collection`` dont dérivent:

    - ``List`` (ensemble ordonné)
    - ``Set`` (ensemble sans doublon)

  - ``Map`` (ensemble de paires clés-valeurs) 

- ``Collection``, ``List``, ``Map``, ``Set`` sont des interfaces, seules les classes concrètes
  qui les implémentent sont instanciables. 

- Les objets de type ``Collection`` sont parcourus d'une manière uniforme
  (iterator ou boucle for étendue). Les objets de type ``Map`` offrent trois vues
  de type ``Collection``.    

Exceptions 
============================

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

Le développeur d'une classe peut indiquer aux clients qu'une méthode est susceptible de lever une exception
avec le mot-clé ``throws`` et peut effectivement **lever une exception** au moment voulu avec le mot-clé ``throw``. 

.. code-block:: java 
   :emphasize-lines: 3,5

        public int pop() throws Exception {
            if ( myNode == null ) 
                throw new Exception();
            else
                myNode = myNode.next(); 
        }


Créer sa propre classe d'exception
----------------------------------

.. code-block:: java 

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

Ex.5. Exceptions (10 min)
---------------------------

TODO interface queue

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

